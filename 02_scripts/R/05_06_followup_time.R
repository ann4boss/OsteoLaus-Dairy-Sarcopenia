# =============================================================================
# R/followup_time.R
# Median follow-up time and range by time point (T1–T4) and total follow-up.
#
# Entry points
# ------------
#   summarise_followup(data, ...)
#     Single dataset (mids or plain df). Returns a tibble.
#
#   summarise_followup_all(analysis, ...)
#     Takes the list returned by run_exclusions() and runs summarise_followup()
#     for $data_shared plus each element of $data_outcome (or $mids for mice).
#     Returns a combined tibble with a "dataset" column.
#
# Follow-up is measured in years via time_since_baseline (derived in
# 02_05_02_derive_visit_time.R). time_since_baseline at T1 is 0 by definition.
# Total follow-up per participant = their maximum time_since_baseline.
#
# MICE: per-imputation summaries are pooled by averaging medians/quantiles
# across imputations. time_since_baseline is derived from exam_date (observed,
# not imputed), so values are identical across imputations in practice.
# =============================================================================


# ── Internal helpers ───────────────────────────────────────────────────────────

.fu_to_long <- function(data) {
    if (inherits(data, "mids")) {
        mice::complete(data, action = "long", include = FALSE) |>
            tibble::as_tibble()
    } else if (".imp" %in% names(data)) {
        tibble::as_tibble(data)
    } else {
        tibble::as_tibble(data) |> dplyr::mutate(.imp = 0L)
    }
}

# Summarise follow-up per time_point for one .imp slice
.fu_per_visit_slice <- function(df, time_col, visit_col) {
    df |>
        dplyr::filter(!is.na(.data[[time_col]]), !is.na(.data[[visit_col]])) |>
        dplyr::group_by(visit = as.character(.data[[visit_col]])) |>
        dplyr::summarise(
            n      = dplyr::n(),
            median = stats::median(.data[[time_col]], na.rm = TRUE),
            q25    = stats::quantile(.data[[time_col]], 0.25, na.rm = TRUE),
            q75    = stats::quantile(.data[[time_col]], 0.75, na.rm = TRUE),
            min    = min(.data[[time_col]], na.rm = TRUE),
            max    = max(.data[[time_col]], na.rm = TRUE),
            .groups = "drop"
        )
}

# Total follow-up per participant = their maximum time_since_baseline,
# then summarise across participants
.fu_total_slice <- function(df, time_col, pt_col) {
    df |>
        dplyr::filter(!is.na(.data[[time_col]])) |>
        dplyr::group_by(.data[[pt_col]]) |>
        dplyr::summarise(
            total_fu = max(.data[[time_col]], na.rm = TRUE),
            .groups  = "drop"
        ) |>
        dplyr::summarise(
            n      = dplyr::n(),
            median = stats::median(total_fu, na.rm = TRUE),
            q25    = stats::quantile(total_fu, 0.25, na.rm = TRUE),
            q75    = stats::quantile(total_fu, 0.75, na.rm = TRUE),
            min    = min(total_fu, na.rm = TRUE),
            max    = max(total_fu, na.rm = TRUE)
        ) |>
        dplyr::mutate(visit = "Total")
}

# Pool per-imputation rows: average numeric summaries, round n
.pool_fu_slices <- function(slices) {
    dplyr::bind_rows(slices) |>
        dplyr::group_by(visit) |>
        dplyr::summarise(
            n      = round(mean(n)),
            median = mean(median),
            q25    = mean(q25),
            q75    = mean(q75),
            min    = mean(min),
            max    = mean(max),
            .groups = "drop"
        )
}

# Format a single row as "median [min–max] (IQR: q25–q75)"
.fmt_fu <- function(median, min, max, q25, q75, digits = 2) {
    fmt <- function(x) formatC(round(x, digits), format = "f", digits = digits)
    paste0(fmt(median), " [", fmt(min), "–", fmt(max), "]")
}


# ── Public: single dataset ─────────────────────────────────────────────────────

#' Summarise median follow-up time and range by time point and overall.
#'
#' @param data        mids object or plain data frame.
#' @param time_col    Column name for time since baseline (years).
#' @param visit_col   Column name for time point (factor T1–T4).
#' @param pt_col      Participant ID column.
#' @param digits      Decimal places for formatted string.
#'
#' @return Tibble with columns: visit, n, median, q25, q75, min, max,
#'   formatted (e.g. "5.23 [4.10–6.80]"), and is_pooled (logical).
summarise_followup <- function(data,
                               time_col  = "time_since_baseline",
                               visit_col = "time_point",
                               pt_col    = "pt",
                               digits    = 2L) {

    long     <- .fu_to_long(data)
    is_mids  <- inherits(data, "mids")
    imp_ids  <- sort(unique(long$.imp))

    if (is_mids) {
        cli::cli_inform("Pooling follow-up stats across {length(imp_ids)} imputation(s).")

        per_visit_slices <- lapply(imp_ids, function(i) {
            sl <- dplyr::filter(long, .imp == i)
            .fu_per_visit_slice(sl, time_col, visit_col)
        })
        total_slices <- lapply(imp_ids, function(i) {
            sl <- dplyr::filter(long, .imp == i)
            .fu_total_slice(sl, time_col, pt_col)
        })

        per_visit <- .pool_fu_slices(per_visit_slices)
        total_row <- .pool_fu_slices(total_slices)

    } else {
        per_visit <- .fu_per_visit_slice(long, time_col, visit_col)
        total_row <- .fu_total_slice(long, time_col, pt_col)
    }

    result <- dplyr::bind_rows(per_visit, total_row) |>
        dplyr::mutate(
            visit     = factor(visit, levels = c("T1", "T2", "T3", "T4", "Total"),
                               ordered = FALSE),
            formatted = purrr::pmap_chr(
                list(median, min, max, q25, q75),
                function(med, mn, mx, q1, q3) .fmt_fu(med, mn, mx, q1, q3, digits)
            ),
            is_pooled = is_mids
        ) |>
        dplyr::arrange(visit)

    result
}


# ── Public: full analysis object ──────────────────────────────────────────────

#' Run summarise_followup() for the shared dataset and each outcome-specific
#' dataset from a run_exclusions() result.
#'
#' @param analysis    List returned by run_exclusions() (CC or MICE route).
#'   Must have $data_shared and either $data or $mids (named list per outcome).
#' @param outcomes    Character vector of outcome names to include.
#'   Defaults to all elements of $data/$mids.
#' @param time_col,visit_col,pt_col,digits  Passed to summarise_followup().
#'
#' @return Tibble with a leading "dataset" column followed by the columns
#'   returned by summarise_followup().
summarise_followup_all <- function(analysis,
                                   outcomes  = NULL,
                                   time_col  = "time_since_baseline",
                                   visit_col = "time_point",
                                   pt_col    = "pt",
                                   digits    = 2L) {

    # Resolve the per-outcome list ($mids for MICE route, $data for CC route)
    outcome_list <- if (!is.null(analysis$mids)) analysis$mids else analysis$data
    if (is.null(outcome_list))
        cli::cli_abort("analysis must have a $mids or $data element.")

    if (is.null(outcomes)) {
        outcomes <- names(outcome_list)
    } else {
        missing_oc <- setdiff(outcomes, names(outcome_list))
        if (length(missing_oc) > 0L)
            cli::cli_warn("Outcomes not found in analysis: {.val {missing_oc}}")
        outcomes <- intersect(outcomes, names(outcome_list))
    }

    datasets <- c(
        list(Shared = analysis$data_shared),
        outcome_list[outcomes]
    )

    cli::cli_h1("Follow-up time summary")

    results <- purrr::imap(datasets, function(dat, nm) {
        cli::cli_h2("{nm}")
        if (is.null(dat)) {
            cli::cli_warn("Dataset {.val {nm}} is NULL — skipping.")
            return(NULL)
        }
        tryCatch(
            summarise_followup(dat, time_col, visit_col, pt_col, digits),
            error = function(e) {
                cli::cli_warn("Error summarising {.val {nm}}: {conditionMessage(e)}")
                NULL
            }
        )
    })

    results <- purrr::compact(results)

    dplyr::bind_rows(
        purrr::imap(results, ~ dplyr::mutate(.x, dataset = .y)),
        .id = NULL
    ) |>
        dplyr::relocate(dataset) |>
        dplyr::arrange(dataset, visit)
}


# ── Printing helper ────────────────────────────────────────────────────────────

#' Print a compact follow-up summary table to the console.
#'
#' @param fu_tbl  Tibble returned by summarise_followup() or
#'   summarise_followup_all().
print_followup_table <- function(fu_tbl) {

    has_dataset <- "dataset" %in% names(fu_tbl)

    tbl <- fu_tbl |>
        dplyr::mutate(
            iqr = paste0("(", formatC(round(q25, 2), format = "f", digits = 2),
                         "–",
                         formatC(round(q75, 2), format = "f", digits = 2), ")")
        ) |>
        dplyr::select(
            dplyr::any_of("dataset"),
            visit, n,
            `Median [Min–Max]` = formatted,
            IQR = iqr,
            is_pooled
        )

    if (has_dataset) {
        datasets <- unique(tbl$dataset)
        for (ds in datasets) {
            cli::cli_h2("{ds}{if (any(tbl$is_pooled[tbl$dataset == ds])) ' (pooled across imputations)' else ''}")
            print(
                dplyr::filter(tbl, dataset == ds) |>
                    dplyr::select(-dataset, -is_pooled),
                n = Inf
            )
        }
    } else {
        pooled_lbl <- if (any(tbl$is_pooled)) " (pooled across imputations)" else ""
        cli::cli_h2("Follow-up time{pooled_lbl}")
        print(dplyr::select(tbl, -is_pooled), n = Inf)
    }

    invisible(fu_tbl)
}
