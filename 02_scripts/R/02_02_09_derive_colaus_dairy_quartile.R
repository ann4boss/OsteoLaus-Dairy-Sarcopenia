# =============================================================================
# R/02_02_09_derive_colaus_dairy_quartile.R
# =============================================================================
# Derives two quartile variables from dairy_total_gday_cumavg.
#
# Functions:
#   derive_dairy_quartile()      — adds dairy_quartile_baseline/_overall
#   export_dairy_quartile_cuts() — recomputes and exports the same cut-points
#                                   to CSV for inspection (CC or MICE data)
#
#   dairy_quartile_baseline
#     Quartile boundaries are computed from the F1 distribution (first measurement time point) of
#     dairy_total_gday_cumavg across all participants. The same cut-points are then
#     applied to every visit, so each participant's intake at F1–F3 is placed
#     into the quartile defined by the Baseline population distribution.
#     This is the primary variable for analysis (avoids reverse causation from
#     using later visits to define the reference distribution).
#
#   dairy_quartile_overall
#     Quartile boundaries are computed from dairy_total_gday_cumavg pooled across
#     all visits and all participants. Each row is then classified into one
#     of these pooled quartiles.
#     Intended as a sensitivity variable.
#
# Both variables are ordered factors:
#   Q1 (lowest) < Q2 < Q3 < Q4 (highest)
#
# Requires dairy_total_gday_cumavg to be present (output of derive_dairy()).
# =============================================================================

# -----------------------------------------------------------------------------
# derive_dairy_quartile()
# -----------------------------------------------------------------------------
#' Derive dairy intake quartile variables for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after derive_dairy() has been applied.
#'   Must contain dairy_total_gday_cumavg, pt, and .visit.
#' @return df with dairy_quartile_baseline and dairy_quartile_overall
#'   (both ordered factors Q1–Q4) added.
derive_dairy_quartile <- function(df) {
    
    # ── Check required columns -----------------------------------------------
    required_cols <- c("pt", ".visit", "dairy_total_gday_cumavg")
    missing_cols  <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_dairy_quartile: missing required columns: {.val {missing_cols}}. \\
             Dairy quartiles will not be derived."
        )
        return(df)
    }
    
    n_rows <- nrow(df)
    
    # ── Helper: classify a numeric vector using pre-computed breaks ----------
    .to_quartile <- function(x, breaks) {
        cut(
            x,
            breaks = breaks,
            labels = c("Q1", "Q2", "Q3", "Q4"),
            include.lowest = TRUE,
            ordered_result = TRUE
        )
    }
    
    # ── Baseline quartile ---------------------------------------------------
    # Compute cut-points from Baseline rows only, then apply to all visits.
    baseline_values <- df |>
        dplyr::filter(.visit == "F1", !is.na(dairy_total_gday_cumavg)) |>
        dplyr::pull(dairy_total_gday_cumavg)
    
    if (length(baseline_values) < 4) {
        cli::cli_warn(
            "derive_dairy_quartile: fewer than 4 non-missing Baseline rows — \\
             {.col dairy_quartile_baseline} will not be derived."
        )
        df$dairy_quartile_baseline <- NA
    } else {
        baseline_breaks <- stats::quantile(
            baseline_values,
            probs = c(0, 0.25, 0.50, 0.75, 1),
            na.rm = TRUE
        )
        
        df <- df |>
            dplyr::mutate(
                dairy_quartile_baseline = .to_quartile(dairy_total_gday_cumavg, baseline_breaks)
            )
    }
    
    # ── Overall quartile ----------------------------------------------------
    # Compute cut-points pooled across all visits and participants.
    overall_values <- df$dairy_total_gday_cumavg[!is.na(df$dairy_total_gday_cumavg)]
    
    if (length(overall_values) < 4) {
        cli::cli_warn(
            "derive_dairy_quartile: fewer than 4 non-missing rows overall — \\
             {.col dairy_quartile_overall} will not be derived."
        )
        df$dairy_quartile_overall <- NA
    } else {
        overall_breaks <- stats::quantile(
            overall_values,
            probs = c(0, 0.25, 0.50, 0.75, 1),
            na.rm = TRUE
        )
        
        df <- df |>
            dplyr::mutate(
                dairy_quartile_overall = .to_quartile(dairy_total_gday_cumavg, overall_breaks)
            )
    }
    
    # ── Summary --------------------------------------------------------------
    cli::cli_h2("Derive Dairy Quartiles")
    
    # Baseline cut-points
    if (exists("baseline_breaks")) {
        cli::cli_inform(c(
            "i" = "Baseline quartile cut-points (g/day, n = {length(baseline_values)}):",
            "*" = "Q1 | Q2 boundary : {round(baseline_breaks[2], 1)}",
            "*" = "Q2 | Q3 boundary : {round(baseline_breaks[3], 1)}",
            "*" = "Q3 | Q4 boundary : {round(baseline_breaks[4], 1)}"
        ))
    }
    
    # Overall cut-points
    if (exists("overall_breaks")) {
        cli::cli_inform(c(
            "i" = "Overall quartile cut-points (g/day, n = {length(overall_values)}):",
            "*" = "Q1 | Q2 boundary : {round(overall_breaks[2], 1)}",
            "*" = "Q2 | Q3 boundary : {round(overall_breaks[3], 1)}",
            "*" = "Q3 | Q4 boundary : {round(overall_breaks[4], 1)}"
        ))
    }
    
    # Distribution per visit for both variables
    for (var in c("dairy_quartile_baseline", "dairy_quartile_overall")) {
        if (!var %in% names(df)) next
        dist <- df |>
            dplyr::count(.visit, !!rlang::sym(var), .drop = FALSE) |>
            dplyr::group_by(.visit) |>
            dplyr::mutate(pct = round(n / sum(n) * 100, 1)) |>
            dplyr::ungroup()
        cli::cli_inform(c("i" = "Distribution of {.col {var}} by visit:"))
        cli::cli_inform(paste(capture.output(print(dist, n = Inf)), collapse = "\n"))
    }
    
    n_derived_baseline <- sum(!is.na(df$dairy_quartile_baseline))
    n_derived_overall  <- sum(!is.na(df$dairy_quartile_overall))
    
    cli::cli_inform(c(
        "v" = "Dairy quartiles derived.",
        "*" = "dairy_quartile_baseline: {n_derived_baseline} / {n_rows} rows non-missing",
        "*" = "dairy_quartile_overall : {n_derived_overall} / {n_rows} rows non-missing"
    ))
    
    # ── Comparison: baseline vs overall classification -----------------------
    if ("dairy_quartile_baseline" %in% names(df) &&
        "dairy_quartile_overall"  %in% names(df)) {
        
        both_present <- !is.na(df$dairy_quartile_baseline) & !is.na(df$dairy_quartile_overall)
        n_both       <- sum(both_present)
        n_differ     <- sum(
            both_present &
                df$dairy_quartile_baseline != df$dairy_quartile_overall
        )
        n_agree <- n_both - n_differ
        
        cli::cli_inform(c(
            "i" = "Baseline vs overall quartile agreement (rows with both non-missing, n = {n_both}):",
            "*" = "Same quartile     : {n_agree} ({round(n_agree / n_both * 100, 1)}%)",
            "*" = "Different quartile: {n_differ} ({round(n_differ / n_both * 100, 1)}%)"
        ))
        
        # Cross-tabulation of disagreements (baseline → overall)
        crosstab <- df |>
            dplyr::filter(both_present & dairy_quartile_baseline != dairy_quartile_overall) |>
            dplyr::count(dairy_quartile_baseline, dairy_quartile_overall) |>
            dplyr::arrange(dairy_quartile_baseline, dairy_quartile_overall)
        
        if (nrow(crosstab) > 0) {
            cli::cli_inform("  Cross-tabulation of differing classifications (baseline → overall):")
            cli::cli_inform(paste(capture.output(print(crosstab, n = Inf)), collapse = "\n"))
        }
        
        # Per-visit breakdown of disagreement rate
        visit_agree <- df |>
            dplyr::filter(both_present) |>
            dplyr::group_by(.visit) |>
            dplyr::summarise(
                n          = dplyr::n(),
                n_differ   = sum(dairy_quartile_baseline != dairy_quartile_overall),
                pct_differ = round(n_differ / n * 100, 1),
                .groups    = "drop"
            )
        
        cli::cli_inform("  Disagreement rate by visit:")
        cli::cli_inform(paste(capture.output(print(visit_agree, n = Inf)), collapse = "\n"))
    }
    
    return(df)
}

# =============================================================================
# Export: dairy quartile cut-points to CSV
# =============================================================================
#
# Recomputes the same breaks used inside derive_dairy_quartile() from
# already-derived datasets so the applied cut-points can be inspected.
# Works on the MERGED datasets (time_point = "T1" maps to CoLaus F1).
#
# For mids objects the breaks are averaged over all m complete datasets.
# =============================================================================

# -----------------------------------------------------------------------------
# export_dairy_quartile_cuts()
# -----------------------------------------------------------------------------
#' Recompute and export dairy quartile cut-points to CSV.
#'
#' @param data_list      Named list of datasets (data.frame or mids objects,
#'   e.g. list("CC" = cc_merged_derived, "MICE" = mice_merged_derived)).
#' @param out_file       Output CSV path. Parent directory is created if needed.
#' @param visit_col      Name of the visit/time-point column. Default "time_point".
#' @param baseline_visit Value of `visit_col` identifying the baseline visit
#'   used for the baseline quartile cut-points. Default "T1".
#' @return `out_file`, invisibly. Writes a CSV with one row per
#'   dataset x quartile type x boundary.
export_dairy_quartile_cuts <- function(data_list,
                                       out_file,
                                       visit_col      = "time_point",
                                       baseline_visit = "T1") {

    # Compute breaks from a single plain data frame.
    .breaks_from_df <- function(df) {
        baseline_vals <- df |>
            dplyr::filter(.data[[visit_col]] == baseline_visit,
                          !is.na(dairy_total_gday_cumavg)) |>
            dplyr::pull(dairy_total_gday_cumavg)

        overall_vals <- df$dairy_total_gday_cumavg[!is.na(df$dairy_total_gday_cumavg)]

        list(
            baseline = if (length(baseline_vals) >= 4)
                stats::quantile(baseline_vals,
                                probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)
            else NULL,
            overall  = if (length(overall_vals) >= 4)
                stats::quantile(overall_vals,
                                probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)
            else NULL
        )
    }

    # Compute breaks for one dataset entry (data.frame or mids).
    .cuts_one <- function(data, nm) {
        if (inherits(data, "mids")) {
            m   <- data$m
            all <- purrr::map(seq_len(m), function(i)
                .breaks_from_df(mice::complete(data, i)))

            avg_breaks <- list(
                baseline = {
                    lst <- purrr::map(all, "baseline") |> purrr::compact()
                    if (length(lst)) rowMeans(do.call(cbind, lst)) else NULL
                },
                overall = {
                    lst <- purrr::map(all, "overall") |> purrr::compact()
                    if (length(lst)) rowMeans(do.call(cbind, lst)) else NULL
                }
            )
            source_note <- sprintf("MICE (m=%d)", m)
            breaks      <- avg_breaks
        } else {
            breaks      <- .breaks_from_df(data)
            source_note <- "CC"
        }

        purrr::imap_dfr(breaks, function(b, type_nm) {
            if (is.null(b)) return(tibble::tibble())
            tibble::tibble(
                dataset      = nm,
                source       = source_note,
                quartile_var = type_nm,
                boundary     = names(b),
                cut_g_day    = round(unname(b), 1)
            )
        }) |>
            dplyr::filter(!.data$boundary %in% c("0%", "100%"))
    }

    result <- purrr::imap_dfr(data_list, .cuts_one)

    dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(result, out_file)
    cli::cli_inform(c("v" = "Dairy quartile cut-points saved to {.path {out_file}}"))
    invisible(out_file)
}