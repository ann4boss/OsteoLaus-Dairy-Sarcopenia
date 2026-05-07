# =============================================================================
# R/derive_colaus_dairy_quartile.R
# =============================================================================
# Derives two quartile variables from dairy_total_gday:
#
#   dairy_quartile_baseline
#     Quartile boundaries are computed from the F1 distribution (first measurement time point) of
#     dairy_total_gday across all participants. The same cut-points are then
#     applied to every visit, so each participant's intake at F1–F3 is placed
#     into the quartile defined by the Baseline population distribution.
#     This is the primary variable for analysis (avoids reverse causation from
#     using later visits to define the reference distribution).
#
#   dairy_quartile_overall
#     Quartile boundaries are computed from dairy_total_gday pooled across
#     all visits and all participants. Each row is then classified into one
#     of these pooled quartiles.
#     Intended as a sensitivity variable.
#
# Both variables are ordered factors:
#   Q1 (lowest) < Q2 < Q3 < Q4 (highest)
#
# Requires dairy_total_gday to be present (output of derive_dairy()).
# =============================================================================

#' Derive dairy intake quartile variables for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after derive_dairy() has been applied.
#'   Must contain dairy_total_gday, pt, and .visit.
#' @return df with dairy_quartile_baseline and dairy_quartile_overall
#'   (both ordered factors Q1–Q4) added.
derive_dairy_quartile <- function(df) {
    
    # ── Check required columns -----------------------------------------------
    required_cols <- c("pt", ".visit", "dairy_total_gday")
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
        dplyr::filter(.visit == "F1", !is.na(dairy_total_gday)) |>
        dplyr::pull(dairy_total_gday)
    
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
                dairy_quartile_baseline = .to_quartile(dairy_total_gday, baseline_breaks)
            )
    }
    
    # ── Overall quartile ----------------------------------------------------
    # Compute cut-points pooled across all visits and participants.
    overall_values <- df$dairy_total_gday[!is.na(df$dairy_total_gday)]
    
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
                dairy_quartile_overall = .to_quartile(dairy_total_gday, overall_breaks)
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