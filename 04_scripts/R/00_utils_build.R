# =============================================================================
# R/00_utils_build.R
# =============================================================================
# Internal helpers shared by the three build_*() functions.
#
# Functions:
#   .apply_range_limit()      Flag implausible values in one column (value kept)
#   .apply_all_range_limits() Apply every RANGE_LIMITS entry to a data frame
#   .match_waves_by_date()    Match OsteoLaus visits to nearest CoLaus wave
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================


#' Flag and replace out-of-range values in one numeric column.
#'
#' Values outside [lo, hi] are flagged in a companion logical column
#' <col>_oob (TRUE = implausible) but the original value is retained.
#' Analysts decide per-analysis whether to exclude flagged rows.
#' If the column is absent from df the function is a silent no-op.
#'
#' @param df  Data frame.
#' @param col Column name (character scalar).
#' @param lo  Lower bound (numeric, inclusive).
#' @param hi  Upper bound (numeric, inclusive).
#' @return df with the column clipped and a <col>_oob flag column appended.
.apply_range_limit <- function(df, col, lo, hi) {
    if (!col %in% names(df)) return(df)
    
    flag_col <- paste0(col, "_oob")
    oob      <- !is.na(df[[col]]) & (df[[col]] < lo | df[[col]] > hi)
    n_oob    <- sum(oob)
    
    if (n_oob > 0)
        cli::cli_warn(
            "{n_oob} value(s) in {.col {col}} outside [{lo}, {hi}] \
       flagged in {.col {flag_col}} (values retained)."
        )
    
    # Flag implausible values but do NOT replace with NA — the original value
    # is kept so analysts can inspect and decide per-analysis how to handle it.
    df[[flag_col]] <- oob
    df
}


#' Apply every entry in RANGE_LIMITS to a data frame.
#'
#' Iterates over the named list RANGE_LIMITS (defined in 00_constants.R) and
#' calls .apply_range_limit() for each entry whose column is present in df.
#' Columns absent from df are silently skipped.
#'
#' @param df Data frame, typically the visits grain.
#' @return df with *_oob flag columns appended; original values are retained.
.apply_all_range_limits <- function(df) {
    for (col in names(RANGE_LIMITS)) {
        df <- .apply_range_limit(
            df  = df,
            col = col,
            lo  = RANGE_LIMITS[[col]][["lo"]],
            hi  = RANGE_LIMITS[[col]][["hi"]]
        )
    }
    df
}


#' Match each OsteoLaus visit to the nearest CoLaus wave by exam date.
#'
#' For each pt x osteo_wave combination, finds the CoLaus wave (F1-F3; Baseline
#' excluded) whose exam date is closest to the OsteoLaus exam date by absolute
#' difference. Participants with no CoLaus data at all receive NA for
#' colaus_wave and gap_days.
#'
#' CoLaus Baseline is excluded from the input before this function is called
#' (enforced in build_exposures()). It was collected 2003-2008, predating all
#' OsteoLaus visits by at least two years and containing no FFQ data.
#'
#' @param osteo_visits  Tibble: pt, osteo_wave, osteo_date (Date).
#' @param colaus_dates  Tibble: pt, colaus_wave, colaus_date (Date).
#'   Must already exclude CoLaus Baseline rows.
#' @param gap_warn_days Integer. Emit a warning for any matched pair where
#'   gap_days exceeds this value. Default 912 (~2.5 years).
#' @return Tibble with columns: pt, osteo_wave, colaus_wave, gap_days.
.match_waves_by_date <- function(osteo_visits,
                                 colaus_dates,
                                 gap_warn_days = 912L) {
    
    # Participants present in OsteoLaus but absent from CoLaus
    osteo_only <- setdiff(
        unique(osteo_visits$pt),
        unique(colaus_dates$pt)
    )
    if (length(osteo_only) > 0)
        cli::cli_inform(
            "{length(osteo_only)} OsteoLaus participant(s) have no CoLaus data — \
       exposure will be NA for all their visits."
        )
    
    # Cross-join then slice the minimum gap row per pt x osteo_wave
    matched <- osteo_visits |>
        dplyr::left_join(
            colaus_dates,
            by           = "pt",
            relationship = "many-to-many"
        ) |>
        dplyr::mutate(
            gap_days = abs(as.numeric(osteo_date - colaus_date))
        ) |>
        dplyr::filter(!is.na(gap_days)) |>
        dplyr::group_by(pt, osteo_wave) |>
        dplyr::slice_min(gap_days, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
    
    # Rows for OsteoLaus-only participants (colaus_wave = NA)
    unmatched <- osteo_visits |>
        dplyr::filter(pt %in% osteo_only) |>
        dplyr::mutate(
            colaus_wave = NA_character_,
            colaus_date = as.Date(NA),
            gap_days    = NA_real_
        )
    
    out <- dplyr::bind_rows(matched, unmatched)
    
    # Diagnostic summary table
    gap_summary <- out |>
        dplyr::filter(!is.na(gap_days)) |>
        dplyr::group_by(osteo_wave, colaus_wave) |>
        dplyr::summarise(
            n          = dplyr::n(),
            median_gap = round(median(gap_days), 0),
            max_gap    = max(gap_days),
            n_over_thr = sum(gap_days > gap_warn_days),
            .groups    = "drop"
        )
    
    cli::cli_inform(c(
        "*" = "Wave-match summary (OsteoLaus \u2192 nearest CoLaus):",
        "*" = paste(capture.output(print(gap_summary, n = Inf)), collapse = "\n")
    ))
    
    # Warn on any osteo_wave -> colaus_wave pairs with large gaps
    over_thr <- dplyr::filter(gap_summary, n_over_thr > 0)
    if (nrow(over_thr) > 0)
        cli::cli_warn(c(
            "Some matched pairs exceed {gap_warn_days} days:",
            glue::glue_data(
                over_thr,
                "  {osteo_wave} \u2192 {colaus_wave}: {n_over_thr}/{n} visits"
            )
        ))
    
    dplyr::select(out, pt, osteo_wave, colaus_wave, gap_days)
}