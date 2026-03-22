# =============================================================================
# R/validate_wave.R
# =============================================================================
# Validation checks for an imported wave tibble.
# Accepts an already-loaded data frame, performs no I/O of its own.
#
# Depends on: R/constants.R (DATE_REGEX, COHORT_META)
#             R/utils_cohort.R (resolve_col)

source("04_scripts/R/00_constants.R")
source("04_scripts/R/00_utils_cohort.R")

#' Validate a raw wave tibble.
#'
#' Runs all fast-fail checks on an imported wave. Errors stop the pipeline
#' immediately; warnings allow it to continue but are always surfaced.
#'
#' Checks performed:
#'   1. Primary key column pt exists.
#'   2. No NA values in pt (unidentifiable rows).
#'   3. No duplicate pt values within the wave.
#'   4. Expected date column exists.
#'   5. Date values match DDMonYYYY format.
#'
#' @param df     Tibble returned by import_wave() before validation.
#' @param cohort "CoLaus" or "OsteoLaus".
#' @param wave   Pipeline wave label, e.g. "Baseline", "F1", "V2".
#' @return df invisibly if all checks pass; otherwise aborts or warns with details.
validate_wave <- function(df, cohort, wave) {
    
    # 1. Primary key column must exist.
    if (!"pt" %in% names(df))
        cli::cli_abort("[{cohort} {wave}] Column {.col pt} not found.")
    
    # 2. Missing primary keys are unidentifiable — error, do not silently carry forward.
    n_na_pt <- sum(is.na(df$pt))
    if (n_na_pt > 0)
        cli::cli_abort(
            "[{cohort} {wave}] {n_na_pt} row(s) with missing {.col pt}. \\
       All participants must be identifiable."
        )
    
    # 3. No duplicate participants within a wave.
    dup_vals <- df$pt[duplicated(df$pt)]
    if (length(dup_vals) > 0)
        cli::cli_abort(
            "[{cohort} {wave}] {length(dup_vals)} duplicate {.col pt} value(s): \\
       {.val {head(dup_vals, 5)}}{if (length(dup_vals) > 5) ' \u2026' else ''}. \\
       This may cause problems downstream."
        )
    
    # 4. Date column must exist.
    date_base <- COHORT_META[[cohort]][["date_col_base"]]
    date_col  <- resolve_col(date_base, wave, cohort)
    if (!date_col %in% names(df))
        cli::cli_abort(
            "[{cohort} {wave}] Expected date column {.col {date_col}} not found. \\
       Columns present: {.val {names(df)}}."
        )
    
    # 5. Date values match expected format; report up to 5 offending values.
    bad_dates <- df[[date_col]][
        !is.na(df[[date_col]]) & !stringr::str_detect(df[[date_col]], DATE_REGEX)
    ]
    if (length(bad_dates) > 0)
        cli::cli_warn(
            "[{cohort} {wave}] {length(bad_dates)} value(s) in {.col {date_col}} \\
       do not match DDMonYYYY (e.g. {.val 21mar2025}). \\
       First offenders: {.val {head(bad_dates, 5)}}{if (length(bad_dates) > 5) ' \u2026' else ''}."
        )
    
    invisible(df)
}