# =============================================================================
# R/utils_cohort.R
# =============================================================================
# Pure helper functions for cohort/wave metadata lookups.
#
# Depends on: R/constants.R (COHORT_META)

source("04_scripts/R/00_constants.R")

# -----------------------------------------------------------------------------
# Internal guards (not exported)
# -----------------------------------------------------------------------------

#' Assert that a cohort name is known.
#' @param cohort Character scalar.
#' @return Invisible NULL; raises an error if unknown.
.assert_known_cohort <- function(cohort) {
    valid <- names(COHORT_META)
    if (!cohort %in% valid)
        cli::cli_abort(
            "Unknown cohort {.val {cohort}}. Must be one of: {.val {valid}}."
        )
}

#' Assert that a wave label is known for a given cohort.
#' @param wave   Character scalar.
#' @param cohort Character scalar; must already be valid.
#' @return Invisible NULL; raises an error if unknown.
.assert_known_wave <- function(wave, cohort) {
    valid <- names(COHORT_META[[cohort]][["wave_prefix"]])
    if (!wave %in% valid)
        cli::cli_abort(
            "Unknown wave {.val {wave}} for cohort {.val {cohort}}. \\
       Must be one of: {.val {valid}}."
        )
}

# -----------------------------------------------------------------------------
# Public helpers
# -----------------------------------------------------------------------------
#' Resolve the actual column name for a base variable in a given wave.
#'
#' Prepends the cohort- and wave-specific CSV prefix to the base name.
#' e.g. resolve_col("age",       "F1",       "CoLaus")    -> "F1age"
#'      resolve_col("SCAN_date", "Baseline", "OsteoLaus") -> "Bsl_SCAN_date"
#'      resolve_col("Age",       "V2",       "OsteoLaus") -> "V2_Age"
#'
#' @param base   Base variable name, e.g. "datexam", "age", "SCAN_date".
#' @param wave   Pipeline wave label, e.g. "Baseline", "F1", "V2".
#' @param cohort "CoLaus" or "OsteoLaus".
#' @return Character scalar: the fully prefixed column name.
resolve_col <- function(base, wave, cohort) {
    .assert_known_cohort(cohort)
    .assert_known_wave(wave, cohort)
    prefix <- COHORT_META[[cohort]][["wave_prefix"]][[wave]]
    paste0(prefix, base)
}


#' Look up the canonical wave_num for a wave label.
#'
#' Centralises the wave -> integer mapping so callers never supply it manually.
#'
#' @param wave   Pipeline wave label, e.g. "Baseline", "F1", "V2".
#' @param cohort "CoLaus" or "OsteoLaus".
#' @return Integer scalar.
wave_to_num <- function(wave, cohort) {
    .assert_known_cohort(cohort)
    .assert_known_wave(wave, cohort)
    COHORT_META[[cohort]][["wave_num"]][[wave]]
}