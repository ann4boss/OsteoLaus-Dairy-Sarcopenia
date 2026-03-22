# =============================================================================
# R/strip_prefix.R
# =============================================================================
# Helpers that strip wave prefixes from column names after import.
# Called at the top of each harmonise_*() function so all downstream code
# works against consistent base names regardless of wave.
#
# Depends on: R/constants.R (COHORT_META)
#             R/utils_cohort.R (.assert_known_cohort, .assert_known_wave)

source("04_scripts/R/00_constants.R")
source("04_scripts/R/00_utils_cohort.R")

#' Strip the wave prefix from every column in df that starts with it.
#'
#' Looks up the prefix for the given cohort/wave from COHORT_META. If the
#' prefix is empty (CoLaus Baseline) the data frame is returned unchanged.
#' Any column whose name begins with the prefix is renamed to the suffix that
#' remains after removing it — no explicit list of base names required.
#'
#' e.g. strip_wave_prefix(df, "F1",       "CoLaus")    renames "F1age"      -> "age"
#'      strip_wave_prefix(df, "Baseline", "OsteoLaus") renames "Bsl_Age"    -> "Age"
#'      strip_wave_prefix(df, "Baseline", "CoLaus")    no-op (empty prefix)
#'
#' @param df     Data frame straight from import_wave().
#' @param wave   Pipeline wave label, e.g. "F1", "V2", "Baseline".
#' @param cohort "CoLaus" or "OsteoLaus".
#' @return df with wave prefix stripped from all affected column names.
strip_wave_prefix <- function(df, wave, cohort) {
    .assert_known_cohort(cohort)
    .assert_known_wave(wave, cohort)
    
    prefix <- COHORT_META[[cohort]][["wave_prefix"]][[wave]]
    if (nchar(prefix) == 0) return(df)
    
    .strip_prefix_literal(df, prefix)
}

#' Strip an arbitrary literal prefix from every column name that begins with it.
#'
#' A generalised version used for cases where the prefix is not wave-derived.
#' Currently also used to remove the extra H_ prefix from DXA column names at
#' OsteoLaus V5 (after the wave prefix has already been stripped).
#'
#' @param df     Data frame.
#' @param prefix Literal string prefix to strip, e.g. "H_".
#' @return df with prefix stripped from all matching column names.
#' @export
strip_prefix_literal <- function(df, prefix) {
    .strip_prefix_literal(df, prefix)
}

# -----------------------------------------------------------------------------
# Internal implementation
# -----------------------------------------------------------------------------

#' Core prefix-stripping logic shared by both public functions.
#' @param df     Data frame.
#' @param prefix Literal string prefix to strip.
#' @return df with prefix stripped from all matching column names.
.strip_prefix_literal <- function(df, prefix) {
    cols      <- names(df)
    has_pfx   <- startsWith(cols, prefix)
    new_names <- ifelse(has_pfx, substring(cols, nchar(prefix) + 1L), cols)
    
    dupes <- new_names[duplicated(new_names) & has_pfx]
    if (length(dupes) > 0)
        cli::cli_warn(
            "strip_prefix_literal('{prefix}'): duplicate column names after stripping: \\
       {.val {dupes}}. Check source data for naming conflicts."
        )
    
    stats::setNames(df, new_names)
}