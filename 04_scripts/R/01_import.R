# =============================================================================
# R/import.R
# =============================================================================
# Imports a single raw CSV wave file.
# All validation is delegated to validate_wave().
#
# Depends on: R/constants.R    (COHORT_META)
#             R/utils_cohort.R (wave_to_num, .assert_known_cohort,
#                               .assert_known_wave)
#             R/validate_wave.R (validate_wave)

source("04_scripts/R/00_constants.R")
source("04_scripts/R/00_utils_cohort.R")
source("04_scripts/R/01_validate_wave.R")

#' Import a single raw CSV wave.
#'
#' Reads a semicolon-delimited CSV file produced by SAS, coerces all columns
#' to character (type coercion is deferred to downstream targets), attaches
#' wave-level metadata columns, and runs all fast-fail validation checks.
#'
#' @param path   File path tracked by a \code{format = "file"} target.
#' @param cohort "CoLaus" or "OsteoLaus".
#' @param wave   "Baseline", "F1"–"F3" (CoLaus) or "Baseline", "V2"–"V5"
#'               (OsteoLaus). \code{wave_num} is derived automatically.
#' @return Tibble; all original columns are character plus three prepended
#'   metadata columns: \code{.cohort}, \code{.wave}, \code{.wave_num}.
import_wave <- function(path, cohort, wave) {
  
  # Validate cohort/wave before touching the file.
  .assert_known_cohort(cohort)
  .assert_known_wave(wave, cohort)
  
  df <- readr::read_delim(
    path,
    delim          = ";",
    col_types      = readr::cols(.default = readr::col_character()),
    na             = c("", "NA", "N/A", "."),
    trim_ws        = TRUE,
    show_col_types = FALSE
  )
  
  df <- dplyr::mutate(df,
                      .cohort   = cohort,
                      .wave     = wave,
                      .wave_num = wave_to_num(wave, cohort),
                      .before   = ".cohort"
  )
  
  validate_wave(df, cohort, wave)
  
  cli::cli_inform(
    "Imported {cohort} {wave}: {nrow(df)} rows \u00d7 {ncol(df)} cols"
  )
  
  return(df)
}