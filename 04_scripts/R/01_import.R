# =============================================================================
# R/import.R
# =============================================================================
# Imports a single raw CSV wave file.
# =============================================================================

#' Import a single raw CSV wave.
#'
#' Reads a semicolon-delimited CSV file produced by SAS, coerces all columns
#' to character (type coercion is deferred to downstream targets), attaches
#' wave-level metadata columns, and runs all fast-fail validation checks.
#'
#' @param path   File path tracked by a format = "file" target.
#' @param cohort "CoLaus" or "OsteoLaus".
#' @param wave   "Baseline", "F1"–"F3" (CoLaus) or "Baseline", "V2"–"V5"
#'               (OsteoLaus).
#' @return Tibble; all original columns are character plus attach
#'   metadata columns: .cohort, .wave.
import_wave <- function(path, cohort, wave) {
  
  
  raw_dt <- data.table::fread(
    file   = path,
    sep    = ";",
    colClasses = "character",
    na.strings = c("", "NA", "N/A", "."),
    strip.white = TRUE
  )
  
  df_lazy <- dtplyr::lazy_dt(raw_dt) |>
    dplyr::mutate(
      .cohort   = cohort,
      .wave = COHORT_META[[cohort]][["wave_num"]][[wave]],
      .before   = 1
    )
  
  df <- dplyr::as_tibble(df_lazy)
  cli::cli_h1("Import Wave")
  cli::cli_inform(c(
    "i" = "Imported {cohort} {wave}: {nrow(df)} rows \u00d7 {ncol(df)} cols"
  ))
  
  return(df)
}
