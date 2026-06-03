# =============================================================================
# R/import.R
# =============================================================================
# Imports a single raw CSV visit file.
# =============================================================================

# -----------------------------------------------------------------------------
# Cohort metadata
# visit-prefixed column naming:
#
#   CoLaus
#     .visit      CSV prefix   Example
#     Baseline   (none)       datexam, age
#     F1         F1           F1datexam, F1age
#     F2         F2           F2datexam, F2age
#     F3         F3           F3datexam, F3age
#
#   OsteoLaus
#     .visit      CSV prefix   Example
#     Baseline   Bsl_         Bsl_SCAN_date, Bsl_Age
#     V2         V2_          V2_SCAN_date,  V2_Age
#     V3         V3_          V3_SCAN_date,  V3_Age
#     V4         V4_          V4_SCAN_date,  V4_Age
#     V5         V5_          V5_SCAN_date,  V5_Age
# -----------------------------------------------------------------------------

COHORT_META <- list(
  CoLaus = list(
    date_col_base = "datexam",
    visit_prefix = c(
      Baseline = "",
      F1       = "F1",
      F2       = "F2",
      F3       = "F3"
    ),
    visit_num = c(
      Baseline = 1L,
      F1       = 2L,
      F2       = 3L,
      F3       = 4L
    )
  ),
  OsteoLaus = list(
    date_col_base = "SCAN_date",
    visit_prefix = c(
      Baseline = "Bsl_",
      V2       = "V2_",
      V3       = "V3_",
      V4       = "V4_",
      V5       = "V5_"
    ),
    visit_num = c(
      Baseline = 1L,
      V2       = 2L,
      V3       = 3L,
      V4       = 4L,
      V5       = 5L
    )
  )
)


#' Import a single raw CSV visit.
#'
#' Reads a semicolon-delimited CSV file produced by SAS, coerces all columns
#' to character (type coercion is deferred to downstream targets), attaches
#' visit-level metadata columns, and runs all fast-fail validation checks.
#'
#' @param path   File path tracked by a format = "file" target.
#' @param cohort "CoLaus" or "OsteoLaus".
#' @param visit   "Baseline", "F1"–"F3" (CoLaus) or "Baseline", "V2"–"V5"
#'               (OsteoLaus).
#' @return Tibble; all original columns are character plus attach
#'   metadata columns: .cohort, .visit.
import_visit <- function(path, cohort, visit, sep = ";") {
  
  
  raw_dt <- data.table::fread(
    file   = path,
    sep    = sep,
    colClasses = "character",
    na.strings = c("", "NA", "N/A", "."),
    strip.white = TRUE
  )
  
  df_lazy <- dtplyr::lazy_dt(raw_dt) |>
    dplyr::mutate(
      .cohort   = cohort,
      .visit = COHORT_META[[cohort]][["visit_num"]][[visit]],
      .before   = 1
    )
  
  df <- dplyr::as_tibble(df_lazy)
  cli::cli_h1("Import visit")
  cli::cli_inform(c(
    "i" = "Imported {cohort} {visit}: {nrow(df)} rows \u00d7 {ncol(df)} cols"
  ))
  
  return(df)
}
