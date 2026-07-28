# =============================================================================
# R/01_01_01_import.R
# =============================================================================
# Reads a single raw CoLaus/OsteoLaus visit CSV into a tibble and attaches
# cohort/visit identifier columns. This is the first step of the pipeline:
# every downstream script (harmonise, QC, stack) consumes the output of
# import_visit().
#
# Defines:
#   COHORT_META    — nested list of per-cohort visit metadata (see below)
#   import_visit() — reads one CSV and attaches .cohort/.visit columns
# =============================================================================

# -----------------------------------------------------------------------------
# COHORT_META
# -----------------------------------------------------------------------------
# (also used in visit-prefixed column naming used by strip_prefix()
# (R/01_02_01_utils_harmonise.R) to recover base column names after import.)
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


# -----------------------------------------------------------------------------
# import_visit()
# -----------------------------------------------------------------------------
#' Import a single raw CSV visit.
#'
#' Reads a semicolon-delimited CSV file produced by SAS, coerces all columns
#' to character (type coercion is deferred to downstream targets) and attaches
#' visit-level metadata columns.
#'
#' @param path   File path tracked by a format = "file" target.
#' @param cohort "CoLaus" or "OsteoLaus".
#' @param visit   "Baseline", "F1"–"F3" (CoLaus) or "Baseline", "V2"–"V5"
#'               (OsteoLaus).
#' @return Tibble; all original columns are character plus attach
#'   metadata columns: .cohort, .visit.
import_visit <- function(path, cohort, visit, sep = ";") {

  # Read every column as character; SAS-style blanks/dots become NA.
  # Numeric/date coercion happens later in harmonise_*() (R/01_02_*.R), not here.
  raw_dt <- data.table::fread(
    file   = path,
    sep    = sep,
    colClasses = "character",
    na.strings = c("", "NA", "N/A", "."),
    strip.white = TRUE
  )

  # dtplyr defers execution to data.table for speed on large raw files.
  df_lazy <- dtplyr::lazy_dt(raw_dt) |>
    dplyr::mutate(
      .cohort   = cohort,
      .visit = COHORT_META[[cohort]][["visit_num"]][[visit]],
      .before   = 1
    )

  # Materialize the lazy_dt plan into a tibble for downstream dplyr code.
  df <- dplyr::as_tibble(df_lazy)
  cli::cli_h1("Import visit")
  cli::cli_inform(c(
    "i" = "Imported {cohort} {visit}: {nrow(df)} rows \u00d7 {ncol(df)} cols"
  ))

  return(df)
}
