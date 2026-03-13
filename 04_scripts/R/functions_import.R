# =============================================================================
# R/functions_import.R
# =============================================================================
# Read everything as character. No type coercion at this stage.
# Attach wave metadata so every downstream function knows where a row came from.
#
# Wave-prefixed column naming:
#   Both cohorts prefix every column name with a wave-specific string.
#   The wave label used in the pipeline (.wave) differs from the CSV prefix:
#
#   CoLaus
#     .wave      CSV prefix   Example
#     Baseline   (none)       datexam, age
#     F1         F1           F1datexam, F1age
#     F2         F2           F2datexam, F2age
#     F3         F3           F3datexam, F3age
#
#   OsteoLaus
#     .wave      CSV prefix   Example
#     Baseline   Bsl_         Bsl_SCAN_date, Bsl_Age
#     V2         V2_          V2_SCAN_date,  V2_Age
#     V3         V3_          V3_SCAN_date,  V3_Age
#     V4         V4_          V4_SCAN_date,  V4_Age
#     V5         V5_          V5_SCAN_date,  V5_Age
#
# Date format in both cohorts: DDMonYYYY with lowercase month abbreviation,
# e.g. "21mar2025". The regex accepts mixed case because SAS exports vary.

DATE_REGEX    <- "^[0-9]{2}[A-Za-z]{3}[0-9]{4}$"
DATE_COL_BASE <- c(CoLaus = "datexam", OsteoLaus = "SCAN_date")

# Maps each pipeline wave label to the column prefix used in the CSV file.
WAVE_PREFIX <- list(
  CoLaus = c(
    Baseline = "",
    F1       = "F1",
    F2       = "F2",
    F3       = "F3"
  ),
  OsteoLaus = c(
    Baseline = "Bsl_",
    V2       = "V2_",
    V3       = "V3_",
    V4       = "V4_",
    V5       = "V5_"
  )
)

#' Resolve the actual column name for a base variable in a given wave.
#'
#' Prepends the cohort- and wave-specific CSV prefix to the base name.
#' e.g. resolve_col("age",       "F1",       "CoLaus")    -> "F1age"
#'      resolve_col("SCAN_date", "Baseline", "OsteoLaus") -> "Bsl_SCAN_date"
#'      resolve_col("Age",       "V2",       "OsteoLaus") -> "V1_Age"
#'
#' @param base   Base variable name, e.g. "datexam", "age", "SCAN_date".
#' @param wave   Pipeline wave label, e.g. "Baseline", "F1", "V2".
#' @param cohort "CoLaus" or "OsteoLaus".
resolve_col <- function(base, wave, cohort) {
  prefix <- WAVE_PREFIX[[cohort]][[wave]]
  paste0(prefix, base)
}

#' Import a single raw CSV wave
#'
#' @param path      File path tracked by a format = "file" target.
#' @param cohort    "CoLaus" or "OsteoLaus"
#' @param wave      "Baseline", "F1"–"F3" for CoLaus; "Baseline", "V2"–"V5" for OsteoLaus
#' @param wave_num  Integer ordering: Baseline=0, F1/V2=1, F2/V3=2, F3/V4=3, V5=4
#' @return Tibble; all columns character plus .cohort, .wave, .wave_num
import_wave <- function(path, cohort, wave, wave_num) {
  
  df <- readr::read_delim(
    path,
    col_types      = readr::cols(.default = readr::col_character()),
    na             = c("", "NA", "N/A", "."),
    trim_ws        = TRUE,
    show_col_types = FALSE,
    delim = ";"
  )
  
  df <- dplyr::mutate(df,
                      .cohort   = cohort,
                      .wave     = wave,
                      .wave_num = as.integer(wave_num),
                      .before   = 1L
  )
  
  # ── Fast-fail checks --------------------------------------------------------
  
  # 1. Primary key must exist, if not stop immediately.
  # The column name is always "pt" in both cohorts and all waves.
  if (!"pt" %in% names(df))
    stop(glue::glue("[{cohort} {wave}] Column 'pt' not found."))
  
  # 2. No duplicate participants within a wave,
  # if not a warning because we can still import the data, 
  # but it may cause problems downstream.
  n_dup <- sum(duplicated(df$pt, incomparables = NA))
  if (n_dup > 0)
    warning(glue::glue("[{cohort} {wave}] {n_dup} duplicate pt value(s)."))
  
  # 3. Date column must exist.
  # The column name is the base date name prefixed per WAVE_PREFIX.
  date_col <- resolve_col(DATE_COL_BASE[[cohort]], wave, cohort)
  if (!date_col %in% names(df))
    stop(glue::glue(
      "[{cohort} {wave}] Expected date column '{date_col}' not found. ",
      "Columns present: {paste(names(df), collapse = ', ')}"
    ))
  
  # 4. Date values match expected format
  n_bad <- sum(!is.na(df[[date_col]]) & !stringr::str_detect(df[[date_col]], DATE_REGEX))
  if (n_bad > 0)
    warning(glue::glue(
      "[{cohort} {wave}] {n_bad} value(s) in '{date_col}' ",
      "do not match DDMonYYYY (e.g. '21mar2025')."
    ))
  
  message(glue::glue("Imported {cohort} {wave}: {nrow(df)} rows x {ncol(df)} cols"))
  
  return(df)
}