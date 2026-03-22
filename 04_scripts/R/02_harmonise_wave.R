# =============================================================================
# R/harmonise_wave.R
# =============================================================================
# Public entry point for harmonisation. Routes to the correct cohort-specific
# function, contains no harmonisation logic itself.
#
# Depends on: R/harmonise_colaus.R (harmonise_colaus)
#             R/harmonise_osteo.R  (harmonise_osteo)
# =============================================================================

source("04_scripts/R/02_harmonise_colaus.R")
source("04_scripts/R/02_harmonise_osteolaus.R")

#' Harmonise a single imported wave.
#'
#' Dispatches to harmonise_colaus() or harmonise_osteo() based on the
#' .cohort metadata column attached by import_wave(). Type coercion, date
#' parsing, and factor coding are applied; no columns are dropped.
#'
#' @param df Output of import_wave().
#' @return Tibble with correctly typed columns.
harmonise_wave <- function(df) {
    stopifnot(length(unique(df$.wave)) == 1)
    
    cohort <- unique(df$.cohort)
    if (cohort == "CoLaus")    return(harmonise_colaus(df))
    if (cohort == "OsteoLaus") return(harmonise_osteo(df))
    
    cli::cli_abort("Unknown cohort {.val {cohort}}.")
}