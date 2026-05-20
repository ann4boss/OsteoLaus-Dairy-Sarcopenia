# =============================================================================
# R/harmonise_visit.R
# =============================================================================
# Public entry point for harmonisation. Routes to the correct cohort-specific
# function, contains no harmonisation logic itself.
# =============================================================================

#' Harmonise a single imported visit.
#'
#' Dispatches to harmonise_colaus() or harmonise_osteo() based on the
#' .cohort metadata column attached by import_visit(). Type coercion, date
#' parsing, and factor coding are applied; no columns are dropped.
#'
#' @param df A data frame, tibble, or lazy_dt object.
#' @return A tibble (if the sub-functions call as_tibble) or a lazy_dt.
harmonise_visit <- function(df) {
    
    cli::cli_h1("Harmonise visit")
    
    # ── Extract metadata -----------------------------------------
    
    cohorts <- df |> dplyr::distinct(.cohort) |> dplyr::pull(.cohort)
    visits   <- df |> dplyr::distinct(.visit) |> dplyr::pull(.visit)
    
    if (length(visits) > 1) {
        cli::cli_abort("harmonise_visit() received multiple visits: {.val {visits}}.")
    }
    
    cohort <- cohorts[1]
    
    # ── Route to specific harmonisation -----------------------------------------
    out <- if (cohort == "CoLaus") {
        harmonise_colaus(df)
    } else if (cohort == "OsteoLaus") {
        harmonise_osteo(df)
    } else {
        cli::cli_abort("Unknown cohort {.val {cohort}}.")
    }
    
    return(out)
}