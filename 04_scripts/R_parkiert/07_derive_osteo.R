# =============================================================================
# R/derive_osteo.R
# =============================================================================
#TODO Description
# =============================================================================

#' Apply all CoLaus-specific derivations to the stacked CoLaus tibble.
#'
#' @param df Output of stack_visits() for CoLaus.
#' @return df with all derived variables appended.
derive_osteo <- function(df) {
    
    cli::cli_h1("Derive Variables for OsteoLaus")
    
    # ── Guard: Single Cohort ------------------------------------------------
    cohorts <- df |> dplyr::distinct(.cohort) |> dplyr::pull(.cohort)
    if (!"OsteoLaus" %in% cohorts) {
        cli::cli_abort("derive_colaus() received non-OsteoLaus data: {.val {cohorts}}")
    }
    
    # ── Execute Derivation Chain ----------------------------------------------
    df_derived <- df |>
        derive_bmi()    |>
        derive_bmi_category()|>
        derive_alm_indices()
    

    return(dplyr::as_tibble(df_derived))
}

