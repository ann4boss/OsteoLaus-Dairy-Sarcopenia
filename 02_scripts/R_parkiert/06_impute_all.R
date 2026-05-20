# =============================================================================
# R/impute_all.R
# =============================================================================
#
impute_osteo <- function(df) {
    
    cohorts <- df |> dplyr::distinct(.cohort) |> dplyr::pull(.cohort)
    
    if (!all(cohorts == "OsteoLaus")) {
        cli::cli_inform("impute_osteo() received non-OsteoLaus data: {.val {cohorts}}")
        return(df)
    }
    
    df |>
        impute_exam_date() |>
        impute_age()
}


# =============================================================================
impute_colaus <- function(df) {
    
    cohorts <- df |> dplyr::distinct(.cohort) |> dplyr::pull(.cohort)
    
    if (!all(cohorts == "CoLaus")) {
        cli::cli_inform("impute_colaus() received non-CoLaus data: {.val {cohorts}}")
        return(df)
    }
    
    df |>
        impute_exam_date() |>
        impute_age() |>
        impute_alcohol() |>
        impute_smoking() |>
        impute_diabetes() |>
        impute_atc() |>
        impute_htn()|>
        impute_hrt()
}