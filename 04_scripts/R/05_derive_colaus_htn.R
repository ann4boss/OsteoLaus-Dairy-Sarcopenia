# =============================================================================
# R/05_derive_colaus_htn.R
# =============================================================================
# Derives HTN_status (hypertension) from three sources:
#
#   1. antiHTA  — documented antihypertensive treatment (factor: No/Yes)
#   2. crbpmed  — self-reported antihypertensive medication (factor: No/Yes)
#   3. HTA      — measured hypertension (BP >= 140/90 mmHg) (factor: No/Yes)
#
# All three are Yes/No factors after harmonisation. Comparisons use "Yes"/"No"
# string labels, NOT 1/0 integers.
#
# HTN_status = "Yes" if ANY indicator is "Yes"
# HTN_status = "No"  if at least one indicator is non-NA and all present are "No"
# HTN_status = NA    if all three indicators are NA
#
# Depends on: nothing
# =============================================================================

#' Derive HTN_status for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with HTN_status (factor No/Yes) added.
derive_htn <- function(df) {
    
    required_vars <- c("antiHTA", "crbpmed", "HTA")
    missing_vars  <- setdiff(required_vars, names(df))
    if (length(missing_vars) > 0)
        cli::cli_abort("derive_htn: required column(s) not found: {.col {missing_vars}}")
    
    # Helper: test a Yes/No factor column safely (NA -> FALSE)
    .is_yes <- function(x) !is.na(x) & x == "Yes"
    .is_no  <- function(x) !is.na(x) & x == "No"
    
    df <- dplyr::mutate(df,
                        HTN_status = dplyr::case_when(
                            # Any positive indicator -> hypertensive
                            .is_yes(antiHTA) | .is_yes(crbpmed) | .is_yes(HTA) ~ "Yes",
                            # At least one indicator is No and none is Yes -> normotensive
                            .is_no(antiHTA)  | .is_no(crbpmed)  | .is_no(HTA)  ~ "No",
                            # All NA -> missing
                            TRUE ~ NA_character_
                        ) |> factor(levels = c("No", "Yes"))
    )
    
    return(df)
}