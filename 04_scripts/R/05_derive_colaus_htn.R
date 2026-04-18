#TODO: currently all variables are treated the same. Should there be a hierarchy like in diabetes?
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
    
    # ── Check Required Columns ------------------------------------------------
    required_vars <- c("antiHTA", "crbpmed", "HTA")
    actual_cols <- names(df)
    missing_vars <- setdiff(required_vars, actual_cols)
    if (length(missing_vars) > 0) {
        cli::cli_abort("derive_htn: required column(s) not found: {.val {missing_vars}}")
        return(df)
    }
    
    # ── Ensure Lazy State ------------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    # ── Main Derivation ------------------------------------------------
    df <- df |>
        dplyr::mutate(
            # Internal helpers for vectorized logic (Yes/No factors/chars)
            tmp_yes = (!is.na(antiHTA) & antiHTA == "Yes") | 
                (!is.na(crbpmed) & crbpmed == "Yes") | 
                (!is.na(HTA)     & HTA     == "Yes"),
            
            tmp_no  = (!is.na(antiHTA) & antiHTA == "No") | 
                (!is.na(crbpmed) & crbpmed == "No") | 
                (!is.na(HTA)     & HTA     == "No"),
            
            HTN_status = dplyr::case_when(
                tmp_yes ~ "Yes",
                tmp_no  ~ "No",
                TRUE    ~ NA_character_
            ) |> factor(levels = c("No", "Yes"))
        ) |>
        dplyr::as_tibble()
    
    # ── Eager Summary ------------------------------------------------
    # Quick count of prevalence for the log
    stats <- df |>
        dplyr::summarise(
            n_total = dplyr::n(),
            n_htn   = sum(HTN_status == "Yes", na.rm = TRUE),
            n_miss  = sum(is.na(HTN_status)),
            .groups = "drop"
        ) |>
        dplyr::as_tibble()
    
    cli::cli_h2("Derive HTN status")
    cli::cli_inform(c(
        "v" = "derive_htn: HTN status derived.",
        " " = "Summary: {stats$n_htn} Yes / {stats$n_total - stats$n_htn - stats$n_miss} No ({stats$n_miss} NA)"
    ))
    

    
    return(df)
}