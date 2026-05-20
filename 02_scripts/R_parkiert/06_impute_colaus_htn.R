# =============================================================================
# R/impute_htn.R
# =============================================================================
# LOCF imputation for HTN_status (within participant).
#
# - Uses last observation carried forward within pt (.visit order)
# - Only fills NA values
# - Never overwrites observed or derived HTN_status values
#
# Output:
#   HTN_status_imp      (No / Yes)
#   HTN_impute_source   (observed / locf)
# =============================================================================

impute_htn <- function(df) {
    
    required_cols <- c("pt", ".visit", "HTN_status")
    missing_cols <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0L) {
        cli::cli_abort(
            "impute_htn_locf: missing required columns: {.val {missing_cols}}"
        )
    }
    
    n_na_before <- sum(is.na(df$HTN_status))
    
    df <- df |>
        dplyr::arrange(pt, .visit) |>
        dplyr::group_by(pt) |>
        dplyr::mutate(
            
            HTN_status_imp = HTN_status,
            
            HTN_impute_source = dplyr::if_else(
                !is.na(HTN_status),
                "observed",
                NA_character_
            )
        ) |>
        tidyr::fill(
            HTN_status_imp,
            .direction = "down"
        ) |>
        dplyr::mutate(
            
            HTN_impute_source = dplyr::case_when(
                !is.na(HTN_impute_source) ~ HTN_impute_source,
                is.na(HTN_status) & !is.na(HTN_status_imp) ~ "locf",
                TRUE ~ NA_character_
            ),
            
            HTN_impute_source = factor(
                HTN_impute_source,
                levels = c("observed", "locf")
            ),
            
            HTN_status_imp = factor(
                HTN_status_imp,
                levels = c("No", "Yes")
            )
        ) |>
        dplyr::ungroup()
    
    n_na_after <- sum(is.na(df$HTN_status_imp))
    
    cli::cli_h2("Impute HTN status (LOCF)")
    cli::cli_inform(c(
        "i" = "NA before: {n_na_before}",
        "i" = "NA after:  {n_na_after}",
        "i" = "Filled:    {n_na_before - n_na_after}"
    ))
    
    if (n_na_after > 0L) {
        cli::cli_inform(
            "impute_htn_locf: remaining NA indicates no prior HTN observation per participant"
        )
    }
    
    return(df)
}