# =============================================================================
# R/impute_hrt.R
# =============================================================================
# LOCF imputation for hrt_status (within participant).
#
# - Carries last observed hrt_status forward within pt (.visit order)
# - Only fills NA values
# - Does not overwrite derived or observed values
#
# Output:
#   hrt_status_imp      (Never / Past HRT / Current HRT)
#   hrt_impute_source   (observed / locf)
# =============================================================================

impute_hrt <- function(df) {
    
    required_cols <- c("pt", ".visit", "hrt_status")
    missing_cols <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0L) {
        cli::cli_abort(
            "impute_hrt_locf: missing required columns: {.val {missing_cols}}"
        )
    }
    
    n_na_before <- sum(is.na(df$hrt_status))
    
    df <- df |>
        dplyr::arrange(pt, .visit) |>
        dplyr::group_by(pt) |>
        dplyr::mutate(
            
            hrt_status_imp = hrt_status,
            
            hrt_impute_source = dplyr::if_else(
                !is.na(hrt_status),
                "observed",
                NA_character_
            )
        ) |>
        tidyr::fill(
            hrt_status_imp,
            .direction = "down"
        ) |>
        dplyr::mutate(
            
            hrt_impute_source = dplyr::case_when(
                !is.na(hrt_impute_source) ~ hrt_impute_source,
                is.na(hrt_status) & !is.na(hrt_status_imp) ~ "locf",
                TRUE ~ NA_character_
            ),
            
            hrt_impute_source = factor(
                hrt_impute_source,
                levels = c("observed", "locf")
            ),
            
            hrt_status_imp = factor(
                hrt_status_imp,
                levels = c("Never / Not current", "Past HRT", "Current HRT")
            )
        ) |>
        dplyr::ungroup()
    
    n_na_after <- sum(is.na(df$hrt_status_imp))
    
    cli::cli_h2("Impute HRT status (LOCF)")
    cli::cli_inform(c(
        "i" = "NA before: {n_na_before}",
        "i" = "NA after:  {n_na_after}",
        "i" = "Filled:    {n_na_before - n_na_after}"
    ))
    
    return(df)
}