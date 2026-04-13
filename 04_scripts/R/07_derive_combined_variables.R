# =============================================================================
# R/derive_combined_variables.R
# =============================================================================
# Applies all derivations to the combined OsteoLaus-CoLaus table.
#
# Derivation order:
#   1. bmi_category — from the unified BMI (prioritized Osteo -> Colaus)
#   2. sarcopenia   — staging using unified HGS_max, ALM, and gait_speed
#
# =============================================================================

#' Apply derivations to the merged OsteoLaus/CoLaus table.
#'
#' @param df Output of merge_closest_exams_final().
#' @return A tibble with BMI_category and sarcopenia stages added.
derive_combined <- function(df) {
    
    cli::cli_h1("Deriving Combined OsteoLaus-CoLaus Variables")
    
    
    # ── Execute Derivation Chain ----------------------------------------------
    df_derived <- df |>
        derive_bmi_category() |>
        derive_sarcopenia()
    
    # ── Finalize --------------------------------------------------------------
    # Convert back to tibble for the final analytical output
    final_df <- dplyr::as_tibble(df_derived)
    

    return(final_df)
}