# =============================================================================
# R/derive_colaus_hrt.R
# =============================================================================
# Derives hrt_status from esthrp and esthrpage.
#
# Source variables:
#   esthrp    — currently on HRT (0=No, 1=Yes, 9=Does not know → NA)
#   esthrpage — age at HRT start (99 = Does not know → NA)
#
# Derivation:
#   hrt_status = "Current HRT"         if esthrp == "Yes"
#   hrt_status = "Past HRT"            if esthrp == "No" and esthrpage is
#                                          non-missing and non-sentinel
#   hrt_status = "Never / Not current" if esthrp == "No" and no past-use
#                                          evidence
#   hrt_status = NA                    when esthrp is NA
#
# =============================================================================

#' Derive Hormone Replacement Therapy (HRT) status using dtplyr
#'
#' @param df CoLaus long tibble (lazy_dt or data.frame)
#' @return df with hrt_status added and source columns dropped.
derive_hrt <- function(df) {
    
    # ── Check Required Columns ----------------------------------------------
    required_cols <- c("esthrp", "esthrpage")
    actual_cols <- df$vars
    
    missing_cols <- setdiff(required_cols, actual_cols)
    
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_hrt: missing required columns: {.val {missing_cols}}. 
            {.col hrt_status} will not be derived."
        )
        return(df)
    }
    
    # ── Ensure Lazy State ----------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    
    # ── Main Derivation ----------------------------------------------
    df <- df %>%
        dplyr::mutate(
            hrt_status = dplyr::case_when(
                is.na(esthrp)                               ~ NA_character_,
                esthrp == "Yes"                             ~ "Current HRT",
                esthrp == "No" & !is.na(esthrpage)          ~ "Past HRT",
                esthrp == "No"                              ~ "Never / Not current",
                TRUE                                        ~ NA_character_
            ) %>% 
                factor(levels = c("Never / Not current", "Past HRT", "Current HRT"))
        )
    
    # ── Cleanup ----------------------------------------------
    # Drop source columns as requested
    df <- df %>%
        dplyr::select(-dplyr::all_of(required_cols))
    
    return(df)
}