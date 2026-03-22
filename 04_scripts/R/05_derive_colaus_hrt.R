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
# Note: hrt_status applies to females only. Male rows will have NA for esthrp
# and will therefore receive NA. No sex filter is applied here so the
# function remains data-agnostic.
#
# Depends on: nothing
# =============================================================================

#' Derive hrt_status for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with hrt_status (factor) added.
derive_hrt <- function(df) {
    
    df <- dplyr::mutate(df,
                        hrt_status = dplyr::case_when(
                            is.na(esthrp)                              ~ NA_character_,
                            esthrp == "Yes"                            ~ "Current HRT",
                            esthrp == "No" & !is.na(esthrpage)         ~ "Past HRT",
                            esthrp == "No"                             ~ "Never / Not current",
                            TRUE                                       ~ NA_character_
                        ) |>
                            factor(levels = c("Never / Not current", "Past HRT", "Current HRT"))
                        
    )
    
    return(df)
}