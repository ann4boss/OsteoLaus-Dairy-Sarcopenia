# =============================================================================
# R/derive_osteo_bmi.R
# =============================================================================
# Derives BMI_category from BMI (kg/m²).
#
# BMI is measured at every OsteoLaus visit.
#
# Categories (WHO standard):
#   < 18.5           -> 1 = Underweight
#   18.5 – < 25.0    -> 2 = Normal        (reference)
#   25.0 – < 30.0    -> 3 = Overweight
#   >= 30.0          -> 4 = Obese
#
# Depends on: nothing
# =============================================================================

#' Derive BMI_category for an OsteoLaus long tibble.
#'
#' @param df OsteoLaus long tibble after harmonisation and stacking.
#' @return df with BMI_category (factor) added or replaced.
derive_bmi_category <- function(df) {
    
    df <- dplyr::mutate(df,
                        BMI_category = dplyr::case_when(
                            is.na(BMI)    ~ NA_integer_,
                            BMI <  18.5   ~ 1L,
                            BMI <  25.0   ~ 2L,
                            BMI <  30.0   ~ 3L,
                            BMI >= 30.0   ~ 4L
                        ) |>
                            factor(
                                levels = 1:4,
                                labels = c("Underweight", "Normal", "Overweight", "Obese")
                            )
    )
    

    return(df)
}