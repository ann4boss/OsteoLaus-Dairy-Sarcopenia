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
# =============================================================================

#' Derive BMI_category for an OsteoLaus long tibble.
#'
#' @param df OsteoLaus long tibble after harmonisation and stacking.
#' @return df with BMI_category (factor) added or replaced.
derive_bmi_category <- function(df) {
    
    # ── Check Required Columns ----------------------------------------------
    actual_cols <- names(df)
    if (!"BMI" %in% actual_cols) {
        cli::cli_warn("derive_bmi_category: column {.col BMI} not found. Skipping derivation.")
        return(df)
    }
    
    # ── Ensure Lazy State ----------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    # ── Main Derivation ----------------------------------------------
    df <- df %>%
        dplyr::mutate(
            BMI_category = dplyr::case_when(
                is.na(BMI)    ~ NA_integer_,
                BMI <  18.5   ~ 1L,
                BMI <  25.0   ~ 2L,
                BMI <  30.0   ~ 3L,
                BMI >= 30.0   ~ 4L,
                TRUE          ~ NA_integer_
            ) %>%
                factor(
                    levels = 1:4,
                    labels = c("Underweight", "Normal", "Overweight", "Obese")
                )
        )
    
    # ── Eager Summary for Reporting ----------------------------------------------
    report_stats <- df %>%
        dplyr::summarise(
            n_total = dplyr::n(),
            n_miss  = sum(is.na(BMI_category)),
            n_obese = sum(BMI_category == "Obese", na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::as_tibble()
    
    cli::cli_h2("Deriving BMI category")
    cli::cli_inform(c(
        "v" = "derive_bmi_category: BMI categories derived.",
        "i" = "Processed {report_stats$n_total} rows ({report_stats$n_miss} missing BMI).",
        " " = "Prevalence of Obesity: {round(report_stats$n_obese / (report_stats$n_total - report_stats$n_miss) * 100, 1)}%"
    ))
    
    return(df)
}