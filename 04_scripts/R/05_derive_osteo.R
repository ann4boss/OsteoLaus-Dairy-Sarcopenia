# =============================================================================
# R/05_derive_osteo.R
# =============================================================================
# Applies all OsteoLaus-specific derivations in order.
#
# Derivation order:
#   1. bmi_category  — from numeric BMI; WHO cut-offs; no dependencies
#   2. hgs_max       — from HGS_R1:R3 / HGS_L1:L3 (V5 only); validates vs
#                      pre-existing HGS_MAX if present
#
# =============================================================================


#' Apply all OsteoLaus-specific derivations to the stacked OsteoLaus tibble.
#'
#' @param df Output of stack_waves() for OsteoLaus.
#' @return df with BMI_category and HGS_MAX derived variables added.
derive_osteo <- function(df) {
    stopifnot("OsteoLaus" %in% unique(df$.cohort))
    
    df |>
        derive_bmi_category() |>
        derive_hgs_max()
}