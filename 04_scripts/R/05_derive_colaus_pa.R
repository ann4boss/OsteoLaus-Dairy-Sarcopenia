# TODO: not sure if I understood the PAFQ values correctly. Currently taken as
# minutes per week and taken WHO standards
# =============================================================================
# R/05_derive_colaus_pa.R
# =============================================================================
# Derives:
#   - me_min_week  (moderate-equivalent minutes/week, used for classification)
#   - pa_levels    (4-level WHO classification)
#
# Source variables:
#   PAFQ_MPA  — moderate physical activity (min/day)
#   PAFQ_VPA  — vigorous physical activity (min/day)
#
# Derivation:
#   me_min_week  = (MPA + 2 * VPA) * 7
#
# pa_levels (WHO-based, using ME-min/week):
#   1 = Inactive            (0)
#   2 = Insufficient        (1–149)
#   3 = Sufficient          (150–299)
#   4 = High                (≥300)
# =============================================================================

#' Derive Physical Activity (PA) levels using dtplyr
#'
#' @param df CoLaus long tibble (lazy_dt or data.frame)
#' @return df with pa_levels added, source and intermediate columns dropped.
derive_pa <- function(df) {
    
    # ── Check Required Columns ------------------------------------------------
    required_cols <- c("PAFQ_MPA", "PAFQ_VPA")
    actual_cols <- df$vars
    
    missing_cols <- setdiff(required_cols, actual_cols)
    
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_pa: missing required columns: {.val {missing_cols}}. 
            {.col pa_levels} will not be derived."
        )
        return(df)
    }
    
    # ── Ensure Lazy State ------------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    # ── Main Derivation ------------------------------------------------
    df <- df %>%
        dplyr::mutate(
            # Moderate-equivalent minutes/week (intermediate helper)
            # 1 min VPA = 2 min MPA
            tmp_me_min_week = dplyr::case_when(
                !is.na(PAFQ_MPA) | !is.na(PAFQ_VPA) ~ 
                    (dplyr::coalesce(PAFQ_MPA, 0) + 2 * dplyr::coalesce(PAFQ_VPA, 0)),
                TRUE ~ NA_real_
            ),
            
            # Categorization based on WHO-like guidelines
            pa_levels = dplyr::case_when(
                is.na(tmp_me_min_week)     ~ NA_integer_,
                tmp_me_min_week == 0       ~ 1L,
                tmp_me_min_week < 150      ~ 2L,
                tmp_me_min_week < 300      ~ 3L,
                tmp_me_min_week >= 300     ~ 4L
            ) %>%
                factor(
                    levels  = 1:4,
                    labels  = c("Inactive", "Insufficient", "Sufficient", "High"),
                    ordered = TRUE
                )
        )
    
    # ── Cleanup ------------------------------------------------
    # Drop raw source columns and the temporary calculation column
    df <- df %>%
        dplyr::select(
            -dplyr::all_of(required_cols),
            -dplyr::starts_with("tmp_")
        )
    
    return(df)
}