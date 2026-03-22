# =============================================================================
# R/05_derive_colaus_pa.R
# =============================================================================
# Derives:
#   - met_min_week (for comparability, optional)
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

derive_pa <- function(df) {
    
    df <- dplyr::mutate(df,
                        # Moderate-equivalent minutes/week (used for classification)
                        me_min_week = dplyr::case_when(
                            !is.na(PAFQ_MPA) | !is.na(PAFQ_VPA) ~
                                (dplyr::coalesce(PAFQ_MPA, 0) +
                                     2 * dplyr::coalesce(PAFQ_VPA, 0)) * 7,
                            TRUE ~ NA_real_
                        ),
                        
                        pa_levels = dplyr::case_when(
                            is.na(me_min_week)     ~ NA_integer_,
                            me_min_week == 0       ~ 1L,
                            me_min_week < 150      ~ 2L,
                            me_min_week < 300      ~ 3L,
                            me_min_week >= 300     ~ 4L
                        ) |>
                            factor(
                                levels  = 1:4,
                                labels  = c("Inactive",
                                            "Insufficient",
                                            "Sufficient",
                                            "High"),
                                ordered = TRUE
                            )
    )
    
    return(df)
}