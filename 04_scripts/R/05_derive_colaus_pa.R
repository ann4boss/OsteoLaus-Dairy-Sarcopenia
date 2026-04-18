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
#   1 = Inactive            (0-30)
#   2 = Insufficient        (31–149)
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
    actual_cols <- names(df)
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
    df <- df |>
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
                tmp_me_min_week < 30      ~ 1L,
                tmp_me_min_week < 150      ~ 2L,
                tmp_me_min_week < 300      ~ 3L,
                tmp_me_min_week >= 300     ~ 4L
            ) |>
                factor(
                    levels  = 1:4,
                    labels  = c("Inactive", "Insufficient", "Sufficient", "High"),
                    ordered = TRUE
                )
        ) |>
        dplyr::as_tibble()
    
    # ── Summary & Reporting --------------------------------------------
    pa_summary <- df |>
        dplyr::group_by(.wave) |>
        dplyr::summarise(
            n_inactive     = sum(pa_levels == "Inactive", na.rm = TRUE),
            n_insufficient = sum(pa_levels == "Insufficient", na.rm = TRUE),
            n_sufficient   = sum(pa_levels == "Sufficient", na.rm = TRUE),
            n_high         = sum(pa_levels == "High", na.rm = TRUE),
            n_total        = sum(!is.na(pa_levels)),
            .groups        = "drop"
        ) |>
        dplyr::mutate(
            prop_inactive     = n_inactive / n_total,
            prop_insufficient = n_insufficient / n_total,
            prop_sufficient   = n_sufficient / n_total,
            prop_high         = n_high / n_total
        )
    
    # Print informative message per wave
    cli::cli_h2("Physical Activity Derivation Report")
    
    purrr::pwalk(pa_summary, function(...) {
        res <- list(...)
        cli::cli_inform(c(
            "v" = "{.strong Wave: {res$.wave}} (N = {res$n_total})",
            " " = "High: {.val {res$n_high}} ({scales::percent(res$prop_high, 0.1)})",
            " " = "Sufficient: {.val {res$n_sufficient}} ({scales::percent(res$prop_sufficient, 0.1)})",
            " " = "Insufficient: {.val {res$n_insufficient}} ({scales::percent(res$prop_insufficient, 0.1)})",
            " " = "Inactive: {.val {res$n_inactive}} ({scales::percent(res$prop_inactive, 0.1)})"
        ))
        cli::cli_text("")
    }) 
    
    
    
    return(df)
}