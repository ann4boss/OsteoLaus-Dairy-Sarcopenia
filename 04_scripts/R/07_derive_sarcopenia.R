# =============================================================================
# R/build_sarcopenia.R
# =============================================================================
# Computes definitive sarcopenia staging using EWGSOP2 and FNIH criteria.
#
# =============================================================================

#' Compute definitive sarcopenia staging.
#'
#' @param df Output of merge_closest_exams_final(). 
#'               Must contain: HGS_MAX, ALM_HT2, gait_speed, ALM_BMI.
#' @return df with sarcopenia staging columns appended.
derive_sarcopenia <- function(df) {
    
    # ── Check Required Columns ------------------------------------------------
    required <- c("HGS_MAX", "ALM_HT2", "gait_speed", "ALM_BMI")
    actual_cols <-  df$vars
    missing <- setdiff(required, actual_cols)
    
    if (length(missing) > 0) {
        cli::cli_abort("build_sarcopenia(): Missing columns: {.val {missing}}")
    }
    
    # Ensure Lazy State for performance if not already
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    # ── EWGSOP2 sarcopenia staging ------------------------------------------------
    # Definitions based on EWGSOP2 thresholds
    df <- df %>%
        dplyr::mutate(
            ewgsop2_low_strength = !is.na(HGS_MAX) & HGS_MAX < EWGSOP2$hgs_kg,
            ewgsop2_low_mass     = !is.na(ALM_HT2) & ALM_HT2 < EWGSOP2$almi_kgm2,
            ewgsop2_low_perf     = !is.na(gait_speed) & gait_speed <= EWGSOP2$gait_ms,
            
            ewgsop2_sarcopenia_stage = dplyr::case_when(
                is.na(HGS_MAX)                                             ~ NA_integer_,
                ewgsop2_low_strength & ewgsop2_low_mass & ewgsop2_low_perf ~ 3L,
                ewgsop2_low_strength & ewgsop2_low_mass                    ~ 2L,
                ewgsop2_low_strength                                       ~ 1L,
                TRUE                                                       ~ 0L
            ) %>%
                factor(
                    levels  = 0:3,
                    labels  = c("No sarcopenia", "Probable", "Confirmed", "Severe"),
                    ordered = TRUE
                )
        )
    
    # ── FNIH sarcopenia (sensitivity) ------------------------------------------------
    df <- df %>%
        dplyr::mutate(
            fnih_low_strength = !is.na(HGS_MAX) & HGS_MAX < FNIH$hgs_kg,
            fnih_low_mass     = !is.na(ALM_BMI) & ALM_BMI < FNIH$alm_bmi,
            
            fnih_sarcopenia = dplyr::case_when(
                is.na(HGS_MAX) | is.na(ALM_BMI)   ~ NA_character_,
                fnih_low_strength & fnih_low_mass ~ "Sarcopenia",
                TRUE                              ~ "No sarcopenia"
            ) %>% factor(levels = c("No sarcopenia", "Sarcopenia"))
        )
    
    # ── Eager Summary for Reporting ------------------------------------------------
    # Collect stats before returning
    report_stats <- df %>%
        dplyr::summarise(
            n_total = dplyr::n(),
            n_staged = sum(!is.na(ewgsop2_sarcopenia_stage)),
            n_sarcopenic = sum(ewgsop2_sarcopenia_stage >= "Confirmed", na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::as_tibble()
    
    cli::cli_h2("Deriving Sarcopenia")
    cli::cli_inform(c(
        "v" = "build_sarcopenia: Sarcopenia staging completed.",
        "i" = "Staged {report_stats$n_staged} rows out of {report_stats$n_total}.",
        " " = "Prevalence (Confirmed/Severe): {round(report_stats$n_sarcopenic / report_stats$n_staged * 100, 1)}%"
    ))
    
    return(df)
}