# =============================================================================
# R/build_sarcopenia.R
# =============================================================================
# Computes definitive sarcopenia staging using EWGSOP2 and FNIH criteria.
#
# =============================================================================
# -----------------------------------------------------------------------------
# EWGSOP2 (2019) sarcopenia thresholds — women only
# OsteoLaus is an all-female cohort; only female cut-offs are needed.
#
# Staging logic (applied in 06_derived_combined.R):
#   Probable sarcopenia : HGS_peak < hgs_kg
#   Confirmed sarcopenia: probable + ALM_HT2 < almi_kgm2
#   Severe sarcopenia   : confirmed + gait_speed <= gait_ms
#
# Reference: Cruz-Jentoft et al. Age Ageing. 2019;48(1):16-31.
# -----------------------------------------------------------------------------

EWGSOP2 <- list(
    hgs_kg    = 16.0,   # kg    low grip strength (women)
    almi_kgm2 =  5.5,   # kg/m2 low ALMI (women)
    gait_ms   =  0.8    # m/s   low gait speed (<=)
)

# -----------------------------------------------------------------------------
# FNIH sarcopenia thresholds — women only
# Sarcopenia: grip strength < hgs_kg AND ALM/BMI < alm_bmi
#
# Reference: Studenski et al. J Gerontol A Biol Sci Med Sci. 2014;69(5):547-558.
# -----------------------------------------------------------------------------

FNIH <- list(
    hgs_kg  = 16.0,    # kg          low grip strength (women)
    alm_bmi =  0.512   # kg/(kg/m2)  low ALM/BMI (women)
)


#' Compute definitive sarcopenia staging.
#'
#' @param df Output of merge_closest_exams_final(). 
#'               Must contain: HGS_MAX, ALM_HT2, gait_speed, ALM_BMI.
#' @return df with sarcopenia staging columns appended.
derive_sarcopenia <- function(df, ewgsop2 = EWGSOP2, fnih = FNIH) {

    required <- c("HGS_MAX", "ALM_HT2", "gait_speed", "ALM_BMI")
    missing <- setdiff(required, names(df))

    if (length(missing) > 0L) {
        cli::cli_abort("derive_sarcopenia(): Missing columns: {.val {missing}}")
    }

    out <- df |>
        dplyr::mutate(
            ewgsop2_low_strength = !is.na(.data$HGS_MAX) &
                .data$HGS_MAX < ewgsop2$hgs_kg,
            ewgsop2_low_mass = !is.na(.data$ALM_HT2) &
                .data$ALM_HT2 < ewgsop2$almi_kgm2,
            ewgsop2_low_perf = !is.na(.data$gait_speed) &
                .data$gait_speed <= ewgsop2$gait_ms,

            ewgsop2_sarcopenia_stage = dplyr::case_when(
                is.na(.data$HGS_MAX) ~ NA_integer_,
                
                .data$ewgsop2_low_strength %in% TRUE &
                    .data$ewgsop2_low_mass %in% TRUE &
                    .data$ewgsop2_low_perf %in% TRUE ~ 3L,
                
                .data$ewgsop2_low_strength %in% TRUE &
                    .data$ewgsop2_low_mass %in% TRUE ~ 2L,
                
                .data$ewgsop2_low_strength %in% TRUE ~ 1L,
                
                TRUE ~ 0L
            ) |> factor(
                levels = 0:3,
                labels = c(
                    "No sarcopenia",
                    "Probable",
                    "Confirmed",
                    "Severe"
                ),
                ordered = TRUE
            ),

            fnih_low_strength = !is.na(.data$HGS_MAX) &
                .data$HGS_MAX < fnih$hgs_kg,
            fnih_low_mass = !is.na(.data$ALM_BMI) &
                .data$ALM_BMI < fnih$alm_bmi,

            fnih_sarcopenia = dplyr::case_when(
                is.na(.data$HGS_MAX) | is.na(.data$ALM_BMI) ~ NA_character_,
                .data$fnih_low_strength & .data$fnih_low_mass ~ "Sarcopenia",
                TRUE ~ "No sarcopenia"
            ) |>
                factor(levels = c("No sarcopenia", "Sarcopenia"))
        )

    report_stats <- out |>
        dplyr::summarise(
            n_total = dplyr::n(),
            
            n_staged = sum(
                !is.na(.data$ewgsop2_sarcopenia_stage)
            ),
            
            n_sarcopenic = sum(
                .data$ewgsop2_sarcopenia_stage %in%
                    c("Confirmed", "Severe"),
                na.rm = TRUE
            ),
            
            .groups = "drop"
        )
    prevalence <- if (report_stats$n_staged > 0L) {
        round(report_stats$n_sarcopenic / report_stats$n_staged * 100, 1)
    } else {
        NA_real_
    }

    cli::cli_h2("Deriving Sarcopenia")
    cli::cli_inform(c(
        "v" = "derive_sarcopenia(): Sarcopenia staging completed.",
        "i" = "Staged {report_stats$n_staged} rows out of {report_stats$n_total}.",
        " " = "Prevalence (Confirmed/Severe): {prevalence}%"
    ))
    
    
    return(out)
}
