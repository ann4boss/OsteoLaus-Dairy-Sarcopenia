# =============================================================================
# R/07b_build_sarcopenia.R
# =============================================================================
# Adds handgrip_max_all and definitive sarcopenia staging to the visits table.
#
# handgrip_max_all combines two sources:
#   - OsteoLaus HGS_peak  (row-wise max of 6 measures; V5 only)
#   - CoLaus handgrip     (single peak measure per wave; Baseline-F3)
# OsteoLaus HGS_peak takes priority at V5 where both may be present.
# CoLaus handgrip fills all other OsteoLaus waves via wave_match.
#
# CoLaus handgrip is taken directly from colaus_derived + wave_match so
# the join is explicit and does not depend on whether handgrip survived
# the any_of() select inside build_exposures.
#
# EWGSOP2 and FNIH sarcopenia staging are then computed from handgrip_max_all,
# ALM_HT2, gait_speed, and ALM_BMI — all of which are in visits.
# Results are appended to visits and returned as visits_staged.
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================

#' Assemble handgrip_max_all and compute definitive sarcopenia staging.
#'
#' @param visits        Output of build_visits().
#' @param colaus_derived Output of derive_colaus() — contains the handgrip column.
#' @param wave_match    Tibble (pt, osteo_wave, colaus_wave, gap_days) produced
#'   by the wave_match target — maps each OsteoLaus visit to the nearest
#'   CoLaus wave (F1-F3; Baseline excluded from matching).
#' @return visits with handgrip_max_all, ewgsop2_sarcopenia_stage,
#'   ewgsop2_low_strength, ewgsop2_low_mass, ewgsop2_low_perf,
#'   fnih_sarcopenia, fnih_low_strength, fnih_low_mass appended.
build_sarcopenia <- function(visits, colaus_derived, wave_match) {
    
    # ── Step 1: Extract CoLaus handgrip per wave ───────────────────────────────
    # Pull handgrip (kg) from colaus_derived. handgrip is a single peak measure
    # recorded at CoLaus Baseline through F3. Match it to OsteoLaus waves via
    # wave_match, which links each OsteoLaus visit to the nearest CoLaus wave.
    if (!"handgrip" %in% names(colaus_derived)) {
        cli::cli_warn(
            "build_sarcopenia(): {.col handgrip} not found in colaus_derived. \
       handgrip_max_all will use OsteoLaus HGS_peak (V5) only."
        )
        colaus_hgs <- tibble::tibble(
            pt             = integer(),
            colaus_wave    = character(),
            handgrip_colaus = numeric()
        )
    } else {
        colaus_hgs <- colaus_derived |>
            dplyr::select(pt, colaus_wave = .wave, handgrip_colaus = handgrip) |>
            dplyr::filter(!is.na(handgrip_colaus))
    }
    
    # Map CoLaus handgrip onto OsteoLaus waves via wave_match
    # (one row per pt x osteo_wave, NA where no CoLaus match exists)
    handgrip_per_osteo_wave <- wave_match |>
        dplyr::left_join(colaus_hgs, by = c("pt", "colaus_wave")) |>
        dplyr::select(pt, osteo_wave, handgrip_colaus)
    
    # ── Step 2: Join onto visits and build handgrip_max_all ───────────────────
    # visits is keyed by (pt, osteo_wave) — same grain as handgrip_per_osteo_wave.
    visits <- visits |>
        dplyr::left_join(handgrip_per_osteo_wave, by = c("pt", "osteo_wave")) |>
        dplyr::mutate(
            # HGS_peak (6-measure max, OsteoLaus V5) takes priority.
            # CoLaus handgrip fills Baseline-V4 where HGS_peak is NA.
            handgrip_max_all = dplyr::coalesce(HGS_peak, handgrip_colaus)
        ) |>
        dplyr::select(-handgrip_colaus)
    
    # ── Step 3: EWGSOP2 sarcopenia staging ────────────────────────────────────
    visits <- dplyr::mutate(visits,
                            
                            ewgsop2_low_strength = !is.na(handgrip_max_all) &
                                handgrip_max_all < EWGSOP2$hgs_kg,
                            ewgsop2_low_mass     = !is.na(ALM_HT2) &
                                ALM_HT2 < EWGSOP2$almi_kgm2,
                            ewgsop2_low_perf     = !is.na(gait_speed) &
                                gait_speed <= EWGSOP2$gait_ms,
                            
                            ewgsop2_sarcopenia_stage = dplyr::case_when(
                                is.na(handgrip_max_all)                                    ~ NA_integer_,
                                ewgsop2_low_strength & ewgsop2_low_mass & ewgsop2_low_perf ~ 3L,
                                ewgsop2_low_strength & ewgsop2_low_mass                    ~ 2L,
                                ewgsop2_low_strength                                       ~ 1L,
                                TRUE                                                       ~ 0L
                            ) |>
                                factor(
                                    levels  = 0:3,
                                    labels  = c("No sarcopenia", "Probable", "Confirmed", "Severe"),
                                    ordered = TRUE
                                )
    )
    
    # ── Step 4: FNIH sarcopenia (sensitivity) ─────────────────────────────────
    visits <- dplyr::mutate(visits,
                            
                            fnih_low_strength = !is.na(handgrip_max_all) &
                                handgrip_max_all < FNIH$hgs_kg,
                            fnih_low_mass     = !is.na(ALM_BMI) &
                                ALM_BMI < FNIH$alm_bmi,
                            
                            fnih_sarcopenia = dplyr::case_when(
                                is.na(handgrip_max_all) | is.na(ALM_BMI) ~ NA_character_,
                                fnih_low_strength & fnih_low_mass         ~ "Sarcopenia",
                                TRUE                                      ~ "No sarcopenia"
                            ) |> factor(levels = c("No sarcopenia", "Sarcopenia"))
    )
    
    # ── Diagnostic summary ────────────────────────────────────────────────────
    n_hgs_peak    <- sum(!is.na(visits$HGS_peak),         na.rm = TRUE)
    n_colaus_only <- sum( is.na(visits$HGS_peak) &
                              !is.na(visits$handgrip_max_all), na.rm = TRUE)
    n_total_grip  <- sum(!is.na(visits$handgrip_max_all), na.rm = TRUE)
    n_staged      <- sum(!is.na(visits$ewgsop2_sarcopenia_stage))
    
    stage_summary <- visits |>
        dplyr::filter(!is.na(ewgsop2_sarcopenia_stage)) |>
        dplyr::count(osteo_wave, ewgsop2_sarcopenia_stage, name = "n")
    
    cli::cli_inform(c(
        "i" = "build_sarcopenia(): handgrip_max_all non-missing: {n_total_grip} / {nrow(visits)} rows",
        "*" = "OsteoLaus HGS_peak (V5):         {n_hgs_peak} rows",
        "*" = "CoLaus handgrip (wave-matched): {n_colaus_only} rows",
        "i" = "EWGSOP2 definitive staging ({n_staged} rows staged):",
        "*" = paste(capture.output(print(stage_summary, n = Inf)), collapse = "\n")
    ))
    
    visits
}