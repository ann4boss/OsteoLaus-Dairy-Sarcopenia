# =============================================================================
# R/07_build_exposures.R
# =============================================================================
# Builds the exposures table: one row per participant x OsteoLaus wave.
#
# Grain:   pt x osteo_wave
# Source:  colaus_derived (FFQ + covariates), matched to visits by exam date
# Output:  exposures -> used by freeze_dataset()
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================


#' Build the exposures table.
#'
#' Matches each OsteoLaus visit to the nearest CoLaus wave (F1-F3; Baseline
#' excluded) by absolute exam-date difference, pulls FFQ-derived dairy
#' variables and time-varying covariates from that matched wave, then computes
#' cumulative average dairy intake.
#'
#' Cumulative average dairy intake
#' ────────────────────────────────
#' dairy_cumavg is the running mean of all non-missing daily dairy intakes from
#' OsteoLaus Baseline up to and including the current wave. This is the primary
#' exposure for Aims 1 and 2. Waves with missing FFQ data are skipped (the
#' running mean carries the last valid average forward); n_ffq_used records how
#' many observations fed each value.
#'
#' dplyr::cummean() is NOT used because it counts NAs in the denominator.
#' An explicit accumulator loop skips NAs correctly.
#'
#' Columns produced
#' ────────────────
#' pt, osteo_wave              join keys -> visits
#' .colaus_wave                matched CoLaus wave label (audit)
#' .gap_days                   |OsteoLaus date - CoLaus date| in days (audit)
#' dairy_total_gday            g/day instantaneous FFQ value
#' dairy_cumavg                g/day cumulative mean (PRIMARY EXPOSURE)
#' dairy_cumavg_lag1           dairy_cumavg lagged by 1 wave (sensitivity)
#' dairy_total_lag1            dairy_total_gday lagged by 1 wave (sensitivity)
#' n_ffq_used                  non-NA FFQ obs feeding this cumavg (audit)
#' dairy_fermented_gday        g/day fermented dairy
#' dairy_highfat_gday          g/day high-fat dairy
#' dairy_lowfat_gday           g/day low-fat dairy
#' dairy_non_fermented_gday    g/day non-fermented dairy
#' energy_kcal                 kcal/day total energy (from sumtot3)
#' protein_pct                 % energy from protein (from pct_prot3)
#' fat_pct                     % energy from fat (from pct_lipi3)
#' calcium_mg                  mg/day calcium (from sumcalc3)
#' vitd_ug                     ug/day vitamin D (from sumvitd3)
#' handgrip                     kg CoLaus grip strength (matched CoLaus wave)
#' sbsmk                       smoking status (factor)
#' alcohol_category            alcohol intake category (ordered factor)
#' pa_levels                   physical activity level (ordered factor)
#' diabetes_status             diabetes classification (factor)
#' cdv_event                   CVD event composite (factor); filled forward
#'                             from Baseline — flags only collected at Baseline
#' HTN_status                  hypertension (factor No/Yes)
#' hrt_status                  HRT status (factor)
#' hypolip_drug_status         statin/lipid-lowering drug (Yes/No)
#' corticoids_status           systemic corticosteroid use (Yes/No)
#' calcium_status              calcium supplement use (Yes/No)
#' vitD_status                 vitamin D supplement use (Yes/No)
#' bisphosphonate_status       bisphosphonate use (Yes/No)
#' dairy_total_gday_oob        TRUE where instantaneous value is implausible (original value retained)
#' energy_kcal_oob             TRUE where energy is outside 500-4200 kcal (original value retained)
#'
#' Integrity checks
#' ────────────────
#' ABORT  Duplicate pt x osteo_wave rows
#' WARN   dairy_cumavg missing in > 20% of rows
#'
#' @param colaus_derived  Output of derive_colaus().
#' @param visits          Output of build_visits() — provides the pt x osteo_wave
#'   skeleton and exam dates.
#' @param gap_warn_days   Days threshold for wave-gap warnings. Default 912.
#' @return Tibble with one row per participant x OsteoLaus wave.
build_exposures <- function(colaus_derived,
                            visits,
                            wave_match    = NULL,
                            gap_warn_days = 912L) {
    
    # ── Step 1: CoLaus date spine (F1-F3 only; Baseline excluded) --------------
    # CoLaus Baseline (2003-2008) predates all OsteoLaus visits and has no FFQ.
    colaus_dates <- colaus_derived |>
        dplyr::filter(.wave != "Baseline") |>
        dplyr::select(
            pt,
            colaus_wave = .wave,
            colaus_date = exam_date_iso
        )
    
    # ── Step 2: OsteoLaus visit skeleton ----------------------------------------
    osteo_visits <- dplyr::select(
        visits, pt, osteo_wave, osteo_date = exam_date
    )
    
    # ── Step 3: Wave matching ----------------------------------------------------
    # If wave_match is supplied (pre-computed by the targets pipeline to avoid
    # running the date-matching algorithm twice), use it directly.
    # Otherwise compute it here.
    if (is.null(wave_match)) {
        wave_match <- .match_waves_by_date(
            osteo_visits  = osteo_visits,
            colaus_dates  = colaus_dates,
            gap_warn_days = gap_warn_days
        )
    }
    
    # ── Step 4: Time-varying variables from CoLaus ------------------------------
    # Column names map from raw post-derive names to self-documenting analytical
    # names. The dairy columns use the _gday suffix produced by derive_dairy().
    # Covariate names match exactly what derive_colaus() produces.
    colaus_tv <- colaus_derived |>
        dplyr::select(
            pt,
            colaus_wave = .wave,
            
            # Dairy exposure variables (produced by derive_dairy() with _gday suffix)
            dplyr::any_of(c(
                "dairy_total_gday",
                "dairy_fermented_gday",
                "dairy_highfat_gday",
                "dairy_lowfat_gday",
                "dairy_non_fermented_gday"
            )),
            
            # Dietary confounders — renamed to self-documenting analytical names.
            # Use any_of() with rename_with() instead of bare = to avoid errors
            # at waves where FFQ columns are absent (e.g. CoLaus Baseline).
            dplyr::any_of(c(
                energy_kcal = "sumtot3",
                protein_pct = "pct_prot3",
                fat_pct     = "pct_lipi3",
                calcium_mg  = "sumcalc3",
                vitd_ug     = "sumvitd3"
            )),
            
            # CoLaus handgrip (single measure per wave; combined with OsteoLaus
            # HGS_MAX into handgrip_max_all in build_sarcopenia() after this step)
            dplyr::any_of("handgrip"),
            
            # Time-varying behavioural covariates (exact names from derive_colaus)
            dplyr::any_of(c(
                "sbsmk",
                "alcohol_category",   # created by derive_alcohol()
                "pa_levels"           # created by derive_pa()
            )),
            
            # Time-varying clinical covariates (exact names from derive_colaus)
            dplyr::any_of(c(
                "diabetes_status",
                "cdv_event",
                "HTN_status",
                "hrt_status",
                "hypolip_drug_status",
                "corticoids_status",
                "vitD_status",
                "calcium_status",
                "bisphosphonate_status"
            ))
        )
    
    # ── Step 4b: Carry Baseline-only variables forward to follow-up waves -------
    # Some CoLaus variables are only collected at Baseline (e.g. cdv_event from
    # CVD flags which are asked once). The wave_match joins F1-F3 rows from
    # colaus_tv, so these variables would be NA for all matched rows.
    # Solution: within each pt, fill these variables forward from Baseline so
    # F1/F2/F3 inherit the Baseline value.
    # cdv_event is a cumulative event indicator — once Yes, always Yes.
    # Other candidates: hrt_status (if only derived from Baseline esthrp).
    .baseline_only_vars <- c("cdv_event")
    .present_bsonly     <- intersect(.baseline_only_vars, names(colaus_tv))
    
    if (length(.present_bsonly) > 0) {
        colaus_tv <- colaus_tv |>
            dplyr::arrange(pt, colaus_wave) |>
            dplyr::group_by(pt) |>
            tidyr::fill(dplyr::all_of(.present_bsonly), .direction = "down") |>
            dplyr::ungroup()
    }
    
    # ── Step 5: Join CoLaus data onto visit skeleton ----------------------------
    exposures <- wave_match |>
        dplyr::left_join(
            colaus_tv,
            by           = c("pt", "colaus_wave"),
            relationship = "many-to-one"
        ) |>
        dplyr::rename(
            .colaus_wave = colaus_wave,
            .gap_days    = gap_days
        )
    
    # ── Step 6: Cumulative average dairy intake ---------------------------------
    wave_order <- dplyr::select(visits, pt, osteo_wave, osteo_wave_num)
    
    exposures <- exposures |>
        dplyr::left_join(wave_order, by = c("pt", "osteo_wave")) |>
        dplyr::arrange(pt, osteo_wave_num) |>
        dplyr::group_by(pt) |>
        dplyr::mutate(
            n_ffq_used        = cumsum(!is.na(dairy_total_gday)),
            dairy_cumavg      = .cumavg_ignore_na(dairy_total_gday),
            dairy_cumavg_lag1 = dplyr::lag(dairy_cumavg,     n = 1L),
            dairy_total_lag1  = dplyr::lag(dairy_total_gday, n = 1L)
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-osteo_wave_num)
    
    # ── Step 7: Range checks ----------------------------------------------------
    exposures <- .apply_range_limit(
        exposures, "dairy_total_gday",
        lo = RANGE_LIMITS[["dairy_total_gday"]][["lo"]],
        hi = RANGE_LIMITS[["dairy_total_gday"]][["hi"]]
    )
    exposures <- .apply_range_limit(
        exposures, "energy_kcal",
        lo = RANGE_LIMITS[["energy_kcal"]][["lo"]],
        hi = RANGE_LIMITS[["energy_kcal"]][["hi"]]
    )
    
    # ── Integrity checks ────────────────────────────────────────────────────────
    if (anyDuplicated(exposures[c("pt", "osteo_wave")]) > 0)
        cli::cli_abort(
            "build_exposures(): duplicate pt \u00d7 osteo_wave rows — \
       check wave_match for many-to-many issues."
        )
    
    pct_na_cumavg <- mean(is.na(exposures$dairy_cumavg))
    if (pct_na_cumavg > 0.20)
        cli::cli_warn(
            "build_exposures(): dairy_cumavg missing for \
       {scales::percent(pct_na_cumavg, accuracy = 0.1)} of rows \
       (threshold 20%) — check FFQ linkage and wave matching."
        )
    
    cli::cli_inform(c(
        "exposures: {nrow(exposures)} rows | \
     {dplyr::n_distinct(exposures$pt)} participants",
        "i" = "dairy_cumavg present for \
           {scales::percent(1 - pct_na_cumavg, accuracy = 0.1)} of visits",
        "i" = "median n_ffq_used per visit: \
           {median(exposures$n_ffq_used, na.rm = TRUE)}"
    ))
    
    exposures
}


# =============================================================================
# PRIVATE HELPER — cumulative mean ignoring NAs
# =============================================================================

#' Compute a running mean over a numeric vector, skipping NA values.
#'
#' When the current element is NA, the running mean is carried forward from
#' the last observed value. Returns NA until the first non-missing value.
#'
#' Used instead of dplyr::cummean() because cummean() includes NAs in the
#' denominator, producing incorrect (downward biased) counts for participants
#' with missing FFQ assessments at intermediate waves.
#'
#' @param x Numeric vector (one participant's dairy intakes in wave order).
#' @return Numeric vector of the same length.
.cumavg_ignore_na <- function(x) {
    out <- numeric(length(x))
    s   <- 0
    k   <- 0L
    for (i in seq_along(x)) {
        if (!is.na(x[i])) {
            s <- s + x[i]
            k <- k + 1L
        }
        out[i] <- if (k > 0L) s / k else NA_real_
    }
    out
}