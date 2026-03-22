# =============================================================================
# R/07_build_visits.R
# =============================================================================
# Builds the visits table: one row per participant x OsteoLaus wave.
#
# Grain:   pt x osteo_wave
# Source:  osteo_derived — output of derive_osteo() only (no cross-cohort step).
#          handgrip_max_all and definitive sarcopenia staging are NOT present
#          yet; they are added by build_sarcopenia() after exposures
#          are built.
# Output:  visits -> used by build_exposures() and build_sarcopenia()
#
# Note on audit columns:
#   .colaus_wave and .gap_days are NOT initialised here as NA placeholders.
#   These columns live in the exposures table (produced by build_exposures()),
#   which is the correct grain for wave-matching metadata. If both tables need
#   to be reported together, join exposures onto visits by pt + osteo_wave.
#
# =============================================================================


#' Build the visits table.
#'
#' Selects outcome and covariate columns from osteo_derived (post-combine),
#' applies physiological range limits (flagging implausible values rather than
#' silently dropping them), and computes time_since_bsl_yr from each
#' participant's OsteoLaus Baseline exam date.
#'
#' Columns produced
#' ────────────────
#' pt, osteo_wave, osteo_wave_num
#' exam_date             Date of this OsteoLaus visit
#' time_since_bsl_yr     Years since OsteoLaus Baseline (0.0 at Baseline)
#' HGS_peak               kg peak grip OsteoLaus V5 only
#'                       (handgrip_max_all added later by build_sarcopenia())
#' ALM_HT2               kg/m2 — primary mass outcome (ALMI)
#' ALM_BMI               ALM/BMI ratio — sensitivity mass outcome
#' gait_speed            m/s — primary performance outcome (6MGS renamed)
#' ewgsop2_sarcopenia_stage  added by build_sarcopenia() — not present here
#' fnih_sarcopenia           added by build_sarcopenia() — not present here
#' Age, BMI, Weight, Height, BMI_category
#' TUG_TIME, TUG_SCORE, TUG step flags (V4/V5 only)
#' SARCF_TOTAL and item scores (V5 only)
#' ALM, ALM_WT, ARMS/LEGS/TRUNK/WBTOT lean mass (DXA sensitivity)
#' *_oob                 TRUE where value is outside RANGE_LIMITS (original value retained)
#'
#' Integrity checks
#' ────────────────
#' ABORT  Duplicate pt x osteo_wave rows
#' ABORT  Any missing exam_date
#' WARN   Non-Baseline rows with time_since_bsl_yr <= 0
#' WARN   Any missing time_since_bsl_yr
#'
#' @param osteo_derived Output of derive_osteo() — OsteoLaus tibble.
#'   handgrip_max_all and sarcopenia staging are not yet present; they are
#'   added by build_sarcopenia() after exposures are available.
#' @param participants  Output of build_participants() (provides baseline dates).
#' @return Tibble with one row per participant x OsteoLaus wave.
build_visits <- function(osteo_derived, participants) {
    
    # ── Rename wave metadata and gait speed -------------------------------------
    # `6MGS` is a syntactically invalid R name (starts with digit); rename to
    # gait_speed here so all downstream code uses a valid column name.
    # `6MGS` was already renamed to `gait_speed` in harmonise_osteo().
    visits <- osteo_derived |>
        dplyr::rename(
            osteo_wave     = .wave,
            osteo_wave_num = .wave_num,
            exam_date      = exam_date_iso
        )
    
    # ── Select outcome and covariate columns ------------------------------------
    visits <- dplyr::select(visits,
                            pt, osteo_wave, osteo_wave_num, exam_date,
                            
                            # Combined grip strength (CoLaus + OsteoLaus)
                            dplyr::any_of("handgrip_max_all"),
                            
                            # Primary outcomes
                            dplyr::any_of(c("HGS_peak", "ALM_HT2", "ALM_BMI", "gait_speed")),
                            
                            # EWGSOP2 sarcopenia classification
                            dplyr::any_of(c(
                                "ewgsop2_sarcopenia_stage",
                                "ewgsop2_low_strength", "ewgsop2_low_mass", "ewgsop2_low_perf"
                            )),
                            
                            # FNIH sarcopenia classification (sensitivity)
                            dplyr::any_of(c(
                                "fnih_sarcopenia",
                                "fnih_low_strength", "fnih_low_mass"
                            )),
                            
                            # Anthropometrics
                            dplyr::any_of(c("Age", "BMI", "Weight", "Height", "BMI_category")),
                            
                            # Physical performance extras (V4/V5 only)
                            dplyr::any_of(c(
                                "TUG_TIME", "TUG_SCORE",
                                "TUG_GETUP", "TUG_GO", "TUG_TURN", "TUG_GOBACKSIT"
                            )),
                            
                            # SARC-F (V5 only)
                            dplyr::any_of(c(
                                "SARCF_TOTAL",
                                "SARCF_STRENGHT", "SARCF_WALK", "SARCF_CHAIR",
                                "SARCF_STAIRS",   "SARCF_FALL"
                            )),
                            
                            # DXA lean mass detail (sensitivity analyses)
                            dplyr::any_of(c(
                                "ALM", "ALM_WT",
                                "ARMS_LEAN_MASS", "LEGS_LEAN_MASS",
                                "TRUNK_LEAN_MASS", "WBTOT_LEAN_MASS"
                            ))
    )
    
    # ── Physiological range limits ----------------------------------------------
    # Out-of-range values are flagged in *_oob columns; original values are retained.
    visits <- .apply_all_range_limits(visits)
    
    # ── time_since_bsl_yr -------------------------------------------------------
    bsl_dates <- dplyr::select(participants, pt, baseline_osteo_date)
    
    visits <- visits |>
        dplyr::left_join(bsl_dates, by = "pt") |>
        dplyr::mutate(
            time_since_bsl_yr = as.numeric(exam_date - baseline_osteo_date) / 365.25
        ) |>
        dplyr::select(-baseline_osteo_date)
    
    # ── Integrity checks ────────────────────────────────────────────────────────
    if (anyDuplicated(visits[c("pt", "osteo_wave")]) > 0)
        cli::cli_abort(
            "build_visits(): duplicate pt \u00d7 osteo_wave rows — \
       check osteo_derived for duplicates."
        )
    
    if (any(is.na(visits$exam_date)))
        cli::cli_abort(
            "build_visits(): {sum(is.na(visits$exam_date))} row(s) with missing \
       exam_date — all visits must have a date."
        )
    
    n_neg <- sum(
        visits$osteo_wave != "Baseline" & visits$time_since_bsl_yr <= 0,
        na.rm = TRUE
    )
    if (n_neg > 0)
        cli::cli_warn(
            "build_visits(): {n_neg} non-Baseline visit(s) with \
       time_since_bsl_yr <= 0 — check baseline date alignment."
        )
    
    n_miss_t <- sum(is.na(visits$time_since_bsl_yr))
    if (n_miss_t > 0)
        cli::cli_warn(
            "build_visits(): {n_miss_t} row(s) with missing time_since_bsl_yr \
       (participant has no OsteoLaus Baseline row)."
        )
    
    cli::cli_inform(
        "visits: {nrow(visits)} rows | \
     {dplyr::n_distinct(visits$pt)} participants | \
     {dplyr::n_distinct(visits$osteo_wave)} waves"
    )
    
    visits
}