# =============================================================================
# R/functions_freeze.R
# =============================================================================
# Apply final study inclusion/exclusion criteria and select the analytical
# columns. After this function the data is frozen for analysis.
#
# Input:  derived_list — output of derive_variables() ($data, $log)
# Output: a long-format tibble (one row per participant × OsteoLaus visit)
#         ready for modelling and descriptives.
#
# Note on in_osteolaus:
#   With OsteoLaus as the backbone, every row is an OsteoLaus row, so
#   in_osteolaus = TRUE for all. It is retained as an explicit column for
#   consistency with downstream code (filters, descriptives) and to support
#   any future sensitivity analyses that may stack CoLaus-only rows.

#' Apply final exclusions and produce the analytical dataset
#'
#' Steps:
#'   1. Flag OsteoLaus membership (always TRUE on this backbone).
#'   2. Exclude participants missing the sarcopenia outcome at OsteoLaus
#'      Baseline (primary outcome required for all longitudinal analyses).
#'   3. Flag FFQ availability (>=1 non-missing dairy_total across any visit).
#'   4. Flag longitudinal eligibility (>=1 follow-up visit with outcome).
#'   5. Select the analytical column set (any_of() for optional columns).
#'
#' @param derived_list Output of derive_variables() ($data, $log).
#' @return A long-format tibble ready for analysis.
freeze_dataset <- function(derived_list) {
    data <- derived_list$data
    
    # ── 1. Flag OsteoLaus membership -------------------------------------------
    # All rows in the OsteoLaus-backbone dataset are OsteoLaus participants.
    # Participants not found in CoLaus have NA in CoLaus-sourced columns but
    # are still OsteoLaus members.
    data <- dplyr::mutate(data, in_osteolaus = .cohort == "OsteoLaus")
    
    # ── 2. Require sarcopenia outcome at OsteoLaus Baseline --------------------
    no_outcome <- data |>
        dplyr::filter(in_osteolaus & .wave == "Baseline" &
                          is.na(ewgsop2_sarcopenia_stage)) |>
        dplyr::pull(pt)
    
    data <- dplyr::filter(data, !pt %in% no_outcome)
    message(glue::glue(
        "{length(no_outcome)} participant(s) excluded: ",
        "missing sarcopenia outcome at OsteoLaus Baseline"
    ))
    
    # ── 3. Flag FFQ availability -----------------------------------------------
    # FFQ data come from CoLaus (F1 onwards), attached to OsteoLaus visits by
    # the "last preceding CoLaus wave" rule. A participant has FFQ data if
    # dairy_total is non-missing at any OsteoLaus visit.
    ffq_pts <- data |>
        dplyr::filter(!is.na(dairy_total)) |>
        dplyr::pull(pt) |>
        unique()
    data <- dplyr::mutate(data, has_ffq = pt %in% ffq_pts)
    
    # ── 4. Flag longitudinal eligibility (>=1 follow-up visit with outcome) ----
    # Follow-up visits are any OsteoLaus wave after Baseline (V2, V3, V4, V5).
    fu_pts <- data |>
        dplyr::filter(in_osteolaus & .wave != "Baseline" &
                          !is.na(ewgsop2_sarcopenia_stage)) |>
        dplyr::pull(pt) |>
        unique()
    data <- dplyr::mutate(data, has_followup_outcome = pt %in% fu_pts)
    
    # ── 5. Select analytical columns -------------------------------------------
    data |>
        dplyr::select(
            # Keys & metadata
            pt, .cohort, .wave, .wave_num, exam_date_iso,
            # Audit columns from merge
            dplyr::any_of(c(".colaus_wave", ".gap_days", ".match_type")),
            # Study flags
            in_osteolaus, has_ffq, has_followup_outcome,
            
            # Fixed covariate — Low/Medium/High (ISCED grouping, from CoLaus)
            dplyr::any_of("education_level"),
            
            # Time-varying covariates
            age,
            dplyr::any_of(c(
                "BMI", "BMI_category", "bmpsc",
                "alcohol_category", "alc_gday",
                "sbsmk", "sbsmk_imputed",
                "diabetes_status",
                "cdv_event",
                "hrt_status",
                "corticoids_status", "vitD_status", "calcium_status",
                "sumtot1", "sumtot1_flag"
            )),
            
            # Exposures (from CoLaus FFQ, attached by last-preceding-wave rule)
            dplyr::any_of(c(
                "dairy_total", "dairy_fermented", "dairy_non_fermented",
                "dairy_lowfat", "dairy_highfat"
            )),
            
            # Outcomes & components (from OsteoLaus DXA + physical tests)
            dplyr::any_of(c(
                "ewgsop2_sarcopenia_stage",
                "handgrip", "handgrip_flag",   # CoLaus grip (earlier waves)
                "HGS_MAX",  "HGS_MAX_flag",    # OsteoLaus grip (V5)
                "ALM",                          # raw grams (OsteoLaus DXA)
                "ALM_HT2",                      # pre-computed kg/m^2 (OsteoLaus)
                "alm_ht2",                      # analytical kg/m^2 (derived)
                "6MGS"                          # gait speed m/s (OsteoLaus)
            ))
        )
}