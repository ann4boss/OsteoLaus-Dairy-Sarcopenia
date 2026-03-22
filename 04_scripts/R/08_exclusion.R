# =============================================================================
# R/08_exclusion.R
# =============================================================================
# Applies exclusion criteria and produces:
#   1. participants enriched with inclusion_status, inclusion_reason, eligible_*
#   2. analysis_long — all pts with inclusion_status != "no"
#   3. flow_log_hard, flow_log_outcomes
#
# Inclusion status
# ----------------
#   "yes"     Passes all hard criteria AND complete for all outcomes
#   "partial" Passes hard criteria BUT missing data for at least one outcome
#             OR missing a dairy sub-category (kept in analysis_long but
#             flagged ineligible for affected sub-category analyses)
#   "no"      Fails at least one hard criterion — removed from analysis_long
#
# Hard criteria (any failure -> "no")
# ------------------------------------
#   QC   assert_no_failures() identity/integrity
#   1.   Missing pt
#   2.   No OsteoLaus Baseline visit
#   3.   Missing baseline exam date
#   4.   Fewer than min_visits OsteoLaus visits (any waves, not necessarily
#        Baseline or consecutive)
#   5.   Any FFQ wave: energy outside 500-4200 kcal/day
#   6.   HGS_peak out-of-range at any wave
#   7.   Required covariate never observed: education_level, alcohol_category,
#        sbsmk, diabetes_status, HTN_status, cdv_event, hrt_status, pa_levels,
#        hypolip_drug_status, corticoids_status, vitD_status, calcium_status,
#        bisphosphonate_status — each must be non-NA at least once
#   8.   dairy_total_gday missing at ANY wave where the participant has an
#        FFQ visit (total dairy must be complete for all measured time points)
#   9.   dairy_total_gday out-of-range at any wave
#
# Outcome-specific / partial criteria (any failure -> "partial")
# --------------------------------------------------------------
#   ewgsop2_sarcopenia_stage (primary):
#     requires ewgsop2_sarcopenia_stage, handgrip_max_all, ALM_HT2, ALM_BMI
#     all non-missing at OsteoLaus Baseline
#
#   handgrip_max_all (grip strength trajectory):
#     requires handgrip_max_all non-missing at OsteoLaus Baseline
#
#   ALM_HT2 (lean mass trajectory):
#     requires ALM_HT2 non-missing at OsteoLaus Baseline
#
#   gait_speed (performance trajectory):
#     Baseline absence expected. Requires gait_speed at >= 1 of V4 or V5.
#
#   fnih_sarcopenia (sensitivity):
#     requires handgrip_max_all AND ALM_BMI non-missing at OsteoLaus Baseline
#
#   dairy sub-categories (exposure completeness):
#     dairy_fermented_gday, dairy_non_fermented_gday, dairy_lowfat_gday,
#     dairy_highfat_gday — each must be non-NA at least once. Failure leads
#     to "partial" (sub-category analysis excluded), not "no".
#
# Loaded by tar_source() in _targets.R - no direct source() calls needed.
# =============================================================================

# Covariates that must be non-NA at least once across all the participant's
# visits in exposures. education_level comes from participants; all others
# from exposures.
.REQUIRED_COVARIATES_PARTICIPANTS <- c(
    "education_level"
)
.REQUIRED_COVARIATES_EXPOSURES <- c(
    "alcohol_category", "sbsmk", "diabetes_status",
    "HTN_status", "cdv_event", "hrt_status", "pa_levels",
    "hypolip_drug_status", "corticoids_status",
    "vitD_status", "calcium_status", "bisphosphonate_status"
)

# Dairy sub-categories that drive partial exclusion only
.DAIRY_SUBCATEGORY_COLS <- c(
    "dairy_fermented_gday", "dairy_non_fermented_gday",
    "dairy_lowfat_gday", "dairy_highfat_gday"
)

#' Apply exclusion criteria and enrich the participants table.
#'
#' @param participants  Output of build_participants().
#' @param visits        Output of build_sarcopenia() (visits_staged).
#' @param exposures     Output of build_exposures().
#' @param qc_exclusions Output of assert_no_failures(). NULL to skip.
#' @param min_visits    Minimum OsteoLaus visits required. Default 2.
#' @return Named list: participants_flagged, data, flow_log_hard,
#'   flow_log_outcomes.
freeze_dataset <- function(participants,
                           visits,
                           exposures,
                           qc_exclusions = NULL,
                           min_visits    = 2L) {
    
    # =========================================================================
    # STEP 1: Compute per-participant hard exclusion flags
    # =========================================================================
    
    # QC failures
    qc_flag <- if (!is.null(qc_exclusions) && nrow(qc_exclusions) > 0) {
        qc_exclusions |>
            dplyr::mutate(pt = as.integer(pt)) |>
            dplyr::select(pt, qc_exclude_reason)
    } else {
        tibble::tibble(pt = integer(), qc_exclude_reason = character())
    }
    
    # [3] Baseline exam date present
    bsl_date_flag <- visits |>
        dplyr::filter(osteo_wave == "Baseline") |>
        dplyr::select(pt, bsl_exam_date = exam_date) |>
        dplyr::mutate(has_bsl_date = !is.na(bsl_exam_date)) |>
        dplyr::select(pt, has_bsl_date)
    
    # [5] Any FFQ wave with implausible energy
    energy_flag <- exposures |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            n_ffq          = sum(!is.na(energy_kcal)),
            n_energy_oob   = sum(energy_kcal_oob, na.rm = TRUE),
            any_energy_oob = n_ffq > 0L & n_energy_oob > 0L,
            .groups = "drop"
        ) |>
        dplyr::select(pt, any_energy_oob)
    
    # [6] Any wave with implausible HGS_peak
    hgs_oob_flag <- if ("HGS_peak_oob" %in% names(visits)) {
        visits |>
            dplyr::group_by(pt) |>
            dplyr::summarise(
                any_hgs_oob = any(HGS_peak_oob, na.rm = TRUE),
                .groups = "drop"
            )
    } else {
        tibble::tibble(pt = unique(visits$pt), any_hgs_oob = FALSE)
    }
    
    # [7] Required covariates: each must be non-NA at least once.
    # education_level comes from participants (fixed); others from exposures.
    # Compute education_level missingness at participant level.
    # The scalar check "education_level" %in% names(participants) must be
    # resolved BEFORE entering dplyr::mutate() — dplyr::if_else() requires
    # all arguments to be the same length, so a scalar condition with a
    # vector true/false is an error.
    edu_flag <- if ("education_level" %in% names(participants)) {
        participants |>
            dplyr::select(pt, education_level) |>
            dplyr::mutate(missing_education = is.na(education_level)) |>
            dplyr::select(pt, missing_education)
    } else {
        tibble::tibble(pt = participants$pt, missing_education = FALSE)
    }
    
    covariate_flag <- exposures |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            dplyr::across(
                dplyr::any_of(.REQUIRED_COVARIATES_EXPOSURES),
                ~ all(is.na(.x)),
                .names = "always_na_{.col}"
            ),
            .groups = "drop"
        )
    
    # Combine: any required covariate always NA -> flag with the first offender
    covariate_missing_reason <- covariate_flag |>
        tidyr::pivot_longer(
            cols      = dplyr::starts_with("always_na_"),
            names_to  = "covariate",
            values_to = "always_na",
            names_prefix = "always_na_"
        ) |>
        dplyr::filter(always_na) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            missing_covariate_reason = paste(
                "Required covariate always missing:", covariate,
                collapse = "; "
            ),
            .groups = "drop"
        )
    
    # [8] dairy_total_gday missing at any wave where participant has an FFQ visit.
    # An "FFQ visit" is any row in exposures for that participant (even if dairy
    # is NA). The rule: for pts who have >= 1 FFQ row, dairy_total_gday must be
    # non-NA at ALL of those rows.
    dairy_complete_flag <- exposures |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            n_ffq_rows       = dplyr::n(),
            n_dairy_total_na = sum(is.na(dairy_total_gday)),
            dairy_total_incomplete = n_ffq_rows > 0L & n_dairy_total_na > 0L,
            .groups = "drop"
        ) |>
        dplyr::select(pt, dairy_total_incomplete)
    
    # [9] dairy_total_gday out-of-range at any wave
    dairy_oob_flag <- if ("dairy_total_gday_oob" %in% names(exposures)) {
        exposures |>
            dplyr::group_by(pt) |>
            dplyr::summarise(
                any_dairy_oob = any(dairy_total_gday_oob, na.rm = TRUE),
                .groups = "drop"
            )
    } else {
        tibble::tibble(pt = unique(exposures$pt), any_dairy_oob = FALSE)
    }
    
    # ── Assemble hard exclusion table ─────────────────────────────────────────
    hard <- participants |>
        dplyr::select(pt, n_osteo_visits) |>
        dplyr::left_join(bsl_date_flag,          by = "pt") |>
        dplyr::left_join(energy_flag,             by = "pt") |>
        dplyr::left_join(hgs_oob_flag,            by = "pt") |>
        dplyr::left_join(edu_flag,                by = "pt") |>
        dplyr::left_join(covariate_missing_reason, by = "pt") |>
        dplyr::left_join(dairy_complete_flag,     by = "pt") |>
        dplyr::left_join(dairy_oob_flag,          by = "pt") |>
        dplyr::left_join(qc_flag,                 by = "pt") |>
        dplyr::mutate(
            hard_exclude        = FALSE,
            hard_exclude_reason = NA_character_,
            
            # QC
            hard_exclude = dplyr::if_else(
                !hard_exclude & !is.na(qc_exclude_reason),
                TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & !is.na(qc_exclude_reason),
                qc_exclude_reason, hard_exclude_reason),
            
            # 1. Missing pt
            hard_exclude = dplyr::if_else(
                !hard_exclude & is.na(pt), TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & is.na(pt),
                "Missing participant ID", hard_exclude_reason),
            
            # 2. No OsteoLaus Baseline visit
            hard_exclude = dplyr::if_else(
                !hard_exclude & is.na(n_osteo_visits), TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & is.na(n_osteo_visits),
                "No OsteoLaus Baseline visit", hard_exclude_reason),
            
            # 3. Missing baseline exam date
            hard_exclude = dplyr::if_else(
                !hard_exclude & (is.na(has_bsl_date) | !has_bsl_date),
                TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & (is.na(has_bsl_date) | !has_bsl_date),
                "Missing baseline exam date", hard_exclude_reason),
            
            # 4. Fewer than min_visits visits (any waves)
            hard_exclude = dplyr::if_else(
                !hard_exclude & !is.na(n_osteo_visits) &
                    n_osteo_visits < min_visits,
                TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & !is.na(n_osteo_visits) &
                    n_osteo_visits < min_visits,
                as.character(glue::glue(
                    "Fewer than {min_visits} OsteoLaus visits")),
                hard_exclude_reason),
            
            # 5. Implausible energy at any FFQ wave
            hard_exclude = dplyr::if_else(
                !hard_exclude & !is.na(any_energy_oob) & any_energy_oob,
                TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & !is.na(any_energy_oob) & any_energy_oob,
                "Any FFQ wave: energy outside 500-4200 kcal/day",
                hard_exclude_reason),
            
            # 6. Implausible HGS_peak
            hard_exclude = dplyr::if_else(
                !hard_exclude & !is.na(any_hgs_oob) & any_hgs_oob,
                TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & !is.na(any_hgs_oob) & any_hgs_oob,
                "Implausible HGS_peak value (outside plausible range)",
                hard_exclude_reason),
            
            # 7a. education_level never observed
            hard_exclude = dplyr::if_else(
                !hard_exclude & !is.na(missing_education) & missing_education,
                TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & !is.na(missing_education) & missing_education,
                "Required covariate always missing: education_level",
                hard_exclude_reason),
            
            # 7b. Any required exposure covariate never observed
            hard_exclude = dplyr::if_else(
                !hard_exclude & !is.na(missing_covariate_reason),
                TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & !is.na(missing_covariate_reason),
                missing_covariate_reason, hard_exclude_reason),
            
            # 8. dairy_total_gday missing at any FFQ visit
            hard_exclude = dplyr::if_else(
                !hard_exclude & !is.na(dairy_total_incomplete) &
                    dairy_total_incomplete,
                TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & !is.na(dairy_total_incomplete) &
                    dairy_total_incomplete,
                "dairy_total_gday missing at one or more FFQ visits",
                hard_exclude_reason),
            
            # 9. dairy_total_gday out of range
            hard_exclude = dplyr::if_else(
                !hard_exclude & !is.na(any_dairy_oob) & any_dairy_oob,
                TRUE, hard_exclude),
            hard_exclude_reason = dplyr::if_else(
                !hard_exclude & !is.na(any_dairy_oob) & any_dairy_oob,
                "dairy_total_gday out of range at one or more FFQ visits",
                hard_exclude_reason)
        ) |>
        dplyr::select(pt, hard_exclude, hard_exclude_reason)
    
    # =========================================================================
    # STEP 2: Hard exclusion flow log
    # =========================================================================
    
    n_qc_excluded <- nrow(qc_flag)
    n_after_qc    <- nrow(hard) - n_qc_excluded
    
    standard_criteria <- c(
        "Missing participant ID",
        "No OsteoLaus Baseline visit",
        "Missing baseline exam date",
        as.character(glue::glue("Fewer than {min_visits} OsteoLaus visits")),
        "Any FFQ wave: energy outside 500-4200 kcal/day",
        "Implausible HGS_peak value (outside plausible range)",
        "Required covariate always missing: education_level",
        # covariate_missing_reason can contain multiple covariates per pt,
        # so we use a prefix match in the flow log helper
        "Required covariate always missing (exposure)",
        "dairy_total_gday missing at one or more FFQ visits",
        "dairy_total_gday out of range at one or more FFQ visits"
    )
    
    flow_after_qc <- dplyr::filter(
        hard,
        is.na(hard_exclude_reason) |
            !hard_exclude_reason %in% qc_flag$qc_exclude_reason
    )
    
    # Covariate criterion 7b uses freeform reason strings — count via prefix
    covariate_excl_n <- sum(
        !is.na(flow_after_qc$hard_exclude_reason) &
            startsWith(flow_after_qc$hard_exclude_reason,
                       "Required covariate always missing:") &
            flow_after_qc$hard_exclude_reason !=
            "Required covariate always missing: education_level",
        na.rm = TRUE
    )
    
    standard_flow_log <- .build_flow_log(
        flow        = flow_after_qc,
        criteria    = standard_criteria[-8L],  # exclude placeholder
        reason_col  = "hard_exclude_reason",
        total_label = "After QC exclusions"
    )
    
    # Insert covariate row after education row
    edu_row_idx <- which(standard_flow_log$reason ==
                             "Required covariate always missing: education_level")
    
    cov_row <- tibble::tibble(
        step        = standard_flow_log$step[edu_row_idx] + 0.5,
        reason      = "Required exposure covariate always missing",
        n_excluded  = covariate_excl_n,
        n_remaining = NA_integer_
    )
    
    standard_flow_log <- dplyr::bind_rows(
        standard_flow_log[seq_len(edu_row_idx), ],
        cov_row,
        standard_flow_log[seq(edu_row_idx + 1L, nrow(standard_flow_log)), ]
    ) |>
        dplyr::mutate(
            step        = seq_len(dplyr::n()) - 1L,
            n_remaining = dplyr::first(n_remaining[step == 0L]) -
                cumsum(dplyr::coalesce(n_excluded, 0L))
        )
    
    flow_log_hard <- dplyr::bind_rows(
        tibble::tibble(step = 0L, reason = "Total enrolled in OsteoLaus",
                       n_excluded = 0L, n_remaining = nrow(hard)),
        tibble::tibble(step = 1L,
                       reason      = "QC FAIL (identity/integrity check)",
                       n_excluded  = n_qc_excluded,
                       n_remaining = n_after_qc),
        dplyr::mutate(standard_flow_log[-1L, ],
                      step        = step + 1L,
                      n_remaining = n_after_qc - cumsum(dplyr::coalesce(n_excluded, 0L)))
    )
    
    cli::cli_inform(c(
        "i" = "Hard exclusion flow:",
        "*" = paste(capture.output(print(flow_log_hard, n = Inf)),
                    collapse = "\n")
    ))
    
    # =========================================================================
    # STEP 3: Outcome-specific eligibility flags (hard-included pts only)
    # =========================================================================
    
    included_pts <- hard |>
        dplyr::filter(!hard_exclude) |>
        dplyr::pull(pt)
    
    # Baseline outcome values
    bsl_outcomes <- visits |>
        dplyr::filter(osteo_wave == "Baseline", pt %in% included_pts) |>
        dplyr::select(pt, dplyr::any_of(c(
            "ewgsop2_sarcopenia_stage", "handgrip_max_all",
            "ALM_HT2", "ALM_BMI"
        )))
    
    # Gait speed at V4 or V5
    gait_v4v5 <- visits |>
        dplyr::filter(osteo_wave %in% c("V4", "V5"),
                      pt %in% included_pts,
                      !is.na(gait_speed)) |>
        dplyr::distinct(pt) |>
        dplyr::mutate(has_gait_v4v5 = TRUE)
    
    # Dairy sub-category completeness: at least one non-NA value across visits
    dairy_subcats <- exposures |>
        dplyr::filter(pt %in% included_pts) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            dplyr::across(
                dplyr::any_of(.DAIRY_SUBCATEGORY_COLS),
                ~ any(!is.na(.x)),
                .names = "has_{.col}"
            ),
            .groups = "drop"
        )
    
    # Two-pass mutate to allow ineligible_reason_* to reference eligible_*
    outcome_flags <- tibble::tibble(pt = included_pts) |>
        dplyr::left_join(bsl_outcomes,  by = "pt") |>
        dplyr::left_join(gait_v4v5,     by = "pt") |>
        dplyr::left_join(dairy_subcats, by = "pt") |>
        dplyr::mutate(
            has_gait_v4v5 = dplyr::coalesce(has_gait_v4v5, FALSE),
            # Default dairy sub-category flags to FALSE if column absent
            dplyr::across(
                dplyr::starts_with("has_dairy_"),
                ~ dplyr::coalesce(.x, FALSE)
            ),
            
            # ── Pass 1: eligibility flags ─────────────────────────────────────
            eligible_ewgsop2 = !is.na(ewgsop2_sarcopenia_stage) &
                !is.na(handgrip_max_all) &
                !is.na(ALM_HT2) & !is.na(ALM_BMI),
            eligible_hgs     = !is.na(handgrip_max_all),
            eligible_alm     = !is.na(ALM_HT2),
            eligible_gait    = has_gait_v4v5,
            eligible_fnih    = !is.na(handgrip_max_all) & !is.na(ALM_BMI),
            
            # Dairy sub-category eligibility (partial only — not hard)
            eligible_dairy_fermented      =
                dplyr::coalesce(has_dairy_fermented_gday,      FALSE),
            eligible_dairy_non_fermented  =
                dplyr::coalesce(has_dairy_non_fermented_gday,  FALSE),
            eligible_dairy_lowfat         =
                dplyr::coalesce(has_dairy_lowfat_gday,         FALSE),
            eligible_dairy_highfat        =
                dplyr::coalesce(has_dairy_highfat_gday,        FALSE)
        ) |>
        dplyr::mutate(
            # ── Pass 2: ineligibility reasons ────────────────────────────────
            ineligible_reason_ewgsop2 = dplyr::case_when(
                eligible_ewgsop2                ~ NA_character_,
                is.na(ewgsop2_sarcopenia_stage) ~ "Missing EWGSOP2 stage at Baseline",
                is.na(handgrip_max_all)         ~ "Missing handgrip_max_all at Baseline",
                is.na(ALM_HT2)                  ~ "Missing ALM_HT2 at Baseline",
                is.na(ALM_BMI)                  ~ "Missing ALM_BMI at Baseline",
                TRUE                            ~ "Missing Baseline component for EWGSOP2"
            ),
            ineligible_reason_hgs = dplyr::if_else(
                eligible_hgs, NA_character_,
                "Missing handgrip_max_all at Baseline"
            ),
            ineligible_reason_alm = dplyr::if_else(
                eligible_alm, NA_character_,
                "Missing ALM_HT2 at Baseline"
            ),
            ineligible_reason_gait = dplyr::if_else(
                eligible_gait, NA_character_,
                "No gait speed measurement at V4 or V5"
            ),
            ineligible_reason_fnih = dplyr::case_when(
                eligible_fnih                             ~ NA_character_,
                is.na(handgrip_max_all) & is.na(ALM_BMI) ~
                    "Missing handgrip_max_all and ALM_BMI at Baseline",
                is.na(handgrip_max_all) ~
                    "Missing handgrip_max_all at Baseline",
                is.na(ALM_BMI) ~ "Missing ALM_BMI at Baseline"
            ),
            ineligible_reason_dairy_fermented = dplyr::if_else(
                eligible_dairy_fermented, NA_character_,
                "dairy_fermented_gday never observed"
            ),
            ineligible_reason_dairy_non_fermented = dplyr::if_else(
                eligible_dairy_non_fermented, NA_character_,
                "dairy_non_fermented_gday never observed"
            ),
            ineligible_reason_dairy_lowfat = dplyr::if_else(
                eligible_dairy_lowfat, NA_character_,
                "dairy_lowfat_gday never observed"
            ),
            ineligible_reason_dairy_highfat = dplyr::if_else(
                eligible_dairy_highfat, NA_character_,
                "dairy_highfat_gday never observed"
            )
        ) |>
        dplyr::select(
            pt,
            dplyr::starts_with("eligible_"),
            dplyr::starts_with("ineligible_reason_")
        )
    
    # =========================================================================
    # STEP 4: Per-outcome flow logs
    # =========================================================================
    
    outcome_specs <- list(
        ewgsop2               = list(
            flag   = "eligible_ewgsop2",
            reason = "ineligible_reason_ewgsop2",
            label  = "EWGSOP2 sarcopenia stage (primary)"),
        hgs                   = list(
            flag   = "eligible_hgs",
            reason = "ineligible_reason_hgs",
            label  = "Grip strength (handgrip_max_all at Baseline)"),
        alm                   = list(
            flag   = "eligible_alm",
            reason = "ineligible_reason_alm",
            label  = "Lean mass index (ALM_HT2 at Baseline)"),
        gait                  = list(
            flag   = "eligible_gait",
            reason = "ineligible_reason_gait",
            label  = "Gait speed (V4 or V5)"),
        fnih                  = list(
            flag   = "eligible_fnih",
            reason = "ineligible_reason_fnih",
            label  = "FNIH sarcopenia (sensitivity)"),
        dairy_fermented       = list(
            flag   = "eligible_dairy_fermented",
            reason = "ineligible_reason_dairy_fermented",
            label  = "Fermented dairy sub-category"),
        dairy_non_fermented   = list(
            flag   = "eligible_dairy_non_fermented",
            reason = "ineligible_reason_dairy_non_fermented",
            label  = "Non-fermented dairy sub-category"),
        dairy_lowfat          = list(
            flag   = "eligible_dairy_lowfat",
            reason = "ineligible_reason_dairy_lowfat",
            label  = "Low-fat dairy sub-category"),
        dairy_highfat         = list(
            flag   = "eligible_dairy_highfat",
            reason = "ineligible_reason_dairy_highfat",
            label  = "High-fat dairy sub-category")
    )
    
    flow_log_outcomes <- purrr::imap(outcome_specs, function(spec, nm) {
        log <- .build_outcome_flow_log(
            outcome_flags   = outcome_flags,
            flag_col        = spec$flag,
            reason_col      = spec$reason,
            n_hard_included = length(included_pts),
            outcome_label   = spec$label
        )
        cli::cli_inform(c(
            "i" = "Outcome flow - {spec$label}:",
            "*" = paste(capture.output(print(log, n = Inf)), collapse = "\n")
        ))
        log
    })
    
    # =========================================================================
    # STEP 5: Derive inclusion_status and inclusion_reason
    # =========================================================================
    
    inelig_reasons <- outcome_flags |>
        tidyr::pivot_longer(
            cols         = dplyr::starts_with("ineligible_reason_"),
            names_to     = "outcome",
            values_to    = "reason",
            names_prefix = "ineligible_reason_"
        ) |>
        dplyr::filter(!is.na(reason)) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            partial_reasons = paste(
                glue::glue("{outcome}: {reason}"), collapse = "; "
            ),
            .groups = "drop"
        )
    
    fully_eligible <- outcome_flags |>
        dplyr::mutate(
            all_eligible = dplyr::if_all(
                dplyr::starts_with("eligible_"), ~ !is.na(.x) & .x
            )
        ) |>
        dplyr::select(pt, all_eligible)
    
    inclusion <- dplyr::bind_rows(
        hard |>
            dplyr::filter(hard_exclude) |>
            dplyr::transmute(
                pt,
                inclusion_status = "no",
                inclusion_reason = hard_exclude_reason
            ),
        tibble::tibble(pt = included_pts) |>
            dplyr::left_join(fully_eligible, by = "pt") |>
            dplyr::left_join(inelig_reasons, by = "pt") |>
            dplyr::transmute(
                pt,
                inclusion_status = dplyr::if_else(
                    all_eligible, "yes", "partial"
                ),
                inclusion_reason = dplyr::if_else(
                    all_eligible, NA_character_, partial_reasons
                )
            )
    ) |>
        dplyr::mutate(
            inclusion_status = factor(
                inclusion_status, levels = c("yes", "partial", "no")
            )
        )
    
    # =========================================================================
    # STEP 6: Enrich participants table
    # =========================================================================
    
    participants_flagged <- participants |>
        dplyr::left_join(inclusion, by = "pt") |>
        dplyr::left_join(
            dplyr::select(outcome_flags, pt,
                          dplyr::starts_with("eligible_")),
            by = "pt"
        ) |>
        dplyr::mutate(dplyr::across(
            dplyr::starts_with("eligible_"),
            ~ dplyr::coalesce(.x, FALSE)
        ))
    
    # =========================================================================
    # STEP 7: Build analysis_long
    # =========================================================================
    
    analysis_pts <- inclusion |>
        dplyr::filter(inclusion_status != "no") |>
        dplyr::pull(pt)
    
    analysis_long <- visits |>
        dplyr::filter(pt %in% analysis_pts) |>
        dplyr::left_join(
            dplyr::select(participants_flagged,
                          -dplyr::any_of(c("n_osteo_visits",
                                           "n_osteo_outcomes"))),
            by = "pt"
        ) |>
        dplyr::left_join(exposures, by = c("pt", "osteo_wave")) |>
        dplyr::arrange(pt, osteo_wave_num)
    
    # =========================================================================
    # STEP 8: Summary
    # =========================================================================
    
    n_yes     <- sum(participants_flagged$inclusion_status == "yes",    na.rm = TRUE)
    n_partial <- sum(participants_flagged$inclusion_status == "partial", na.rm = TRUE)
    n_no      <- sum(participants_flagged$inclusion_status == "no",     na.rm = TRUE)
    
    eligibility_lines <- purrr::imap_chr(outcome_specs, function(spec, nm) {
        n_elig <- sum(outcome_flags[[spec$flag]], na.rm = TRUE)
        glue::glue("  {spec$label}: {n_elig} / {length(included_pts)} eligible")
    })
    
    cli::cli_inform(c(
        "v" = "freeze_dataset() complete.",
        "i" = "Inclusion status among {nrow(participants_flagged)} participants:",
        "*" = "  yes     (all outcomes eligible): {n_yes}",
        "*" = "  partial (some outcomes missing):  {n_partial}",
        "*" = "  no      (hard excluded):           {n_no}",
        "i" = "analysis_long: {nrow(analysis_long)} rows | {length(analysis_pts)} participants",
        "i" = "Per-outcome eligibility (among {length(included_pts)} hard-included):",
        "*" = paste(eligibility_lines, collapse = "\n")
    ))
    
    list(
        participants_flagged = participants_flagged,
        data                 = analysis_long,
        flow_log_hard        = flow_log_hard,
        flow_log_outcomes    = flow_log_outcomes
    )
}


# =============================================================================
# PRIVATE HELPERS
# =============================================================================

#' Build a cumulative hard-exclusion flow log.
.build_flow_log <- function(flow, criteria, reason_col, total_label) {
    n_total <- nrow(flow)
    
    step_rows <- purrr::imap_dfr(criteria, function(reason, idx) {
        n_this <- sum(flow[[reason_col]] == reason, na.rm = TRUE)
        tibble::tibble(step = as.integer(idx), reason = reason,
                       n_excluded = n_this)
    })
    
    dplyr::bind_rows(
        tibble::tibble(step = 0L, reason = total_label,
                       n_excluded = 0L, n_remaining = n_total),
        step_rows
    ) |>
        dplyr::mutate(n_remaining = n_total - cumsum(n_excluded))
}

#' Build a per-outcome eligibility flow log.
.build_outcome_flow_log <- function(outcome_flags, flag_col, reason_col,
                                    n_hard_included, outcome_label) {
    
    reasons <- unique(outcome_flags[[reason_col]][
        !outcome_flags[[flag_col]] & !is.na(outcome_flags[[reason_col]])
    ])
    
    enrol_row <- tibble::tibble(
        step       = 0L,
        reason     = as.character(
            glue::glue("Hard-included for {outcome_label}")),
        n_excluded = 0L,
        n_remaining = n_hard_included
    )
    
    if (length(reasons) == 0L) {
        return(dplyr::bind_rows(
            enrol_row,
            tibble::tibble(step = 1L,
                           reason = "No outcome-specific exclusions",
                           n_excluded = 0L, n_remaining = n_hard_included)
        ))
    }
    
    step_rows <- purrr::imap_dfr(reasons, function(reason, idx) {
        n_this <- sum(outcome_flags[[reason_col]] == reason, na.rm = TRUE)
        tibble::tibble(step = as.integer(idx), reason = reason,
                       n_excluded = n_this)
    })
    
    dplyr::bind_rows(enrol_row, step_rows) |>
        dplyr::mutate(n_remaining = n_hard_included - cumsum(n_excluded))
}