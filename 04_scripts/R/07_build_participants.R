# =============================================================================
# R/07_build_participants.R
# =============================================================================
# Builds the participants table: one row per person.
#
# Grain:   pt
# Source:  CoLaus Baseline (fixed covariates) + OsteoLaus Baseline (enrolment)
# Output:  participants -> used by build_visits() and freeze_dataset()
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================


#' Build the participants table.
#'
#' Combines time-constant variables recorded once at CoLaus Baseline with
#' OsteoLaus Baseline enrolment data. The OsteoLaus Baseline is the backbone:
#' every OsteoLaus participant appears; CoLaus-only participants are excluded.
#'
#' Columns produced
#' ────────────────
#' pt                        PK (integer)
#' sex                       Factor Female/Male          [CoLaus Baseline]
#' ethnic                    Factor (ethori_self)        [CoLaus Baseline]
#' education_level           Ordered factor Low/Medium/High (ISCED)
#'                           derived from edtyp4 by derive_education()
#' mrtsts2                   Factor marital status       [CoLaus Baseline]
#' baseline_colaus_date      Date of CoLaus Baseline exam
#' baseline_colaus_age       Age at CoLaus Baseline (yr)
#' baseline_osteo_date       Date of OsteoLaus Baseline exam
#' baseline_osteo_age        Age at OsteoLaus Baseline (yr)
#' baseline_bmi              BMI at OsteoLaus Baseline (kg/m2)
#' baseline_height           Height at OsteoLaus Baseline (cm)
#' baseline_weight           Weight at OsteoLaus Baseline (kg)
#' baseline_dairy_total_gday Dairy intake at first CoLaus FFQ wave (g/day)
#' baseline_dairy_quartile   Quartile of baseline dairy intake (Q1-Q4)
#' n_osteo_visits            OsteoLaus waves with any data
#' n_osteo_outcomes          OsteoLaus waves with both HGS_MAX and ALM_HT2
#' *_oob                     TRUE where baseline anthropometric is implausible (original value retained)
#'
#' Integrity checks
#' ────────────────
#' ABORT  Duplicate pt values in output
#' WARN   sex missing for > 5% of participants
#' WARN   education_level missing for > 10% of participants
#'
#' @param colaus_derived Output of derive_colaus().
#' @param osteo_derived  Output of derive_osteo() or derive_combined().
#' @return Tibble with one row per OsteoLaus participant.
build_participants <- function(colaus_derived, osteo_derived) {
    
    # ── Fixed covariates from CoLaus Baseline -----------------------------------
    # education_level (3-level ISCED grouping) is taken from the derived column
    # produced by derive_education(), not the raw 4-level edtyp4 factor.
    colaus_fixed <- colaus_derived |>
        dplyr::filter(.wave == "Baseline") |>
        dplyr::select(
            pt,
            sex,
            ethnic               = ethori_self,
            education_level,          # derived ISCED grouping (Low/Medium/High)
            mrtsts2,
            baseline_colaus_date = exam_date_iso,
            baseline_colaus_age  = age
        )
    
    # ── Baseline dairy intake from first available FFQ wave (F1) ---------------
    # CoLaus Baseline has no FFQ; F1 is the earliest dietary assessment and is
    # used as the "baseline" exposure value for quartile assignment.
    colaus_dairy_f1 <- colaus_derived |>
        dplyr::filter(.wave == "F1") |>
        dplyr::select(pt,
                      dplyr::any_of("dairy_total_gday")
        ) |>
        dplyr::rename_with(
            ~ "baseline_dairy_total_gday",
            dplyr::any_of("dairy_total_gday")
        )
    
    # ── OsteoLaus Baseline enrolment data ---------------------------------------
    osteo_bsl <- osteo_derived |>
        dplyr::filter(.wave == "Baseline") |>
        dplyr::select(
            pt,
            baseline_osteo_date = exam_date_iso,
            baseline_osteo_age  = Age,
            baseline_bmi        = BMI,
            baseline_height     = Height,
            baseline_weight     = Weight
        )
    
    # ── Visit and outcome counts ────────────────────────────────────────────────
    visit_counts <- osteo_derived |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            n_osteo_visits   = dplyr::n(),
            n_osteo_outcomes = sum(!is.na(HGS_MAX) & !is.na(ALM_HT2)),
            .groups = "drop"
        )
    
    # ── Assemble: OsteoLaus backbone + CoLaus fixed + dairy + counts ------------
    participants <- osteo_bsl |>
        dplyr::left_join(colaus_fixed,    by = "pt") |>
        dplyr::left_join(colaus_dairy_f1, by = "pt") |>
        dplyr::left_join(visit_counts,    by = "pt")
    
    # ── Baseline dairy quartile --------------------------------------------------
    # Quartile boundaries computed among participants with non-missing F1 dairy.
    if ("baseline_dairy_total_gday" %in% names(participants)) {
        q_breaks <- quantile(
            participants$baseline_dairy_total_gday,
            probs = c(0, 0.25, 0.50, 0.75, 1),
            na.rm = TRUE
        )
        participants <- dplyr::mutate(participants,
                                      baseline_dairy_quartile = cut(
                                          baseline_dairy_total_gday,
                                          breaks         = q_breaks,
                                          labels         = c("Q1", "Q2", "Q3", "Q4"),
                                          include.lowest = TRUE
                                      )
        )
    }
    
    # ── Range limits on baseline anthropometrics --------------------------------
    # Lower bound for age is 40 (OsteoLaus enrols from age 50, but a small
    # margin allows for age-calculation rounding with generic birth dates).
    participants <- .apply_range_limit(
        participants, "baseline_osteo_age", lo = 40,  hi = 100
    )
    participants <- .apply_range_limit(
        participants, "baseline_bmi",       lo = 10,  hi =  70
    )
    participants <- .apply_range_limit(
        participants, "baseline_height",    lo = 130, hi = 200
    )
    participants <- .apply_range_limit(
        participants, "baseline_weight",    lo =  30, hi = 200
    )
    
    # ── Integrity checks ────────────────────────────────────────────────────────
    if (anyDuplicated(participants$pt) > 0)
        cli::cli_abort(
            "build_participants(): duplicate pt values — \
       check OsteoLaus Baseline for duplicate rows."
        )
    
    pct_sex_na <- mean(is.na(participants$sex))
    if (pct_sex_na > 0.05)
        cli::cli_warn(
            "build_participants(): sex missing for \
       {scales::percent(pct_sex_na, accuracy = 0.1)} of participants \
       (expected < 5%; OsteoLaus-only pts have no CoLaus Baseline)."
        )
    
    pct_edu_na <- mean(is.na(participants$education_level))
    if (pct_edu_na > 0.10)
        cli::cli_warn(
            "build_participants(): education_level missing for \
       {scales::percent(pct_edu_na, accuracy = 0.1)} of participants \
       (expected < 10%)."
        )
    
    cli::cli_inform(c(
        "participants: {nrow(participants)} rows",
        "i" = "sex present for {scales::percent(1 - pct_sex_na, accuracy = 0.1)}",
        "i" = "education_level present for \
           {scales::percent(1 - pct_edu_na, accuracy = 0.1)}",
        "i" = "baseline dairy quartile assigned for \
           {sum(!is.na(participants$baseline_dairy_quartile))} / \
           {nrow(participants)} participants"
    ))
    
    participants
}