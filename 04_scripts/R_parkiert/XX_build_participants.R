# =============================================================================
# R/build_participants.R
# =============================================================================
# Builds the participants table: one row per person.
#
# Grain:   pt
# Backbone: CoLaus (all waves) — every CoLaus participant appears.
#           OsteoLaus data are joined where available (OsteoLaus is a
#           sub-cohort of CoLaus, so all OsteoLaus participants also appear
#           in CoLaus, but not vice-versa).
#
# Columns produced
# ────────────────
# pt                        PK (integer)
# sex                       Factor Female/Male          [CoLaus Baseline]
# ethnic                    Factor (ethori_self)        [CoLaus Baseline]
# education_level           Ordered factor Low/Medium/High (ISCED)
# mrtsts2                   Factor marital status       [CoLaus Baseline]
# baseline_colaus_age       Age at CoLaus Baseline (yr)
# baseline_osteo_age        Age at OsteoLaus Baseline (yr); NA if not in OsteoLaus
# baseline_bmi              BMI at OsteoLaus Baseline (kg/m²); NA if not in OsteoLaus
# baseline_height           Height at OsteoLaus Baseline (cm)
# baseline_weight           Weight at OsteoLaus Baseline (kg)
# baseline_dairy_total_gday Dairy intake at CoLaus F1 (g/day)
# dairy_ok                  Factor (No/Yes): first non-NA Dairy_OK across all
#                           CoLaus waves (>= 3 dairy servings/day per Swiss guidelines)
# n_colaus_visits           Number of CoLaus waves with any data
# n_osteo_visits            Number of OsteoLaus waves with any data; 0 for non-OsteoLaus pts
#
# Inclusion flags (all logical: TRUE = included for this purpose)
# ────────────────────────────────────────────────────────────────
# in_osteolaus              TRUE if participant has >= 1 OsteoLaus visit
# include_global            TRUE if participant passes all hard criteria:
#                             • has CoLaus Baseline
#                             • has >= 2 OsteoLaus visits
#                             • has baseline OsteoLaus exam date
#                             • baseline anthropometrics in plausible range
# include_grip              TRUE if include_global AND handgrip_max_all
#                             non-missing at OsteoLaus Baseline
# include_alm               TRUE if include_global AND ALM_HT2 non-missing
#                             at OsteoLaus Baseline
# include_gait              TRUE if include_global AND gait_speed non-missing
#                             at V4 or V5
# include_ewgsop2           TRUE if include_global AND ewgsop2_sarcopenia_stage
#                             non-missing at OsteoLaus Baseline
# exclusion_reason          Semicolon-separated string of all reasons a
#                             participant does NOT qualify for any flag above;
#                             NA when include_global AND all outcome flags are TRUE
#
# *_oob                     TRUE where baseline anthropometric is implausible
#                           (original value retained)
#
# Integrity checks
# ────────────────
# ABORT  Duplicate pt values in output
#
#' @param colaus_derived Output of derive_colaus().
#' @param osteo_derived  Output of derive_osteo()
#' @param min_osteo_visits Minimum OsteoLaus visits required for include_global.
#   Default 2L.
#' @return Tibble with one row per (CoLaus) participant.

build_participants <- function(colaus_derived,
                               osteo_derived,
                               min_osteo_visits = 2L) {
    
    # =========================================================================
    # STEP 1: CoLaus backbone
    # =========================================================================
    
    colaus_fixed <- colaus_derived |>
        dplyr::filter(.wave == "Baseline") |>
        dplyr::select(
            pt,
            sex,
            ethnic = ethori_self,
            education_level,
            mrtsts2,
            baseline_colaus_age = age
        )
    
    n_colaus <- colaus_derived |>
        dplyr::group_by(pt) |>
        dplyr::summarise(n_colaus_visits = dplyr::n(), .groups = "drop")
    
    dairy_ok_first <- colaus_derived |>
        dplyr::arrange(pt, .wave_num) |>
        dplyr::filter(!is.na(Dairy_OK)) |>
        dplyr::group_by(pt) |>
        dplyr::slice(1L) |>
        dplyr::ungroup() |>
        dplyr::select(pt, dairy_ok = Dairy_OK)
    
    colaus_dairy_f1 <- colaus_derived |>
        dplyr::filter(.wave == "F1") |>
        dplyr::select(pt, dplyr::any_of("dairy_total_gday")) |>
        dplyr::rename_with(~ "baseline_dairy_total_gday",
                           dplyr::any_of("dairy_total_gday"))
    
    backbone <- colaus_fixed |>
        dplyr::left_join(n_colaus, by = "pt") |>
        dplyr::left_join(dairy_ok_first, by = "pt") |>
        dplyr::left_join(colaus_dairy_f1, by = "pt")
    
    # =========================================================================
    # STEP 2: OsteoLaus
    # =========================================================================
    
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
    
    n_osteo <- osteo_derived |>
        dplyr::group_by(pt) |>
        dplyr::summarise(n_osteo_visits = dplyr::n(), .groups = "drop")
    
    has_gait <- if ("gait_speed" %in% names(osteo_derived)) {
        osteo_derived |>
            dplyr::filter(.wave %in% c("V4", "V5"), !is.na(gait_speed)) |>
            dplyr::distinct(pt) |>
            dplyr::mutate(has_gait_v4v5 = TRUE)
    } else {
        tibble::tibble(pt = integer(), has_gait_v4v5 = logical())
    }
    
    participants <- backbone |>
        dplyr::left_join(osteo_bsl, by = "pt") |>
        dplyr::left_join(n_osteo, by = "pt") |>
        dplyr::left_join(has_gait, by = "pt") |>
        dplyr::mutate(
            n_osteo_visits = dplyr::coalesce(n_osteo_visits, 0L),
            has_gait_v4v5  = dplyr::coalesce(has_gait_v4v5, FALSE),
            in_osteolaus   = n_osteo_visits > 0L
        )
    
    # =========================================================================
    # STEP 3: Inclusion + exclusion_reason
    # =========================================================================
    
    participants <- participants |>
        dplyr::mutate(
            .fail_not_in_osteo   = !in_osteolaus,
            .fail_no_bsl_date    = in_osteolaus & is.na(baseline_osteo_date),
            .fail_too_few_visits = in_osteolaus & n_osteo_visits < min_osteo_visits,
            
            include_global = !(
                .fail_not_in_osteo |
                    .fail_no_bsl_date |
                    .fail_too_few_visits
            ),
            
            include_gait = include_global & has_gait_v4v5
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
            exclusion_reason = {
                r <- character(0)
                
                if (.fail_not_in_osteo)
                    r <- c(r, "Not enrolled in OsteoLaus")
                
                if (.fail_no_bsl_date)
                    r <- c(r, "Missing OsteoLaus Baseline date")
                
                if (.fail_too_few_visits)
                    r <- c(r, glue::glue("Fewer than {min_osteo_visits} OsteoLaus visits"))
                
                if (include_global && !include_gait)
                    r <- c(r, "No gait speed at V4 or V5")
                
                if (length(r) == 0L) NA_character_ else paste(r, collapse = "; ")
            }
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-dplyr::starts_with(".fail_"), -has_gait_v4v5)
    
    # =========================================================================
    # STEP 4: Final columns
    # =========================================================================
    
    participants |>
        dplyr::select(
            pt,
            sex,
            ethnic,
            education_level,
            mrtsts2,
            baseline_colaus_age,
            baseline_osteo_age,
            baseline_bmi,
            baseline_height,
            baseline_weight,
            baseline_dairy_total_gday,
            dairy_ok,
            n_colaus_visits,
            n_osteo_visits,
            in_osteolaus,
            include_global,
            include_gait,
            exclusion_reason
        )
}