# =============================================================================
# R/functions_clean.R
# =============================================================================
# Apply cleaning rules from variable_definitions.md.
# At this stage columns have been resolved by merge_cohorts():
#   - ht / wt / BMI reflect OsteoLaus values (CoLaus equivalents dropped)
#   - age reflects OsteoLaus Age where available, CoLaus age otherwise
#   - education_level has been remapped to Low/Medium/High (3-level ISCED)
#   - HGS trials (HGS_R1..L3) and HGS_MAX are present from OsteoLaus rows
#
# Returns a named list: $data (cleaned tibble), $log (cleaning log tibble)

#' Apply data cleaning rules to the merged long dataset
#'
#' @param merged_long Output of merge_cohorts().
#' @return Named list: $data (cleaned tibble), $log (cleaning log tibble).
clean_data <- function(merged_long) {
    
    log  <- list()
    data <- merged_long
    
    record <- function(step, n, detail = NA_character_) {
        log[[length(log) + 1]] <<- tibble::tibble(step = step, n_affected = n, detail = detail)
    }
    
    # ── 1. Baseline exclusions --------------------------------------------------
    # Exclude entire participants when a mandatory variable is missing at baseline.
    # Per variable_definitions.md:
    #   pt            : "if not recorded participant is excluded"
    #   exam_date_iso : "if not recorded participant is excluded"
    #   age           : "needs to be recorded at baseline otherwise excluded"
    #   education_level: "if not recorded participant is excluded"
    #   sbsmk         : "Exclude if not available at baseline"
    baseline <- dplyr::filter(data, .wave == "Baseline")
    
    excl_pts <- unique(c(
        baseline$pt[is.na(baseline$pt)],
        baseline$pt[is.na(baseline$exam_date_iso)],
        baseline$pt[is.na(baseline$age)],
        baseline$pt[is.na(baseline$education_level)],
        baseline$pt[is.na(baseline$sbsmk)]
    ))
    
    record("baseline_exclusions", length(excl_pts),
           "Missing pt / exam_date / age / education_level / sbsmk at baseline")
    
    data <- dplyr::filter(data, !pt %in% excl_pts)
    
    # ── 2. Height — carry forward/backward within participant -------------------
    # Fill gaps across waves (height is expected to be stable over time).
    data <- data |>
        dplyr::group_by(pt) |>
        dplyr::arrange(.wave_num, .by_group = TRUE) |>
        tidyr::fill(ht, .direction = "downup") |>
        dplyr::ungroup()
    
    record("ht_filled",
           sum(is.na(merged_long$ht)) - sum(is.na(data$ht)),
           "Height filled across waves")
    
    # ── 3. Age — back-calculate from exam date where missing at follow-up ------
    data <- data |>
        dplyr::group_by(pt) |>
        dplyr::mutate(
            .age_bl  = age[.wave == "Baseline"][1],
            .date_bl = exam_date_iso[.wave == "Baseline"][1],
            age = dplyr::if_else(
                is.na(age) & !is.na(exam_date_iso) & !is.na(.age_bl),
                .age_bl + as.numeric(exam_date_iso - .date_bl) / 365.25,
                age
            )
        ) |>
        dplyr::select(-.age_bl, -.date_bl) |>
        dplyr::ungroup()
    
    # ── 4. BMI — derive where missing but ht/wt available ----------------------
    data <- data |>
        dplyr::mutate(
            BMI = dplyr::case_when(
                !is.na(BMI)                         ~ BMI,
                !is.na(wt) & !is.na(ht) & ht > 0   ~ wt / (ht / 100)^2,
                TRUE                                ~ NA_real_
            )
        )
    
    # ── 5. HGS_MAX — derive from 6 individual trials (OsteoLaus V5 only) -------
    # HGS_R1/R2/R3 (right hand) and HGS_L1/L2/L3 (left hand).
    # Prefer a pre-existing HGS_MAX; compute from trials only when absent.
    hgs_trials  <- c("HGS_R1", "HGS_R2", "HGS_R3", "HGS_L1", "HGS_L2", "HGS_L3")
    hgs_present <- intersect(hgs_trials, names(data))
    
    if (length(hgs_present) > 0) {
        data <- data |>
            dplyr::mutate(
                HGS_MAX = dplyr::if_else(
                    !is.na(HGS_MAX),
                    HGS_MAX,
                    {
                        m <- pmax(!!!rlang::syms(hgs_present), na.rm = TRUE)
                        dplyr::if_else(is.infinite(m), NA_real_, m)
                    }
                )
            )
    }
    
    # ── 6. Smoking — carry baseline status to missing follow-ups ----------------
    baseline_smoke <- data |>
        dplyr::filter(.wave == "Baseline") |>
        dplyr::select(pt, sbsmk_bl = sbsmk)
    
    data <- data |>
        dplyr::left_join(baseline_smoke, by = "pt") |>
        dplyr::mutate(
            sbsmk_imputed = is.na(sbsmk) & .wave != "Baseline",
            sbsmk = dplyr::if_else(sbsmk_imputed, sbsmk_bl, sbsmk)
        ) |>
        dplyr::select(-sbsmk_bl)
    
    record("sbsmk_imputed", sum(data$sbsmk_imputed, na.rm = TRUE),
           "Smoking status carried from baseline to missing follow-ups")
    
    # ── 7. Energy intake — range exclusion + carry-forward/backward (sumtot1) --
    # Per variable_definitions.md: flag values outside [500, 4200] kcal/day,
    # then fill gaps from adjacent waves (within range values carried forward
    # or backward within participant).
    ENERGY_MIN <- 500L; ENERGY_MAX <- 4200L
    
    data <- data |>
        dplyr::mutate(
            sumtot1_flag = !is.na(sumtot1) & (sumtot1 < ENERGY_MIN | sumtot1 > ENERGY_MAX),
            sumtot1      = dplyr::if_else(sumtot1_flag, NA_real_, sumtot1)
        )
    record("energy_out_of_range", sum(data$sumtot1_flag, na.rm = TRUE),
           glue::glue("sumtot1 outside [{ENERGY_MIN}, {ENERGY_MAX}] kcal/day — set to NA"))
    
    n_energy_before_fill <- sum(is.na(data$sumtot1))
    data <- data |>
        dplyr::group_by(pt) |>
        dplyr::arrange(.wave_num, .by_group = TRUE) |>
        tidyr::fill(sumtot1, .direction = "downup") |>
        dplyr::ungroup()
    record("energy_filled",
           n_energy_before_fill - sum(is.na(data$sumtot1)),
           "sumtot1 filled from adjacent wave after range exclusion")
    
    # ── 8. Handgrip (CoLaus) — implausible value flag ---------------------------
    if ("handgrip" %in% names(data)) {
        data <- data |>
            dplyr::mutate(
                handgrip_flag = !is.na(handgrip) & (handgrip < 1 | handgrip > 90),
                handgrip      = dplyr::if_else(handgrip_flag, NA_real_, handgrip)
            )
        record("handgrip_implausible", sum(data$handgrip_flag, na.rm = TRUE),
               "CoLaus handgrip outside [1, 90] kg")
    }
    
    # ── 9. HGS_MAX (OsteoLaus) — implausible value flag -------------------------
    if ("HGS_MAX" %in% names(data)) {
        data <- data |>
            dplyr::mutate(
                HGS_MAX_flag = !is.na(HGS_MAX) & (HGS_MAX < 1 | HGS_MAX > 90),
                HGS_MAX      = dplyr::if_else(HGS_MAX_flag, NA_real_, HGS_MAX)
            )
        record("HGS_MAX_implausible", sum(data$HGS_MAX_flag, na.rm = TRUE),
               "OsteoLaus HGS_MAX outside [1, 90] kg")
    }
    
    list(data = data, log = dplyr::bind_rows(log))
}