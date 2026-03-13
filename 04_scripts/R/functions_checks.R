# =============================================================================
# R/functions_checks.R
# =============================================================================
# Explicit data quality checks designed to run as named targets in _targets.R.
#
# Each check function:
#   - Returns a structured tibble (never stops the pipeline)
#   - Uses result codes: "PASS", "INFO", "WARN", "FAIL"
#   - Is inspectable with tar_read(qc_*)
#
# Call assert_no_failures(...) as a terminal target if you want the pipeline
# to hard-stop before derived outputs when FAILs are present.
#
# Suggested _targets.R entries:
#
#   # Raw file checks (run immediately after import_wave())
#   tar_target(qc_pt_uniqueness,
#       check_pt_uniqueness(list(
#           "CoLaus Baseline" = colaus_bsl_raw,
#           "CoLaus F1"       = colaus_f1_raw,
#           "CoLaus F2"       = colaus_f2_raw,
#           "CoLaus F3"       = colaus_f3_raw,
#           "CoLaus F4"       = colaus_f4_raw,
#           "OsteoLaus Bsl"   = osteo_bsl_raw,
#           "OsteoLaus V2"    = osteo_v2_raw,
#           "OsteoLaus V3"    = osteo_v3_raw,
#           "OsteoLaus V4"    = osteo_v4_raw,
#           "OsteoLaus V5"    = osteo_v5_raw
#       ))
#   ),
#
#   # Cross-cohort pt identity check (run after harmonise, before stack)
#   tar_target(qc_pt_identity,
#       check_pt_identity(
#           colaus_waves = list(
#               colaus_bsl_harm, colaus_f1_harm, colaus_f2_harm,
#               colaus_f3_harm,  colaus_f4_harm
#           ),
#           osteo_waves = list(
#               osteo_bsl_harm, osteo_v2_harm, osteo_v3_harm,
#               osteo_v4_harm,  osteo_v5_harm
#           )
#       )
#   ),
#
#   # Stacked long-format checks (run after stack_*())
#   tar_target(qc_pt_overlap,
#       check_pt_overlap(colaus_long, osteo_long)
#   ),
#
#   # Hard stop before any derivation if failures exist
#   tar_target(qc_all_pass,
#       assert_no_failures(qc_pt_uniqueness, qc_pt_identity, qc_pt_overlap)
#   )

# =============================================================================
# check_pt_uniqueness()
# =============================================================================

#' Check that pt is unique within every imported wave file.
#'
#' A duplicate pt within a single wave file means one participant has two rows
#' in that export — a data extraction error that must be resolved upstream.
#'
#' @param wave_list Named list of raw imported tibbles (one per wave file).
#'   Names become the "file" label in the output.
#'   e.g. list("CoLaus F1" = colaus_f1_raw, "OsteoLaus V2" = osteo_v2_raw)
#' @return Tibble with one row per file and columns:
#'   file, n_rows, n_pt, n_pt_na, n_distinct, n_duplicate, result, detail
check_pt_uniqueness <- function(wave_list) {
    
    purrr::imap_dfr(wave_list, function(df, file_label) {
        
        if (!"pt" %in% names(df)) {
            return(tibble::tibble(
                file = file_label, n_rows = nrow(df),
                n_pt = NA_integer_, n_pt_na = NA_integer_,
                n_distinct = NA_integer_, n_duplicate = NA_integer_,
                result = "FAIL",
                detail = "Column 'pt' not found in this file."
            ))
        }
        
        pt_vals     <- df$pt
        n_rows      <- nrow(df)
        n_pt_na     <- sum(is.na(pt_vals))
        n_pt        <- sum(!is.na(pt_vals))
        n_distinct  <- dplyr::n_distinct(pt_vals, na.rm = TRUE)
        n_duplicate <- n_pt - n_distinct
        
        dup_pts <- df |>
            dplyr::filter(!is.na(pt)) |>
            dplyr::count(pt) |>
            dplyr::filter(n > 1) |>
            dplyr::pull(pt)
        
        result <- dplyr::case_when(
            n_duplicate > 0 ~ "FAIL",
            n_pt_na     > 0 ~ "WARN",
            TRUE            ~ "PASS"
        )
        
        detail <- dplyr::case_when(
            n_duplicate > 0 ~ glue::glue(
                "{n_duplicate} duplicate pt(s): ",
                "{paste(head(dup_pts, 20), collapse = ', ')}",
                "{if (length(dup_pts) > 20) paste0(' ... and ', length(dup_pts)-20, ' more') else ''}"
            ),
            n_pt_na > 0 ~ glue::glue(
                "{n_pt_na} row(s) with NA pt — cannot verify uniqueness."
            ),
            TRUE ~ glue::glue(
                "{n_distinct} unique participants, all pt values present and unique."
            )
        )
        
        tibble::tibble(
            file        = file_label,
            n_rows      = n_rows,
            n_pt        = n_pt,
            n_pt_na     = n_pt_na,
            n_distinct  = n_distinct,
            n_duplicate = n_duplicate,
            result      = result,
            detail      = as.character(detail)
        )
    })
}

# =============================================================================
# check_pt_identity()
# =============================================================================

#' Verify that pt refers to the same person across all wave files and cohorts.
#'
#' pt=1042 in CoLaus F1 must be the same biological individual as pt=1042 in
#' OsteoLaus V3. This cannot be confirmed from pt alone — it requires anchor
#' variables that are stable within a person over time and whose values should
#' therefore be consistent across all files where a pt appears.
#'
#' ANCHORS USED:
#'
#'   sex (CoLaus only)
#'     Biologically stable. Any pt where sex changes across CoLaus waves is a
#'     FAIL — either a data extraction error or a pt collision between two
#'     different people.
#'
#'   ethnic (CoLaus Baseline AND OsteoLaus Baseline)
#'     Ethnicity is recorded once at baseline in both cohorts. Within each
#'     cohort there is only one row, so no within-wave inconsistency is
#'     possible. The key test is cross-cohort: if pt=1042 is White in
#'     CoLaus Baseline but African in OsteoLaus Baseline, that is a FAIL —
#'     a strong signal of a pt ID collision between two different people.
#'
#'   height / Height (both cohorts)
#'     Adult height is stable. We allow up to ht_tol cm variation (default 3)
#'     for measurement error. Beyond that, flagged as WARN (could indicate
#'     a pt ID collision or data entry error). CoLaus uses "ht" (numeric, cm
#'     after harmonisation); OsteoLaus uses "Height" (numeric, cm).
#'     Cross-cohort height comparison is the key identity test: if pt=1042 is
#'     162 cm in CoLaus F1 but 178 cm in OsteoLaus Baseline, they are likely
#'     different people.
#'
#'   age trajectory (within each cohort)
#'     Age should increase monotonically with wave number. For each pt, we
#'     check that age increases between consecutive waves at a plausible rate:
#'     between (inter_wave_gap_years - 1) and (inter_wave_gap_years + 2) years.
#'     A pt whose age goes backwards or jumps by >10 years is flagged as FAIL.
#'     Uses exam_date_iso for precise gap calculation where available.
#'
#' WHAT THIS DOES NOT CATCH:
#'   - A systematic swap (pt 1042 and 1043 swapped in all files) — no
#'     statistical check can detect this without a gold-standard roster.
#'   - pt collisions between people with very similar characteristics.
#'
#' @param colaus_waves List of harmonised CoLaus wave tibbles in wave order
#'   (Baseline, F1, F2, F3, F4). Each must have .wave, .wave_num, pt.
#' @param osteo_waves  List of harmonised OsteoLaus wave tibbles in wave order
#'   (Baseline, V2, V3, V4, V5). Each must have .wave, .wave_num, pt.
#' @param ht_tol       Numeric. Maximum height difference (cm) allowed before
#'   flagging. Default 3 cm.
#' @param age_tol      Numeric. Extra years of tolerance on top of the expected
#'   inter-wave gap for age trajectory check. Default 2.
#' @return Tibble with one row per flagged pt per check, plus summary rows.
#'   Columns: check, cohort, pt, result, detail.
#'   If all pts pass a check, one PASS summary row is returned for that check.
check_pt_identity <- function(colaus_waves,
                              osteo_waves,
                              ht_tol  = 3,
                              age_tol = 2) {
    
    results <- list()
    
    # Stack each cohort into a long frame for easier cross-wave comparison
    colaus_long <- dplyr::bind_rows(colaus_waves)
    osteo_long  <- dplyr::bind_rows(osteo_waves)
    
    # ── 1. sex consistency within CoLaus ─────────────────────────────────────
    # sex is only collected in CoLaus. It must be the same at every wave for
    # each pt. A change in sex across waves = FAIL (data error or pt collision).
    if ("sex" %in% names(colaus_long)) {
        sex_check <- colaus_long |>
            dplyr::filter(!is.na(sex), !is.na(pt)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(
                n_values    = dplyr::n(),
                n_sex_vals  = dplyr::n_distinct(as.character(sex)),
                sex_vals    = paste(sort(unique(as.character(sex))), collapse = " / "),
                .groups     = "drop"
            ) |>
            dplyr::filter(n_sex_vals > 1)
        
        if (nrow(sex_check) == 0) {
            results[["sex"]] <- tibble::tibble(
                check = "sex stable within CoLaus", cohort = "CoLaus",
                pt = NA_character_, result = "PASS",
                detail = "All pts have consistent sex across CoLaus waves."
            )
        } else {
            results[["sex"]] <- sex_check |>
                dplyr::transmute(
                    check               = "sex stable within CoLaus",
                    cohort              = "CoLaus",
                    pt                  = as.character(pt),
                    result              = "FAIL",
                    detail              = glue::glue("sex changes across waves: {sex_vals} ({n_values} wave(s) with data)"),
                    age_osteolaus       = NA_real_,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    # ── 2a. ethnicity at baseline only (CoLaus) ───────────────────────────────
    # Ethnicity is recorded only at CoLaus Baseline. Since there is only one
    # row per pt at baseline, there can be no within-wave inconsistency here.
    # We still extract the value for use in the cross-cohort check below.
    ethnic_col_c <- intersect(c("ethnic", "ethnicity"), names(colaus_long))[1]
    colaus_ethnic <- NULL
    if (!is.na(ethnic_col_c)) {
        colaus_ethnic <- colaus_long |>
            dplyr::filter(.wave == "Baseline", !is.na(pt),
                          !is.na(.data[[ethnic_col_c]])) |>
            dplyr::select(pt, colaus_ethnic = dplyr::all_of(ethnic_col_c)) |>
            dplyr::mutate(colaus_ethnic = as.character(colaus_ethnic))
        
        results[["ethnicity_colaus"]] <- tibble::tibble(
            check  = "ethnicity available at CoLaus Baseline",
            cohort = "CoLaus",
            pt     = NA_character_,
            result = "INFO",
            detail = glue::glue(
                "{nrow(colaus_ethnic)} pts have ethnicity recorded at CoLaus Baseline."
            )
        )
    }
    
    # ── 2b. ethnicity at baseline only (OsteoLaus) ────────────────────────────
    # Ethnicity is also recorded only at OsteoLaus Baseline. Same logic.
    ethnic_col_o <- intersect(c("ethnic", "ethnicity"), names(osteo_long))[1]
    osteo_ethnic <- NULL
    if (!is.na(ethnic_col_o)) {
        osteo_ethnic <- osteo_long |>
            dplyr::filter(.wave == "Baseline", !is.na(pt),
                          !is.na(.data[[ethnic_col_o]])) |>
            dplyr::select(pt, osteo_ethnic = dplyr::all_of(ethnic_col_o)) |>
            dplyr::mutate(osteo_ethnic = as.character(osteo_ethnic))
        
        results[["ethnicity_osteo"]] <- tibble::tibble(
            check  = "ethnicity available at OsteoLaus Baseline",
            cohort = "OsteoLaus",
            pt     = NA_character_,
            result = "INFO",
            detail = glue::glue(
                "{nrow(osteo_ethnic)} pts have ethnicity recorded at OsteoLaus Baseline."
            )
        )
    }
    
    # ── 2c. Cross-cohort ethnicity agreement ──────────────────────────────────
    # For pts appearing in both cohorts, ethnicity recorded at CoLaus Baseline
    # must match ethnicity recorded at OsteoLaus Baseline. A mismatch is a
    # strong signal of a pt ID collision — two different people sharing the
    # same pt number. Treated as FAIL (unlike height, where measurement
    # variation is plausible; self-reported ethnicity should not change).
    if (!is.null(colaus_ethnic) && !is.null(osteo_ethnic)) {
        ethnic_cross <- dplyr::inner_join(
            colaus_ethnic, osteo_ethnic, by = "pt"
        ) |>
            dplyr::filter(colaus_ethnic != osteo_ethnic)
        
        n_compared <- nrow(dplyr::inner_join(colaus_ethnic, osteo_ethnic, by = "pt"))
        
        if (nrow(ethnic_cross) == 0) {
            results[["ethnicity_cross"]] <- tibble::tibble(
                check  = "ethnicity agrees cross-cohort",
                cohort = "Both",
                pt     = NA_character_,
                result = "PASS",
                detail = glue::glue(
                    "All {n_compared} pts with ethnicity in both cohorts agree. ",
                    "Strong evidence pt refers to the same person across cohorts."
                )
            )
        } else {
            results[["ethnicity_cross"]] <- ethnic_cross |>
                dplyr::transmute(
                    check               = "ethnicity agrees cross-cohort",
                    cohort              = "Both",
                    pt                  = as.character(pt),
                    result              = "FAIL",
                    detail              = glue::glue("CoLaus ethnicity '{colaus_ethnic}' != OsteoLaus ethnicity '{osteo_ethnic}'. Likely pt ID collision — verify this is the same person."),
                    age_osteolaus       = NA_real_,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    # ── 3. Height consistency within CoLaus ───────────────────────────────────
    ht_col_c <- intersect(c("ht", "Height"), names(colaus_long))[1]
    if (!is.na(ht_col_c)) {
        ht_colaus <- colaus_long |>
            dplyr::filter(!is.na(.data[[ht_col_c]]), !is.na(pt)) |>
            dplyr::mutate(ht_num = suppressWarnings(as.numeric(.data[[ht_col_c]]))) |>
            dplyr::filter(!is.na(ht_num)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(
                ht_min = min(ht_num), ht_max = max(ht_num),
                ht_range = max(ht_num) - min(ht_num),
                ht_vals  = paste(round(sort(unique(ht_num)), 1), collapse = " / "),
                .groups = "drop"
            ) |>
            dplyr::filter(ht_range > ht_tol)
        
        if (nrow(ht_colaus) == 0) {
            results[["ht_colaus"]] <- tibble::tibble(
                check = glue::glue("height stable within CoLaus (tol={ht_tol}cm)"),
                cohort = "CoLaus", pt = NA_character_, result = "PASS",
                detail = glue::glue("All pts have height variation <= {ht_tol} cm across CoLaus waves.")
            )
        } else {
            results[["ht_colaus"]] <- ht_colaus |>
                dplyr::transmute(
                    check               = glue::glue("height stable within CoLaus (tol={ht_tol}cm)"),
                    cohort              = "CoLaus",
                    pt                  = as.character(pt),
                    result              = "WARN",
                    detail              = glue::glue("Height range {round(ht_range, 1)} cm > {ht_tol} cm tolerance. Values: {ht_vals}"),
                    age_osteolaus       = NA_real_,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    # ── 4. Height consistency within OsteoLaus ────────────────────────────────
    ht_col_o <- intersect(c("Height", "ht"), names(osteo_long))[1]
    if (!is.na(ht_col_o)) {
        ht_osteo <- osteo_long |>
            dplyr::filter(!is.na(.data[[ht_col_o]]), !is.na(pt)) |>
            dplyr::mutate(ht_num = suppressWarnings(as.numeric(.data[[ht_col_o]]))) |>
            dplyr::filter(!is.na(ht_num)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(
                ht_min = min(ht_num), ht_max = max(ht_num),
                ht_range = max(ht_num) - min(ht_num),
                ht_vals  = paste(round(sort(unique(ht_num)), 1), collapse = " / "),
                .groups = "drop"
            ) |>
            dplyr::filter(ht_range > ht_tol)
        
        if (nrow(ht_osteo) == 0) {
            results[["ht_osteo"]] <- tibble::tibble(
                check = glue::glue("height stable within OsteoLaus (tol={ht_tol}cm)"),
                cohort = "OsteoLaus", pt = NA_character_, result = "PASS",
                detail = glue::glue("All pts have height variation <= {ht_tol} cm across OsteoLaus waves.")
            )
        } else {
            results[["ht_osteo"]] <- ht_osteo |>
                dplyr::transmute(
                    check               = glue::glue("height stable within OsteoLaus (tol={ht_tol}cm)"),
                    cohort              = "OsteoLaus",
                    pt                  = as.character(pt),
                    result              = "WARN",
                    detail              = glue::glue("Height range {round(ht_range, 1)} cm > {ht_tol} cm tolerance. Values: {ht_vals}"),
                    age_osteolaus       = NA_real_,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    # ── 5. Cross-cohort height agreement ─────────────────────────────────────
    # For pts appearing in both cohorts, median CoLaus height vs median
    # OsteoLaus height should agree within ht_tol. This is the key test that
    # pt refers to the SAME person across cohorts.
    if (!is.na(ht_col_c) && !is.na(ht_col_o)) {
        ht_c <- colaus_long |>
            dplyr::filter(!is.na(.data[[ht_col_c]]), !is.na(pt)) |>
            dplyr::mutate(ht_num = suppressWarnings(as.numeric(.data[[ht_col_c]]))) |>
            dplyr::filter(!is.na(ht_num)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(ht_colaus = median(ht_num), .groups = "drop")
        
        ht_o <- osteo_long |>
            dplyr::filter(!is.na(.data[[ht_col_o]]), !is.na(pt)) |>
            dplyr::mutate(ht_num = suppressWarnings(as.numeric(.data[[ht_col_o]]))) |>
            dplyr::filter(!is.na(ht_num)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(ht_osteo = median(ht_num), .groups = "drop")
        
        ht_cross <- dplyr::inner_join(ht_c, ht_o, by = "pt") |>
            dplyr::mutate(ht_diff = abs(ht_colaus - ht_osteo)) |>
            dplyr::filter(ht_diff > ht_tol)
        
        n_compared <- nrow(dplyr::inner_join(ht_c, ht_o, by = "pt"))
        
        if (nrow(ht_cross) == 0) {
            results[["ht_cross"]] <- tibble::tibble(
                check  = glue::glue("height agrees cross-cohort (tol={ht_tol}cm)"),
                cohort = "Both",
                pt     = NA_character_,
                result = "PASS",
                detail = glue::glue(
                    "All {n_compared} pts with height in both cohorts agree within {ht_tol} cm. ",
                    "Strong evidence pt refers to the same person across CoLaus and OsteoLaus."
                )
            )
        } else {
            results[["ht_cross"]] <- ht_cross |>
                dplyr::transmute(
                    check               = glue::glue("height agrees cross-cohort (tol={ht_tol}cm)"),
                    cohort              = "Both",
                    pt                  = as.character(pt),
                    result              = "WARN",
                    detail              = glue::glue("CoLaus height {round(ht_colaus,1)} cm vs OsteoLaus height {round(ht_osteo,1)} cm — difference {round(ht_diff,1)} cm > {ht_tol} cm tolerance. Verify this pt refers to the same person."),
                    age_osteolaus       = NA_real_,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    # ── 6. Age trajectory within CoLaus ──────────────────────────────────────
    # Age should increase between consecutive waves, at a rate consistent with
    # the actual calendar gap between exam_date_iso values.
    # Flag pts where age goes backwards (FAIL) or jumps implausibly (WARN).
    age_col_c <- intersect(c("age", "Age"), names(colaus_long))[1]
    if (!is.na(age_col_c) && "exam_date_iso" %in% names(colaus_long)) {
        age_traj_c <- colaus_long |>
            dplyr::filter(!is.na(.data[[age_col_c]]), !is.na(pt),
                          !is.na(exam_date_iso)) |>
            dplyr::mutate(age_num = suppressWarnings(as.numeric(.data[[age_col_c]]))) |>
            dplyr::filter(!is.na(age_num)) |>
            dplyr::arrange(pt, exam_date_iso) |>
            dplyr::group_by(pt) |>
            dplyr::filter(dplyr::n() > 1) |>
            dplyr::mutate(
                age_diff      = age_num - dplyr::lag(age_num),
                calendar_gap  = as.numeric(exam_date_iso - dplyr::lag(exam_date_iso)) / 365.25,
                expected_min  = calendar_gap - 1,
                expected_max  = calendar_gap + age_tol
            ) |>
            dplyr::filter(!is.na(age_diff)) |>
            dplyr::mutate(
                traj_result = dplyr::case_when(
                    age_diff < 0                          ~ "FAIL",   # age went backwards
                    age_diff < expected_min               ~ "WARN",   # aged too slowly
                    age_diff > expected_max               ~ "WARN",   # aged too fast
                    TRUE                                  ~ "PASS"
                )
            ) |>
            dplyr::filter(traj_result != "PASS") |>
            dplyr::ungroup()
        
        if (nrow(age_traj_c) == 0) {
            results[["age_colaus"]] <- tibble::tibble(
                check = "age trajectory within CoLaus", cohort = "CoLaus",
                pt = NA_character_, result = "PASS",
                detail = "All pts show plausible age progression across CoLaus waves."
            )
        } else {
            results[["age_colaus"]] <- age_traj_c |>
                dplyr::transmute(
                    check               = "age trajectory within CoLaus",
                    cohort              = "CoLaus",
                    pt                  = as.character(pt),
                    result              = traj_result,
                    detail              = glue::glue("Wave {.wave}: age change {round(age_diff, 1)} yr over {round(calendar_gap, 1)} calendar yr (expected {round(expected_min,1)}-{round(expected_max,1)} yr)"),
                    age_osteolaus       = NA_real_,
                    age_colaus          = age_num,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = exam_date_iso,
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = .wave
                )
        }
    }
    
    # ── 7. Age trajectory within OsteoLaus ────────────────────────────────────
    age_col_o <- intersect(c("Age", "age"), names(osteo_long))[1]
    if (!is.na(age_col_o) && "exam_date_iso" %in% names(osteo_long)) {
        age_traj_o <- osteo_long |>
            dplyr::filter(!is.na(.data[[age_col_o]]), !is.na(pt),
                          !is.na(exam_date_iso)) |>
            dplyr::mutate(age_num = suppressWarnings(as.numeric(.data[[age_col_o]]))) |>
            dplyr::filter(!is.na(age_num)) |>
            dplyr::arrange(pt, exam_date_iso) |>
            dplyr::group_by(pt) |>
            dplyr::filter(dplyr::n() > 1) |>
            dplyr::mutate(
                age_diff     = age_num - dplyr::lag(age_num),
                calendar_gap = as.numeric(exam_date_iso - dplyr::lag(exam_date_iso)) / 365.25,
                expected_min = calendar_gap - 1,
                expected_max = calendar_gap + age_tol
            ) |>
            dplyr::filter(!is.na(age_diff)) |>
            dplyr::mutate(
                traj_result = dplyr::case_when(
                    age_diff < 0            ~ "FAIL",
                    age_diff < expected_min ~ "WARN",
                    age_diff > expected_max ~ "WARN",
                    TRUE                    ~ "PASS"
                )
            ) |>
            dplyr::filter(traj_result != "PASS") |>
            dplyr::ungroup()
        
        if (nrow(age_traj_o) == 0) {
            results[["age_osteo"]] <- tibble::tibble(
                check = "age trajectory within OsteoLaus", cohort = "OsteoLaus",
                pt = NA_character_, result = "PASS",
                detail = "All pts show plausible age progression across OsteoLaus waves."
            )
        } else {
            results[["age_osteo"]] <- age_traj_o |>
                dplyr::transmute(
                    check               = "age trajectory within OsteoLaus",
                    cohort              = "OsteoLaus",
                    pt                  = as.character(pt),
                    result              = traj_result,
                    detail              = glue::glue("Wave {.wave}: age change {round(age_diff, 1)} yr over {round(calendar_gap, 1)} calendar yr (expected {round(expected_min,1)}-{round(expected_max,1)} yr)"),
                    age_osteolaus       = age_num,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = exam_date_iso,
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = .wave,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    # ── 8. Cross-cohort age agreement ─────────────────────────────────────────
    # For pts in both cohorts, age at nearest matched exam dates should agree.
    # We join on pt and find the closest CoLaus/OsteoLaus exam pair, then check
    # that the age difference is consistent with the calendar gap between exams.
    if (!is.na(age_col_c) && !is.na(age_col_o) &&
        "exam_date_iso" %in% names(colaus_long) &&
        "exam_date_iso" %in% names(osteo_long)) {
        
        c_dates <- colaus_long |>
            dplyr::filter(!is.na(pt), !is.na(exam_date_iso),
                          !is.na(.data[[age_col_c]])) |>
            dplyr::mutate(age_num = suppressWarnings(as.numeric(.data[[age_col_c]]))) |>
            dplyr::filter(!is.na(age_num)) |>
            dplyr::select(pt, colaus_date = exam_date_iso, colaus_age = age_num,
                          colaus_wave = .wave)
        
        o_dates <- osteo_long |>
            dplyr::filter(!is.na(pt), !is.na(exam_date_iso),
                          !is.na(.data[[age_col_o]])) |>
            dplyr::mutate(age_num = suppressWarnings(as.numeric(.data[[age_col_o]]))) |>
            dplyr::filter(!is.na(age_num)) |>
            dplyr::select(pt, osteo_date = exam_date_iso, osteo_age = age_num,
                          osteo_wave = .wave)
        
        # For each OsteoLaus visit, find nearest CoLaus visit and compare ages
        cross_age <- o_dates |>
            dplyr::inner_join(c_dates, by = "pt", relationship = "many-to-many") |>
            dplyr::mutate(
                gap_days      = abs(as.numeric(osteo_date - colaus_date)),
                calendar_gap  = as.numeric(osteo_date - colaus_date) / 365.25,
                age_diff      = osteo_age - colaus_age,
                expected_age_diff = calendar_gap,
                age_discrepancy   = abs(age_diff - expected_age_diff)
            ) |>
            dplyr::group_by(pt, osteo_date) |>
            dplyr::slice_min(gap_days, n = 1, with_ties = FALSE) |>
            dplyr::ungroup() |>
            dplyr::filter(age_discrepancy > (age_tol + 1))  # allow tol+1 yr
        
        if (nrow(cross_age) == 0) {
            results[["age_cross"]] <- tibble::tibble(
                check  = "age agrees cross-cohort",
                cohort = "Both",
                pt     = NA_character_,
                result = "PASS",
                detail = glue::glue(
                    "All matched CoLaus/OsteoLaus exam pairs show consistent age. ",
                    "Strong evidence pt refers to the same person across cohorts."
                )
            )
        } else {
            results[["age_cross"]] <- cross_age |>
                dplyr::transmute(
                    check               = "age agrees cross-cohort",
                    cohort              = "Both",
                    pt                  = as.character(pt),
                    result              = "WARN",
                    detail              = glue::glue("OsteoLaus age {round(osteo_age,1)} vs CoLaus age {round(colaus_age,1)} ({round(calendar_gap,1)} yr apart by date): discrepancy {round(age_discrepancy,1)} yr — verify same person."),
                    age_osteolaus       = osteo_age,
                    age_colaus          = colaus_age,
                    exam_date_osteolaus = osteo_date,
                    exam_date_colaus    = colaus_date,
                    wave_osteolaus      = osteo_wave,
                    wave_colaus         = colaus_wave
                )
        }
    }
    
    # ── Combine, enforce column structure, and return ─────────────────────────
    # bind_rows() produces NA for columns absent in some tibbles (PASS summary
    # rows, or within-cohort checks where the other side has no value).
    out <- dplyr::bind_rows(results) |>
        # Guarantee all six extra columns exist (handles PASS/INFO rows)
        dplyr::mutate(
            age_osteolaus       = if ("age_osteolaus"       %in% names(pick(everything()))) age_osteolaus       else NA_real_,
            age_colaus          = if ("age_colaus"          %in% names(pick(everything()))) age_colaus          else NA_real_,
            exam_date_osteolaus = if ("exam_date_osteolaus" %in% names(pick(everything()))) exam_date_osteolaus else as.Date(NA),
            exam_date_colaus    = if ("exam_date_colaus"    %in% names(pick(everything()))) exam_date_colaus    else as.Date(NA),
            wave_osteolaus      = if ("wave_osteolaus"      %in% names(pick(everything()))) wave_osteolaus      else NA_character_,
            wave_colaus         = if ("wave_colaus"         %in% names(pick(everything()))) wave_colaus         else NA_character_
        ) |>
        dplyr::select(
            check, cohort, pt, result, detail,
            age_osteolaus, age_colaus,
            exam_date_osteolaus, exam_date_colaus,
            wave_osteolaus, wave_colaus
        ) |>
        dplyr::arrange(
            factor(result, levels = c("FAIL","WARN","INFO","PASS")),
            check, pt
        )
    
    summary_tbl <- out |>
        dplyr::count(check, result) |>
        dplyr::arrange(check, factor(result, levels = c("FAIL","WARN","INFO","PASS")))
    message("pt identity check summary:\n",
            paste(capture.output(print(summary_tbl, n = Inf)), collapse = "\n"))
    
    out
}

# =============================================================================
# check_pt_overlap()
# =============================================================================

#' Check participant overlap between harmonised CoLaus and OsteoLaus stacks.
#'
#' @param colaus_long Harmonised CoLaus long tibble (output of stack_colaus()).
#' @param osteo_long  Harmonised OsteoLaus long tibble (output of stack_osteo()).
#' @return Tibble with one row per check: check, result, n, pct, detail.
check_pt_overlap <- function(colaus_long, osteo_long) {
    
    colaus_pts <- unique(colaus_long$pt[!is.na(colaus_long$pt)])
    osteo_pts  <- unique(osteo_long$pt[!is.na(osteo_long$pt)])
    
    in_both             <- intersect(colaus_pts, osteo_pts)
    osteo_only          <- setdiff(osteo_pts,  colaus_pts)
    colaus_only         <- setdiff(colaus_pts, osteo_pts)
    pct_osteo_in_colaus <- round(length(in_both) / length(osteo_pts) * 100, 1)
    
    tibble::tribble(
        ~check,                                  ~result,                                          ~n,                    ~pct,                   ~detail,
        "CoLaus pt universe",                    "INFO",                                           length(colaus_pts),    NA_real_,               glue::glue("{length(colaus_pts)} unique pts across all CoLaus waves"),
        "OsteoLaus pt universe",                 "INFO",                                           length(osteo_pts),     NA_real_,               glue::glue("{length(osteo_pts)} unique pts across all OsteoLaus waves"),
        "OsteoLaus pts found in CoLaus",         if (pct_osteo_in_colaus >= 90) "PASS" else "WARN", length(in_both),     pct_osteo_in_colaus,    glue::glue("{length(in_both)}/{length(osteo_pts)} OsteoLaus pts ({pct_osteo_in_colaus}%) appear in CoLaus"),
        "OsteoLaus-only pts (not in CoLaus)",    if (length(osteo_only) == 0) "PASS" else "WARN",   length(osteo_only),  NA_real_,               if (length(osteo_only) == 0) "All OsteoLaus pts found in CoLaus" else glue::glue("Unexpected: {length(osteo_only)} OsteoLaus pts absent from CoLaus"),
        "CoLaus-only pts (expected)",            "INFO",                                           length(colaus_only),   NA_real_,               glue::glue("{length(colaus_only)} CoLaus pts not in OsteoLaus (expected — CoLaus is the parent cohort)")
    )
}

# =============================================================================
# assert_no_failures()
# =============================================================================

#' Hard-stop the pipeline if any QC check returned FAIL.
#'
#' Add this as a terminal QC target that all derivation targets depend on.
#' The pipeline will refuse to build derived outputs until all FAILs are fixed.
#'
#' @param ... Any number of QC result tibbles from check_* functions.
#' @return TRUE invisibly if all pass; stops with a formatted error if not.
assert_no_failures <- function(...) {
    all_results <- dplyr::bind_rows(list(...))
    failures    <- dplyr::filter(all_results, result == "FAIL")
    warnings_n  <- nrow(dplyr::filter(all_results, result == "WARN"))
    
    if (warnings_n > 0)
        message(glue::glue(
            "{warnings_n} QC WARN(s) — review with tar_read(qc_pt_identity) etc. ",
            "Pipeline will continue but these should be investigated."
        ))
    
    if (nrow(failures) > 0) {
        fail_detail <- failures |>
            dplyr::mutate(
                msg = glue::glue("  [FAIL] {check} | pt={pt} | {detail}")
            ) |>
            dplyr::pull(msg) |>
            paste(collapse = "\n")
        
        stop(paste(
            glue::glue("{nrow(failures)} QC check(s) FAILED — fix before proceeding:"),
            fail_detail,
            sep = "\n"
        ))
    }
    
    message(glue::glue(
        "QC passed: {nrow(all_results)} checks, 0 failures, {warnings_n} warnings."
    ))
    invisible(TRUE)
}