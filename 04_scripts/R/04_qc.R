# =============================================================================
# R/04_qc.R
# =============================================================================
# QC check functions for participant identity and cohort overlap.
#
# Every check_*() function:
#   - Returns a structured tibble, never stops the pipeline itself
#   - Result codes: "PASS", "INFO", "WARN", "FAIL"
#   - Is inspectable with tar_read(qc_*)
#
# assert_no_failures() is the gate target. It reads those tibbles and stops
# the pipeline only when a flagged pt is in the OsteoLaus cohort (i.e. will
# enter the analysis). FAILs and WARNs on pts that exist only in CoLaus are
# downgraded to informational messages and do not stop the pipeline.
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================


# =============================================================================
# INTERNAL HELPER — standardise extra columns
# =============================================================================

# Ensure the six audit columns always exist in every result tibble, regardless
# of which check produced it.
.ensure_audit_cols <- function(tbl) {
    if (!"age_osteolaus"       %in% names(tbl)) tbl$age_osteolaus       <- NA_real_
    if (!"age_colaus"          %in% names(tbl)) tbl$age_colaus          <- NA_real_
    if (!"exam_date_osteolaus" %in% names(tbl)) tbl$exam_date_osteolaus <- as.Date(NA)
    if (!"exam_date_colaus"    %in% names(tbl)) tbl$exam_date_colaus    <- as.Date(NA)
    if (!"wave_osteolaus"      %in% names(tbl)) tbl$wave_osteolaus      <- NA_character_
    if (!"wave_colaus"         %in% names(tbl)) tbl$wave_colaus         <- NA_character_
    tbl
}

# One-row PASS summary tibble — used when a check finds no problems.
.pass_row <- function(check, cohort, detail) {
    tibble::tibble(
        check               = check,
        cohort              = cohort,
        pt                  = NA_character_,
        result              = "PASS",
        detail              = detail,
        age_osteolaus       = NA_real_,
        age_colaus          = NA_real_,
        exam_date_osteolaus = as.Date(NA),
        exam_date_colaus    = as.Date(NA),
        wave_osteolaus      = NA_character_,
        wave_colaus         = NA_character_
    )
}

# One-row INFO summary tibble.
.info_row <- function(check, cohort, detail) {
    .pass_row(check, cohort, detail) |>
        dplyr::mutate(result = "INFO")
}


# =============================================================================
# check_pt_identity()
# =============================================================================

#' Verify that pt refers to the same biological person across all wave files
#' and both cohorts.
#'
#' Uses anchor variables that are fairly stable within a person over time. Returning
#' a structured tibble means the pipeline can inspect all flags before the gate
#' target decides what to stop on.
#'
#' Anchors
#' ───────
#' sex (CoLaus only)
#'   Biologically stable. Any pt where sex changes across CoLaus waves is a
#'   FAIL — either a data extraction error or a pt ID collision.
#'
#' height (both cohorts, within and cross)
#'   Adult height is stable. Variation > ht_tol cm flags as WARN. Cross-cohort
#'   height disagreement is the key identity test.
#'
#' age trajectory (both cohorts, within)
#'   Age must increase monotonically. Age going backwards = FAIL; implausible
#'   rate = WARN. Calculated from exam_date_iso where available.
#'
#' age cross-cohort
#'   At the nearest paired exam dates, age should agree within calendar gap
#'   ± age_tol years. Discrepancy = WARN.
#'
#' @param colaus_long Stacked harmonised CoLaus tibble (output of stack_waves()).
#' @param osteo_long  Stacked harmonised OsteoLaus tibble (output of stack_waves()).
#' @param ht_tol      Maximum height variation (cm) before flagging. Default 3.
#' @param age_tol     Extra year tolerance on top of calendar gap. Default 2.
#' @return Tibble: check, cohort, pt, result, detail + six audit columns.
check_pt_identity <- function(colaus_long,
                              osteo_long,
                              ht_tol  = 3,
                              age_tol = 2) {
    
    results <- list()
    
    # ── 1. Sex stability within CoLaus ─────────────────────────────────────────
    if ("sex" %in% names(colaus_long)) {
        
        sex_check <- colaus_long |>
            dplyr::filter(!is.na(sex), !is.na(pt)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(
                n_waves    = dplyr::n(),
                n_distinct = dplyr::n_distinct(as.character(sex)),
                vals       = paste(sort(unique(as.character(sex))), collapse = " / "),
                .groups    = "drop"
            ) |>
            dplyr::filter(n_distinct > 1)
        
        if (nrow(sex_check) == 0) {
            results[["sex"]] <- .pass_row(
                "sex stable within CoLaus", "CoLaus",
                "All pts have consistent sex across CoLaus waves."
            )
        } else {
            results[["sex"]] <- sex_check |>
                dplyr::transmute(
                    check               = "sex stable within CoLaus",
                    cohort              = "CoLaus",
                    pt                  = as.character(pt),
                    result              = "FAIL",
                    detail              = glue::glue(
                        "sex changes across {n_waves} wave(s): {vals}"
                    ),
                    age_osteolaus       = NA_real_,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    
    # ── 2. Height stability within CoLaus ──────────────────────────────────────
    ht_col_c <- intersect(c("ht", "Height"), names(colaus_long))[1]
    
    if (!is.na(ht_col_c)) {
        ht_c_sum <- colaus_long |>
            dplyr::filter(!is.na(.data[[ht_col_c]]), !is.na(pt)) |>
            dplyr::mutate(ht = suppressWarnings(as.numeric(.data[[ht_col_c]]))) |>
            dplyr::filter(!is.na(ht)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(
                ht_range = max(ht) - min(ht),
                ht_vals  = paste(round(sort(unique(ht)), 1), collapse = " / "),
                .groups  = "drop"
            ) |>
            dplyr::filter(ht_range > ht_tol)
        
        check_lbl <- paste0("height stable within CoLaus (tol=", ht_tol, "cm)")
        
        if (nrow(ht_c_sum) == 0) {
            results[["ht_colaus"]] <- .pass_row(
                check_lbl, "CoLaus",
                glue::glue("All pts have height variation <= {ht_tol} cm across CoLaus waves.")
            )
        } else {
            results[["ht_colaus"]] <- ht_c_sum |>
                dplyr::transmute(
                    check               = check_lbl,
                    cohort              = "CoLaus",
                    pt                  = as.character(pt),
                    result              = "WARN",
                    detail              = glue::glue(
                        "Height range {round(ht_range, 1)} cm > {ht_tol} cm tolerance. Values: {ht_vals}"
                    ),
                    age_osteolaus       = NA_real_,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    # ── 3. Height stability within OsteoLaus ───────────────────────────────────
    ht_col_o <- intersect(c("Height", "ht"), names(osteo_long))[1]
    
    if (!is.na(ht_col_o)) {
        ht_o_sum <- osteo_long |>
            dplyr::filter(!is.na(.data[[ht_col_o]]), !is.na(pt)) |>
            dplyr::mutate(ht = suppressWarnings(as.numeric(.data[[ht_col_o]]))) |>
            dplyr::filter(!is.na(ht)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(
                ht_range = max(ht) - min(ht),
                ht_vals  = paste(round(sort(unique(ht)), 1), collapse = " / "),
                .groups  = "drop"
            ) |>
            dplyr::filter(ht_range > ht_tol)
        
        check_lbl <- paste0("height stable within OsteoLaus (tol=", ht_tol, "cm)")
        
        if (nrow(ht_o_sum) == 0) {
            results[["ht_osteo"]] <- .pass_row(
                check_lbl, "OsteoLaus",
                glue::glue("All pts have height variation <= {ht_tol} cm across OsteoLaus waves.")
            )
        } else {
            results[["ht_osteo"]] <- ht_o_sum |>
                dplyr::transmute(
                    check               = check_lbl,
                    cohort              = "OsteoLaus",
                    pt                  = as.character(pt),
                    result              = "WARN",
                    detail              = glue::glue(
                        "Height range {round(ht_range, 1)} cm > {ht_tol} cm tolerance. Values: {ht_vals}"
                    ),
                    age_osteolaus       = NA_real_,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    # ── 4. Cross-cohort height agreement ───────────────────────────────────────
    if (!is.na(ht_col_c) && !is.na(ht_col_o)) {
        
        ht_c_med <- colaus_long |>
            dplyr::filter(!is.na(.data[[ht_col_c]]), !is.na(pt)) |>
            dplyr::mutate(ht = suppressWarnings(as.numeric(.data[[ht_col_c]]))) |>
            dplyr::filter(!is.na(ht)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(ht_colaus = median(ht), .groups = "drop")
        
        ht_o_med <- osteo_long |>
            dplyr::filter(!is.na(.data[[ht_col_o]]), !is.na(pt)) |>
            dplyr::mutate(ht = suppressWarnings(as.numeric(.data[[ht_col_o]]))) |>
            dplyr::filter(!is.na(ht)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(ht_osteo = median(ht), .groups = "drop")
        
        ht_cross   <- dplyr::inner_join(ht_c_med, ht_o_med, by = "pt") |>
            dplyr::mutate(ht_diff = abs(ht_colaus - ht_osteo)) |>
            dplyr::filter(ht_diff > ht_tol)
        n_compared <- nrow(dplyr::inner_join(ht_c_med, ht_o_med, by = "pt"))
        
        check_lbl <- paste0("height agrees cross-cohort (tol=", ht_tol, "cm)")
        
        if (nrow(ht_cross) == 0) {
            results[["ht_cross"]] <- .pass_row(
                check_lbl, "Both",
                glue::glue(
                    "All {n_compared} pts with height in both cohorts agree within {ht_tol} cm. Strong evidence pt IDs are consistent cross-cohort."
                )
            )
        } else {
            results[["ht_cross"]] <- ht_cross |>
                dplyr::transmute(
                    check               = check_lbl,
                    cohort              = "Both",
                    pt                  = as.character(pt),
                    result              = "WARN",
                    detail              = glue::glue(
                        "CoLaus {round(ht_colaus, 1)} cm vs OsteoLaus {round(ht_osteo, 1)} cm (diff {round(ht_diff, 1)} cm > {ht_tol} cm). Verify same person."
                    ),
                    age_osteolaus       = NA_real_,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    # ── 5. Age trajectory within CoLaus ────────────────────────────────────────
    age_col_c <- intersect(c("age", "Age"), names(colaus_long))[1]
    
    if (!is.na(age_col_c) && "exam_date_iso" %in% names(colaus_long)) {
        
        age_c <- colaus_long |>
            dplyr::filter(!is.na(.data[[age_col_c]]), !is.na(pt),
                          !is.na(exam_date_iso)) |>
            dplyr::mutate(age = suppressWarnings(as.numeric(.data[[age_col_c]]))) |>
            dplyr::filter(!is.na(age)) |>
            dplyr::arrange(pt, exam_date_iso) |>
            dplyr::group_by(pt) |>
            dplyr::filter(dplyr::n() > 1) |>
            dplyr::mutate(
                age_diff     = age - dplyr::lag(age),
                cal_gap      = as.numeric(exam_date_iso - dplyr::lag(exam_date_iso)) / 365.25,
                exp_min      = cal_gap - 1,
                exp_max      = cal_gap + age_tol
            ) |>
            dplyr::filter(!is.na(age_diff)) |>
            dplyr::mutate(
                traj_result = dplyr::case_when(
                    age_diff < 0       ~ "FAIL",
                    age_diff < exp_min ~ "WARN",
                    age_diff > exp_max ~ "WARN",
                    .default           = "PASS"
                )
            ) |>
            dplyr::filter(traj_result != "PASS") |>
            dplyr::ungroup()
        
        if (nrow(age_c) == 0) {
            results[["age_colaus"]] <- .pass_row(
                "age trajectory within CoLaus", "CoLaus",
                "All pts show plausible age progression across CoLaus waves."
            )
        } else {
            results[["age_colaus"]] <- age_c |>
                dplyr::transmute(
                    check               = "age trajectory within CoLaus",
                    cohort              = "CoLaus",
                    pt                  = as.character(pt),
                    result              = traj_result,
                    detail              = glue::glue(
                        "Wave {.wave}: age change {round(age_diff, 1)} yr over {round(cal_gap, 1)} calendar yr (expected {round(exp_min, 1)}-{round(exp_max, 1)} yr)"
                    ),
                    age_osteolaus       = NA_real_,
                    age_colaus          = age,
                    exam_date_osteolaus = as.Date(NA),
                    exam_date_colaus    = exam_date_iso,
                    wave_osteolaus      = NA_character_,
                    wave_colaus         = .wave
                )
        }
    }
    
    # ── 6. Age trajectory within OsteoLaus ─────────────────────────────────────
    age_col_o <- intersect(c("Age", "age"), names(osteo_long))[1]
    
    if (!is.na(age_col_o) && "exam_date_iso" %in% names(osteo_long)) {
        
        age_o <- osteo_long |>
            dplyr::filter(!is.na(.data[[age_col_o]]), !is.na(pt),
                          !is.na(exam_date_iso)) |>
            dplyr::mutate(age = suppressWarnings(as.numeric(.data[[age_col_o]]))) |>
            dplyr::filter(!is.na(age)) |>
            dplyr::arrange(pt, exam_date_iso) |>
            dplyr::group_by(pt) |>
            dplyr::filter(dplyr::n() > 1) |>
            dplyr::mutate(
                age_diff = age - dplyr::lag(age),
                cal_gap  = as.numeric(exam_date_iso - dplyr::lag(exam_date_iso)) / 365.25,
                exp_min  = cal_gap - 1,
                exp_max  = cal_gap + age_tol
            ) |>
            dplyr::filter(!is.na(age_diff)) |>
            dplyr::mutate(
                traj_result = dplyr::case_when(
                    age_diff < 0       ~ "FAIL",
                    age_diff < exp_min ~ "WARN",
                    age_diff > exp_max ~ "WARN",
                    .default           = "PASS"
                )
            ) |>
            dplyr::filter(traj_result != "PASS") |>
            dplyr::ungroup()
        
        if (nrow(age_o) == 0) {
            results[["age_osteo"]] <- .pass_row(
                "age trajectory within OsteoLaus", "OsteoLaus",
                "All pts show plausible age progression across OsteoLaus waves."
            )
        } else {
            results[["age_osteo"]] <- age_o |>
                dplyr::transmute(
                    check               = "age trajectory within OsteoLaus",
                    cohort              = "OsteoLaus",
                    pt                  = as.character(pt),
                    result              = traj_result,
                    detail              = glue::glue(
                        "Wave {.wave}: age change {round(age_diff, 1)} yr over {round(cal_gap, 1)} calendar yr (expected {round(exp_min, 1)}-{round(exp_max, 1)} yr)"
                    ),
                    age_osteolaus       = age,
                    age_colaus          = NA_real_,
                    exam_date_osteolaus = exam_date_iso,
                    exam_date_colaus    = as.Date(NA),
                    wave_osteolaus      = .wave,
                    wave_colaus         = NA_character_
                )
        }
    }
    
    # ── 7. Cross-cohort age agreement ──────────────────────────────────────────
    if (!is.na(age_col_c) && !is.na(age_col_o) &&
        "exam_date_iso" %in% names(colaus_long) &&
        "exam_date_iso" %in% names(osteo_long)) {
        
        c_dates <- colaus_long |>
            dplyr::filter(!is.na(pt), !is.na(exam_date_iso),
                          !is.na(.data[[age_col_c]])) |>
            dplyr::mutate(age = suppressWarnings(as.numeric(.data[[age_col_c]]))) |>
            dplyr::filter(!is.na(age)) |>
            dplyr::select(pt, colaus_date = exam_date_iso, colaus_age = age,
                          colaus_wave = .wave)
        
        o_dates <- osteo_long |>
            dplyr::filter(!is.na(pt), !is.na(exam_date_iso),
                          !is.na(.data[[age_col_o]])) |>
            dplyr::mutate(age = suppressWarnings(as.numeric(.data[[age_col_o]]))) |>
            dplyr::filter(!is.na(age)) |>
            dplyr::select(pt, osteo_date = exam_date_iso, osteo_age = age,
                          osteo_wave = .wave)
        
        #TODO: does this join make sense?
        cross_age <- o_dates |>
            dplyr::inner_join(c_dates, by = "pt", relationship = "many-to-many") |>
            dplyr::mutate(
                gap_days        = abs(as.numeric(osteo_date - colaus_date)),
                cal_gap         = as.numeric(osteo_date - colaus_date) / 365.25,
                age_diff        = osteo_age - colaus_age,
                age_discrepancy = abs(age_diff - cal_gap)
            ) |>
            dplyr::group_by(pt, osteo_date) |>
            dplyr::slice_min(gap_days, n = 1, with_ties = FALSE) |>
            dplyr::ungroup() |>
            dplyr::filter(age_discrepancy > (age_tol + 1))
        
        if (nrow(cross_age) == 0) {
            results[["age_cross"]] <- .pass_row(
                "age agrees cross-cohort", "Both",
                "All nearest CoLaus/OsteoLaus exam pairs show consistent age."
            )
        } else {
            results[["age_cross"]] <- cross_age |>
                dplyr::transmute(
                    check               = "age agrees cross-cohort",
                    cohort              = "Both",
                    pt                  = as.character(pt),
                    result              = "WARN",
                    detail              = glue::glue(
                        "OsteoLaus age {round(osteo_age, 1)} vs CoLaus age {round(colaus_age, 1)} ({round(cal_gap, 1)} yr apart by date): discrepancy {round(age_discrepancy, 1)} yr — verify same person."
                    ),
                    age_osteolaus       = osteo_age,
                    age_colaus          = colaus_age,
                    exam_date_osteolaus = osteo_date,
                    exam_date_colaus    = colaus_date,
                    wave_osteolaus      = osteo_wave,
                    wave_colaus         = colaus_wave
                )
        }
    }
    
    # ── Combine and return ──────────────────────────────────────────────────────
    out <- dplyr::bind_rows(results) |>
        .ensure_audit_cols() |>
        dplyr::select(
            check, cohort, pt, result, detail,
            age_osteolaus, age_colaus,
            exam_date_osteolaus, exam_date_colaus,
            wave_osteolaus, wave_colaus
        ) |>
        dplyr::arrange(
            factor(result, levels = c("FAIL", "WARN", "INFO", "PASS")),
            check, pt
        )
    
    summary_tbl <- dplyr::count(out, check, result) |>
        dplyr::arrange(check, factor(result, levels = c("FAIL", "WARN", "INFO", "PASS")))
    
    cli::cli_inform(c(
        "i" = "pt identity check summary:",
        "*" = paste(capture.output(print(summary_tbl, n = Inf)), collapse = "\n")
    ))
    
    return(out)
}


# =============================================================================
# check_pt_overlap()
# =============================================================================

#' Check participant overlap between CoLaus and OsteoLaus.
#'
#' Since OsteoLaus is a sub-cohort of CoLaus, virtually all OsteoLaus pts
#' should appear in CoLaus. Returns an INFO/PASS/WARN summary tibble.
#'
#' @param colaus_long Stacked harmonised CoLaus tibble.
#' @param osteo_long  Stacked harmonised OsteoLaus tibble.
#' @return Tibble: check, result, n, pct, detail.
check_pt_overlap <- function(colaus_long, osteo_long) {
    
    colaus_pts <- unique(colaus_long$pt[!is.na(colaus_long$pt)])
    osteo_pts  <- unique(osteo_long$pt[!is.na(osteo_long$pt)])
    
    in_both             <- intersect(colaus_pts, osteo_pts)
    osteo_only          <- setdiff(osteo_pts,  colaus_pts)
    colaus_only         <- setdiff(colaus_pts, osteo_pts)
    pct_osteo_in_colaus <- round(length(in_both) / length(osteo_pts) * 100, 1)
    
    tibble::tribble(
        ~check,                               ~result,
        ~n,                  ~pct,
        ~detail,
        
        "CoLaus pt universe",
        "INFO",
        length(colaus_pts),  NA_real_,
        glue::glue("{length(colaus_pts)} unique pts across all CoLaus waves"),
        
        "OsteoLaus pt universe",
        "INFO",
        length(osteo_pts),   NA_real_,
        glue::glue("{length(osteo_pts)} unique pts across all OsteoLaus waves"),
        
        "OsteoLaus pts found in CoLaus",
        if (pct_osteo_in_colaus >= 90) "PASS" else "WARN",
        length(in_both),     pct_osteo_in_colaus,
        glue::glue(
            "{length(in_both)}/{length(osteo_pts)} OsteoLaus pts ({pct_osteo_in_colaus}%) appear in CoLaus"
        ),
        
        "OsteoLaus-only pts (not in CoLaus)",
        if (length(osteo_only) == 0) "PASS" else "WARN",
        length(osteo_only),  NA_real_,
        if (length(osteo_only) == 0)
            "All OsteoLaus pts found in CoLaus"
        else
            glue::glue(
                "{length(osteo_only)} OsteoLaus pt(s) absent from CoLaus — exposure variables will be NA for these participants"
            ),
        
        "CoLaus-only pts (expected)",
        "INFO",
        length(colaus_only), NA_real_,
        glue::glue(
            "{length(colaus_only)} CoLaus pts not in OsteoLaus (expected — CoLaus is the parent cohort)"
        )
    )
}

#TODO: make more light weight, get rid of any unnecessary descriptions
# =============================================================================
# assert_no_failures()
# =============================================================================

#' QC gate: classify all flagged participants and return an exclusion tibble.
#'
#' Rationale
#' ─────────
#' Rather than stopping the pipeline on OsteoLaus FAILs, this function returns
#' a structured tibble of QC-flagged participants. freeze_dataset() consumes
#' this tibble as an additional hard exclusion source (criterion 0, applied
#' before all other criteria), so excluded participants are fully traceable
#' in the flow log with their QC reason.
#'
#' The pipeline never aborts. All FAILs and WARNs are surfaced as messages so
#' the analyst can review them, and any OsteoLaus FAIL automatically propagates
#' into the exclusion flow without manual intervention.
#'
#' Classification logic
#' ────────────────────
#'   OsteoLaus FAIL   -> printed as error-level message; pt added to exclusion
#'                       tibble with reason "QC FAIL [<check>]: <detail>"
#'   OsteoLaus WARN   -> printed as warning-level message; pt NOT excluded
#'                       (sporadic biological variation is expected)
#'   CoLaus-only FAIL -> printed as info (downgraded); pt not in analysis
#'   CoLaus-only WARN -> printed as info (lowest visibility)
#'
#' @param ... Any number of QC result tibbles from check_*().
#' @return Tibble with columns pt (character) and qc_exclude_reason (character).
#'   Has zero rows when no OsteoLaus FAILs were found. Passed to
#'   freeze_dataset() via the qc_exclusions argument.
assert_no_failures <- function(...) {
    
    all_results <- dplyr::bind_rows(list(...))
    
    # ── Determine which pts appear in OsteoLaus ───────────────────────────────────────────
    osteo_pts_flagged <- all_results |>
        dplyr::filter(!is.na(pt), cohort %in% c("OsteoLaus", "Both")) |>
        dplyr::pull(pt) |>
        unique()
    
    # ── Classify every FAIL/WARN by cohort relevance ──────────────────────────────
    flagged <- all_results |>
        dplyr::filter(result %in% c("FAIL", "WARN")) |>
        dplyr::mutate(
            affects_osteo = dplyr::case_when(
                is.na(pt)                          ~ TRUE,
                cohort %in% c("OsteoLaus", "Both") ~ TRUE,
                pt %in% osteo_pts_flagged          ~ TRUE,
                .default                           = FALSE
            )
        )
    
    osteo_fails  <- dplyr::filter(flagged, result == "FAIL",  affects_osteo)
    osteo_warns  <- dplyr::filter(flagged, result == "WARN",  affects_osteo)
    colaus_fails <- dplyr::filter(flagged, result == "FAIL", !affects_osteo)
    colaus_warns <- dplyr::filter(flagged, result == "WARN", !affects_osteo)
    
    # ── OsteoLaus FAILs — message + mark for exclusion (no abort) ────────────────────
    if (nrow(osteo_fails) > 0) {
        fail_msgs <- purrr::map_chr(
            seq_len(nrow(osteo_fails)),
            ~ glue::glue(
                "  [QC FAIL] {osteo_fails$check[.x]} | pt={osteo_fails$pt[.x]} | {osteo_fails$detail[.x]}"
            )
        )
        cli::cli_inform(c(
            "x" = "{nrow(osteo_fails)} QC FAIL(s) on OsteoLaus participant(s) \u2014 these pts will be excluded from analysis:",
            "*" = paste(fail_msgs, collapse = "\n")
        ))
    }
    
    # ── OsteoLaus WARNs — printed, not excluded ───────────────────────────────────────
    if (nrow(osteo_warns) > 0) {
        warn_msgs <- purrr::map_chr(
            seq_len(nrow(osteo_warns)),
            ~ glue::glue(
                "  [QC WARN] {osteo_warns$check[.x]} | pt={osteo_warns$pt[.x]} | {osteo_warns$detail[.x]}"
            )
        )
        cli::cli_warn(c(
            "{nrow(osteo_warns)} QC WARN(s) on OsteoLaus pt(s) \u2014 pipeline continues; review recommended:",
            "*" = paste(warn_msgs, collapse = "\n")
        ))
    }
    
    # ── CoLaus-only FAILs — downgraded to info ───────────────────────────────────
    if (nrow(colaus_fails) > 0) {
        cli::cli_inform(c(
            "!" = "{nrow(colaus_fails)} FAIL(s) on CoLaus-only pt(s) (not in OsteoLaus) \u2014 downgraded to informational.",
            "*" = paste(purrr::map_chr(
                seq_len(nrow(colaus_fails)),
                ~ glue::glue(
                    "  [CoLaus-only FAIL] {colaus_fails$check[.x]} | pt={colaus_fails$pt[.x]} | {colaus_fails$detail[.x]}"
                )
            ), collapse = "\n")
        ))
    }
    
    # ── CoLaus-only WARNs — low-visibility info ──────────────────────────────────
    if (nrow(colaus_warns) > 0) {
        cli::cli_inform(
            "{nrow(colaus_warns)} WARN(s) on CoLaus-only pt(s) \u2014 not in OsteoLaus, no action required."
        )
    }
    
    # ── Cohort-level FAILs (no individual pt) ─────────────────────────────────────
    cohort_level_fails <- dplyr::filter(osteo_fails, is.na(pt))
    if (nrow(cohort_level_fails) > 0) {
        cli::cli_warn(c(
            "{nrow(cohort_level_fails)} cohort-level QC FAIL(s) have no individual pt attribution \u2014 manual review required:",
            "*" = paste(purrr::map_chr(
                seq_len(nrow(cohort_level_fails)),
                ~ glue::glue(
                    "  {cohort_level_fails$check[.x]}: {cohort_level_fails$detail[.x]}"
                )
            ), collapse = "\n")
        ))
    }
    
    # ── Summary ──────────────────────────────────────────────────────────────────────────
    n_excl <- nrow(dplyr::filter(osteo_fails, !is.na(pt)))
    cli::cli_inform(c(
        "v" = "QC gate complete.",
        "i" = "{n_excl} OsteoLaus pt(s) flagged for exclusion | {nrow(osteo_warns)} OsteoLaus WARNs | {nrow(colaus_fails)} CoLaus-only FAILs (downgraded) | {nrow(colaus_warns)} CoLaus-only WARNs"
    ))
    
    # ── Return exclusion tibble ────────────────────────────────────────────────────
    # One row per unique pt with at least one OsteoLaus FAIL.
    # Multiple FAILs on the same pt are concatenated into one reason string.
    osteo_fails |>
        dplyr::filter(!is.na(pt)) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            qc_exclude_reason = paste(
                glue::glue("QC FAIL [{check}]: {detail}"),
                collapse = "; "
            ),
            .groups = "drop"
        ) |>
        dplyr::select(pt, qc_exclude_reason)
}