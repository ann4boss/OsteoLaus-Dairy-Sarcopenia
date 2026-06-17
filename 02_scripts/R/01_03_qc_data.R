# =============================================================================
# R/qc.R
# =============================================================================
# QC check functions for participant identity, cohort consistency, and visit integrity.
#
# This script provides functions to perform quality control on harmonised cohort 
# data (CoLaus and OsteoLaus). QC checks are performed per participant per visit 
# and produce a separate QC table for troubleshooting.
#
# Quality checks include:
#   - Presence of primary key (`pt`) and exam date (`exam_date_iso`)
#   - Duplicate `pt` values within a visit
#   - Presence in OsteoLaus cohort and cross-cohort overlap with CoLaus
#   - Stability of sex across visits (CoLaus only)
#   - datbirth present at Baseline (both CoLaus and OsteoLaus)
#
# Each QC check produces a TRUE/FALSE flag per participant row. Additionally, a
# troubleshooting table can include key columns (pt, cohort,exam_date_iso,datbirth, visit, sex)
# along with the QC flags to inspect why a participant failed any check.
#
# =============================================================================

#' Perform QC checks on harmonised cohort files
#'
#' @param harmonised_list A named list of data frames, one per visit/cohort,
#'   containing harmonised participant data including `pt`, `cohort`, `visit`,
#'    `sex`, `exam_date_iso`, and `datbirth`.
#'
#' @return A data frame (qc_tbl) with one row per participant per visit, containing:
#'   - Key identifiers: `pt`, `cohort`, `visit`,`sex`, `exam_date_iso`
#'   - QC flags: `qc_pt_present`, `qc_exam_date`, `qc_in_osteolaus`,
#'     `qc_datbirth_baseline`, `qc_sex_stable`, etc.
#'   - Flags indicate TRUE for passing the QC check, FALSE if the check failed.
#'
#' This table can be used to summarize QC failures, inspect problematic participants,
#' and generate summary messages for data quality reporting.
# =============================================================================
qc <- function(harmonised_list) {
    cli::cli_h1("QC Report")
    
    # ── Select and bind ------------------------------------------------------
    df_all <- harmonised_list |>
        lapply(function(df) {
            df |>
                dplyr::select(
                    dplyr::any_of(c("pt", ".cohort", ".visit", "sex",
                                    "exam_date_iso", "datbirth"))
                )
        }) |>
        dplyr::bind_rows() |>
        dplyr::arrange(pt, .cohort, .visit)
    
    # ── Initialize basic QC flags  -----------------------------------------
    qc_tbl <- df_all |>
        dplyr::mutate(
            qc_pt_present = !is.na(pt),
            qc_exam_date  = !is.na(exam_date_iso)
        ) |>
        
        # create pt-level flag
        dplyr::group_by(pt) |>
        dplyr::mutate(
            qc_in_osteolaus = any(.cohort == "OsteoLaus", na.rm = TRUE)
        ) |>
        dplyr::ungroup()
    
    # ── Sex stability (ignores NAs)  -----------------------------------------
    sex_check <- df_all |>
        dplyr::filter(.cohort == "CoLaus") |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            qc_sex_stable = dplyr::n_distinct(sex, na.rm = TRUE) <= 1, 
            .groups = "drop"
        )
    
    # ── datbirth present at Baseline (both cohorts)  -------------------------
    datbirth_check <- df_all |>
        dplyr::filter(.visit == "Baseline") |>
        dplyr::group_by(pt, .cohort) |>
        dplyr::summarise(
            qc_datbirth_baseline = any(!is.na(datbirth)),
            .groups = "drop"
        )
    
    # ── Duplicate pts -----------------------------------------
    dup_check <- df_all |>
        dplyr::group_by(pt, .cohort) |>
        dplyr::summarise(
            qc_pt_unique = dplyr::n() == dplyr::n_distinct(.visit), 
            .groups = "drop"
        )
    
    # ── Join and Osteo/CoLaus Overlap -----------------------------------------
    colaus_pts <- df_all |> dplyr::filter(.cohort == "CoLaus") |> dplyr::distinct(pt)
    
    qc_tbl <- qc_tbl |>
        dplyr::left_join(sex_check,      by = "pt") |>
        dplyr::left_join(datbirth_check, by = c("pt", ".cohort")) |>
        dplyr::left_join(dup_check,      by = c("pt", ".cohort")) |>
        dplyr::left_join(colaus_pts |> dplyr::mutate(in_colaus = TRUE), by = "pt") |>
        dplyr::mutate(
            # Participants that never appear at Baseline get NA from the left-join;
            # treat NA conservatively as FALSE (missing datbirth).
            qc_datbirth_baseline = dplyr::coalesce(qc_datbirth_baseline, FALSE),
            qc_osteo_in_colaus   = dplyr::if_else(.cohort == "OsteoLaus", !is.na(in_colaus), TRUE)
        ) |>
        dplyr::select(-in_colaus)
    
    # ── Finalize and Summarize -----------------------------------------
    res_tbl <- dplyr::as_tibble(qc_tbl)
    
    qc_summary <- res_tbl |>
        dplyr::summarise(
            n_total_pt                    = dplyr::n_distinct(pt),
            n_total_pt_in_osteo           = dplyr::n_distinct(pt[qc_in_osteolaus == TRUE]),
            n_fail_pt_present             = dplyr::n_distinct(pt[qc_pt_present == FALSE]),
            n_fail_exam_date              = dplyr::n_distinct(pt[qc_exam_date == FALSE]),
            n_fail_exam_date_osteo        = dplyr::n_distinct(pt[qc_exam_date == FALSE & qc_in_osteolaus == TRUE]),
            n_fail_datbirth_colaus        = dplyr::n_distinct(pt[qc_datbirth_baseline == FALSE & .cohort == "CoLaus"]),
            n_fail_datbirth_osteo         = dplyr::n_distinct(pt[qc_datbirth_baseline == FALSE & .cohort == "OsteoLaus"]),
            n_fail_sex_stable             = dplyr::n_distinct(pt[qc_sex_stable == FALSE]),
            n_fail_sex_stable_osteo       = dplyr::n_distinct(pt[qc_sex_stable == FALSE & qc_in_osteolaus == TRUE]),
            n_qc_pt_unique                = dplyr::n_distinct(pt[qc_pt_unique == FALSE]),
            n_fail_osteo_in_colaus        = dplyr::n_distinct(pt[qc_osteo_in_colaus == FALSE])
        )
    
    cli::cli_inform(c(
        "i" = "Total unique participants: {qc_summary$n_total_pt}",
        "i" = "Participants in OsteoLaus: {qc_summary$n_total_pt_in_osteo}",
        "x" = "Missing 'pt': {qc_summary$n_fail_pt_present}",
        "x" = "Missing 'exam_date_iso': {qc_summary$n_fail_exam_date}",
        "!" = "Missing 'exam_date_iso' (OsteoLaus): {qc_summary$n_fail_exam_date_osteo}",
        "x" = "Missing 'datbirth' at Baseline (CoLaus): {qc_summary$n_fail_datbirth_colaus}",
        "x" = "Missing 'datbirth' at Baseline (OsteoLaus): {qc_summary$n_fail_datbirth_osteo}",
        "x" = "Inconsistent sex (CoLaus): {qc_summary$n_fail_sex_stable}",
        "!" = "Inconsistent sex (OsteoLaus): {qc_summary$n_fail_sex_stable_osteo}",
        "x" = "Duplicate entries in visit: {qc_summary$n_qc_pt_unique}",
        "x" = "OsteoLaus participants missing in CoLaus: {qc_summary$n_fail_osteo_in_colaus}"
    ))
    
    return(list(
        tbl     = res_tbl,
        summary = qc_summary
    ))
}