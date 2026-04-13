# =============================================================================
# R/exclusion.R
# =============================================================================
# Applies exclusion criteria and produces the analysis-ready dataset,
# and builds a CONSORT-style flow diagram with DiagrammeR.
#
# Inclusion status
# ----------------
#   "yes"     Passes all hard criteria AND has no data quality issues
#   "partial" Passes hard criteria BUT has at least one data quality issue:
#               - missing outcome at Baseline (EWGSOP2 stage)
#               - energy intake out of range
#               - implausible HGS value
#               - required covariate always missing
#               - dairy missing at a follow-up visit
#   "no"      Fails at least one hard (structural) criterion
#
# Hard exclusion criteria
# ------------------------------------------
#   1.  No OsteoLaus visits at all
#   2.  Missing participant ID (pt)
#   3.  Failing pt integrity checks: unstable sex, pt not unique within wave,
#       or age trajectory not strictly increasing
#   4.  Missing exam date at any visit
#   5.  Energy intake out of range at any visit
#  Visit-specific
#   6.  Any required covariate missing across all visits
#   7.  dairy_total_gday missing at any visit
# -> 8. Fewer than min_visits OsteoLaus visits
# Partial exclusion criteria (outcome-specific)
# ------------------------------------------
#   9.  Implausible or missing HGS_peak
#   10. No gait speed measurement at V4 or V5
#   11. Missing ALM_HT2
#   12. Missing EWGSOP2 sarcopenia stage at OsteoLaus Baseline
# =============================================================================

# -----------------------------------------------------------------------------
# Required covariate lists
# -----------------------------------------------------------------------------
# Time-varying covariates pulled from the exposures table.
# Each must be non-NA at least once across a participant's OsteoLaus visits.
.REQUIRED_COVARIATES_EXPOSURES <- c(
    "alcohol_category", "sbsmk",
    "diabetes_status",  "HTN_status",
    "cdv_event",        "hrt_status",
    "pa_levels",        "hypolip_drug_status",
    "corticoids_status","vitD_status",
    "calcium_status",   "bisphosphonate_status"
)

# Energy plausibility window (kcal/day)
.ENERGY_MIN_KCAL <- 500
.ENERGY_MAX_KCAL <- 5000

# HGS plausibility window (kg)
.HGS_MIN_KG <- 1
.HGS_MAX_KG <- 99


# =============================================================================
#' Apply exclusion criteria and produce the analysis-ready dataset
#'
#' @param merged_table_derived  Data frame with one row per participant per
#'   wave, containing harmonised and derived variables (e.g. HGS_peak,
#'   gait_speed, ALM_HT2, EWGSOP2_stage, energy_kcal, dairy_total_gday,
#'   and all .REQUIRED_COVARIATES_EXPOSURES). Must contain `.wave_osteo` columns.
#' @param qc_tbl     Per-participant-per-wave QC flag table returned by qc().
#' @param qc_summary Single-row summary table returned by qc(), used to carry
#'   forward the n_fail_exam_date_osteo count from step 3 into the CONSORT.
#' @param min_visits Minimum number of distinct OsteoLaus visits required
#'   (hard criterion 8). Default: 2.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{data}{The full merged_table_derived with an `inclusion` column
#'       added ("yes" / "partial" / "no").}
#'     \item{consort}{A DiagrammeR grViz object (CONSORT-style flow diagram).}
#'     \item{counts}{Named list of participant counts at each exclusion step,
#'       suitable for downstream reporting.}
#'   }
# =============================================================================
apply_exclusions <- function(merged_table_derived,
                             qc_tbl,
                             qc_summary,
                             min_visits = 2) {
    
    cli::cli_h1("Exclusion Criteria")
    
    # =========================================================================
    # 1.  Build a participant-level flag table
    # =========================================================================
    
    # ── QC flags: collapse per-wave flags to per-participant ─────────────────
    # A participant fails a check if they fail it at *any* wave (conservative).
    qc_per_pt <- qc_tbl |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            has_osteolaus_visit = any(qc_in_osteolaus,   na.rm = TRUE),
            qc_pt_present       = all(qc_pt_present,     na.rm = TRUE),
            qc_exam_date        = all(qc_exam_date,      na.rm = TRUE),
            qc_sex_stable       = all(qc_sex_stable,     na.rm = TRUE),
            qc_pt_unique        = all(qc_pt_unique,      na.rm = TRUE),
            qc_age_increasing   = all(qc_age_increasing, na.rm = TRUE),
            .groups = "drop"
        )
    
    # ── OsteoLaus visit count per participant ─────────────────────────────────
    osteo_visit_counts <- merged_table_derived |>
        dplyr::filter(is.na(.wave_osteo)) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(n_osteo_visits = dplyr::n_distinct(.wave_osteo), .groups = "drop")
    
    # ── Energy plausibility (hard criterion 5) ────────────────────────────────
    energy_check <- .flag_energy(merged_table_derived)
    
    # ── Required covariates: each must appear at least once (hard criterion 6) ─
    cov_check <- .flag_covariates(merged_table_derived)
    
    # ── Dairy: must be present at every OsteoLaus visit (hard criterion 7) ───
    dairy_check <- .flag_dairy(merged_table_derived)
    
    # ── Partial / outcome-specific flags (criteria 9-12) ─────────────────────
    partial_checks <- .flag_partial_outcomes(merged_table_derived)
    
    # ── Assemble ──────────────────────────────────────────────────────────────
    all_pts <- dplyr::distinct(merged_table_derived, pt)
    
    pt_flags <- all_pts |>
        dplyr::left_join(qc_per_pt,          by = "pt") |>
        dplyr::left_join(osteo_visit_counts,  by = "pt") |>
        dplyr::left_join(energy_check,        by = "pt") |>
        dplyr::left_join(cov_check,           by = "pt") |>
        dplyr::left_join(dairy_check,         by = "pt") |>
        dplyr::left_join(partial_checks,      by = "pt") |>
        dplyr::mutate(
            n_osteo_visits      = dplyr::coalesce(n_osteo_visits, 0L),
            has_osteolaus_visit = dplyr::coalesce(has_osteolaus_visit, FALSE)
        )
    
    # =========================================================================
    # 2.  Sequential waterfall exclusion (hard criteria 1-8)
    # =========================================================================
    counts  <- list()
    current <- pt_flags   # shrinks with each exclusion step
    
    counts$n_start <- nrow(current)
    cli::cli_inform(c("i" = "Starting N: {counts$n_start}"))
    
    # Helper: apply one exclusion step, record counts, and report to CLI
    .step <- function(current, counts, cond, label, key_n, key_after) {
        excl     <- dplyr::filter(current, {{ cond }})
        retained <- dplyr::filter(current, !{{ cond }})
        counts[[key_n]]     <- nrow(excl)
        counts[[key_after]] <- nrow(retained)
        cli::cli_inform(c("x" = "Excl. {label}: {nrow(excl)}  ->  {nrow(retained)} remaining"))
        list(current = retained, counts = counts)
    }
    
    # 1. No OsteoLaus visits
    r <- .step(current, counts, !has_osteolaus_visit,
               "no OsteoLaus visits", "n_excl_no_osteo", "n_after_1")
    current <- r$current; counts <- r$counts
    
    # 2. Missing pt
    r <- .step(current, counts, !qc_pt_present,
               "missing participant ID", "n_excl_no_pt", "n_after_2")
    current <- r$current; counts <- r$counts
    
    # 3. Pt integrity (sex stability, uniqueness within wave, age trajectory)
    r <- .step(current, counts,
               qc_sex_stable == FALSE | qc_pt_unique == FALSE | qc_age_increasing == FALSE,
               "pt integrity failure (sex / unique / age)",
               "n_excl_integrity", "n_after_3")
    current <- r$current; counts <- r$counts
    
    # 4. Missing exam date
    #    Apply the flag from qc_tbl; also preserve the Step-3 QC count from
    #    qc_summary for the CONSORT side-box label as a cross-check.
    r <- .step(current, counts, !qc_exam_date,
               "missing exam date", "n_excl_exam_date", "n_after_4")
    current <- r$current; counts <- r$counts
    counts$n_excl_exam_date_from_qc <- qc_summary$n_fail_exam_date_osteo
    
    # 5. Energy intake out of range
    r <- .step(current, counts, !qc_energy_ok,
               "energy intake out of range", "n_excl_energy", "n_after_5")
    current <- r$current; counts <- r$counts
    
    # # 6. Required covariate always missing
    # r <- .step(current, counts, !qc_required_covs,
    #            "required covariate always missing", "n_excl_covs", "n_after_6")
    # current <- r$current; counts <- r$counts
    # 
    # # 7. Dairy missing at any visit
    # r <- .step(current, counts, !qc_dairy_ok,
    #            "dairy_total_gday missing at a visit", "n_excl_dairy", "n_after_7")
    # current <- r$current; counts <- r$counts
    # 
    # # 8. Fewer than min_visits OsteoLaus visits
    # r <- .step(current, counts, n_osteo_visits < min_visits,
    #            paste0("< ", min_visits, " OsteoLaus visits"),
    #            "n_excl_min_visits", "n_after_8")
    # current <- r$current; counts <- r$counts
    
    hard_included_pts <- current$pt
    
    # =========================================================================
    # 3.  Partial inclusions (criteria 9-12, among hard-included only)
    # =========================================================================
    partial_cols  <- c("qc_hgs_ok", "qc_gait_v4v5_ok", "qc_alm_ht2_ok", "qc_ewgsop2_bsl_ok")
    avail_partial <- intersect(partial_cols, names(pt_flags))
    
    hard_included_flags <- dplyr::filter(pt_flags, pt %in% hard_included_pts)
    
    if (length(avail_partial) > 0) {
        hard_included_flags <- hard_included_flags |>
            dplyr::mutate(
                is_partial = rowSums(
                    dplyr::across(dplyr::all_of(avail_partial), ~ !. | is.na(.)),
                    na.rm = TRUE
                ) > 0
            )
    } else {
        hard_included_flags <- dplyr::mutate(hard_included_flags, is_partial = FALSE)
    }
    
    counts$n_full_include    <- sum(!hard_included_flags$is_partial, na.rm = TRUE)
    counts$n_partial         <- sum( hard_included_flags$is_partial,  na.rm = TRUE)
    counts$n_partial_hgs     <- if ("qc_hgs_ok"         %in% names(hard_included_flags))
        sum(!hard_included_flags$qc_hgs_ok,        na.rm = TRUE) else 0L
    counts$n_partial_gait    <- if ("qc_gait_v4v5_ok"   %in% names(hard_included_flags))
        sum(!hard_included_flags$qc_gait_v4v5_ok,  na.rm = TRUE) else 0L
    counts$n_partial_alm     <- if ("qc_alm_ht2_ok"     %in% names(hard_included_flags))
        sum(!hard_included_flags$qc_alm_ht2_ok,    na.rm = TRUE) else 0L
    counts$n_partial_ewgsop2 <- if ("qc_ewgsop2_bsl_ok" %in% names(hard_included_flags))
        sum(!hard_included_flags$qc_ewgsop2_bsl_ok, na.rm = TRUE) else 0L
    
    cli::cli_inform(c(
        "!" = "Partial inclusions (outcome-specific issues): {counts$n_partial}",
        " " = "  Missing / implausible HGS:     {counts$n_partial_hgs}",
        " " = "  No gait speed at V4/V5:        {counts$n_partial_gait}",
        " " = "  Missing ALM_HT2:               {counts$n_partial_alm}",
        " " = "  Missing EWGSOP2 at Baseline:   {counts$n_partial_ewgsop2}",
        "v" = "Full inclusions: {counts$n_full_include}"
    ))
    
    # =========================================================================
    # 4.  Stamp inclusion status and exclusion reasons on the full dataset
    # =========================================================================
    partial_pts <- hard_included_flags |>
        dplyr::filter(is_partial) |>
        dplyr::pull(pt)
    
    # ── Build a semicolon-separated reason string per participant ─────────────
    # Hard-exclusion reasons (for inclusion == "no")
    # Partial reasons (for inclusion == "partial")
    # Participants with inclusion == "yes" get NA.
    reason_map <- pt_flags |>
        dplyr::mutate(
            .hard_reasons = purrr::pmap_chr(
                list(
                    has_osteolaus_visit,
                    qc_pt_present,
                    qc_sex_stable, qc_pt_unique, qc_age_increasing,
                    qc_exam_date,
                    qc_energy_ok,
                    qc_required_covs,
                    qc_dairy_ok,
                    n_osteo_visits
                ),
                function(osteo, pt_ok, sex, uniq, age, exam, energy, covs, dairy, n_vis) {
                    r <- character(0)
                    if (!isTRUE(osteo))  r <- c(r, "no OsteoLaus visits")
                    if (!isTRUE(pt_ok))  r <- c(r, "missing participant ID")
                    if (!isTRUE(sex))    r <- c(r, "unstable sex")
                    if (!isTRUE(uniq))   r <- c(r, "pt not unique within wave")
                    if (!isTRUE(age))    r <- c(r, "non-monotonic age trajectory")
                    if (!isTRUE(exam))   r <- c(r, "missing exam date")
                    if (!isTRUE(energy)) r <- c(r, "energy intake out of range")
                    if (!isTRUE(covs))   r <- c(r, "required covariate always missing")
                    if (!isTRUE(dairy))  r <- c(r, "dairy_total_gday missing at follow-up visit")
                    if (!is.na(n_vis) && n_vis < min_visits)
                        r <- c(r, paste0("< ", min_visits, " OsteoLaus visits"))
                    if (length(r) == 0) NA_character_ else paste(r, collapse = "; ")
                }
            )
        ) |>
        dplyr::select(pt, .hard_reasons)
    
    # Partial reasons (only meaningful for hard-included participants)
    partial_reason_map <- pt_flags |>
        dplyr::filter(pt %in% hard_included_pts) |>
        dplyr::mutate(
            .partial_reasons = purrr::pmap_chr(
                list(
                    if ("qc_hgs_ok"         %in% names(pt_flags)) qc_hgs_ok         else rep(TRUE, dplyr::n()),
                    if ("qc_gait_v4v5_ok"   %in% names(pt_flags)) qc_gait_v4v5_ok   else rep(TRUE, dplyr::n()),
                    if ("qc_alm_ht2_ok"     %in% names(pt_flags)) qc_alm_ht2_ok     else rep(TRUE, dplyr::n()),
                    if ("qc_ewgsop2_bsl_ok" %in% names(pt_flags)) qc_ewgsop2_bsl_ok else rep(TRUE, dplyr::n())
                ),
                function(hgs, gait, alm, ewgsop2) {
                    r <- character(0)
                    if (!isTRUE(hgs))     r <- c(r, "missing/implausible HGS_peak")
                    if (!isTRUE(gait))    r <- c(r, "no gait speed at V4/V5")
                    if (!isTRUE(alm))     r <- c(r, "missing ALM_HT2")
                    if (!isTRUE(ewgsop2)) r <- c(r, "missing EWGSOP2 stage at Baseline")
                    if (length(r) == 0) NA_character_ else paste(r, collapse = "; ")
                }
            )
        ) |>
        dplyr::select(pt, .partial_reasons)
    
    inclusion_map <- pt_flags |>
        dplyr::left_join(reason_map,         by = "pt") |>
        dplyr::left_join(partial_reason_map, by = "pt") |>
        dplyr::transmute(
            pt,
            inclusion = dplyr::case_when(
                !(pt %in% hard_included_pts) ~ "no",
                pt %in% partial_pts          ~ "partial",
                TRUE                         ~ "yes"
            ),
            exclusion_reason = dplyr::case_when(
                !(pt %in% hard_included_pts) ~ .hard_reasons,
                pt %in% partial_pts          ~ .partial_reasons,
                TRUE                         ~ NA_character_
            )
        )
    
    analysis_data <- merged_table_derived |>
        dplyr::left_join(inclusion_map, by = "pt") |>
        dplyr::mutate(
            inclusion        = dplyr::coalesce(inclusion, "no"),
            exclusion_reason = dplyr::if_else(inclusion == "no" & is.na(exclusion_reason),
                                              "unmatched in merged table", exclusion_reason)
        )
    
    # =========================================================================
    # 5.  CONSORT diagram
    # =========================================================================
    consort <- build_consort(counts, min_visits)
    
    list(
        data    = analysis_data,
        consort = consort,
        counts  = counts
    )
}


# =============================================================================
# Internal flag helpers
# =============================================================================

#' Energy plausibility flag per participant (OsteoLaus visits only)
#' @keywords internal
.flag_energy <- function(dat) {
    if (!"energy_kcal" %in% names(dat)) {
        return(dplyr::mutate(dplyr::distinct(dat, pt), qc_energy_ok = TRUE))
    }
    dat |>
        dplyr::filter(is.na(.wave_osteo)) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            qc_energy_ok = !any(
                !is.na(energy_kcal) &
                    (energy_kcal < .ENERGY_MIN_KCAL | energy_kcal > .ENERGY_MAX_KCAL)
            ),
            .groups = "drop"
        )
}

#' Required-covariate presence flag per participant (OsteoLaus visits only)
#' Each covariate must be non-NA at least once across all OsteoLaus visits.
#' @keywords internal
.flag_covariates <- function(dat) {
    avail <- intersect(.REQUIRED_COVARIATES_EXPOSURES, names(dat))
    if (length(avail) == 0) {
        return(dplyr::mutate(dplyr::distinct(dat, pt), qc_required_covs = TRUE))
    }
    dat |>
        dplyr::filter(is.na(.wave_osteo)) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            qc_required_covs = all(
                vapply(
                    avail,
                    function(v) any(!is.na(dplyr::pick(dplyr::all_of(v))[[v]])),
                    logical(1)
                )
            ),
            .groups = "drop"
        )
}

#' Dairy completeness flag per participant (OsteoLaus follow-up visits only)
#' Fails if dairy_total_gday is NA at any OsteoLaus visit *excluding* Baseline,
#' where dairy is always missing by design.
#' @keywords internal
.flag_dairy <- function(dat) {
    if (!"dairy_total_gday" %in% names(dat)) {
        return(dplyr::mutate(dplyr::distinct(dat, pt), qc_dairy_ok = TRUE))
    }
    dat |>
        dplyr::filter(is.na(.wave_osteo), .wave_osteo != "Baseline") |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            qc_dairy_ok = !any(is.na(dairy_total_gday)),
            .groups = "drop"
        )
}

#' Partial-outcome flags per participant (criteria 9-12, OsteoLaus only)
#' Returns one column per criterion; TRUE = passes (no issue).
#' @keywords internal
.flag_partial_outcomes <- function(dat) {
    base <- dplyr::distinct(dat, pt)
    
    # 9. HGS_peak — at least one plausible value anywhere across OsteoLaus visits
    if ("HGS_peak" %in% names(dat)) {
        hgs <- dat |>
            dplyr::filter(is.na(.wave_osteo)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(
                qc_hgs_ok = any(
                    !is.na(HGS_peak) & HGS_peak >= .HGS_MIN_KG & HGS_peak <= .HGS_MAX_KG
                ),
                .groups = "drop"
            )
        base <- dplyr::left_join(base, hgs, by = "pt")
    }
    
    # 10. Gait speed — at least one non-NA value at V4 or V5
    if ("gait_speed" %in% names(dat)) {
        gait <- dat |>
            dplyr::filter(is.na(.wave_osteo), .wave_osteo %in% c("V4", "V5")) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(qc_gait_v4v5_ok = any(!is.na(gait_speed)), .groups = "drop")
        base <- dplyr::left_join(base, gait, by = "pt") |>
            # Participants with no V4/V5 row at all are implicitly missing
            dplyr::mutate(qc_gait_v4v5_ok = dplyr::coalesce(qc_gait_v4v5_ok, FALSE))
    }
    
    # 11. ALM_HT2 — non-NA at least once
    if ("ALM_HT2" %in% names(dat)) {
        alm <- dat |>
            dplyr::filter(is.na(.wave_osteo)) |>
            dplyr::group_by(pt) |>
            dplyr::summarise(qc_alm_ht2_ok = any(!is.na(ALM_HT2)), .groups = "drop")
        base <- dplyr::left_join(base, alm, by = "pt")
    }
    
    # 12. EWGSOP2 stage at OsteoLaus Baseline
    if ("EWGSOP2_stage" %in% names(dat)) {
        ewgsop2 <- dat |>
            dplyr::filter(is.na(.wave_osteo), .wave_osteo == "Baseline") |>
            dplyr::group_by(pt) |>
            dplyr::summarise(
                qc_ewgsop2_bsl_ok = any(!is.na(EWGSOP2_stage)),
                .groups = "drop"
            )
        base <- dplyr::left_join(base, ewgsop2, by = "pt") |>
            dplyr::mutate(qc_ewgsop2_bsl_ok = dplyr::coalesce(qc_ewgsop2_bsl_ok, FALSE))
    }
    
    base
}


# =============================================================================
#' Build a CONSORT-style participant flow diagram
#'
#' @param counts     Named list of counts produced by apply_exclusions().
#' @param min_visits Minimum OsteoLaus visits threshold (used in node label).
#'
#' @return A DiagrammeR \code{grViz} object.
# =============================================================================
build_consort <- function(counts, min_visits = 2) {
    
    excl_label <- function(n, text) sprintf("Excluded: %d\\n%s", n, text)
    
    dot <- sprintf('
digraph consort {

  graph [layout = dot, rankdir = TB, splines = ortho,
         nodesep = 0.7, ranksep = 0.65, fontname = "Helvetica"]

  # ── Default node style ───────────────────────────────────────────────────────
  node [shape = box, style = "filled,rounded",
        fillcolor = "#FAFAFA", color = "#3D3D3D", penwidth = 1.2,
        fontname = "Helvetica", fontsize = 11, margin = "0.20,0.13",
        width = 3.0]

  edge [color = "#3D3D3D", penwidth = 1.1, arrowsize = 0.75]

  # ── Main flow nodes ──────────────────────────────────────────────────────────
  n0  [label = "All participants in merged dataset\\nN = %d",
       fillcolor = "#D6EAF8", color = "#1A5276"]
  n1  [label = "Participants with \\u22651 OsteoLaus visit\\nN = %d"]
  n2  [label = "Participant ID present\\nN = %d"]
  n3  [label = "Pt integrity checks passed\\nN = %d"]
  n4  [label = "Exam date present at all visits\\nN = %d"]
  n5  [label = "Energy intake in plausible range\\nN = %d"]
  n6  [label = "Required covariates present\\nN = %d"]
  n7  [label = "Dairy data complete at all visits\\nN = %d"]
  n8  [label = "\\u2265%d OsteoLaus visits\\nN = %d",
       fillcolor = "#D5F5E3", color = "#1E8449"]

  ninc [label = "Full analysis set\\n(inclusion = \\"yes\\")\\nN = %d",
        fillcolor = "#A9DFBF", color = "#1E8449", penwidth = 2.0, width = 3.2]
  npar [label = "Partial analysis set\\n(inclusion = \\"partial\\")\\nN = %d",
        fillcolor = "#FAD7A0", color = "#B7770D", penwidth = 1.5, width = 3.2]

  # ── Hard-exclusion boxes ─────────────────────────────────────────────────────
  node [shape = box, style = filled, fillcolor = "#FDEDEC",
        color = "#C0392B", penwidth = 1.0, fontsize = 10,
        width = 2.8, margin = "0.15,0.10"]

  e1 [label = "%s"]
  e2 [label = "%s"]
  e3 [label = "%s"]
  e4 [label = "%s"]
  e5 [label = "%s"]
  e6 [label = "%s"]
  e7 [label = "%s"]
  e8 [label = "%s"]

  # ── Partial-exclusion detail box ─────────────────────────────────────────────
  node [fillcolor = "#FEF9E7", color = "#B7770D", width = 3.2]

  ep [label = "Partial inclusions: %d\\n  Missing/implausible HGS:    %d\\n  No gait speed at V4/V5:     %d\\n  Missing ALM_HT2:             %d\\n  Missing EWGSOP2 at Baseline: %d"]

  # ── Main flow edges ──────────────────────────────────────────────────────────
  n0 -> n1 -> n2 -> n3 -> n4 -> n5 -> n6 -> n7 -> n8
  n8 -> ninc
  n8 -> npar [style = dashed]

  # ── Rank constraints (exclusion boxes sit beside their flow node) ─────────────
  { rank = same; n1;   e1 }
  { rank = same; n2;   e2 }
  { rank = same; n3;   e3 }
  { rank = same; n4;   e4 }
  { rank = same; n5;   e5 }
  { rank = same; n6;   e6 }
  { rank = same; n7;   e7 }
  { rank = same; n8;   e8 }
  { rank = same; npar; ep }

  # ── Dashed exclusion branch edges ────────────────────────────────────────────
  n0 -> e1 [style = dashed, color = "#C0392B", arrowhead = open]
  n1 -> e2 [style = dashed, color = "#C0392B", arrowhead = open]
  n2 -> e3 [style = dashed, color = "#C0392B", arrowhead = open]
  n3 -> e4 [style = dashed, color = "#C0392B", arrowhead = open]
  n4 -> e5 [style = dashed, color = "#C0392B", arrowhead = open]
  n5 -> e6 [style = dashed, color = "#C0392B", arrowhead = open]
  n6 -> e7 [style = dashed, color = "#C0392B", arrowhead = open]
  n7 -> e8 [style = dashed, color = "#C0392B", arrowhead = open]
  npar -> ep [style = dashed, color = "#B7770D", arrowhead = open]
}
',
# ── Main flow counts ──────────────────────────────────────────────────
counts$n_start,
counts$n_after_1,
counts$n_after_2,
counts$n_after_3,
counts$n_after_4,
counts$n_after_5,
counts$n_after_6,
counts$n_after_7,
min_visits, counts$n_after_8,
counts$n_full_include,
counts$n_partial,
# ── Exclusion box labels ──────────────────────────────────────────────
excl_label(counts$n_excl_no_osteo,   "No OsteoLaus visits"),
excl_label(counts$n_excl_no_pt,      "Missing participant ID"),
excl_label(counts$n_excl_integrity,  "Integrity failure\\n(sex / unique / age trajectory)"),
excl_label(
    counts$n_excl_exam_date,
    sprintf(
        "Missing exam date\\n(Step-3 QC count: %d)",
        counts$n_excl_exam_date_from_qc
    )
),
excl_label(counts$n_excl_energy,     "Energy intake out of range"),
excl_label(counts$n_excl_covs,       "Required covariate always missing"),
excl_label(counts$n_excl_dairy,      "dairy_total_gday missing at a visit"),
excl_label(
    counts$n_excl_min_visits,
    sprintf("< %d OsteoLaus visits", min_visits)
),
# ── Partial detail ────────────────────────────────────────────────────
counts$n_partial,
counts$n_partial_hgs,
counts$n_partial_gait,
counts$n_partial_alm,
counts$n_partial_ewgsop2
    )
    
    dot
}