# =============================================================================
# R/functions_merge.R
# =============================================================================
# OsteoLaus is the backbone: one row per participant × OsteoLaus visit.
#
# Matching rule:
#   For each OsteoLaus visit, attach the nearest CoLaus wave by smallest
#   absolute date gap (before OR after). CoLaus Baseline is excluded —
#   collected 2003–2008, it predates all OsteoLaus visits by ≥2 years and
#   represents a fundamentally different life-stage snapshot.
#   Eligible CoLaus waves: F1 (2009–2013), F2 (2014–2018),
#                          F3 (2018–2021), F4 (2022–2026).
#
# Column naming:
#   ALL columns from both sources are retained. Any column whose name appears
#   in BOTH harmonised datasets is prefixed before the join:
#     osteolaus_<n>  for the OsteoLaus version
#     colaus_<n>     for the CoLaus version
#   Columns unique to one source are kept with their original name.
#   Join keys and pipeline metadata are never prefixed.
#   Audit columns added: .colaus_wave, .gap_days.

# =============================================================================
# stack_waves()
# =============================================================================

#' Row-bind harmonised per-wave tibbles into a single long tibble.
#'
#' Call separately for CoLaus waves and OsteoLaus waves before merge.
#'
#' @param ... Harmonised wave tibbles (output of harmonise_*() functions).
#' @return A single long tibble.
stack_waves <- function(...) dplyr::bind_rows(list(...))

# =============================================================================
# merge_cohorts()
# =============================================================================

#' Merge CoLaus and OsteoLaus with OsteoLaus as the backbone.
#'
#' For each OsteoLaus visit, the nearest eligible CoLaus wave (F1-F4) by
#' absolute exam-date gap is attached. All columns from both sources are kept;
#' columns present in both are prefixed osteolaus_ / colaus_.
#'
#' @param colaus_long   Output of stack_waves() for CoLaus.
#' @param osteo_long    Output of stack_waves() for OsteoLaus.
#' @param gap_threshold Integer. Days beyond which a matched gap triggers a
#'                      warning. Default 912 (~2.5 years).
#' @return Long tibble, one row per participant x OsteoLaus visit.
#'   - osteolaus_* columns: from OsteoLaus
#'   - colaus_*    columns: from CoLaus
#'   - unique columns: original name, no prefix
#'   - .colaus_wave: which CoLaus wave was matched
#'   - .gap_days:    absolute date gap (days) between the two exam dates
merge_cohorts <- function(colaus_long, osteo_long, gap_threshold = 912) {
    
    # -- 1. Participant overlap diagnostics -------------------------------------
    osteo_only  <- setdiff(unique(osteo_long$pt),  unique(colaus_long$pt))
    colaus_only <- setdiff(unique(colaus_long$pt), unique(osteo_long$pt))
    
    if (length(osteo_only) > 0)
        message(glue::glue(
            "{length(osteo_only)} OsteoLaus participant(s) not found in CoLaus. ",
            "Retained in backbone with NA in all CoLaus-sourced columns."
        ))
    message(glue::glue(
        "{length(colaus_only)} CoLaus-only participant(s) not in OsteoLaus -- ",
        "excluded (OsteoLaus backbone)."
    ))
    
    # -- 2. Identify shared column names BEFORE any other manipulation ----------
    # Computed against raw inputs so that pipeline columns added later
    # (.colaus_wave, .gap_days) are never accidentally included.
    #
    # Never prefixed:
    #   pt, .wave, .wave_num, .cohort  -- join keys / backbone metadata
    #   exam_date_iso                  -- backbone date (OsteoLaus owns this)
    #   date_parse_fail, datexam,
    #   SCAN_date                      -- raw date helpers, source-specific
    no_prefix <- c("pt", ".wave", ".wave_num", ".cohort","exam_date_iso", "date_parse_fail")
    
    osteo_cols_raw  <- setdiff(names(osteo_long),  no_prefix)
    colaus_cols_raw <- setdiff(names(colaus_long), no_prefix)
    shared_cols     <- intersect(osteo_cols_raw, colaus_cols_raw)
    
    if (length(shared_cols) > 0) {
        message(glue::glue(
            "{length(shared_cols)} column(s) present in both datasets -- ",
            "prefixing with osteolaus_ / colaus_:\n  ",
            paste(sort(shared_cols), collapse = ", ")
        ))
    } else {
        message("No shared column names between datasets -- no prefixing needed.")
    }
    
    # -- 3. Apply prefixes ------------------------------------------------------
    if (length(shared_cols) > 0) {
        osteo_long  <- dplyr::rename_with(
            osteo_long,
            .fn   = ~ paste0("osteolaus_", .x),
            .cols = dplyr::all_of(shared_cols)
        )
        colaus_long <- dplyr::rename_with(
            colaus_long,
            .fn   = ~ paste0("colaus_", .x),
            .cols = dplyr::all_of(shared_cols)
        )
    }
    
    # -- 4. Match each OsteoLaus visit to the nearest eligible CoLaus wave ------
    # CoLaus Baseline (2003-2008) is excluded: predates all OsteoLaus visits
    # (2010-2022) by at least 2 years. Expected matches by study calendar:
    #   OsteoLaus Baseline (2010-2012) -> CoLaus F1  (2009-2013)
    #   OsteoLaus V2       (2012-2015) -> CoLaus F1 or F2
    #   OsteoLaus V3       (2015-2018) -> CoLaus F2 or F3
    #   OsteoLaus V4       (2017-2020) -> CoLaus F3
    #   OsteoLaus V5       (2020-2022) -> CoLaus F3 or F4
    #
    # Nearest by absolute gap (before OR after) -- no directionality constraint.
    colaus_dates <- colaus_long |>
        dplyr::select(pt, colaus_wave = .wave, colaus_date = exam_date_iso) |>
        dplyr::filter(colaus_wave != "Baseline")
    
    osteo_visits <- osteo_long |>
        dplyr::select(pt, osteo_wave = .wave, osteo_date = exam_date_iso)
    
    wave_match <- osteo_visits |>
        dplyr::left_join(colaus_dates, by = "pt", relationship = "many-to-many") |>
        dplyr::mutate(gap_days = abs(as.numeric(osteo_date - colaus_date))) |>
        dplyr::filter(!is.na(gap_days)) |>
        dplyr::group_by(pt, osteo_wave) |>
        dplyr::slice_min(gap_days, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
    
    # OsteoLaus-only participants: NA match row so backbone row is retained
    unmatched <- osteo_visits |>
        dplyr::filter(pt %in% osteo_only) |>
        dplyr::mutate(
            colaus_wave = NA_character_,
            colaus_date = as.Date(NA),
            gap_days    = NA_real_
        )
    
    wave_match <- dplyr::bind_rows(wave_match, unmatched)
    
    # -- 5. Diagnostics ---------------------------------------------------------
    n_matched <- sum(!is.na(wave_match$colaus_wave))
    message(glue::glue(
        "{n_matched} OsteoLaus visit(s) matched to an eligible CoLaus wave ",
        "(F1-F4). CoLaus Baseline excluded from all matching."
    ))
    
    # Gap summary: which OsteoLaus wave matched to which CoLaus wave
    gap_summary <- wave_match |>
        dplyr::filter(!is.na(gap_days)) |>
        dplyr::group_by(osteo_wave, colaus_wave) |>
        dplyr::summarise(
            n          = dplyr::n(),
            median_gap = round(median(gap_days), 0),
            max_gap    = max(gap_days),
            n_over_thr = sum(gap_days > gap_threshold),
            .groups    = "drop"
        )
    message(
        "Match summary (OsteoLaus wave -> CoLaus wave, gap in days):\n",
        paste(capture.output(print(gap_summary, n = Inf)), collapse = "\n")
    )
    
    over_thr <- dplyr::filter(gap_summary, n_over_thr > 0)
    if (nrow(over_thr) > 0) {
        warn_lines <- glue::glue_data(
            over_thr,
            "  {osteo_wave} -> {colaus_wave}: {n_over_thr}/{n} visits > {gap_threshold} days"
        )
        warning(paste(
            glue::glue("Some CoLaus attachment gaps exceed {gap_threshold} days:"),
            paste(warn_lines, collapse = "\n"),
            sep = "\n"
        ))
    }
    
    shared_waves <- wave_match |>
        dplyr::filter(!is.na(colaus_wave)) |>
        dplyr::count(pt, colaus_wave, name = "n_osteo_visits") |>
        dplyr::filter(n_osteo_visits > 1)
    if (nrow(shared_waves) > 0)
        message(glue::glue(
            "{nrow(shared_waves)} pt x CoLaus-wave pair(s) matched to >1 OsteoLaus visit ",
            "(expected -- 5 OsteoLaus visits, 4 eligible CoLaus waves). ",
            "All OsteoLaus rows retained; CoLaus data appears in those rows."
        ))
    
    # -- 6. Attach audit columns to OsteoLaus backbone --------------------------
    osteo_long <- osteo_long |>
        dplyr::left_join(
            dplyr::select(wave_match,
                          pt,
                          .osteo_wave_key = osteo_wave,
                          .colaus_wave    = colaus_wave,
                          .gap_days       = gap_days),
            by = c("pt", ".wave" = ".osteo_wave_key")
        )
    
    # -- 7. Build CoLaus sidecar ------------------------------------------------
    # Drop CoLaus pipeline metadata that the OsteoLaus backbone owns.
    # Rename .wave -> .colaus_wave to serve as join key.
    drop_colaus_meta <- c(".wave_num", ".cohort",
                          "datexam", "exam_date_iso", "date_parse_fail")
    colaus_sidecar <- colaus_long |>
        dplyr::select(-dplyr::any_of(drop_colaus_meta)) |>
        dplyr::rename(.colaus_wave = .wave)
    
    # -- 8. Join ----------------------------------------------------------------
    # All shared columns prefixed in step 3 -- no collisions, no suffix needed.
    merged <- osteo_long |>
        dplyr::left_join(colaus_sidecar, by = c("pt", ".colaus_wave"))
    
    
    # -- 9. Post-merge checks --------------------------------------------------
    n_dup <- nrow(dplyr::count(merged, pt, .wave) |> dplyr::filter(n > 1))
    if (n_dup > 0)
        warning(glue::glue("{n_dup} duplicate pt x OsteoLaus-wave rows after merge."))
    
    message(glue::glue(
        "Merged dataset: {nrow(merged)} rows | ",
        "{dplyr::n_distinct(merged$pt)} participants | ",
        "{dplyr::n_distinct(merged$.wave)} OsteoLaus waves | ",
        "{ncol(merged)} columns"
    ))
    
    return(merged)
}