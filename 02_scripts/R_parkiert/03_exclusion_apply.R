# =============================================================================
# R/03_exclusion_apply.R
# =============================================================================
#
# Pipeline overview
# -----------------
#  SHARED (derived once from the observed data, applied uniformly):
#    1. QC exclusions (participant-level, from qc_table)
#    2. Exclude participants missing exposure at every observed visit
#    3. Exclude participants with < visit_min observed visits
#
#  PER-OUTCOME (applied to each imputed dataset separately):
#    4. [gait_speed only] Lag covariates + exposure; drop T1 rows
#    5. [sarcopenia outcomes] Exclude prevalent cases at baseline
#    6. Exclude rows with missing covariates
#    7. Exclude rows where the outcome is NA
#    8. Re-check visit counts; exclude participants now below visit_min
#
#  MICE vs complete-case
#  ---------------------
#  Steps 1-3 are evaluated on the OBSERVED data (.imp == 0 or the plain CC
#  data frame) to produce a stable set of keep/exclude IDs that is then
#  applied identically to every imputed dataset. This guarantees that CONSORT
#  counts are consistent across imputations.
#
#  Steps 4-8 are applied per imputation because they depend on imputed values
#  (covariate completeness, outcome availability).
#
#  OUTPUTS (both routes return the same named list shape):
#    $data     : named list — one long-format tibble per outcome
#    $mids     : named list — one mids object per outcome (NULL for CC route)
#    $consort  : tibble of pooled CONSORT counts (averaged n across imputations
#                for MICE; direct counts for CC)
#    $excluded : tibble of participants excluded at shared stages
#
# =============================================================================

`%||%` <- function(x, y) if (!is.null(x)) x else y


#' Apply the full exclusion pipeline to the analysis dataset.
#'
#' @param data        Analysis dataset. Accepts:
#'   * A plain data frame / tibble (complete-case route).
#'   * A `mids` object (MICE route).
#'   * A `list(long, mids, m)` from the derivation pipeline (MICE route).
#' @param qc_table    QC flag table with columns `qc_in_osteolaus` and the
#'   columns named in `qc_flag_cols` inside `run_pipeline()`.
#' @param outcomes    Character vector of outcome column names to process.
#' @param covariates  Named list mapping each outcome name to its covariate
#'   column names, e.g. `list(HGS_MAX = c("Age", "BMI"))`. Outcomes not
#'   listed receive no covariate exclusions.
#' @param visit_min   Minimum number of observed visits required.
#' @param pt_col      Participant ID column name.
#' @param visit_col   Visit / time-point column name.
#' @param exposure    Exposure column name used for step 2.
#'
#' @return Named list:
#'   * `$data`     Named list of long-format tibbles, one per outcome. For the
#'                 MICE route each tibble contains all imputed datasets stacked
#'                 with `.imp` column.
#'   * `$mids`     Named list of `mids` objects, one per outcome. `NULL` for
#'                 each outcome in the complete-case route.
#'   * `$consort`  Tibble with columns `outcome`, `stage`, `n_excluded`,
#'                 `n_remaining`. For MICE, counts are averaged across
#'                 imputations (rounded to nearest integer).
#'   * `$excluded` Tibble of participants excluded at the shared stages
#'                 (QC, exposure, visit_min). Imputation-invariant.
apply_exclusions <- function(
    data,
    qc_table,
    outcomes   = c(
      "ewgsop2_sarcopenia_stage",
      "HGS_MAX",
      "ALM_HT2_harmonised",
      "gait_speed"
    ),
    covariates = list(),
    visit_min  = 2L,
    pt_col     = "pt",
    visit_col  = "time_point",
    exposure   = "dairy_total_gday_cumavg"
) {
  
  # ── Normalise input ───────────────────────────────────────────────────────
  normed  <- normalise_exclusion_input(data)
  long    <- normed$long
  is_mice <- normed$is_mice
  m       <- normed$m
  
  cli::cli_h1("Exclusion pipeline ({if (is_mice) paste('MICE, m =', m) else 'complete-case'})")
  
  # ── Shared exclusions on observed data (.imp == 0 or CC) ─────────────────
  # Evaluated once; resulting IDs applied to all imputations.
  obs_data <- if (is_mice) {
    dplyr::filter(long, .imp == 0L)
  } else {
    long
  }
  
  shared <- .apply_shared_exclusions(
    obs_data, qc_table, pt_col, visit_col, visit_min, exposure
  )
  
  shared_keep_ids  <- shared$keep_ids
  shared_consort   <- shared$consort
  shared_excluded  <- shared$excluded_participants
  
  # ── Route ─────────────────────────────────────────────────────────────────
  if (!is_mice) {
    
    # Complete-case: filter long data by shared IDs, run outcome pipeline
    cc_data <- dplyr::filter(long, .data[[pt_col]] %in% shared_keep_ids)
    
    pipe_result <- run_pipeline(
      cc_data, qc_table, outcomes, covariates,
      pt_col, visit_col, visit_min, exposure,
      shared_consort = shared_consort
    )
    
    return(list(
      data             = pipe_result$data,
      mids             = stats::setNames(
        vector("list", length(outcomes)), outcomes),
      consort          = pipe_result$consort_long,
      excluded         = shared_excluded,
      excluded_by_oc   = pipe_result$excluded_by_oc,
      data_post_shared = cc_data
    ))
  }
  
  # ── MICE: long → per-outcome exclusions → mids ───────────────────────────
  # All imputations (.imp 0..m) are filtered to shared_keep_ids and then
  # processed together for each outcome. Running lags on all imps including
  # .imp == 0 (observed data) gives mice::as.mids() a proper observed-data
  # slot: lag columns are NA only where T1 was genuinely missing, not
  # everywhere, so $imp is populated correctly without any manual injection.
  long_shared <- dplyr::filter(long, .data[[pt_col]] %in% shared_keep_ids)
  key_cols    <- c(pt_col, visit_col)

  outcome_data    <- list()
  outcome_mids    <- list()
  outcome_excl    <- list()
  outcome_consort <- list()

  for (oc in outcomes) {

    oc_long <- long_shared
    covars  <- covariates[[oc]] %||% character(0)
    n_start <- dplyr::n_distinct(dplyr::filter(oc_long, .imp == 0L)[[pt_col]])
    consort <- .add_consort(.empty_consort(), oc, "Start outcome",
                            n_remaining = n_start)

    # 4. Lag (gait_speed only) — applied to ALL imps including .imp == 0.
    #    create_lags() groups by .imp + pt so each imputation's lags are
    #    independent. T1 rows are dropped after lagging.
    if (oc == "gait_speed") {
      oc_long <- create_lags(oc_long, pt_col, visit_col) |>
        dplyr::filter(.data[[visit_col]] != "T1")
      covars <- paste0(covars, "_lag")
    }

    # 5. Prevalent baseline (sarcopenia) — evaluated on .imp == 0
    if (oc %in% c("ewgsop2_sarcopenia_stage", "fnih_sarcopenia")) {
      level    <- if (oc == "fnih_sarcopenia") "Sarcopenia" else "Confirmed"
      prev_ids <- dplyr::filter(oc_long, .imp == 0L,
                                .data[[visit_col]] == "T1",
                                .data[[oc]] == level) |>
        dplyr::pull(.data[[pt_col]]) |> unique()
      oc_long  <- dplyr::filter(oc_long, !(.data[[pt_col]] %in% prev_ids))
      n_after  <- dplyr::n_distinct(dplyr::filter(oc_long, .imp == 0L)[[pt_col]])
      consort  <- .add_consort(consort, oc, "Prevalent baseline",
                               n_excluded = n_start - n_after, n_remaining = n_after)
      n_start  <- n_after
    }

    # 5b. sumtot1 row-level exclusion — identify bad rows via .imp > 0,
    #     then remove those pt×visit keys from ALL imps for consistency
    if ("sumtot1" %in% covars || "sumtot1_lag" %in% covars) {
      bad_keys <- exclude_invalid_sumtot1(
        dplyr::filter(oc_long, .imp > 0L), pt_col, visit_col
      )$excluded_rows |>
        dplyr::distinct(.data[[pt_col]], .data[[visit_col]])
      if (nrow(bad_keys) > 0L)
        oc_long <- dplyr::anti_join(oc_long, bad_keys, by = key_cols)
      n_pts_after <- dplyr::n_distinct(dplyr::filter(oc_long, .imp == 0L)[[pt_col]])
      consort <- .add_consort(consort, oc, "sumtot1 visits outside 500-4200",
                              n_excluded = nrow(bad_keys), n_remaining = n_pts_after)
      n_start <- n_pts_after
    }

    # 6-7. Missing covariates + missing outcome — on .imp > 0 only (imputed
    #      data should be complete; any remaining NAs are structural).
    imps_ok <- dplyr::filter(oc_long, .imp > 0L)
    if (length(covars) > 0L)
      imps_ok <- imps_ok[rowSums(is.na(imps_ok[covars])) == 0L, ]
    imps_ok <- dplyr::filter(imps_ok, !is.na(.data[[oc]]))

    # 8. Visit recheck — participant must have >= visit_min in EVERY imputation
    keep_ids <- imps_ok |>
      dplyr::distinct(.imp, .data[[pt_col]], .data[[visit_col]]) |>
      dplyr::count(.imp, .data[[pt_col]], name = "n_v") |>
      dplyr::filter(n_v >= visit_min) |>
      dplyr::group_by(.data[[pt_col]]) |>
      dplyr::filter(dplyr::n() == m) |>
      dplyr::pull(.data[[pt_col]]) |>
      unique()
    imps_ok <- dplyr::filter(imps_ok, .data[[pt_col]] %in% keep_ids)

    # Intersection of surviving pt×visit keys across all imps > 0
    key_sets    <- lapply(split(imps_ok, imps_ok$.imp), function(df)
      dplyr::distinct(df, .data[[pt_col]], .data[[visit_col]]))
    common_keys <- Reduce(
      function(a, b) dplyr::semi_join(a, b, by = key_cols),
      key_sets
    )

    obs_final  <- dplyr::semi_join(
      dplyr::filter(oc_long, .imp == 0L), common_keys, by = key_cols
    )
    imps_final <- dplyr::semi_join(imps_ok, common_keys, by = key_cols)
    n_final    <- dplyr::n_distinct(obs_final[[pt_col]])
    consort    <- .add_consort(consort, oc, paste0("<", visit_min, " visits final"),
                               n_excluded = n_start - n_final, n_remaining = n_final)

    # Bind all imps, assign consistent .id across imps, convert to mids.
    # Sorting before row_number() ensures the same observation gets the same
    # .id in every .imp slice, which is required by mice::as.mids().
    oc_final <- dplyr::bind_rows(obs_final, imps_final) |>
      dplyr::arrange(.imp, .data[[pt_col]], .data[[visit_col]]) |>
      dplyr::group_by(.imp) |>
      dplyr::mutate(.id = dplyr::row_number()) |>
      dplyr::ungroup()

    outcome_data[[oc]]    <- dplyr::filter(oc_final, .imp > 0L)
    outcome_mids[[oc]]    <- mice::as.mids(oc_final)
    outcome_excl[[oc]]    <- tibble::tibble()
    outcome_consort[[oc]] <- consort
  }

  pooled_consort <- dplyr::bind_rows(shared_consort, dplyr::bind_rows(outcome_consort))

  list(
    data             = outcome_data,
    mids             = outcome_mids,
    consort          = pooled_consort,
    excluded         = shared_excluded,
    excluded_by_oc   = outcome_excl,
    data_post_shared = dplyr::filter(long_shared, .imp > 0L)
  )
}


# =============================================================================
# Internal: shared exclusion stages (QC, exposure, visit_min)
# Operates on the observed data only; returns keep_ids + consort rows.
# =============================================================================

.apply_shared_exclusions <- function(obs_data, qc_table,
                                     pt_col, visit_col, visit_min, exposure) {
  
  consort      <- .empty_consort()
  excluded_all <- tibble::tibble()
  
  n_initial <- dplyr::n_distinct(obs_data[[pt_col]])
  consort    <- .add_consort(consort, "shared", "Initial sample",
                             n_remaining = n_initial)
  
  # 1. QC
  qc_excl  <- qc_exclude_participants(
    qc_table, pt_col,
    qc_flag_cols = c(
      "qc_pt_present", "qc_exam_date", "qc_sex_stable",
      "qc_datbirth_baseline", "qc_pt_unique"
    )
  )
  obs_data    <- dplyr::filter(obs_data,
                               !(.data[[pt_col]] %in% qc_excl[[pt_col]]))
  n_after_qc  <- dplyr::n_distinct(obs_data[[pt_col]])
  consort      <- .add_consort(consort, "shared", "Excluded QC",
                               n_excluded  = n_initial - n_after_qc,
                               n_remaining = n_after_qc)
  excluded_all <- dplyr::bind_rows(excluded_all, qc_excl)
  
  # 2. Exposure
  exp_res      <- exclude_missing_exposure(obs_data, pt_col, exposure)
  obs_data     <- exp_res$data
  n_after_exp  <- dplyr::n_distinct(obs_data[[pt_col]])
  consort      <- .add_consort(consort, "shared",
                               paste0("Missing ", exposure),
                               n_excluded  = n_after_qc - n_after_exp,
                               n_remaining = n_after_exp)
  excluded_all <- dplyr::bind_rows(excluded_all, exp_res$excl)
  
  # 3. Minimum visits
  visit_res     <- compute_valid_visit_participants(
    obs_data, pt_col, visit_col, visit_min)
  obs_data      <- visit_res$data
  n_after_visit <- dplyr::n_distinct(obs_data[[pt_col]])
  consort       <- .add_consort(consort, "shared",
                                paste0("<", visit_min, " visits"),
                                n_excluded  = n_after_exp - n_after_visit,
                                n_remaining = n_after_visit)
  excluded_all  <- dplyr::bind_rows(excluded_all, visit_res$excl)
  
  list(
    keep_ids              = dplyr::pull(obs_data, .data[[pt_col]]) |> unique(),
    consort               = consort,
    excluded_participants = excluded_all
  )
}


# =============================================================================
# Internal: per-outcome pipeline (called per imputed dataset)
# Receives data already filtered to shared_keep_ids.
# =============================================================================

run_pipeline <- function(
    data,
    qc_table,
    outcomes,
    covariates,
    pt_col         = "pt",
    visit_col      = "time_point",
    visit_min      = 2L,
    exposure       = "dairy_total_gday_cumavg",
    shared_consort = NULL
) {
  
  get_covs <- function(oc) covariates[[oc]] %||% character(0)
  
  # Start consort from the shared rows already accumulated
  consort          <- shared_consort %||% .empty_consort()
  outcome_data     <- list()
  outcome_excluded <- list()
  
  for (oc in outcomes) {
    
    oc_data               <- data
    covars                <- get_covs(oc)
    exp_var               <- exposure   # may be updated to lagged version below
    n_start               <- dplyr::n_distinct(oc_data[[pt_col]])
    oc_data_pre_sumtot    <- NULL       # snapshot before sumtot1 row removal
    outcome_excluded_rows <- NULL       # full rows of participants excluded here
    
    consort <- .add_consort(consort, oc, "Start outcome",
                            n_remaining = n_start)
    
    # ── 4. Lag (gait_speed only) ─────────────────────────────────────────
    if (oc == "gait_speed") {

      lag_vars <- setdiff(
        names(oc_data),
        c(".imp", pt_col, visit_col, "exam_date", "gait_speed")
      )

      oc_data <- oc_data |>
        dplyr::group_by(.data[[pt_col]]) |>
        dplyr::arrange(.data[[visit_col]], .by_group = TRUE) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(lag_vars),
            dplyr::lag,
            .names = "{.col}_lag"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(.data[[visit_col]] != "T1")

      covars  <- paste0(covars,  "_lag")
      exp_var <- paste0(exposure, "_lag")
    }
    
    # ── 5. Prevalent baseline (sarcopenia outcomes only) ──────────────────
    if (oc %in% c("ewgsop2_sarcopenia_stage", "fnih_sarcopenia")) {
      
      prev_ids <- if (oc == "fnih_sarcopenia") {
        oc_data |>
          dplyr::filter(.data[[visit_col]] == "T1",
                        .data[[oc]] == "Sarcopenia") |>
          dplyr::pull(.data[[pt_col]]) |> unique()
      } else {
        oc_data |>
          dplyr::filter(.data[[visit_col]] == "T1",
                        .data[[oc]] %in% "Confirmed") |>
          dplyr::pull(.data[[pt_col]]) |> unique()
      }
      
      oc_data      <- dplyr::filter(oc_data,
                                    !(.data[[pt_col]] %in% prev_ids))
      n_after_base <- dplyr::n_distinct(oc_data[[pt_col]])
      consort      <- .add_consort(consort, oc, "Prevalent baseline",
                                   n_excluded  = n_start - n_after_base,
                                   n_remaining = n_after_base)
      n_start <- n_after_base
    }
    
    # ── 5b. Invalid sumtot1 (row-level) ──────────────────────────────────
    # Remove only the visits where sumtot1 is out of range, not the whole
    # participant. Whether the participant then falls below visit_min is
    # handled by the visit recheck at step 8.
    if ("sumtot1" %in% covars || "sumtot1_lag" %in% covars) {
      
      # Snapshot before row removal so we can return complete records for
      # participants who are subsequently dropped by the visit recheck.
      oc_data_pre_sumtot <- oc_data
      sumtot_res  <- exclude_invalid_sumtot1(oc_data, pt_col, visit_col)
      oc_data     <- sumtot_res$data
      n_rows_rm   <- sumtot_res$n_rows_removed
      n_pts_after <- dplyr::n_distinct(oc_data[[pt_col]])
      
      # CONSORT: count visits removed (row-level), not participants.
      # Participant-level impact is captured later by the visit recheck.
      consort <- .add_consort(consort, oc,
                              "sumtot1 visits outside 500-4200",
                              n_excluded  = n_rows_rm,
                              n_remaining = n_pts_after)
      n_start <- n_pts_after
    }
    
    # ── 6. Missing covariates (row-level) ─────────────────────────────────
    if (length(covars) > 0L) {
      miss_cov <- rowSums(is.na(oc_data[covars])) > 0L
    } else {
      miss_cov <- rep(FALSE, nrow(oc_data))
    }
    oc_data <- oc_data[!miss_cov, ]
    
    # ── 7. Missing outcome (row-level) ────────────────────────────────────
    oc_data <- dplyr::filter(oc_data, !is.na(.data[[oc]]))
    
    # ── 8. Visit recheck ──────────────────────────────────────────────────
    visit_counts <- oc_data |>
      dplyr::distinct(.data[[pt_col]], .data[[visit_col]]) |>
      dplyr::count(.data[[pt_col]], name = "n_visits")
    
    keep_ids <- visit_counts |>
      dplyr::filter(n_visits >= visit_min) |>
      dplyr::pull(.data[[pt_col]])
    
    drop_ids <- visit_counts |>
      dplyr::filter(n_visits < visit_min) |>
      dplyr::pull(.data[[pt_col]])
    
    final_data <- dplyr::filter(oc_data, .data[[pt_col]] %in% keep_ids)
    n_final    <- dplyr::n_distinct(final_data[[pt_col]])
    
    consort <- .add_consort(consort, oc,
                            paste0("<", visit_min, " visits final"),
                            n_excluded  = n_start - n_final,
                            n_remaining = n_final)
    
    # Collect complete records (all time points from oc_data_pre_sumtot, i.e.
    # before visit-rows were stripped) for participants dropped at this step.
    # This gives a complete picture of why they were excluded.
    if (length(drop_ids) > 0L && !is.null(oc_data_pre_sumtot)) {
      excluded_at_recheck <- dplyr::filter(
        oc_data_pre_sumtot, .data[[pt_col]] %in% drop_ids
      ) |>
        dplyr::mutate(exclusion_stage  = paste0(oc, "_visit_recheck"),
                      exclusion_reason = paste0("<", visit_min, " visits after sumtot1 row removal"))
      outcome_excluded_rows <- dplyr::bind_rows(
        outcome_excluded_rows %||% tibble::tibble(),
        excluded_at_recheck
      )
    }
    
    outcome_data[[oc]]          <- final_data
    outcome_excluded[[oc]]      <- outcome_excluded_rows %||% tibble::tibble()
  }
  
  list(
    data             = outcome_data,
    excluded_by_oc   = outcome_excluded,
    consort_long     = consort
  )
}


# =============================================================================
# Internal: CONSORT helpers
# =============================================================================

.empty_consort <- function() {
  tibble::tibble(
    outcome     = character(),
    stage       = character(),
    n_excluded  = integer(),
    n_remaining = integer()
  )
}

.add_consort <- function(consort, outcome, stage,
                         n_excluded  = NA_integer_,
                         n_remaining) {
  dplyr::bind_rows(
    consort,
    tibble::tibble(
      outcome     = outcome,
      stage       = stage,
      n_excluded  = as.integer(n_excluded),
      n_remaining = as.integer(n_remaining)
    )
  )
}

# =============================================================================
# Utilities
# =============================================================================

#' Exclude participants with sumtot1 outside the plausible range (500-4200).
#'
#' @param data   Data frame containing `sumtot1`.
#' @param pt_col Participant ID column name.
#' @return List with `$data` (filtered) and `$excluded`.
#' Exclude individual visits where sumtot1 is outside the plausible range.
#'
#' Removes only the offending rows (visits), not the whole participant.
#' The caller is responsible for subsequently checking whether the remaining
#' visit count per participant still meets the minimum threshold.
#'
#' @param data    Data frame containing `sumtot1` and `visit_col`.
#' @param pt_col  Participant ID column name.
#' @param visit_col Visit / time-point column name.
#'
#' @return List:
#'   * `$data`          Rows with valid sumtot1 (bad visits removed).
#'   * `$excluded_rows` All rows (visits) that were removed, with all their
#'                      original columns plus `exclusion_reason`.
#'   * `$n_rows_removed` Number of visit-rows removed.
exclude_invalid_sumtot1 <- function(data, pt_col, visit_col = "time_point") {
  
  bad_rows <- data |>
    dplyr::filter(!is.na(sumtot1), (sumtot1 < 500 | sumtot1 > 3500)) |>
    dplyr::mutate(exclusion_reason = "sumtot1 outside 500-3500")
  
  list(
    data          = dplyr::filter(data,
                                  !(is.na(sumtot1) == FALSE &
                                      (sumtot1 < 500 | sumtot1 > 3500))),
    excluded_rows = bad_rows,
    n_rows_removed = nrow(bad_rows)
  )
}