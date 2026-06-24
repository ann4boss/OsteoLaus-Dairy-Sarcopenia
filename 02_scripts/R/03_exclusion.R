# =============================================================================
# R/03_exclusion.R
# =============================================================================
# Complete exclusion pipeline — shared + per-outcome.
#
# Entry point
# -----------
#   run_exclusions(data, qc_tbl, outcomes, covariates, exposure,
#                  pt_col, visit_col, min_visit)
#
# Accepts a plain data frame/tibble (complete-case) or a mids object.
# For mids: converts to long format via mice::complete(), runs exclusions
# per .imp, then reconstructs with mice::as.mids().
#
# Returns
# -------
#   $data_shared          — dataset after shared exclusions only
#                           (mids or plain df, matching the input type)
#   $data_excluded_shared — all rows of pts excluded at shared stages
#                           (always plain df; derived from observed data)
#   $data_outcome         — named list, one per outcome, after all exclusions
#                           (mids or plain df per element, matching input type)
#   $audit                — tibble with one row per exclusion step
#                           columns: step, step_type, reason, n_excluded, n_remaining
# =============================================================================


# ── Public entry point ────────────────────────────────────────────────────────

#' Run the full exclusion pipeline.
#'
#' @param data        data.frame/tibble or mids object.
#' @param qc_tbl      QC flag table with columns "qc_in_osteolaus",
#'   "qc_pt_present", "qc_exam_date", "qc_sex_stable",
#'   "qc_datbirth_baseline", "qc_pt_unique", and `pt_col`.
#' @param outcomes    Character vector of outcome column names.
#' @param covariates  Named list mapping outcome name to covariate column names.
#'   Outcomes not listed receive no covariate check.
#' @param exposure    Column name of the exposure variable.
#' @param pt_col      Participant ID column.
#' @param visit_col   Visit / time-point column.
#' @param min_visit   Minimum number of valid visits required to retain a pt.
#'
#' @return Named list: $data_shared, $data_excluded_shared, $data_outcome, $audit.
#'   $audit has one row per exclusion step with columns: step, step_type, reason,
#'   n_excluded, n_remaining.
run_exclusions <- function(
    data,
    qc_tbl,
    outcomes,
    covariates         = list(),
    shared_covariates  = character(0L),
    exposure           = "dairy_total_gday_cumavg",
    pt_col             = "pt",
    visit_col          = "time_point",
    min_visit          = 2L
) {
  stopifnot(is.character(outcomes), length(outcomes) >= 1L)

  # ── 1. Normalise input ──────────────────────────────────────────────────────
  is_mids <- inherits(data, "mids")

  if (is_mids) {
    m    <- data$m
    long <- mice::complete(data, action = "long", include = TRUE) |>
      tibble::as_tibble() |>
      dplyr::mutate(dplyr::across(where(is.list), ~ unlist(.x, use.names = FALSE)))
  } else {
    stopifnot(is.data.frame(data))
    m    <- NULL
    long <- tibble::as_tibble(data)
    # Attach a dummy .imp column so the rest of the code is uniform.
    long[[".imp"]] <- 0L
  }

  # Observed-data slice (true values, no imputed fills).
  obs <- dplyr::filter(long, .imp == 0L)

  # ── 2. Shared exclusions — evaluated once on observed data ─────────────────
  #   Decisions are derived from .imp == 0 and applied uniformly to all .imp
  #   so that CONSORT counts are imputation-invariant.
  shared <- .shared_exclusions(obs, qc_tbl, exposure, pt_col, visit_col, min_visit)
  keep_ids_shared  <- shared$keep_ids
  audit            <- shared$audit   # starts the running audit tibble

  # Dataset after shared exclusions (all imps):
  # filter by surviving participants, then also drop the specific sumtot1
  # out-of-range visit-rows identified in the observed data. These are row-level
  # drops that keep_ids alone cannot express.
  long_shared <- dplyr::filter(long, .data[[pt_col]] %in% keep_ids_shared)
  if (nrow(shared$bad_dairy_keys) > 0L)
    long_shared <- dplyr::anti_join(long_shared, shared$bad_dairy_keys,
                                    by = c(pt_col, visit_col))
  if (nrow(shared$bad_sumtot1_keys) > 0L)
    long_shared <- dplyr::anti_join(long_shared, shared$bad_sumtot1_keys,
                                    by = c(pt_col, visit_col))

  # ── 2b. Shared covariate exclusion ────────────────────────────────────────────
  # For mids: use .imp == 1 (imputed — post-mice NAs are structural, not random).
  # For CC:   use the plain data (no .imp grouping).
  # Bad visit-rows are removed from ALL .imp so every slice stays consistent.
  if (length(shared_covariates) > 0L) {
    ref_slice    <- if (is_mids) dplyr::filter(long_shared, .imp == 1L) else long_shared
    present_covs <- intersect(shared_covariates, names(ref_slice))
    if (length(present_covs) > 0L) {
      bad_cov_keys <- ref_slice[
        ref_slice[[visit_col]] != "T4" & rowSums(is.na(ref_slice[present_covs])) > 0L, ] |>
        dplyr::distinct(.data[[pt_col]], .data[[visit_col]])
      if (nrow(bad_cov_keys) > 0L) {
        pts_before  <- unique(ref_slice[[pt_col]])
        long_shared <- dplyr::anti_join(long_shared, bad_cov_keys, by = c(pt_col, visit_col))
        ref_after   <- if (is_mids) dplyr::filter(long_shared, .imp == 1L) else long_shared
        lost_cov    <- setdiff(pts_before, unique(ref_after[[pt_col]]))
        audit       <- .record_excl(audit, lost_cov, "shared",
                                    "shared_covariate_missing_lost_all_visits", pt_col)
      }
    }
  }

  # ── 2c. min_visit check — applied once after all shared row-level exclusions ──
  ref_final    <- if (is_mids) dplyr::filter(long_shared, .imp == 1L) else long_shared
  visit_counts <- ref_final |>
    dplyr::distinct(.data[[pt_col]], .data[[visit_col]]) |>
    dplyr::count(.data[[pt_col]], name = "n_visits")
  too_few <- dplyr::filter(visit_counts, n_visits < min_visit) |>
    dplyr::pull(.data[[pt_col]])
  if (length(too_few) > 0L) {
    long_shared <- dplyr::filter(long_shared, !(.data[[pt_col]] %in% too_few))
    audit <- .record_excl(audit, too_few, "shared", "too_few_visits", pt_col)
  }

  # All rows of pts excluded at shared stages (observed data only).
  data_excluded_shared <- dplyr::filter(obs, !(.data[[pt_col]] %in% keep_ids_shared))

  # ── 3a. Build data_shared_out (needed by outcome loop below) ───────────────
  if (is_mids) {
    shared_keys     <- dplyr::distinct(
      dplyr::filter(long_shared, .imp == 1L),
      dplyr::across(dplyr::all_of(c(pt_col, visit_col)))
    )
    data_shared_out <- .filter_mids(data, shared_keys, c(pt_col, visit_col))
  } else {
    data_shared_out <- dplyr::select(long_shared, -.imp)
  }

  # ── 3b. Per-outcome exclusions ─────────────────────────────────────────────
  outcome_long <- list()   # plain long dfs per outcome (all .imp)
  outcome_mids <- list()   # mids objects per outcome (NULL for CC)

  for (oc in outcomes) {
    covars <- covariates[[oc]] %||% character(0L)

    if (is_mids) {
      # For sarcopenia outcomes, compute the union of prevalent-at-T1 IDs across
      # ALL imputations upfront so every .imp drops the same participants.
      prevalent_ids <- character(0L)
      if (oc %in% c("ewgsop2_sarcopenia_stage", "fnih_sarcopenia")) {
        prev_level    <- if (oc == "fnih_sarcopenia") "Sarcopenia" else "Confirmed"
        prevalent_ids <- long_shared |>
          dplyr::filter(.data[[visit_col]] == "T1", .data[[oc]] == prev_level) |>
          dplyr::pull(.data[[pt_col]]) |>
          unique()
      }

      key_cols <- c(pt_col, visit_col)

      if (oc == "gait_speed") {
        # Lagging adds new columns and drops T1, so the column structure differs
        # from data_shared_out. Build mids directly from the lagged long format.
        # Use long_shared (from mice::complete(data,"long",include=TRUE)) so that
        # imputed values of gait_speed are present in .imp 1..m. The .imp==0 slice
        # carries the original observed data (NAs intact), which as.mids() needs.
        lagged <- lapply(0:m, function(i) {
          .create_lags(dplyr::filter(long_shared, .imp == i), pt_col, visit_col) |>
            dplyr::mutate(.imp = i)
        })

        imp_keep <- lapply(seq_len(m), function(i) {
          .outcome_exclusions(lagged[[i + 1L]], oc, covars, pt_col, visit_col,
                              min_visit = min_visit, prevalent_ids = prevalent_ids)
        })
        audit <- dplyr::bind_rows(audit, imp_keep[[1L]]$audit)

        imp_data_list <- lapply(imp_keep, `[[`, "data")
        common_keys   <- Reduce(
          function(a, b) dplyr::semi_join(a, b, by = key_cols),
          lapply(imp_data_list, function(d)
            dplyr::distinct(d, dplyr::across(dplyr::all_of(key_cols))))
        )

        oc_long_all <- dplyr::bind_rows(
          lapply(lagged, function(s) dplyr::semi_join(s, common_keys, by = key_cols))
        ) |>
          dplyr::arrange(.imp, .data[[pt_col]], .data[[visit_col]]) |>
          dplyr::group_by(.imp) |>
          dplyr::mutate(.id = dplyr::row_number()) |>
          dplyr::ungroup()

outcome_mids[[oc]] <- mice::as.mids(oc_long_all)
        outcome_long[[oc]] <- dplyr::filter(oc_long_all, .imp > 0L)

      } else {
        # All other outcomes: run exclusions on .imp 1..m, then filter the
        # shared mids directly (no column structure change).
        imp_keep <- lapply(seq_len(m), function(i) {
          slice <- dplyr::filter(long_shared, .imp == i)
          .outcome_exclusions(slice, oc, covars, pt_col, visit_col, min_visit,
                              prevalent_ids = prevalent_ids)
        })
        audit <- dplyr::bind_rows(audit, imp_keep[[1L]]$audit)

        imp_data_list <- lapply(imp_keep, `[[`, "data")
        common_keys   <- Reduce(
          function(a, b) dplyr::semi_join(a, b, by = key_cols),
          lapply(imp_data_list, function(d)
            dplyr::distinct(d, dplyr::across(dplyr::all_of(key_cols))))
        )

        outcome_mids[[oc]] <- .filter_mids(data_shared_out, common_keys, key_cols)
        outcome_long[[oc]] <- mice::complete(outcome_mids[[oc]], action = "long",
                                             include = FALSE) |> tibble::as_tibble()
      }

    } else {
      # Complete-case: single pass, no .imp grouping.
      oc_data <- long_shared
      if (oc == "gait_speed")
        oc_data <- .create_lags(oc_data, pt_col, visit_col)
      oc_res <- .outcome_exclusions(oc_data, oc, covars, pt_col, visit_col, min_visit)
      audit  <- dplyr::bind_rows(audit, oc_res$audit)
      outcome_long[[oc]] <- dplyr::select(oc_res$data, -.imp)
      outcome_mids[[oc]] <- NULL
    }
  }

  audit <- dplyr::mutate(audit, step = dplyr::row_number(), .before = 1L)

  list(
    data_shared          = data_shared_out,
    data_excluded_shared = data_excluded_shared,
    data_outcome         = if (is_mids) outcome_mids else outcome_long,
    audit                = audit
  )
}


# ── Internal: shared exclusions ───────────────────────────────────────────────

.shared_exclusions <- function(obs, qc_tbl, exposure, pt_col, visit_col, min_visit) {

  audit <- .empty_audit()

  # Stage 1 — QC flags
  qc_fail <- .qc_failed_pts(qc_tbl, pt_col)
  obs     <- dplyr::filter(obs, !(.data[[pt_col]] %in% qc_fail))
  audit   <- .record_excl(audit, qc_fail, "shared", "qc_flag",
                           n_remaining = dplyr::n_distinct(obs[[pt_col]]))

  # Stage 2a — drop rows where exposure is NA, except at T4 (no FFQ at T4)
  pts_before_exp  <- unique(obs[[pt_col]])
  bad_dairy_keys  <- dplyr::filter(
    obs,
    .data[[visit_col]] != "T4" & is.na(.data[[exposure]])
  ) |>
    dplyr::distinct(.data[[pt_col]], .data[[visit_col]])
  obs <- dplyr::anti_join(obs, bad_dairy_keys, by = c(pt_col, visit_col))
  lost_exp <- setdiff(pts_before_exp, unique(obs[[pt_col]]))
  audit <- .record_excl(audit, lost_exp, "shared", "exposure_missing_lost_all_visits",
                         n_remaining = dplyr::n_distinct(obs[[pt_col]]))

  # Stage 2b — sumtot1 out of range (row-level: drop the visit, then check pt)
  # bad_sumtot1_keys is returned so the caller can also drop these visit-rows
  # from the full long data — keep_ids alone is pt-level and cannot capture this.
  bad_sumtot1_keys <- dplyr::tibble("{pt_col}" := character(0L),
                                    "{visit_col}" := character(0L))
  if ("sumtot1" %in% names(obs)) {
    bad_visits       <- dplyr::filter(obs, !is.na(sumtot1), sumtot1 < 500 | sumtot1 > 3500)
    bad_sumtot1_keys <- dplyr::distinct(bad_visits, .data[[pt_col]], .data[[visit_col]])
    obs              <- dplyr::anti_join(obs, bad_sumtot1_keys, by = c(pt_col, visit_col))
    lost_pts         <- setdiff(unique(bad_visits[[pt_col]]), unique(obs[[pt_col]]))
    audit <- .record_excl(audit, lost_pts, "shared", "sumtot1_out_of_range_lost_all_visits",
                           n_remaining = dplyr::n_distinct(obs[[pt_col]]))
  }

  list(
    keep_ids         = unique(obs[[pt_col]]),
    bad_dairy_keys   = bad_dairy_keys,
    bad_sumtot1_keys = bad_sumtot1_keys,
    audit            = audit
  )
}


# ── Internal: per-outcome exclusions ─────────────────────────────────────────

# @param prevalent_ids Optional character vector of participant IDs to exclude as
#   prevalent cases. When supplied (mids route), this is the union across all
#   imputations pre-computed by the caller so every .imp drops the same pts.
#   When NULL (CC route), the IDs are derived from `df` directly.
.outcome_exclusions <- function(df, oc, covars, pt_col, visit_col, min_visit,
                                prevalent_ids = NULL) {

  audit <- .empty_audit()

  # Stage 4 — prevalent sarcopenia at T1 (excluded before any other filter)
  if (oc %in% c("ewgsop2_sarcopenia_stage", "fnih_sarcopenia")) {
    if (is.null(prevalent_ids)) {
      prev_level    <- if (oc == "fnih_sarcopenia") "Sarcopenia" else "Confirmed"
      prevalent_ids <- df |>
        dplyr::filter(.data[[visit_col]] == "T1", .data[[oc]] == prev_level) |>
        dplyr::pull(.data[[pt_col]]) |>
        unique()
    }
    df    <- dplyr::filter(df, !(.data[[pt_col]] %in% prevalent_ids))
    audit <- .record_excl(audit, prevalent_ids, oc, "prevalent_baseline",
                           n_remaining = dplyr::n_distinct(df[[pt_col]]))
  }

  # Stage 5 — missing outcome (row-level; track pts that lose all visits)
  pts_before_oc <- unique(df[[pt_col]])
  df            <- dplyr::filter(df, !is.na(.data[[oc]]))
  lost_all_oc   <- setdiff(pts_before_oc, unique(df[[pt_col]]))
  audit <- .record_excl(audit, lost_all_oc, oc, "outcome_always_na",
                         n_remaining = dplyr::n_distinct(df[[pt_col]]))

  # Stage 6 — missing covariates (row-level)
  if (length(covars) > 0L) {
    present_cols <- intersect(covars, names(df))
    if (length(present_cols) > 0L) {
      pts_before  <- unique(df[[pt_col]])
      df          <- df[rowSums(is.na(df[present_cols])) == 0L, ]
      lost_cov    <- setdiff(pts_before, unique(df[[pt_col]]))
      audit <- .record_excl(audit, lost_cov, oc, "covariate_missing_lost_all_visits",
                             n_remaining = dplyr::n_distinct(df[[pt_col]]))
    }
  }

  # Stage 7 — visit recheck after row-level removals
  visit_counts <- df |>
    dplyr::distinct(.data[[pt_col]], .data[[visit_col]]) |>
    dplyr::count(.data[[pt_col]], name = "n_visits")

  too_few <- visit_counts |>
    dplyr::filter(n_visits < min_visit) |>
    dplyr::pull(.data[[pt_col]])
  df    <- dplyr::filter(df, !(.data[[pt_col]] %in% too_few))
  audit <- .record_excl(audit, too_few, oc, "too_few_visits_after_row_exclusions",
                         n_remaining = dplyr::n_distinct(df[[pt_col]]))

  list(data = df, audit = audit)
}


# ── Audit helpers ─────────────────────────────────────────────────────────────

.empty_audit <- function() {
  tibble::tibble(
    step_type   = character(0L),
    reason      = character(0L),
    n_excluded  = integer(0L),
    n_remaining = integer(0L)
  )
}

# Records one summary row per exclusion step.
# `n_remaining` is the number of unique participants still in the dataset
# *after* this exclusion (passed by the caller).
.record_excl <- function(audit, pt_ids, step_type, reason, n_remaining) {
  new_row <- tibble::tibble(
    step_type   = step_type,
    reason      = reason,
    n_excluded  = length(pt_ids),
    n_remaining = as.integer(n_remaining)
  )
  dplyr::bind_rows(audit, new_row)
}

# ── QC helper ─────────────────────────────────────────────────────────────────

.qc_failed_pts <- function(qc_tbl, pt_col) {
  qc_flag_cols <- c(
    "qc_pt_present", "qc_exam_date", "qc_sex_stable",
    "qc_datbirth_baseline", "qc_pt_unique"
  )

  in_osteolaus <- dplyr::filter(qc_tbl, qc_in_osteolaus %in% TRUE)
  present_flags <- intersect(qc_flag_cols, names(in_osteolaus))

  if (length(present_flags) == 0L) return(character(0L))

  failed <- in_osteolaus |>
    dplyr::filter(
      dplyr::if_any(dplyr::all_of(present_flags), ~ .x == FALSE | is.na(.x))
    )

  unique(as.character(failed[[pt_col]]))
}


# ── Lag helper (gait_speed only) ─────────────────────────────────────────────

#' Shift covariate/exposure columns one visit forward and drop T1.
#'
#' Creates `{col}_lag` columns for every column except `pt_col`, `visit_col`,
#' and `gait_speed`. Values at T2 move to T3, T3 to T4, etc. T1 rows are
#' dropped because they carry no lag.  For mids long format, lags are computed
#' independently within each `.imp` × pt group.
.create_lags <- function(df, pt_col, visit_col) {
  exclude_cols <- c(pt_col, visit_col, "gait_speed", ".imp", ".id")
  lag_cols     <- setdiff(names(df), exclude_cols)
  group_cols   <- intersect(c(".imp", pt_col), names(df))

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::arrange(.data[[visit_col]], .by_group = TRUE) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(lag_cols), dplyr::lag, .names = "{.col}_lag")
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data[[visit_col]] != "T1")
}


# ── mids row filter ───────────────────────────────────────────────────────────

#' Filter a mids object to a specific set of pt × visit rows.
#'
#' Avoids the as.mids() long-format roundtrip and the .imp == 0 reconstruction
#' problem. Works by finding the row indices in mids$data that match `keys`,
#' then subsetting $data, $imp, and $where in place.
#'
#' @param mids_obj  A mids object.
#' @param keys      Data frame with the key columns identifying rows to keep.
#' @param key_cols  Character vector of key column names (e.g. c("pt", "time_point")).
#' @return A mids object containing only the matching rows.
.filter_mids <- function(mids_obj, keys, key_cols) {
  keep_vec <- do.call(paste, as.data.frame(keys)[key_cols])
  data_vec <- do.call(paste, as.data.frame(mids_obj$data)[key_cols])
  row_idx  <- which(data_vec %in% keep_vec)

  out      <- mids_obj
  out$data <- mids_obj$data[row_idx, , drop = FALSE]

  # $imp[[var]] has one row per *missing* observation of that variable, not one
  # row per observation. Filter by finding which missing-value rows fall within
  # row_idx, then indexing into the (shorter) imp data frame by position.
  out$imp <- lapply(names(mids_obj$imp), function(var) {
    imp_df   <- mids_obj$imp[[var]]
    if (is.null(imp_df) || nrow(imp_df) == 0L) return(imp_df)
    miss_idx <- which(is.na(mids_obj$data[[var]]))   # positions of NAs in original data
    keep_pos <- which(miss_idx %in% row_idx)          # subset of those that we keep
    imp_df[keep_pos, , drop = FALSE]
  })
  names(out$imp) <- names(mids_obj$imp)

  if (!is.null(mids_obj$where))
    out$where <- mids_obj$where[row_idx, , drop = FALSE]
  if (!is.null(mids_obj$nmis))
    out$nmis  <- colSums(is.na(out$data))

  out
}


# ── Misc ──────────────────────────────────────────────────────────────────────

`%||%` <- function(x, y) if (!is.null(x)) x else y
