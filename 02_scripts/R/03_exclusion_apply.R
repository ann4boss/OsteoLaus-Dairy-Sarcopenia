# =============================================================================
# R/exclusion.R
# =============================================================================
#
# Pipeline overview
# -----------------
#  SHARED (runs once, outcome-agnostic):
#    1. Carry-forward dairy_total_gday_cumavg from T3 → T4
#    2. QC exclusions (participant-level)
#    3. Exclude participants missing dairy_total_gday_cumavg at *any* remaining visit
#    4. Exclude participants with < visit_min visits
#
#  PER-OUTCOME (runs separately for each of the four outcomes):
#    5. [gait_speed only] Lag covariates + exposure by one visit; drop T1 rows
#    6. Exclude rows with missing covariates
#    7. Drop rows where the outcome variable is NA
#    8. Re-check visit counts; exclude participants now below visit_min
#
#  OUTPUTS:
#    - data            : named list of four cleaned datasets (one per outcome)
#    - consort_counts  : single tibble, columns stage / outcome / n_participants
#    - not_in_any      : participants + all their data excluded from every outcome
#    - excluded_participants / excluded_rows : tracking tables (long format)
#
# =============================================================================

apply_exclusions <- function(
    data,
    qc_table,
    outcomes = c(
      "ewgsop2_sarcopenia_stage",
      "HGS_MAX",
      "ALM_HT2_harmonised",
      "gait_speed"
    ),
    covariates = list(),
    visit_min = 2L,
    pt_col = "pt",
    visit_col = "time_point",
    impute = FALSE,
    imp_col = ".imp",
    exposure = "dairy_total_gday_cumavg"
) {
  
  
  if (!impute) {
    # Run pipeline once for complete case
    result <- run_pipeline(
      data, qc_table, outcomes,
      covariates, pt_col, visit_col, visit_min, exposure,
      imp_col = imp_col
    )
    
    # Return in SAME STRUCTURE as impute = TRUE
    # For complete case, wrap everything in lists with a single element
    return(list(
      data = stats::setNames(
        lapply(outcomes, function(oc) {
          df <- result$data[[oc]]
          # Add .imp = 1 for consistency with imputed structure
          if (!is.null(df) && nrow(df) > 0) {
            df[[imp_col]] <- 1L
          }
          df
        }),
        outcomes  # Set names explicitly
      ),
      consort = {
        df <- result$consort_long
        if (!is.null(df) && nrow(df) > 0) {
          df[[imp_col]] <- 1L
        }
        list(df)  # Wrap in list to match impute = TRUE structure
      },
      excluded = {
        df <- result$excluded_participants
        if (!is.null(df) && nrow(df) > 0) {
          df[[imp_col]] <- 1L
        }
        list(df)  # Wrap in list to match impute = TRUE structure
      }
    ))
  }
  
  # For impute = TRUE, process each imputation
  res <- run_by_imputation(
    data, qc_table, imp_col,
    function(d, q, i) {
      run_pipeline(
        d, q, outcomes,
        covariates, pt_col, visit_col, visit_min, exposure,
        imp_col = imp_col
      )
    }
  )
  
  # Return with .imp column preserved
  list(
    data = stats::setNames(
      lapply(outcomes, function(oc) {
        dplyr::bind_rows(lapply(seq_along(res), function(idx) {
          df <- res[[idx]]$data[[oc]]
          if (!is.null(df) && nrow(df) > 0) {
            df[[imp_col]] <- as.integer(names(res)[idx] %||% idx)
          }
          df
        }))
      }),
      outcomes
    ),
    consort = lapply(seq_along(res), function(idx) {
      df <- res[[idx]]$consort_long
      if (!is.null(df) && nrow(df) > 0) {
        df[[imp_col]] <- as.integer(names(res)[idx] %||% idx)
      }
      df
    }),
    excluded = lapply(seq_along(res), function(idx) {
      df <- res[[idx]]$excluded_participants
      if (!is.null(df) && nrow(df) > 0) {
        df[[imp_col]] <- as.integer(names(res)[idx] %||% idx)
      }
      df
    })
  )
}


add_consort_row <- function(consort, outcome, stage,
                            n_excluded = NA_integer_,
                            n_remaining) {
  dplyr::bind_rows(
    consort,
    tibble::tibble(
      outcome = outcome,
      stage = stage,
      n_excluded = n_excluded,
      n_remaining = n_remaining
    )
  )
}


run_pipeline <- function(
    data,
    qc_table,
    outcomes,
    covariates,
    pt_col = "pt",
    visit_col = "time_point",
    visit_min = 2,
    exposure = "dairy_total_gday_cumavg",
    imp_col = NULL
) {
  
  `%||%` <- function(x, y) if (!is.null(x)) x else y
  
  get_covariates <- function(oc) {
    covariates[[oc]] %||% character(0)
  }
  
  
  
  consort <- tibble::tibble(
    outcome = character(),
    stage = character(),
    n_excluded = integer(),
    n_remaining = integer()
  )
  
  excluded_all <- tibble::tibble()
  
  # =========================================================
  # 1. QC FILTER
  # =========================================================
  n_initial <- dplyr::n_distinct(data[[pt_col]])
  
  qc_excl <- qc_exclude_participants(
    qc_table,
    pt_col,
    qc_flag_cols = c(
      "qc_pt_present",
      "qc_exam_date",
      "qc_sex_stable",
      "qc_datbirth_baseline",
      "qc_pt_unique"
    )
  )
  
  data <- dplyr::filter(data, !(.data[[pt_col]] %in% qc_excl[[pt_col]]))
  
  n_after_qc <- dplyr::n_distinct(data[[pt_col]])
  
  consort <- add_consort_row(consort, "shared", "Initial sample",
                             n_remaining = n_initial)
  
  consort <- add_consort_row(consort, "shared", "Excluded QC",
                             n_excluded = n_initial - n_after_qc,
                             n_remaining = n_after_qc)
  
  excluded_all <- dplyr::bind_rows(excluded_all, qc_excl)
  
  # =========================================================
  # 2. EXPOSURE FILTER
  # =========================================================
  exp_res <- exclude_missing_exposure(data, pt_col, exposure)
  
  data <- exp_res$data
  
  n_after_exp <- dplyr::n_distinct(data[[pt_col]])
  
  consort <- add_consort_row(consort, "shared",
                             paste0("Missing ", exposure),
                             n_excluded = n_after_qc - n_after_exp,
                             n_remaining = n_after_exp)
  
  excluded_all <- dplyr::bind_rows(excluded_all, exp_res$excluded)
  
  # =========================================================
  # 3. MIN VISITS FILTER
  # =========================================================
  visit_res <- compute_valid_visit_participants(
    data, pt_col, visit_col, visit_min
  )
  
  data <- visit_res$data
  
  n_after_visit <- dplyr::n_distinct(data[[pt_col]])
  
  consort <- add_consort_row(consort, "shared",
                             paste0("<", visit_min, " visits"),
                             n_excluded = n_after_exp - n_after_visit,
                             n_remaining = n_after_visit)
  
  excluded_all <- dplyr::bind_rows(excluded_all, visit_res$excl)
  
  shared_data <- data
  
  # =========================================================
  # 4. OUTCOME LOOP
  # =========================================================
  outcome_data <- list()
  outcome_excl <- list()
  summary <- list()
  
  for (oc in outcomes) {
    
    oc_data <- shared_data
    covars <- get_covariates(oc)
    
    n_start <- dplyr::n_distinct(oc_data[[pt_col]])
    
    consort <- add_consort_row(consort, oc,
                               "Start outcome",
                               n_remaining = n_start)
    
    # -------------------------
    # LAG (ONLY gait_speed)
    # -------------------------
    lag_excl <- tibble::tibble()
    
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
        dplyr::ungroup()
      
      lag_excl <- oc_data |>
        dplyr::filter(.data[[visit_col]] == "T1")
      
      covars <- paste0(covars, "_lag")
      exposure <- paste0(exposure, "_lag")
    }
    
    # =====================================================
    # BASELINE EXCLUSION
    # =====================================================
    baseline_excl <- tibble::tibble()
    
    if (oc %in% c("ewgsop2_sarcopenia_stage", "fnih_sarcopenia")) {
      
      ids <- if (oc == "fnih_sarcopenia") {
        oc_data |>
          dplyr::filter(.data[[visit_col]] == "T1",
                        .data[[oc]] == "Sarcopenia") |>
          dplyr::pull(.data[[pt_col]]) |>
          unique()
      } else {
        oc_data |>
          dplyr::filter(.data[[visit_col]] == "T1",
                        .data[[oc]] %in% c("Confirmed")) |>
          dplyr::pull(.data[[pt_col]]) |>
          unique()
      }
      
      baseline_excl <- tibble::tibble(!!pt_col := ids)
      
      oc_data <- dplyr::filter(oc_data, !(.data[[pt_col]] %in% ids))
      
      n_after_base <- dplyr::n_distinct(oc_data[[pt_col]])
      
      consort <- add_consort_row(consort, oc,
                                 "Prevalent baseline",
                                 n_excluded = n_start - n_after_base,
                                 n_remaining = n_after_base)
    }
    
    
    if ("sumtot1" %in% covars || "sumtot1_lag" %in% covars) {
      
      sumtot_res <- exclude_invalid_sumtot1(
        oc_data,
        pt_col
      )
      
      oc_data <- sumtot_res$data
      
      excluded_all <- dplyr::bind_rows(
        excluded_all,
        sumtot_res$excluded
      )
      
      n_after_sumtot <- dplyr::n_distinct(oc_data[[pt_col]])
      
      consort <- add_consort_row(
        consort,
        oc,
        "sumtot1 outside 500-4200",
        n_excluded = n_start - n_after_sumtot,
        n_remaining = n_after_sumtot
      )
      
      n_start <- n_after_sumtot
    }
    
    # =====================================================
    # ROW EXCLUSIONS
    # =====================================================
    if (length(covars) > 0) {
      miss_cov <- rowSums(is.na(oc_data[covars])) > 0
    } else {
      miss_cov <- rep(FALSE, nrow(oc_data))
    }
    
    cov_excl <- oc_data[miss_cov, ]
    oc_data <- oc_data[!miss_cov, ]
    
    out_excl <- oc_data |>
      dplyr::filter(is.na(.data[[oc]]))
    
    oc_data <- dplyr::filter(oc_data, !is.na(.data[[oc]]))
    
    # =====================================================
    # VISIT RECHECK
    # =====================================================
    visit_counts <- oc_data |>
      dplyr::distinct(.data[[pt_col]], .data[[visit_col]]) |>
      dplyr::count(.data[[pt_col]], name = "n_visits")
    
    
    keep_ids <- visit_counts |>
      dplyr::filter(n_visits >= visit_min) |>
      dplyr::pull(.data[[pt_col]])
    
    visit_excl <- visit_counts |>
      dplyr::filter(n_visits < visit_min)
    
    final_data <- dplyr::filter(oc_data, .data[[pt_col]] %in% keep_ids)
    
    n_final <- dplyr::n_distinct(final_data[[pt_col]])
    
    consort <- add_consort_row(consort, oc,
                               paste0("<", visit_min, " visits final"),
                               n_excluded = n_start - n_final,
                               n_remaining = n_final)
    
    # =====================================================
    # STORE OUTPUTS
    # =====================================================
    outcome_data[[oc]] <- final_data
    
    outcome_excl[[oc]] <- dplyr::bind_rows(
      lag_excl,
      baseline_excl,
      cov_excl,
      out_excl,
      visit_excl
    )
    
    summary[[oc]] <- tibble::tibble(
      outcome = oc,
      n_participants = n_final,
      n_rows = nrow(final_data)
    )
  }
  
  list(
    shared_data = shared_data,
    data = outcome_data,
    consort_long = consort,
    consort_wide = tidyr::pivot_wider(
      consort,
      names_from = outcome,
      values_from = n_remaining
    ),
    summary = dplyr::bind_rows(summary),
    excluded_participants = excluded_all,
    excluded_rows = dplyr::bind_rows(outcome_excl)
  )
}


exclude_invalid_sumtot1 <- function(data, pt_col) {
  
  bad_ids <- data |>
    dplyr::filter(!is.na(sumtot1),
                  (sumtot1 < 500 | sumtot1 > 4200)) |>
    dplyr::pull(.data[[pt_col]]) |>
    unique()
  
  list(
    data = dplyr::filter(data, !(.data[[pt_col]] %in% bad_ids)),
    excluded = tibble::tibble(
      !!pt_col := bad_ids,
      exclusion_reason = "sumtot1 outside 500-4200"
    )
  )
}