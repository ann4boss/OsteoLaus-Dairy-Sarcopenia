# =============================================================================
# R/exclusion.R
# =============================================================================
# Sequential exclusion of participants and visits from the analysis dataset.
#TODO description
# =============================================================================
apply_exclusions <- function(data,
                             qc_table,
                             covariant_list,
                             exposure,
                             outcome = NULL,
                             visit_min = 2L,
                             pt_col = "pt",
                             visit_col = ".visit_osteo",
                             impute = FALSE,
                             imp_col = ".imp",
                             return_tracking = TRUE) {
  stopifnot(is.data.frame(data), is.data.frame(qc_table))

  normalize_column_names <- function(cols) {
    cols <- as.character(cols)
    cols[!is.na(cols) & nzchar(cols)]
  }

  covariant_list <- normalize_column_names(covariant_list)
  exposure <- normalize_column_names(exposure)
  outcome <- normalize_column_names(outcome)
  
  qc_flag_cols <- c(
    "qc_pt_present",
    "qc_exam_date",
    "qc_sex_stable",
    "qc_age_increasing",
    "qc_pt_unique"
  )
  
  needed_data_cols <- unique(c(pt_col, visit_col, covariant_list, exposure, outcome))
  if (isTRUE(impute)) {
    needed_data_cols <- unique(c(imp_col, needed_data_cols))
  }
  
  missing_data_cols <- setdiff(needed_data_cols, names(data))
  if (length(missing_data_cols) > 0) {
    stop("Missing column(s) in data: ", paste(missing_data_cols, collapse = ", "))
  }
  
  needed_qc_cols <- c(pt_col, "qc_in_osteolaus", qc_flag_cols)
  missing_qc_cols <- setdiff(needed_qc_cols, names(qc_table))
  if (length(missing_qc_cols) > 0) {
    stop("Missing column(s) in qc_table: ", paste(missing_qc_cols, collapse = ", "))
  }
  
  row_complete_cols <- unique(c(covariant_list, exposure, outcome))
  
  collapse_flagged_columns <- function(df, cols, flag_fun) {
    if (nrow(df) == 0) {
      return(character())
    }
    
    flags <- as.data.frame(lapply(df[cols], flag_fun))
    apply(flags, 1, function(row_flags) {
      paste(names(flags)[as.logical(row_flags)], collapse = ";")
    })
  }
  
  add_imp_column <- function(df, imp_value) {
    if (!isTRUE(impute)) {
      return(df)
    }
    
    df[[imp_col]] <- imp_value
    df
  }
  
  run_one_dataset <- function(data_one, qc_one, imp_value = NULL) {
    data_with_row_id <- data_one |>
      dplyr::mutate(.exclusion_row_id = dplyr::row_number())
    
    qc_candidates <- qc_one |>
      dplyr::filter(.data[["qc_in_osteolaus"]] %in% TRUE)
    
    qc_excluded_participants <- qc_candidates |>
      dplyr::mutate(
        .qc_failed_flags = collapse_flagged_columns(
          qc_candidates,
          qc_flag_cols,
          function(x) !(x %in% TRUE)
        )
      ) |>
      dplyr::filter(.data[[".qc_failed_flags"]] != "") |>
      dplyr::distinct(.data[[pt_col]], .data[[".qc_failed_flags"]]) |>
      dplyr::mutate(
        exclusion_stage = "participant_qc",
        exclusion_reason = "failed_qc",
        visit_count_before_exclusion = NA_integer_,
        visit_count_after_row_exclusions = NA_integer_
      )
    
    qc_failed_pt <- qc_excluded_participants |>
      dplyr::pull(.data[[pt_col]])
    
    after_qc <- data_with_row_id |>
      dplyr::filter(!(.data[[pt_col]] %in% qc_failed_pt))
    
    if (length(row_complete_cols) > 0) {
      missing_var_candidates <- after_qc |>
        dplyr::filter(dplyr::if_any(dplyr::all_of(row_complete_cols), is.na))
    } else {
      missing_var_candidates <- after_qc |>
        dplyr::filter(FALSE)
    }
    
    missing_var_rows <- missing_var_candidates |>
      dplyr::mutate(
        exclusion_stage = "row",
        exclusion_reason = "missing_required_variable",
        exclusion_detail = collapse_flagged_columns(
          missing_var_candidates,
          row_complete_cols,
          is.na
        )
      )
    
    if (length(row_complete_cols) > 0) {
      after_missing_vars <- after_qc |>
        dplyr::filter(dplyr::if_all(dplyr::all_of(row_complete_cols), ~ !is.na(.x)))
    } else {
      after_missing_vars <- after_qc
    }
    
    range_excluded_rows <- after_missing_vars |>
      dplyr::filter(FALSE) |>
      dplyr::mutate(
        exclusion_stage = character(),
        exclusion_reason = character(),
        exclusion_detail = character()
      )
    
    filtered <- after_missing_vars
    
    if ("sumtot1" %in% covariant_list) {
      sumtot1_excluded_rows <- filtered |>
        dplyr::filter(!dplyr::between(.data[["sumtot1"]], 500, 4200)) |>
        dplyr::mutate(
          exclusion_stage = "row",
          exclusion_reason = "sumtot1_out_of_range",
          exclusion_detail = "valid range: 500-4200"
        )
      range_excluded_rows <- dplyr::bind_rows(range_excluded_rows, sumtot1_excluded_rows)
      filtered <- filtered |>
        dplyr::filter(dplyr::between(.data[["sumtot1"]], 500, 4200))
    }
    
    if ("HGS_MAX" %in% outcome) {
      hgs_excluded_rows <- filtered |>
        dplyr::filter(.data[["HGS_MAX"]] < 0.1) |>
        dplyr::mutate(
          exclusion_stage = "row",
          exclusion_reason = "HGS_MAX_not_positive",
          exclusion_detail = "valid range: >0"
        )
      range_excluded_rows <- dplyr::bind_rows(range_excluded_rows, hgs_excluded_rows)
      filtered <- filtered |>
        dplyr::filter(.data[["HGS_MAX"]] > 0.1)
    }
    
    # outliers for dairy consumption
    if ("dairy_total_gday" %in% exposure) {
      dairy_excluded_rows <- filtered |>
        dplyr::filter(!dplyr::between(.data[["sumtot1"]], 0, 1000)) |>
        dplyr::mutate(
          exclusion_stage = "row",
          exclusion_reason = "dairy_out_of_range",
          exclusion_detail = "valid range: 0-1000"
        )
      range_excluded_rows <- dplyr::bind_rows(range_excluded_rows, dairy_excluded_rows)
      filtered <- filtered |>
        dplyr::filter(dplyr::between(.data[["dairy_total_gday"]], 0, 1000))
    }
    
    # -------------------------------------------------------------------------
    # Baseline sarcopenia exclusion
    # -------------------------------------------------------------------------
    
    if ("ewgsop2_sarcopenia_stage" %in% outcome) {
      
      baseline_excluded_pt <- filtered |>
        dplyr::filter(.data[[visit_col]] == 1) |>
        dplyr::filter(
          is.na(.data[["ewgsop2_sarcopenia_stage"]]) |
            .data[["ewgsop2_sarcopenia_stage"]] %in% c(
              "Confirmed",
              "Severe"
            )
        ) |>
        dplyr::pull(.data[[pt_col]]) |>
        unique()
      
      baseline_excluded_rows <- filtered |>
        dplyr::filter(.data[[pt_col]] %in% baseline_excluded_pt) |>
        dplyr::mutate(
          exclusion_stage = "participant_baseline",
          exclusion_reason = "baseline_sarcopenia_or_missing",
          exclusion_detail = "baseline EWGSOP2 not 'No sarcopenia'"
        )
      
      range_excluded_rows <- dplyr::bind_rows(
        range_excluded_rows,
        baseline_excluded_rows
      )
      
      filtered <- filtered |>
        dplyr::filter(!(.data[[pt_col]] %in% baseline_excluded_pt))
    }
    
    
    if ("fnih_sarcopenia" %in% outcome) {
      
      baseline_excluded_pt <- filtered |>
        dplyr::filter(.data[[visit_col]] == 1) |>
        dplyr::filter(
          is.na(.data[["fnih_sarcopenia"]]) |
            .data[["fnih_sarcopenia"]] == "Sarcopenia"
        ) |>
        dplyr::pull(.data[[pt_col]]) |>
        unique()
      
      baseline_excluded_rows <- filtered |>
        dplyr::filter(.data[[pt_col]] %in% baseline_excluded_pt) |>
        dplyr::mutate(
          exclusion_stage = "participant_baseline",
          exclusion_reason = "baseline_sarcopenia_or_missing",
          exclusion_detail = "baseline FNIH sarcopenia or missing"
        )
      
      range_excluded_rows <- dplyr::bind_rows(
        range_excluded_rows,
        baseline_excluded_rows
      )
      
      filtered <- filtered |>
        dplyr::filter(!(.data[[pt_col]] %in% baseline_excluded_pt))
    }
    
    visit_counts_before <- after_qc |>
      dplyr::group_by(.data[[pt_col]]) |>
      dplyr::summarise(
        visit_count_before_exclusion = sum(!is.na(.data[[visit_col]])),
        .groups = "drop"
      )
    
    visit_counts_after <- filtered |>
      dplyr::group_by(.data[[pt_col]]) |>
      dplyr::summarise(
        visit_count_after_row_exclusions = sum(!is.na(.data[[visit_col]])),
        .groups = "drop"
      )
    
    visit_excluded_participants <- visit_counts_before |>
      dplyr::left_join(visit_counts_after, by = pt_col) |>
      dplyr::mutate(
        visit_count_after_row_exclusions = dplyr::coalesce(.data[["visit_count_after_row_exclusions"]], 0L)
      ) |>
      dplyr::filter(.data[["visit_count_after_row_exclusions"]] < visit_min) |>
      dplyr::mutate(
        exclusion_stage = "participant_visits",
        exclusion_reason = dplyr::if_else(
          .data[["visit_count_before_exclusion"]] < visit_min,
          "too_few_visits_before_row_exclusions",
          "too_few_visits_after_row_exclusions"
        ),
        .qc_failed_flags = NA_character_
      )
    
    final_data <- filtered |>
      dplyr::semi_join(
        visit_counts_after |>
          dplyr::filter(.data[["visit_count_after_row_exclusions"]] >= visit_min),
        by = pt_col
      ) |>
      dplyr::select(-dplyr::all_of(".exclusion_row_id")) |>
      dplyr::ungroup()
    
    if (!return_tracking) {
      return(final_data)
    }
    
    excluded_rows <- dplyr::bind_rows(missing_var_rows, range_excluded_rows) |>
      dplyr::arrange(.data[[".exclusion_row_id"]])
    
    excluded_participants <- dplyr::bind_rows(
      qc_excluded_participants,
      visit_excluded_participants
    ) |>
      dplyr::arrange(.data[[pt_col]], .data[["exclusion_stage"]])
    
    
    # define participant sets at each stage
    pt_initial  <- unique(data_one[[pt_col]])
    pt_qc       <- unique(after_qc[[pt_col]])
    pt_missing  <- unique(after_missing_vars[[pt_col]])
    pt_range    <- unique(filtered[[pt_col]])
    pt_final    <- unique(final_data[[pt_col]])
    
    # exclusion counts (participant-level differences)
    n_qc_excl     <- length(setdiff(pt_initial, pt_qc))
    n_missing_excl <- length(setdiff(pt_qc, pt_missing))
    n_range_excl   <- length(setdiff(pt_missing, pt_range))
    n_visit_excl   <- length(setdiff(pt_range, pt_final))
    
    consort_counts <- dplyr::bind_rows(
      
      dplyr::tibble(
        stage = "Initial sample",
        n_participants = length(pt_initial),
        n_rows = nrow(data_one)
      ),
      
      dplyr::tibble(
        stage = "Excluded for QC",
        n_participants = n_qc_excl,
        n_rows = sum(data_one[[pt_col]] %in% setdiff(pt_initial, pt_qc))
      ),
      
      dplyr::tibble(
        stage = "Remaining after QC",
        n_participants = length(pt_qc),
        n_rows = nrow(after_qc)
      ),
      
      dplyr::tibble(
        stage = "Excluded for missing covariates",
        n_participants = n_missing_excl,
        n_rows = nrow(missing_var_rows)
      ),
      
      dplyr::tibble(
        stage = "Remaining after missing exclusions",
        n_participants = length(pt_missing),
        n_rows = nrow(after_missing_vars)
      ),
      
      dplyr::tibble(
        stage = "Excluded for implausible values",
        n_participants = n_range_excl,
        n_rows = nrow(range_excluded_rows)
      ),
      
      dplyr::tibble(
        stage = "Remaining after range exclusions",
        n_participants = length(pt_range),
        n_rows = nrow(filtered)
      ),
      
      dplyr::tibble(
        stage = "Excluded for insufficient visits",
        n_participants = n_visit_excl,
        n_rows = NA_integer_
      ),
      
      dplyr::tibble(
        stage = "Final analytic sample",
        n_participants = length(pt_final),
        n_rows = nrow(final_data)
      )
    )
    
    
    
    list(
      data = add_imp_column(final_data, imp_value),
      excluded_participants = add_imp_column(excluded_participants, imp_value),
      excluded_rows = add_imp_column(excluded_rows, imp_value),
      consort_counts = add_imp_column(consort_counts, imp_value)
    )
  }
  
  if (!isTRUE(impute)) {
    return(run_one_dataset(data, qc_table))
  }
  
  imp_values <- sort(unique(data[[imp_col]]))
  results <- lapply(imp_values, function(imp_value) {
    data_one <- data |>
      dplyr::filter(.data[[imp_col]] == imp_value)
    
    if (imp_col %in% names(qc_table)) {
      qc_one <- qc_table |>
        dplyr::filter(.data[[imp_col]] == imp_value)
    } else {
      qc_one <- qc_table
    }
    
    run_one_dataset(data_one, qc_one, imp_value)
  })
  
  if (!return_tracking) {
    return(dplyr::bind_rows(results))
  }
  
  list(
    data = dplyr::bind_rows(lapply(results, `[[`, "data")),
    excluded_participants = dplyr::bind_rows(lapply(results, `[[`, "excluded_participants")),
    excluded_rows = dplyr::bind_rows(lapply(results, `[[`, "excluded_rows")),
    consort_counts = dplyr::bind_rows(lapply(results, `[[`, "consort_counts"))
  )
}
