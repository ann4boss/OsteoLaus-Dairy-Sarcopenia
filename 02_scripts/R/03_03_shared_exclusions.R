exclude_missing_exposure <- function(df, pt_col, exposure) {
    
    keep <- df |>
        dplyr::group_by(.data[[pt_col]]) |>
        dplyr::summarise(any_obs = any(!is.na(.data[[exposure]])),
                         .groups = "drop") |>
        dplyr::filter(any_obs) |>
        dplyr::pull(.data[[pt_col]])
    
    list(
        data  = dplyr::filter(df, .data[[pt_col]] %in% keep),
        excl  = dplyr::filter(df, !(.data[[pt_col]] %in% keep)) |>
            dplyr::distinct(.data[[pt_col]]) |>
            dplyr::mutate(
                exclusion_stage = "exposure_missing",
                exclusion_reason = paste0("no_", exposure),
                exclusion_detail = "never observed"
            )
    )
}

compute_valid_visit_participants <- function(df, pt_col, visit_col, min_visits) {
    
    # counts UNIQUE observed visits (NOT imputed rows)
    visit_counts <- df |>
        dplyr::distinct(.data[[pt_col]], .data[[visit_col]]) |>
        dplyr::count(.data[[pt_col]], name = "n_visits")
    
    keep <- visit_counts |>
        dplyr::filter(n_visits >= min_visits) |>
        dplyr::pull(.data[[pt_col]])
    
    list(
        data = dplyr::filter(df, .data[[pt_col]] %in% keep),
        excl = visit_counts |>
            dplyr::filter(n_visits < min_visits) |>
            dplyr::mutate(
                exclusion_stage = "visit_min",
                exclusion_reason = "too_few_observed_visits",
                exclusion_detail = paste0("n<", min_visits)
            )
    )
}