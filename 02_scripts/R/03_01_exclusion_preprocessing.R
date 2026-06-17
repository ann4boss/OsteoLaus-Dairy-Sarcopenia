create_lags <- function(df,
                        pt_col = "pt",
                        time_col = "time_point") {
    
    exclude_cols <- c(".imp", pt_col, time_col, "exam_date", "gait_speed")
    
    lag_cols <- setdiff(names(df), exclude_cols)
    
    df |>
        dplyr::group_by(.data[[pt_col]]) |>
        dplyr::arrange(.data[[time_col]], .by_group = TRUE) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(lag_cols),
                dplyr::lag,
                .names = "{.col}_lag"
            )
        ) |>
        dplyr::ungroup()
}

