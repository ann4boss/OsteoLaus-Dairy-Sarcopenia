#TODO description
derive_visit_time <- function(df,
                              id_var = "pt",
                              date_var = "exam_date",
                              age_var = "Age",
                              visit_var = "visit_num",
                              time_var = "time_since_baseline",
                              age_baseline_var = "age_at_baseline") {
    
    df <- df |>
        dplyr::mutate(
            "{date_var}" := as.Date(.data[[date_var]])
        ) |>
        dplyr::group_by(.data[[id_var]]) |>
        dplyr::arrange(.data[[date_var]], .by_group = TRUE) |>
        dplyr::mutate(
            "{visit_var}" := dplyr::row_number(),
            baseline_date = first(.data[[date_var]]),
            baseline_age = first(.data[[age_var]]),
            "{time_var}" := as.numeric(
                difftime(.data[[date_var]], baseline_date, units = "days")
            ) / 365.25,
            "{age_baseline_var}" := baseline_age
        ) |>
        dplyr::ungroup()
    
    df <- df |> dplyr::select(-baseline_date, -baseline_age)
    
    if (".visit_colaus" %in% names(df)) {
        df <- df |>
            dplyr::relocate(
                all_of(c(visit_var, time_var, age_baseline_var)),
                .after = .visit_colaus
            )
    }
    
    df
}