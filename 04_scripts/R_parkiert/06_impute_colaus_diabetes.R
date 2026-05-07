impute_diabetes <- function(df) {
    
    # ── Checks ---------------------------------------------------------------
    required_cols <- c("pt", ".visit", "diabetes_status")
    missing_cols  <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0L) {
        cli::cli_abort(
            "impute_diabetes_locf: missing required columns: {.val {missing_cols}}."
        )
    }
    
    # ── LOCF ----------------------------------------------------------------
    n_na_before <- sum(is.na(df$diabetes_status))
    
    df <- df |>
        dplyr::arrange(pt, .visit) |>
        dplyr::group_by(pt) |>
        dplyr::mutate(
            diabetes_status_imp = diabetes_status,
            diabetes_impute_source = dplyr::if_else(
                !is.na(diabetes_status), "observed", NA_character_
            )
        ) |>
        tidyr::fill(diabetes_status_imp, .direction = "down") |>
        dplyr::mutate(
            diabetes_impute_source = dplyr::case_when(
                !is.na(diabetes_impute_source) ~ diabetes_impute_source,
                is.na(diabetes_status) & !is.na(diabetes_status_imp) ~ "locf",
                TRUE ~ NA_character_
            ),
            diabetes_impute_source = factor(
                diabetes_impute_source,
                levels = c("observed", "locf")
            )
        ) |>
        dplyr::ungroup()
    
    # ── Diagnostics ----------------------------------------------------------
    n_na_after <- sum(is.na(df$diabetes_status_imp))
    n_filled   <- n_na_before - n_na_after
    
    cli::cli_h2("Impute Diabetes Status (LOCF)")
    cli::cli_inform(c(
        "i" = "LOCF summary:",
        "*" = "NA before: {n_na_before}",
        "*" = "Filled:    {n_filled}",
        "*" = "Remaining: {n_na_after}"
    ))
    
    return(df)
}