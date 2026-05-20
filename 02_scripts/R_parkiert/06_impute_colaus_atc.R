impute_atc <- function(df) {
    
    # ── Derived variables from your script -------------------------------
    atc_vars <- names(ATC_PREFIXES)
    
    required_cols <- c("pt", ".visit", atc_vars)
    missing_cols <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0L) {
        cli::cli_abort(
            "impute_atc_locf: missing required columns: {.val {missing_cols}}."
        )
    }
    
    # ── Ensure ordering --------------------------------------------------
    df <- df |>
        dplyr::arrange(pt, .visit) |>
        dplyr::group_by(pt)
    
    # ── Apply LOCF to all derived variables -----------------------------
    for (v in atc_vars) {
        
        imp_col <- paste0(v, "_imp")
        src_col <- paste0(v, "_impute_source")
        
        df <- df |>
            dplyr::mutate(
                # initialize if first time
                "{imp_col}" := dplyr::if_else(
                    is.na(.data[[v]]),
                    NA,
                    .data[[v]]
                ),
                
                "{src_col}" := dplyr::if_else(
                    !is.na(.data[[v]]),
                    "observed",
                    NA_character_
                )
            ) |>
            tidyr::fill(
                dplyr::all_of(imp_col),
                .direction = "down"
            ) |>
            dplyr::mutate(
                "{src_col}" := dplyr::case_when(
                    !is.na(.data[[src_col]]) ~ .data[[src_col]],
                    is.na(.data[[v]]) & !is.na(.data[[imp_col]]) ~ "locf",
                    TRUE ~ NA_character_
                )
            )
    }
    
    df <- df |>
        dplyr::ungroup() |>
        dplyr::mutate(
            dplyr::across(
                dplyr::ends_with("_impute_source"),
                ~ factor(.x, levels = c("observed", "locf"))
            )
        )
    
    # ── Diagnostics ------------------------------------------------------
    summary <- df |>
        dplyr::summarise(
            total_rows = dplyr::n(),
            .groups = "drop"
        )
    #TODO: message is not ideal
    cli::cli_h2("ATC LOCF Imputation")
    cli::cli_inform(c(
        "i" = "Created *_imp and *_impute_source columns for {length(atc_vars)} variables.",
        "*" = "Rows processed: {summary$total_rows}"
    ))
    
    df
}