# =============================================================================
# TODO: description
# =============================================================================

#' Apply derive_colaus() to every imputed CoLaus dataset.
#'
#' @param mice_result Named list returned by impute_mice_colaus().
#' @return Long-format tibble with all derived variables, .imp preserved.
mice_derive_colaus <- function(mice_result) {
    cli::cli_h1("MICE: Applying derive_colaus() to {mice_result$m} imputed datasets")
    
    long_df  <- mice_result$long
    imp_ids  <- sort(setdiff(unique(long_df$.imp), 0L))
    
    out <- purrr::map(imp_ids, function(i) {
        cli::cli_inform("  [{i}/{mice_result$m}] derive_colaus() ...")
        long_df |>
            dplyr::filter(.imp == i) |>
            dplyr::select(-.imp) |>
            derive_colaus() |>
            dplyr::mutate(.imp = i, .before = 1L)
    }) |> dplyr::bind_rows()
    
    cli::cli_inform(c(
        "v" = "mice_derive_all() complete.",
        "i" = "{nrow(out)} rows across {mice_result$m} datasets \\
               ({nrow(out) / mice_result$m} rows each)."
    ))
    return(out)
}


mice_derive_osteo <- function(mice_result) {
    cli::cli_h1("MICE: Applying derive_colaus() to {mice_result$m} imputed datasets")
    
    long_df  <- mice_result$long
    imp_ids  <- sort(setdiff(unique(long_df$.imp), 0L))
    
    out <- purrr::map(imp_ids, function(i) {
        cli::cli_inform("  [{i}/{mice_result$m}] derive_colaus() ...")
        long_df |>
            dplyr::filter(.imp == i) |>
            dplyr::select(-.imp) |>
            derive_osteo() |>
            dplyr::mutate(.imp = i, .before = 1L)
    }) |> dplyr::bind_rows()
    
    cli::cli_inform(c(
        "v" = "mice_derive_all() complete.",
        "i" = "{nrow(out)} rows across {mice_result$m} datasets \\
               ({nrow(out) / mice_result$m} rows each)."
    ))
    return(out)
}

