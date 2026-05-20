# =============================================================================
# R/impute_colaus_smoking.R
# =============================================================================
# impute_smoking_locf()
#     Carries the most recent non-NA smoking_status forward within each
#     participant (Baseline -> F1 -> F2 -> F3). Only fills NA cells;
#     corrected and observed values are never overwritten.
#
# Output columns
# --------------
#   smoking_status_imp    Factor: Never / Former / Current
#                         smoking_status with remaining NAs filled by LOCF.
#   smoking_impute_source Factor: observed / locf / NA
#                         Indicates how each smoking_status_imp value was obtained.
#
# =============================================================================

#' Carry smoking_status forward within each participant (LOCF).
#'
#' Fills NA values in \code{smoking_status} using the most recent non-NA
#' value for the same participant. Visits are processed in ascending
#' \code{.visit} order. Rows already holding an observed or corrected value
#' are never modified.
#'
#' @param df CoLaus long tibble after \code{correct_smoking_trajectory()};
#'   must contain \code{pt}, \code{.visit}, \code{smoking_status}, and
#'   \code{smoking_impute_source}.
#' @return \code{df} with \code{smoking_status} imputed in-place and
#'   \code{smoking_impute_source} updated to \code{"locf"} for filled rows.
impute_smoking <- function(df) {
    
    required_cols <- c("pt", ".visit", "smoking_status", "smoking_impute_source")
    missing_cols  <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0L) {
        cli::cli_warn(
            "impute_smoking: missing required columns: {.val {missing_cols}}."
        )
        return(df)
    }
    
    n_na_before <- sum(is.na(df$smoking_status))
    
    df <- df |>
        dplyr::arrange(pt, .visit) |>
        dplyr::group_by(pt) |>
        dplyr::mutate(
            smoking_status_imp = smoking_status,
            
            # preserve observed / derived
            smoking_impute_source = dplyr::if_else(
                !is.na(smoking_status),
                as.character(smoking_impute_source),
                NA_character_
            )
        ) |>
        tidyr::fill(smoking_status_imp, .direction = "down") |>
        dplyr::mutate(
            smoking_impute_source = dplyr::case_when(
                !is.na(smoking_impute_source) ~ smoking_impute_source,
                is.na(smoking_status) & !is.na(smoking_status_imp) ~ "locf",
                TRUE ~ NA_character_
            ),
            
            smoking_impute_source = factor(
                smoking_impute_source,
                levels = c("observed", "derived", "locf")
            )
        ) |>
        dplyr::ungroup()
    
    # ── Correct diagnostics --------------------------------------------------
    n_na_after <- sum(is.na(df$smoking_status_imp))
    n_filled   <- n_na_before - n_na_after
    
    visit_summary <- df |>
        dplyr::group_by(.visit) |>
        dplyr::summarise(
            n_total         = dplyr::n(),
            n_missing_final = sum(is.na(smoking_status_imp)),
            n_observed      = sum(smoking_impute_source == "observed", na.rm = TRUE),
            n_derived       = sum(smoking_impute_source == "derived", na.rm = TRUE),
            n_locf          = sum(smoking_impute_source == "locf", na.rm = TRUE),
            .groups = "drop"
        )
    
    cli::cli_h2("Impute Smoking Status (LOCF)")
    cli::cli_inform(c(
        "i" = "LOCF summary:",
        "*" = "NA before: {n_na_before}",
        "*" = "Filled:    {n_filled}",
        "*" = "Remaining: {n_na_after}"
    ))
    
    cli::cli_inform(
        paste(capture.output(print(visit_summary, n = Inf)), collapse = "\n")
    )
    
    return(df)
}
