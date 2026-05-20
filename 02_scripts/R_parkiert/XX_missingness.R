# =============================================================================
# R/missingness.R
# =============================================================================
# Computes missingness statistics for a long-format dataset.
#
# Provides:
#   - Overall missingness per variable
#   - Missingness stratified by wave (.wave_num)
#
# Returns tidy tibbles for downstream reporting or plotting.
# No modification of input data.
# =============================================================================


# -----------------------------------------------------------------------------
# Internal helper: compute missingness for a data frame
# -----------------------------------------------------------------------------

#' Compute missingness for all variables in a data frame.
#'
#' @param df Data frame
#' @param exclude Optional character vector of columns to exclude
#' @return Tibble with variable, n_missing, n_non_missing, pct_missing
.compute_missingness <- function(df, exclude = NULL) {
    
    df %>%
        dplyr::select(-dplyr::any_of(exclude)) %>%
        dplyr::summarise(
            dplyr::across(
                dplyr::everything(),
                list(
                    n_missing     = ~ sum(is.na(.)),
                    n_non_missing = ~ sum(!is.na(.)),
                    pct_missing   = ~ mean(is.na(.)) * 100
                ),
                .names = "{.col}__{.fn}"
            )
        ) %>%
        tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = c("variable", ".value"),
            names_sep = "__"
        ) %>%
        dplyr::arrange(dplyr::desc(pct_missing))
}


# -----------------------------------------------------------------------------
# Missingness per wave
# -----------------------------------------------------------------------------

#' Compute missingness stratified by wave.
#'
#' @param df Long dataset containing .wave_num
#' @return Tibble with missingness per variable and wave
.compute_missingness_by_wave <- function(df) {
    
    if (!".wave_num" %in% names(df)) {
        cli::cli_abort("Column {.col .wave_num} not found in data.")
    }
    
    df %>%
        dplyr::group_by(.wave_num) %>%
        dplyr::summarise(
            dplyr::across(
                - .wave_num,
                list(
                    n_missing     = ~ sum(is.na(.)),
                    n_non_missing = ~ sum(!is.na(.)),
                    pct_missing   = ~ mean(is.na(.)) * 100
                ),
                .names = "{.col}__{.fn}"
            ),
            .groups = "drop"
        ) %>%
        tidyr::pivot_longer(
            cols = - .wave_num,
            names_to = c("variable", ".value"),
            names_sep = "__"
        ) %>%
        dplyr::arrange(.wave_num, dplyr::desc(pct_missing))
}


# -----------------------------------------------------------------------------
# Public function: analyse_missingness
# -----------------------------------------------------------------------------

#' Analyse missingness in a long dataset
#'
#' Computes overall and wave-specific missingness.
#'
#' @param df Long-format dataset (e.g. colaus_long)
#' @return List with:
#'   - overall: missingness across entire dataset
#'   - by_wave: missingness stratified by .wave_num
analyse_missingness <- function(df) {
    
    # Basic validation
    if (!is.data.frame(df)) {
        cli::cli_abort("Input must be a data frame.")
    }
    
    # Compute outputs
    overall <- .compute_missingness(df, exclude = ".wave_num")
    by_wave <- .compute_missingness_by_wave(df)
    
    # Inform user
    cli::cli_inform(c(
        "i" = "Missingness analysis completed.",
        "*" = "{nrow(overall)} variables analysed (overall).",
        "*" = "{dplyr::n_distinct(df$.wave_num)} wave(s) detected."
    ))
    
    return(list(
        overall = overall,
        by_wave = by_wave
    ))
}


