# =============================================================================
# R/03_01_exclusion_preprocessing.R
# =============================================================================
# Input normalisation shared across the exclusion pipeline.
#
# Functions
# ---------
#   normalise_exclusion_input(data)
#       Accepts a plain data frame or a mids object (long-format, as produced
#       by build_analysis_dataset()). Returns a canonical list(long, m, is_mice).
#       - long     : long-format tibble with .imp column (mids) or plain tibble
#                    (CC). For mids, includes .imp == 0 observed-data slice.
#       - m        : number of imputed datasets, or NULL for CC.
#       - is_mice  : logical.
#
#   create_lags(df, pt_col, time_col)
#       Adds lagged versions of all non-key columns.
# =============================================================================


#' Normalise the `data` argument of `apply_exclusions()`.
#'
#' @param data Either a plain data frame / tibble (CC route) or a `mids`
#'   object (MICE route) as returned by `build_analysis_dataset()`.
#'   The mids is expected to be in long format (one row per pt x visit),
#'   produced by `mice::as.mids()` at the end of the derivation pipeline.
#'
#' @return Named list: `long`, `m`, `is_mice`.
normalise_exclusion_input <- function(data) {
    
    # ── mids object (MICE route) ──────────────────────────────────────────────
    if (inherits(data, "mids")) {
        long <- mice::complete(data, action = "long", include = TRUE) |>
            tibble::as_tibble() |>
            dplyr::mutate(dplyr::across(where(is.list),
                                        ~ unlist(.x, use.names = FALSE)))
        return(list(
            long    = long,
            m       = data$m,
            is_mice = TRUE
        ))
    }
    
    # ── Plain data frame (CC route) ───────────────────────────────────────────
    if (is.data.frame(data)) {
        return(list(
            long    = tibble::as_tibble(data),
            m       = NULL,
            is_mice = FALSE
        ))
    }
    
    cli::cli_abort(c(
        "x" = "{.fn apply_exclusions}: unrecognised {.arg data} type: \\
               {.cls {class(data)}}.",
        "i" = "Supply a plain data frame (CC route) or a {.cls mids} object \\
               (MICE route) from {.fn build_analysis_dataset}."
    ))
}


#' Add lagged versions of all non-key columns.
#'
#' Used for the `gait_speed` outcome, where exposure and covariates must
#' reflect the *previous* visit.
#'
#' @param df       Data frame to lag.
#' @param pt_col   Participant ID column name.
#' @param time_col Visit/time column name.
#' @return `df` with `{col}_lag` columns appended for every non-key column.
create_lags <- function(df,
                        pt_col   = "pt",
                        time_col = "time_point") {

    exclude_cols <- c(".imp", ".id", pt_col, time_col, "exam_date", "gait_speed")
    lag_cols     <- setdiff(names(df), exclude_cols)

    # Group by .imp when present so each imputation's lags are computed
    # independently — without this, lag() would bleed across imputations.
    group_cols <- intersect(c(".imp", pt_col), names(df))

    df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
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