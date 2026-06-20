# =============================================================================
# R/03_03_shared_exclusions.R
# =============================================================================
# Outcome-agnostic participant-level exclusions applied once before the
# per-outcome loop.
#
# MICE note
# ---------
# Both functions here operate on the OBSERVED data slice (.imp == 0 or plain
# CC data). They return participant IDs to keep/exclude. Those IDs are then
# applied identically across all imputed datasets — ensuring that shared
# exclusion decisions are not re-made per imputation (which would produce
# inconsistent CONSORT counts).
#
# Functions
# ---------
#   exclude_missing_exposure(df, pt_col, exposure)
#   compute_valid_visit_participants(df, pt_col, visit_col, min_visits)
# =============================================================================


#' Exclude participants who never have an observed exposure value.
#'
#' A participant is retained only if they have at least one non-NA value of
#' `exposure` across all their visits. This is evaluated on the observed-data
#' slice only; imputed exposure values do not count.
#'
#' @param df       Data frame (should be .imp == 0 or CC data).
#' @param pt_col   Participant ID column name.
#' @param exposure Column name of the exposure variable.
#'
#' @return List with `$data` (filtered) and `$excl` (excluded participant IDs
#'   with exclusion metadata).
exclude_missing_exposure <- function(df, pt_col, exposure) {
    
    keep <- df |>
        dplyr::group_by(.data[[pt_col]]) |>
        dplyr::summarise(any_obs = any(!is.na(.data[[exposure]])),
                         .groups = "drop") |>
        dplyr::filter(any_obs) |>
        dplyr::pull(.data[[pt_col]])
    
    list(
        data = dplyr::filter(df, .data[[pt_col]] %in% keep),
        excl = dplyr::filter(df, !(.data[[pt_col]] %in% keep)) |>
            dplyr::distinct(.data[[pt_col]]) |>
            dplyr::mutate(
                exclusion_stage  = "exposure_missing",
                exclusion_reason = paste0("no_", exposure),
                exclusion_detail = "never observed"
            )
    )
}


#' Exclude participants with fewer than `min_visits` observed visits.
#'
#' Visit counts are computed from distinct (pt, visit) combinations, so this
#' is robust to long format with multiple rows per visit (e.g. from MICE).
#' Should always be called on the observed-data slice (.imp == 0 or CC data).
#'
#' @param df         Data frame.
#' @param pt_col     Participant ID column name.
#' @param visit_col  Visit / time-point column name.
#' @param min_visits Minimum number of visits required to be retained.
#'
#' @return List with `$data` (filtered) and `$excl` (excluded participant IDs
#'   with visit count and exclusion metadata).
compute_valid_visit_participants <- function(df, pt_col, visit_col, min_visits) {
    
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
                exclusion_stage  = "visit_min",
                exclusion_reason = "too_few_observed_visits",
                exclusion_detail = paste0("n<", min_visits)
            )
    )
}