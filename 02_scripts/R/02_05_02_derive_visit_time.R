# =============================================================================
# R/02_05_02_derive_visit_time.R
# =============================================================================
# Derives visit number, time since baseline (years), and baseline age for a
# stacked long-format cohort tibble. Defines one function: derive_visit_time().
# =============================================================================

# -----------------------------------------------------------------------------
# derive_visit_time()
# -----------------------------------------------------------------------------
#' Derive visit number, time since baseline, and baseline age.
#'
#' Within each participant (grouped by \code{id_var}), sorts rows by exam date,
#' numbers visits sequentially starting at 1, and computes time since the
#' participant's first (baseline) exam in years (365.25-day years). The
#' participant's baseline age is carried onto every row.
#'
#' @param df               Long-format tibble with one row per participant-visit.
#' @param id_var           Column name identifying the participant.
#' @param date_var         Column name holding the exam date (coerced to Date).
#' @param age_var          Column name holding age at each visit.
#' @param visit_var        Output column name for the derived visit number.
#' @param time_var         Output column name for time since baseline (years).
#' @param age_baseline_var Output column name for age at baseline.
#' @return \code{df} with \code{visit_var}, \code{time_var}, and
#'   \code{age_baseline_var} added; relocated after \code{.visit_colaus} when
#'   that column is present.
derive_visit_time <- function(df,
                              id_var = "pt",
                              date_var = "exam_date",
                              age_var = "Age",
                              visit_var = "visit_num",
                              time_var = "time_since_baseline",
                              age_baseline_var = "age_at_baseline") {
    
    # ── Order visits and derive per-participant fields ------------------------
    # Sorting by date within each participant makes row_number() equivalent to
    # chronological visit order, and first() picks the baseline (earliest) row.
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

    # Drop the intermediate helper columns; only the named output columns remain.
    df <- df |> dplyr::select(-baseline_date, -baseline_age)

    # Keep new columns next to the other visit-order columns when present.
    if (".visit_colaus" %in% names(df)) {
        df <- df |>
            dplyr::relocate(
                all_of(c(visit_var, time_var, age_baseline_var)),
                .after = .visit_colaus
            )
    }
    
    df
}