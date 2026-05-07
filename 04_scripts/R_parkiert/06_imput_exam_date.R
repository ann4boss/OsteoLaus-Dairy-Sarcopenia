# =============================================================================
# R/impute_exam_date.R
# =============================================================================
# Imputes exam_date_iso for visits where the date is missing but Age is known,
# using a (Age, exam_date_iso) anchor from another visit of the same subject.
#
# Formula:
#   exam_date_iso_imp = ref_exam_date + round((Age - ref_Age) * 365.25) days
#
# Reference-visit selection (within subject, across all other visits):
#   1. Smallest |Age - ref_Age|  -- minimises accumulated rounding error.
#   2. Earliest ref_exam_date    -- tie-break.
#
# =============================================================================


# -----------------------------------------------------------------------------
# Helper
# -----------------------------------------------------------------------------

#' Select the best anchor visit for each subject x visit needing imputation.
#'
#' @param needs_imp Tibble of rows where exam_date_iso is NA and Age is not NA.
#' @param ref       Tibble of anchor rows: pt, ref_visit, ref_Age, ref_date.
#'
#' @return Tibble with columns `pt`, `.visit`, `exam_date_iso_imp`.
.best_anchor <- function(needs_imp, ref) {
    needs_imp |>
        # Attach every anchor available for the same subject
        dplyr::left_join(ref, by = "pt", relationship = "many-to-many") |>
        
        # Drop self: target visit cannot serve as its own anchor
        # (guard only -- ref already requires non-NA exam_date_iso)
        dplyr::filter(.visit != ref_visit) |>
        
        # Rank anchors by closeness in age, then by chronological order
        dplyr::mutate(age_diff = abs(Age - ref_Age)) |>
        dplyr::arrange(
            dplyr::across(dplyr::all_of("pt")), .visit, age_diff, ref_date
        ) |>
        dplyr::group_by(dplyr::across(dplyr::all_of("pt")), .visit) |>
        dplyr::slice(1L) |>
        dplyr::ungroup() |>
        
        # Impute: shift anchor date by the age difference converted to days
        dplyr::mutate(
            exam_date_iso_imp = ref_date + lubridate::days(
                round((Age - ref_Age) * 365.25)
            )
        ) |>
        dplyr::select("pt", .visit, exam_date_iso_imp)
}


# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------

#' Impute missing exam_date_iso values using cross-visit Age anchoring.
#'
#' For each row where \code{exam_date_iso} is \code{NA} and \code{Age} is
#' available, the function locates the closest (by age) other visit of the same
#' subject that has both \code{Age} and \code{exam_date_iso}, then back- or
#' forward-calculates the missing date. Rows that already have a date, or that
#' cannot be anchored, are left unchanged in \code{exam_date_iso_imp}.
#'
#' @param df Combined tibble spanning all visits (output of a targets
#'   binding step), containing at minimum: \code{pt}, \code{.visit},
#'   \code{Age} (decimal years, numeric), and \code{exam_date_iso}
#'   (Date or \code{NA}).
#'
#' @return The input tibble with one additional column \code{exam_date_iso_imp}
#'   (Date). Non-imputable rows carry \code{NA_Date_}.
impute_exam_date <- function(df) {
    
    # ── Ensure source columns are present -------------------------------------
    required_cols <- c("pt", ".visit", "Age", "exam_date_iso")
    missing_cols  <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0L) {
        cli::cli_inform(
            "impute_exam_date: missing required columns: {.val {missing_cols}}. \\
             Returning df unchanged."
        )
        return(df)
    }
    
    # ── Build anchor table ----------------------------------------------------
    # All (subject, visit) pairs with a known Age AND a known exam date.
    ref <- df |>
        dplyr::filter(!is.na(Age), !is.na(exam_date_iso)) |>
        dplyr::select(
            "pt",
            ref_visit = .visit,
            ref_Age   = Age,
            ref_date  = exam_date_iso
        )
    
    # ── Identify rows needing imputation --------------------------------------
    # Requirement: exam_date_iso is NA, Age is available (needed for arithmetic).
    needs_imp    <- dplyr::filter(df, is.na(exam_date_iso), !is.na(Age))
    n_candidates <- nrow(needs_imp)
    
    # ── Early exit if nothing to do -------------------------------------------
    if (n_candidates == 0L) {
        message(
            "impute_exam_date: no missing exam_date_iso rows with available Age -- ",
            "nothing to impute."
        )
        return(dplyr::mutate(df, exam_date_iso_imp = exam_date_iso))
    }
    
    # ── Compute imputed dates via best anchor ---------------------------------
    imputed <- .best_anchor(needs_imp, ref)
    
    # ── Merge imputed dates back and resolve final column ---------------------
    out <- df |>
        dplyr::left_join(imputed, by = c("pt", ".visit")) |>
        dplyr::mutate(
            exam_date_iso_imp = dplyr::case_when(
                !is.na(exam_date_iso)     ~ exam_date_iso,      # observed: keep as-is
                !is.na(exam_date_iso_imp) ~ exam_date_iso_imp,  # successfully imputed
                TRUE                      ~ NA_Date_             # no anchor found
            )
        )
    
    # ── Diagnostics -----------------------------------------------------------
    n_imputed  <- sum(!is.na(out$exam_date_iso_imp) & is.na(df$exam_date_iso))
    n_still_na <- sum(is.na(out$exam_date_iso_imp))
    
    message(sprintf(
        paste0(
            "\nimpute_exam_date\n",
            "  Candidates (NA date, known Age) : %d\n",
            "  Successfully imputed            : %d\n",
            "  Still NA in exam_date_iso_imp   : %d"
        ),
        n_candidates, n_imputed, n_still_na
    ))
    
    
    return(out)
}