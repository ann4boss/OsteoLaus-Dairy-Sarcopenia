# =============================================================================
# R/impute_age.R
# =============================================================================
# Imputes Age for visits where it is missing but exam_date_iso is known,
# using a (Age, exam_date_iso) anchor from another visit of the same subject.
#
# Formula:
#   Age_imp = ref_Age + as.numeric(exam_date_iso - ref_date) / 365.25
#
# Reference-visit selection (within subject, across all other visits):
#   1. Smallest |exam_date_iso - ref_date|  -- minimises accumulated error.
#   2. Earliest ref_date                    -- tie-break.
#
# =============================================================================


# -----------------------------------------------------------------------------
# Helper
# -----------------------------------------------------------------------------

#' Select the best anchor visit for each subject x visit needing Age imputation.
#'
#' @param needs_imp Tibble of rows where Age is NA and exam_date_iso is not NA.
#' @param ref       Tibble of anchor rows: pt, ref_visit, ref_Age, ref_date.
#'
#' @return Tibble with columns `pt`, `.visit`, `Age_imp`.
.best_anchor_age <- function(needs_imp, ref) {
    needs_imp |>
        # Attach every anchor available for the same subject
        dplyr::left_join(ref, by = "pt", relationship = "many-to-many") |>
        
        # Drop self: target visit cannot serve as its own anchor
        # (guard only -- ref already requires non-NA Age)
        dplyr::filter(.visit != ref_visit) |>
        
        # Rank anchors by closeness in exam date, then by chronological order
        dplyr::mutate(date_diff = abs(as.numeric(exam_date_iso - ref_date))) |>
        dplyr::arrange(
            dplyr::across(dplyr::all_of("pt")), .visit, date_diff, ref_date
        ) |>
        dplyr::group_by(dplyr::across(dplyr::all_of("pt")), .visit) |>
        dplyr::slice(1L) |>
        dplyr::ungroup() |>
        
        # Impute: shift anchor age by the date difference converted to years
        dplyr::mutate(
            Age_imp = ref_Age + as.numeric(exam_date_iso - ref_date) / 365.25
        ) |>
        dplyr::select("pt", .visit, Age_imp)
}


# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------

#' Impute missing Age values using cross-visit exam-date anchoring.
#'
#' For each row where \code{Age} is \code{NA} and \code{exam_date_iso} is
#' available, the function locates the closest (by exam date) other visit of
#' the same subject that has both \code{Age} and \code{exam_date_iso}, then
#' back- or forward-calculates the missing age. Rows that already have an Age,
#' or that cannot be anchored, are left unchanged in \code{Age_imp}.
#'
#' @param df Combined tibble spanning all visits (output of a targets
#'   binding step), containing at minimum: \code{pt}, \code{.visit},
#'   \code{Age} (decimal years, numeric or \code{NA}), and
#'   \code{exam_date_iso} (Date).
#'
#' @return The input tibble with one additional column \code{Age_imp}
#'   (numeric). Non-imputable rows carry \code{NA_real_}.
impute_age <- function(df) {
    
    # ── Ensure source columns are present -------------------------------------
    required_cols <- c("pt", ".visit", "Age", "exam_date_iso")
    missing_cols  <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0L) {
        cli::cli_inform(
            "impute_age: missing required columns: {.val {missing_cols}}. \\
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
    # Requirement: Age is NA, exam_date_iso is available (needed for arithmetic).
    needs_imp    <- dplyr::filter(df, is.na(Age), !is.na(exam_date_iso))
    n_candidates <- nrow(needs_imp)
    
    # ── Early exit if nothing to do -------------------------------------------
    if (n_candidates == 0L) {
        message(
            "impute_age: no missing Age rows with available exam_date_iso -- ",
            "nothing to impute."
        )
        return(dplyr::mutate(df, Age_imp = Age))
    }
    
    # ── Compute imputed ages via best anchor ----------------------------------
    imputed <- .best_anchor_age(needs_imp, ref)
    
    # ── Merge imputed ages back and resolve final column ----------------------
    out <- df |>
        dplyr::left_join(imputed, by = c("pt", ".visit")) |>
        dplyr::mutate(
            Age_imp = dplyr::case_when(
                !is.na(Age)     ~ Age,       # observed: keep as-is
                !is.na(Age_imp) ~ Age_imp,   # successfully imputed
                TRUE            ~ NA_real_   # no anchor found
            )
        )
    
    # ── Diagnostics -----------------------------------------------------------
    n_imputed  <- sum(!is.na(out$Age_imp) & is.na(df$Age))
    n_still_na <- sum(is.na(out$Age_imp))
    
    message(sprintf(
        paste0(
            "\nimpute_age\n",
            "  Candidates (NA Age, known date) : %d\n",
            "  Successfully imputed            : %d\n",
            "  Still NA in Age_imp             : %d"
        ),
        n_candidates, n_imputed, n_still_na
    ))
    
    if (n_imputed > 0L) {
        preview <- out |>
            dplyr::filter(is.na(Age), !is.na(Age_imp)) |>
            dplyr::select("pt", .visit, exam_date_iso, Age, Age_imp) |>
            head(10L)
        message("\nSample of imputed rows:")
        print(preview)
    }
    
    return(out)
}