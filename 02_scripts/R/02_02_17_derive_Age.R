# =============================================================================
# R/02_02_17_derive_Age.R
# =============================================================================
# Derives Age (decimal years) from datbirth and exam_date_iso. Defines one
# function: derive_age().
#
#   Age = (exam_date_iso - datbirth) / 365.25
#
# datbirth is only recorded on Baseline rows; it is carried forward to all
# visits for each participant before computing age.
#
# The pre-existing Age column (if present) is renamed to Age_source for
# audit purposes before the derived value is written.
#
# Both datbirth and exam_date_iso are expected to be Date class (or
# ISO-8601 character strings, which are coerced automatically).
# =============================================================================

# -----------------------------------------------------------------------------
# derive_age()
# -----------------------------------------------------------------------------
#' Derive Age (decimal years) for a CoLaus long tibble.
#'
#' Fills datbirth from Baseline rows down to all visits per participant,
#' renames any existing Age column to Age_source, then computes decimal age
#' as (exam_date_iso - datbirth) / 365.25. Provides a summary including
#' agreement with Age_source values where both are present.
#'
#' @param df  CoLaus long tibble after harmonisation and stacking.
#'            Must contain columns: id, visit, datbirth, exam_date_iso.
#' @return df with Age_source (original, if present) and Age (derived) columns.
derive_age <- function(df) {
    
    # ── Check required columns -----------------------------------------------
    required_cols <- c("pt", ".visit", "datbirth", "exam_date_iso")
    missing_cols  <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_age: missing required columns: {.val {missing_cols}}. \\
             Age will not be derived."
        )
        return(df)
    }
    
    # ── Coerce dates ---------------------------------------------------------
    df <- df |>
        dplyr::mutate(
            datbirth     = as.Date(datbirth),
            exam_date_iso = as.Date(exam_date_iso)
        )
    
    # ── Carry datbirth forward from Baseline to all visits -------------------
    # datbirth is only populated on Baseline rows; fill within each participant.
    n_baseline_dob <- df |>
        dplyr::filter(.visit == "Baseline", !is.na(datbirth)) |>
        nrow()
    
    # Zero-out datbirth on non-Baseline rows so only the recorded Baseline value
    # is used as the fill source, then propagate down (later visits) and up
    # (in case Baseline is not the first row after sorting).
    df <- df |>
        dplyr::mutate(
            datbirth = dplyr::if_else(.visit == "Baseline", datbirth, NA_Date_)
        ) |>
        dplyr::group_by(pt) |>
        tidyr::fill(datbirth, .direction = "downup") |>
        dplyr::ungroup()
    
    n_filled_dob <- sum(!is.na(df$datbirth))
    
    # ── Rename existing Age column -------------------------------------------
    has_source_age <- "Age" %in% names(df)
    if (has_source_age) {
        df <- df |> dplyr::rename(Age_source = Age)
    }
    
    # ── Derive Age (decimal years) -------------------------------------------
    df <- df |>
        dplyr::mutate(
            Age = dplyr::if_else(
                !is.na(datbirth) & !is.na(exam_date_iso),
                round(as.numeric(exam_date_iso - datbirth) / 365.25, 1),
                NA_real_
            )
        )
    
    # ── Summary --------------------------------------------------------------
    cli::cli_h2("Derive Age")
    
    n_rows    <- nrow(df)
    n_derived <- sum(!is.na(df$Age))
    n_missing <- n_rows - n_derived
    
    cli::cli_inform(c(
        "v" = "Age derived from datbirth (Baseline, filled forward/back) and exam_date_iso.",
        "i" = "Total rows: {n_rows} | derived: {n_derived} | missing: {n_missing}",
        "i" = "Age range (non-missing): {round(min(df$Age, na.rm = TRUE), 1)} – \\
               {round(max(df$Age, na.rm = TRUE), 1)} years",
        "i" = "Mean ± SD: {round(mean(df$Age, na.rm = TRUE), 1)} ± \\
               {round(sd(df$Age, na.rm = TRUE), 1)} years"
    ))
    
    # ── Agreement with Age_source --------------------------------------------
    if (has_source_age) {
        both   <- !is.na(df$Age) & !is.na(df$Age_source)
        n_both <- sum(both)
        
        if (n_both == 0) {
            cli::cli_inform(c("i" = "Age_source exists but no rows have both Age and Age_source non-missing; skipping agreement check."))
        } else {
            diff <- abs(df$Age[both] - df$Age_source[both])
            
            n_exact  <- sum(diff < 0.1)
            n_close  <- sum(diff >= 0.1 & diff < 1.0)
            n_differ <- sum(diff >= 1.0)
            
            cli::cli_inform(c(
                "i" = "Agreement with Age_source (rows with both non-missing, n = {n_both}):",
                "*" = "Difference < 0.1 yr  (effectively identical): {n_exact} ({round(n_exact / n_both * 100, 1)}%)",
                "*" = "Difference 0.1–1 yr  (minor rounding/timing): {n_close} ({round(n_close / n_both * 100, 1)}%)",
                "*" = "Difference >= 1 yr   (meaningful mismatch)  : {n_differ} ({round(n_differ / n_both * 100, 1)}%)",
                "i" = "Median absolute difference: {round(median(diff), 3)} years",
                "i" = "Max absolute difference   : {round(max(diff), 3)} years"
            ))
        }
    } else {
        cli::cli_inform(c("i" = "No pre-existing Age column found; no agreement check performed."))
    }
    
    return(df)
}