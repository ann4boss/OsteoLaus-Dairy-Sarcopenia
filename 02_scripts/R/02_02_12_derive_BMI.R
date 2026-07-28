# =============================================================================
# R/02_02_12_derive_BMI.R
# =============================================================================
# Derives BMI from Weight (kg) and Height (cm) using the standard formula.
# Defines one function: derive_bmi().
#
#   Formula:
#
#   BMI = Weight / (Height / 100)^2
#
# The pre-existing BMI column (if present) is renamed to BMI_source for
# audit purposes before the derived value is written.
#
# Weight is in kg and Height is in cm (as harmonised by harmonise_colaus()).
# =============================================================================

# -----------------------------------------------------------------------------
# derive_bmi()
# -----------------------------------------------------------------------------
#' Derive BMI for a CoLaus long tibble.
#'
#' Renames any existing BMI column to BMI_source, then computes BMI from
#' Weight (kg) and Height (cm). Provides a brief summary including agreement
#' with the original BMI_source values.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with BMI_source (original, if present) and BMI (derived) columns.
derive_bmi <- function(df) {
    
    # ── Check required columns -----------------------------------------------
    required_cols <- c("Weight", "Height")
    missing_cols  <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_bmi: missing required columns: {.val {missing_cols}}. \\
             BMI will not be derived."
        )
        return(df)
    }
    
    # ── Rename existing BMI column -------------------------------------------
    has_source_bmi <- "BMI" %in% names(df)
    if (has_source_bmi) {
        df <- df |> dplyr::rename(BMI_source = BMI)
    }
    
    # ── Derive BMI -----------------------------------------------------------
    df <- df |>
        dplyr::mutate(
            BMI = dplyr::if_else(
                !is.na(Weight) & !is.na(Height) & Height > 0,
                round(Weight / (Height / 100)^2, 2),
                NA_real_
            )
        )
    
    # ── Summary --------------------------------------------------------------
    cli::cli_h2("Derive BMI")
    
    n_rows    <- nrow(df)
    n_derived <- sum(!is.na(df$BMI))
    n_missing <- n_rows - n_derived
    
    cli::cli_inform(c(
        "v" = "BMI derived from Weight and Height.",
        "i" = "Total rows: {n_rows} | derived: {n_derived} | missing: {n_missing}",
        "i" = "BMI range (non-missing): {round(min(df$BMI, na.rm = TRUE), 1)} – \\
               {round(max(df$BMI, na.rm = TRUE), 1)} kg/m²",
        "i" = "Mean ± SD: {round(mean(df$BMI, na.rm = TRUE), 1)} ± \\
               {round(sd(df$BMI, na.rm = TRUE), 1)} kg/m²"
    ))
    
    # ── Agreement with BMI_source --------------------------------------------
    if (has_source_bmi) {
        both   <- !is.na(df$BMI) & !is.na(df$BMI_source)
        n_both <- sum(both)
        diff   <- abs(df$BMI[both] - df$BMI_source[both])
        
        n_exact  <- sum(diff < 0.01)
        n_close  <- sum(diff >= 0.01 & diff < 0.5)
        n_differ <- sum(diff >= 0.5)
        
        cli::cli_inform(c(
            "i" = "Agreement with BMI_source (rows with both non-missing, n = {n_both}):",
            "*" = "Difference < 0.01 (effectively identical): \\
                   {n_exact} ({round(n_exact / n_both * 100, 1)}%)",
            "*" = "Difference 0.01–0.5 (minor rounding):     \\
                   {n_close} ({round(n_close / n_both * 100, 1)}%)",
            "*" = "Difference >= 0.5  (meaningful mismatch): \\
                   {n_differ} ({round(n_differ / n_both * 100, 1)}%)",
            "i" = "Median absolute difference: {round(median(diff), 3)} kg/m²",
            "i" = "Max absolute difference   : {round(max(diff), 3)} kg/m²"
        ))
        
        if (n_differ > 0) {
            cli::cli_inform(
                "derive_bmi: {n_differ} row(s) show a BMI difference >= 0.5 kg/m² \\
                 between derived and source values. Inspect BMI_source for data issues."
            )
        }
    } else {
        cli::cli_inform(c("i" = "No pre-existing BMI column found; no agreement check performed."))
    }
    
    return(df)
}