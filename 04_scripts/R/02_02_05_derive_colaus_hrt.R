# =============================================================================
# R/derive_colaus_hrt.R
# =============================================================================
# Derives Hormone Replacement Therapy (HRT) hrt_status from esthrp and esthrpage.
#
# Source variables:
#   esthrp    — currently on HRT (0=No, 1=Yes, 9=Does not know → NA)
#   esthrpage — age at HRT start (99 = Does not know → NA)
#
# Derivation:
#   hrt_status = "Current HRT"         if esthrp == "Yes"
#   hrt_status = "Past HRT"            if esthrp == "No" and esthrpage is
#                                          non-missing and non-sentinel
#   hrt_status = "Never / Not current" if esthrp == "No" and no past-use
#                                          evidence
#   hrt_status = NA                    when esthrp is NA
#
# =============================================================================

#' Derive Hormone Replacement Therapy (HRT) status for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with hrt_status (factor) added.
derive_hrt <- function(df) {
    
    # ── Check required columns -----------------------------------------------
    required_cols <- c("esthrp", "esthrpage")
    missing_cols  <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_hrt: missing required columns: {.val {missing_cols}}. \\
             {.col hrt_status} will not be derived."
        )
        return(df)
    }
    
    # ── Main derivation ------------------------------------------------------
    df <- df |>
        dplyr::mutate(
            hrt_status = dplyr::case_when(
                is.na(esthrp)                      ~ NA_character_,
                esthrp == "Yes"                    ~ "Current HRT",
                esthrp == "No" & !is.na(esthrpage) ~ "Past HRT",
                esthrp == "No"                     ~ "Never / Not current",
                TRUE                               ~ NA_character_
            ) |>
                factor(levels = c("Never / Not current", "Past HRT", "Current HRT"))
        )
    
    # ── Summary --------------------------------------------------------------
    cli::cli_h2("Derive HRT Status")
    
    n_rows    <- nrow(df)
    n_derived <- sum(!is.na(df$hrt_status))
    n_missing <- n_rows - n_derived
    
    dist <- df |>
        dplyr::count(hrt_status, .drop = FALSE) |>
        dplyr::mutate(pct = round(n / n_derived * 100, 1))
    
    cli::cli_inform(c(
        "v" = "hrt_status derived.",
        "i" = "Total rows: {n_rows} | derived: {n_derived} | missing (esthrp NA): {n_missing}",
        "i" = "Distribution (among derived rows):"
    ))
    cli::cli_inform(paste(capture.output(print(dist, n = Inf)), collapse = "\n"))
    
    return(df)
}