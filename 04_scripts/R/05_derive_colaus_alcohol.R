# =============================================================================
# R/derive_colaus_alcohol.R
# =============================================================================
# Derives alcohol_category from conso_hebdo (units/week) or sumalco (g/week).
#
# conso_hebdo is the primary source. sumalco is used as fallback where
# conso_hebdo is missing.
#
# Thresholds are applied on a per-weekly basis:
#   0 units/week (0 g/day)              -> 1 = Non-drinker
#   ≥1 to 3 units/week (>0–6 g/day)     -> 2 = Light
#   ≥4 to 7 units/week (>6–12 g/day)    -> 3 = Moderate
#   >7 units/week (>12 g/day)           -> 4 = Heavy
#
# The g/day thresholds are derived from the Swiss national guidelines defining a standard drink
# as containing approximately 10-12 g of pure ethanol.
# The conversion from units/week to g/week uses a standard value of 10 g/unit, the lower end of the typical range.
# 
#
# Validation: alcool4 (0=Non-drinker, 1=Drinker) is used to cross-check.
# A warning is raised when alcohol_category == 1 but alcool4 == "Yes", or
# when alcohol_category > 1 but alcool4 == "No".
#
# =============================================================================


# -----------------------------------------------------------------------------
# Internal helper: compare conso_hebdo vs sumalco_units classification
# -----------------------------------------------------------------------------

#' Compare alcohol category agreement between conso_hebdo and sumalco.
#'
#' Applies the same categorisation thresholds to sumalco_units independently
#' of conso_hebdo, then cross-tabulates the two classifications among rows
#' where both sources are non-missing. Results are emitted as cli messages.
#'
#' @param df   Data frame after sumalco_units and alcohol_category are added.
#' @param g_per_unit Grams of ethanol per unit (same as passed to derive_alcohol).
#' @return Invisibly NULL. Called for side effects only.
.validate_source_agreement <- function(df, g_per_unit) {
    
    # Only run when both sources are present in the data
    if (!all(c("conso_hebdo", "sumalco_units") %in% names(df))) return(invisible(NULL))
    
    # Rows where both conso_hebdo and sumalco are non-missing
    both <- dplyr::filter(df, !is.na(conso_hebdo), !is.na(sumalco_units))
    n_both <- nrow(both)
    
    if (n_both == 0) {
        cli::cli_inform(c("i" = "derive_alcohol: no rows with both conso_hebdo and sumalco available — source agreement check skipped."))
        return(invisible(NULL))
    }
    
    # Classify sumalco_units independently using the same thresholds
    cat_labels <- c("Non-drinker", "Light", "Moderate", "Heavy")
    
    both <- dplyr::mutate(both,
                          cat_conso = dplyr::case_when(
                              conso_hebdo == 0  ~ 1L,
                              conso_hebdo <= 3  ~ 2L,
                              conso_hebdo <= 7  ~ 3L,
                              conso_hebdo >  7  ~ 4L
                          ) |> factor(levels = 1:4, labels = cat_labels, ordered = TRUE),
                          
                          cat_sumalco = dplyr::case_when(
                              sumalco_units == 0  ~ 1L,
                              sumalco_units <= 3  ~ 2L,
                              sumalco_units <= 7  ~ 3L,
                              sumalco_units >  7  ~ 4L
                          ) |> factor(levels = 1:4, labels = cat_labels, ordered = TRUE)
    )
    
    n_agree    <- sum(both$cat_conso == both$cat_sumalco, na.rm = TRUE)
    n_disagree <- sum(both$cat_conso != both$cat_sumalco, na.rm = TRUE)
    pct_agree  <- round(n_agree / n_both * 100, 1)
    
    cli::cli_inform(c(
        "i" = "derive_alcohol: source agreement (conso_hebdo vs sumalco_units) among {n_both} rows with both available:",
        "*" = "{n_agree} / {n_both} ({pct_agree}%) classify to the same level.",
        "*" = "{n_disagree} / {n_both} ({round(100 - pct_agree, 1)}%) differ."
    ))
    
    if (n_disagree > 0) {
        # Cross-tabulation of discordant pairs
        discord <- both |>
            dplyr::filter(cat_conso != cat_sumalco) |>
            dplyr::count(cat_conso, cat_sumalco, name = "n") |>
            dplyr::arrange(dplyr::desc(n)) |>
            dplyr::mutate(
                direction = dplyr::case_when(
                    as.integer(cat_sumalco) > as.integer(cat_conso) ~
                        "sumalco higher",
                    as.integer(cat_sumalco) < as.integer(cat_conso) ~
                        "sumalco lower",
                    TRUE ~ "same"
                ),
                label = glue::glue(
                    "{cat_conso} (conso) vs {cat_sumalco} (sumalco): n = {n}"
                )
            )
        
        n_sumalco_higher <- sum(discord$n[discord$direction == "sumalco higher"])
        n_sumalco_lower  <- sum(discord$n[discord$direction == "sumalco lower"])
        
        cli::cli_inform(c(
            "i" = "derive_alcohol: discordant classification breakdown:",
            "*" = "sumalco classifies higher than conso_hebdo: {n_sumalco_higher} row(s)",
            "*" = "sumalco classifies lower  than conso_hebdo: {n_sumalco_lower} row(s)",
            "*" = paste(discord$label, collapse = "\n")
        ))
    }
    
    invisible(NULL)
}

#' Derive alcohol_category for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with alcohol_units_week (numeric) and alcohol_category (ordered factor) added.
derive_alcohol <- function(df, g_per_unit = 10) {
    
    df <- dplyr::mutate(df,
                        
                        # Convert sumalco (g/week) → units/week, then unify with conso_hebdo (units/week)
                        sumalco_units      = sumalco / g_per_unit,
                        alcohol_units_week = dplyr::coalesce(conso_hebdo, sumalco_units),
                        
                        alcohol_category = dplyr::case_when(
                            is.na(alcohol_units_week) ~ NA_integer_,
                            alcohol_units_week == 0   ~ 1L,
                            alcohol_units_week <= 3   ~ 2L,
                            alcohol_units_week <= 7   ~ 3L,
                            alcohol_units_week >  7   ~ 4L
                        ) |>
                            factor(
                                levels  = 1:4,
                                labels  = c("Non-drinker", "Light", "Moderate", "Heavy"),
                                ordered = TRUE
                            )
    )
    
    # ── Validation: conso_hebdo vs sumalco_units classification agreement ────
    # Among rows where both sources are available, compare how often they
    # assign the same alcohol_category level. Discordance is expected at
    # category boundaries (sumalco uses a 10 g/unit conversion which may
    # differ from the actual drink sizes recorded in conso_hebdo).
    .validate_source_agreement(df, g_per_unit)
    
    # ── Validation against alcool4 ──────────────────────────────────────────
    if ("alcool4" %in% names(df)) {
        n_mismatch_nondrinker <- sum(
            !is.na(df$alcohol_category) & !is.na(df$alcool4) &
                df$alcohol_category == "Non-drinker" & df$alcool4 == "Yes",
            na.rm = TRUE
        )
        n_mismatch_drinker <- sum(
            !is.na(df$alcohol_category) & !is.na(df$alcool4) &
                df$alcohol_category != "Non-drinker" & df$alcool4 == "No",
            na.rm = TRUE
        )
        if (n_mismatch_nondrinker > 0)
            cli::cli_warn(
                "derive_alcohol: {n_mismatch_nondrinker} row(s) classified as \\
         Non-drinker by units/week but {.col alcool4} = Yes."
            )
        if (n_mismatch_drinker > 0)
            cli::cli_warn(
                "derive_alcohol: {n_mismatch_drinker} row(s) classified as drinker \\
         by units/week but {.col alcool4} = No."
            )
    }
    
    return(df)
}