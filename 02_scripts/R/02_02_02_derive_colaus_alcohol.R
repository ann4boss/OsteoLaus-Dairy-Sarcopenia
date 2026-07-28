# =============================================================================
# R/02_02_02_derive_colaus_alcohol.R
# =============================================================================
# Derives alcohol_category from conso_hebdo (units/week) and from sumalco (grams/day)
# for sensitivity analysis. Defines one function: derive_alcohol().
#
# conso_hebdo is the primary source. sumalco is used as comparison.
#
# Thresholds are applied on a per-weekly basis:
#   0 units/week (0 g/day)              -> 0 = Non-drinker
#   ≥1 to 3 units/week (>0–6 g/day)     -> 1 = Light
#   ≥4 to 7 units/week (>6–12 g/day)    -> 2 = Moderate
#   >7 units/week (>12 g/day)           -> 3 = Heavy
#
# The g/day thresholds are derived from the Swiss national guidelines defining
# a standard drink as containing approximately 10 g of pure ethanol.
# The conversion from units/week to g/week uses a standard value of 10 g/unit,
# the lower end of the typical range.
# =============================================================================

# -----------------------------------------------------------------------------
# derive_alcohol()
# -----------------------------------------------------------------------------
#' Derive alcohol_category for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @param g_per_unit Grams of ethanol per standard drink unit. Default 10.
#' @return df with sumalco_units, alcohol_category_conso,
#'   alcohol_category_sumalco, and alcohol_agreement.
derive_alcohol <- function(df, g_per_unit = 10) {
    
    # ── Ensure source columns are present ------------------------------------
    required_cols <- c("conso_hebdo", "sumalco")
    missing_cols  <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_alcohol: missing required columns: {.val {missing_cols}}. \\
             Alcohol category will not be derived."
        )
        return(df)
    }
    
    n_rows <- nrow(df)
    
    # ── Calculate units and categories ---------------------------------------
    df <- df |>
        dplyr::mutate(
            # Convert sumalco (g/day) → units/week
            sumalco_units = (sumalco * 7) / g_per_unit,
            
            # Category from conso_hebdo
            alcohol_category_conso = dplyr::case_when(
                is.na(conso_hebdo) ~ NA_integer_,
                conso_hebdo == 0   ~ 0L,
                conso_hebdo <= 3   ~ 1L,
                conso_hebdo <= 7   ~ 2L,
                conso_hebdo >  7   ~ 3L
            ) |> factor(
                levels  = 0:3,
                labels  = c("Non-drinker", "Light", "Moderate", "Heavy"),
                ordered = TRUE
            ),
            
            # Category from sumalco
            alcohol_category_sumalco = dplyr::case_when(
                is.na(sumalco_units) ~ NA_integer_,
                sumalco_units == 0   ~ 0L,
                sumalco_units <= 3   ~ 1L,
                sumalco_units <= 7   ~ 2L,
                sumalco_units >  7   ~ 3L
            ) |> factor(
                levels  = 0:3,
                labels  = c("Non-drinker", "Light", "Moderate", "Heavy"),
                ordered = TRUE
            )
        )
    
    # ── Agreement between the two sources -----------------------------------
    df <- df |>
        dplyr::mutate(
            alcohol_agreement = dplyr::case_when(
                is.na(alcohol_category_conso) | is.na(alcohol_category_sumalco) ~ NA_character_,
                alcohol_category_conso == alcohol_category_sumalco              ~ "Agree",
                alcohol_category_conso >  alcohol_category_sumalco              ~ "Conso higher",
                alcohol_category_conso <  alcohol_category_sumalco              ~ "Sumalco higher"
            ) |> factor(levels = c("Agree", "Conso higher", "Sumalco higher"))
        )
    
    # ── Relocate derived variables -------------------------------------------
    df <- df |>
        dplyr::relocate(
            conso_hebdo, sumalco, sumalco_units,
            alcohol_category_conso,
            alcohol_category_sumalco,
            alcohol_agreement,
            .after = dplyr::last_col()
        )
    
    # ── Summaries -----------------------------------------------------------
    cli::cli_h2("Derive Alcohol Category")
    
    # Coverage
    n_conso_present   <- sum(!is.na(df$conso_hebdo))
    n_sumalco_present <- sum(!is.na(df$sumalco))
    n_derived_conso    <- sum(!is.na(df$alcohol_category_conso))
    n_derived_sumalco  <- sum(!is.na(df$alcohol_category_sumalco))
    
    n_missing_conso    <- n_rows - n_derived_conso
    n_missing_sumalco  <- n_rows - n_derived_sumalco
    
    cli::cli_inform(c(
        "i" = "Total rows: {n_rows}",
        
        "*" = "conso_hebdo present (primary source): {n_conso_present} ({round(n_conso_present / n_rows * 100, 1)}%)",
        "*" = "sumalco present (comparison source) : {n_sumalco_present} ({round(n_sumalco_present / n_rows * 100, 1)}%)",
        
        "v" = "alcohol_category_conso derived   : {n_derived_conso} ({round(n_derived_conso / n_rows * 100, 1)}%)",
        "v" = "alcohol_category_sumalco derived: {n_derived_sumalco} ({round(n_derived_sumalco / n_rows * 100, 1)}%)"
    ))
    
    # Distribution of final category
    dist_conso <- df |>
        dplyr::count(alcohol_category_conso, .drop = FALSE) |>
        dplyr::mutate(
            pct = round(n / n_derived_conso * 100, 1)
        )
    
    dist_sumalco <- df |>
        dplyr::count(alcohol_category_sumalco, .drop = FALSE) |>
        dplyr::mutate(
            pct = round(n / n_derived_sumalco * 100, 1)
        )
    
    cli::cli_inform(c(
        "i" = "Distribution of alcohol_category_conso:"
    ))
    cli::cli_inform(
        paste(capture.output(print(dist_conso, n = Inf)), collapse = "\n")
    )
    
    cli::cli_inform(c(
        "i" = "Distribution of alcohol_category_sumalco:"
    ))
    cli::cli_inform(
        paste(capture.output(print(dist_sumalco, n = Inf)), collapse = "\n")
    )
    
    # Agreement between sources
    diag <- df |>
        dplyr::filter(!is.na(alcohol_agreement)) |>
        dplyr::summarise(
            total            = dplyr::n(),
            n_agree          = sum(alcohol_agreement == "Agree"),
            n_disagree       = sum(alcohol_agreement != "Agree"),
            n_conso_higher   = sum(alcohol_agreement == "Conso higher"),
            n_sumalco_higher = sum(alcohol_agreement == "Sumalco higher")
        )
    
    cli::cli_inform(c(
        "i" = "Source agreement (rows where both present, n = {diag$total}):",
        "*" = "Agree         : {diag$n_agree} ({round(diag$n_agree / diag$total * 100, 1)}%)",
        "*" = "Conso higher  : {diag$n_conso_higher} ({round(diag$n_conso_higher / diag$total * 100, 1)}%)",
        "*" = "Sumalco higher: {diag$n_sumalco_higher} ({round(diag$n_sumalco_higher / diag$total * 100, 1)}%)"
    ))
    
    if (diag$n_disagree > 0) {
        pairings <- df |>
            dplyr::filter(!is.na(alcohol_agreement), alcohol_agreement != "Agree") |>
            dplyr::count(alcohol_category_conso, alcohol_category_sumalco) |>
            dplyr::mutate(
                msg = glue::glue(
                    "conso: {alcohol_category_conso} / sumalco: {alcohol_category_sumalco} (n = {n})"
                )
            )
        cli::cli_inform("  Breakdown of mismatches:")
        cli::cli_li(pairings$msg)
    }
    
    return(df)
}