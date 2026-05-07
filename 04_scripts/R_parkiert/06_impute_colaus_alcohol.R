# =============================================================================
# R/impute_colaus_alcohol.R
# =============================================================================
# Imputes alcohol_category for CoLaus visits where conso_hebdo was missing.
#
# Imputation hierarchy (applied in order, first non-NA wins):
#   1. sumalco-derived category
#      If conso_hebdo was NA but sumalco (g/week) is available, compute
#      sumalco_units = (sumalco * 7) / g_per_unit and apply the same
#      thresholds used in derive_alcohol():
#        0 units/week -> Non-drinker
#        1-3          -> Light
#        4-7          -> Moderate
#        >7           -> Heavy
#
#   2. Previous-visit carry-forward (LOCF)
#      If sumalco is also missing (or yields NA), carry the participant's
#      most recent non-NA alcohol_category forward to the current visit.
#      visits are visited in ascending .visit order so the carry runs
#      Baseline -> F1 -> F2 -> F3.
#
#   3. NA
#      If neither source is available, alcohol_category remains NA.
#
# Audit columns added
# -------------------
#   alcohol_impute_source  Factor indicating how each row was filled:
#     "conso_hebdo"          original derive_alcohol() value (no imputation)
#     "sumalco"              filled from sumalco in step 1
#     "locf"                 filled by previous-visit carry-forward in step 2
#     NA                     still missing after all steps
#
# =============================================================================


#' Impute alcohol_category for a CoLaus long tibble.
#'
#' @param df        CoLaus long tibble after derive_alcohol() has been applied.
#'   Must contain columns: pt, .visit, alcohol_category, sumalco.
#' @param g_per_unit Grams of ethanol per standard drink unit. Default 10.
#' @return df with alcohol_category imputed (in place) and
#'   alcohol_impute_source (factor) appended.
impute_alcohol <- function(df, g_per_unit = 10) {
    
    # TODO change this that it will not abort but return df
    # ── Guards ---------------------------------------------------------------
    if (!"alcohol_category" %in% names(df))
        cli::cli_abort("impute_alcohol(): alcohol_category not found.")
    
    for (col in c("pt", ".visit", "sumalco")) {
        if (!col %in% names(df))
            cli::cli_abort("impute_alcohol(): missing {.col {col}}.")
    }
    
    # ── Preserve original levels --------------------------------------------
    orig_levels  <- levels(df$alcohol_category)
    orig_ordered <- is.ordered(df$alcohol_category)
    
    # ── Initialise IMPUTED column -------------------------------------------
    df <- df |>
        dplyr::mutate(
            alcohol_category_imp = alcohol_category,
            alcohol_impute_source = dplyr::if_else(
                !is.na(alcohol_category),
                "conso_hebdo",
                NA_character_
            )
        )
    
    n_original_na <- sum(is.na(df$alcohol_category_imp))
    
    # ── Step 1: sumalco imputation ------------------------------------------
    df <- df |>
        dplyr::mutate(
            .sumalco_units = (sumalco * 7) / g_per_unit,
            
            .sumalco_cat = dplyr::case_when(
                is.na(.sumalco_units) ~ NA_character_,
                .sumalco_units == 0   ~ "Non-drinker",
                .sumalco_units <= 3   ~ "Light",
                .sumalco_units <= 7   ~ "Moderate",
                .sumalco_units >  7   ~ "Heavy"
            ),
            
            alcohol_category_imp = dplyr::if_else(
                is.na(alcohol_category_imp) & !is.na(.sumalco_cat),
                .sumalco_cat,
                alcohol_category_imp
            ),
            
            alcohol_impute_source = dplyr::case_when(
                !is.na(alcohol_impute_source) ~ alcohol_impute_source,
                is.na(alcohol_category) & !is.na(.sumalco_cat) ~ "sumalco",
                TRUE ~ NA_character_
            )
        ) |>
        dplyr::select(-.sumalco_units, -.sumalco_cat)
    
    # ── Step 2: LOCF ---------------------------------------------------------
    df <- df |>
        dplyr::arrange(pt, .visit) |>
        dplyr::group_by(pt) |>
        dplyr::mutate(.before_locf = alcohol_category_imp) |>
        tidyr::fill(alcohol_category_imp, .direction = "down") |>
        dplyr::mutate(
            alcohol_impute_source = dplyr::case_when(
                !is.na(alcohol_impute_source) ~ alcohol_impute_source,
                is.na(.before_locf) & !is.na(alcohol_category_imp) ~ "locf",
                TRUE ~ NA_character_
            )
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-.before_locf)
    
    # ── Final factor formatting ---------------------------------------------
    df <- df |>
        dplyr::mutate(
            alcohol_category_imp = factor(
                alcohol_category_imp,
                levels = orig_levels,
                ordered = orig_ordered
            ),
            alcohol_impute_source = factor(
                alcohol_impute_source,
                levels = c("conso_hebdo", "sumalco", "locf")
            )
        )
    
    # ── Diagnostics ----------------------------------------------------------
    cli::cli_h2("Impute alcohol_category_imp")
    
    cli::cli_inform(c(
        "i" = "Rows: {nrow(df)}",
        "*" = "Initial NA: {n_original_na}",
        "*" = "Still NA: {sum(is.na(df$alcohol_category_imp))}"
    ))
    
    df
}