# TODO check why these variables do not match so many times
# derive_alcohol: 5915 row(s) (55.15%) have disagreement.
# ℹ 336 are higher by `conso_hebdo`; 5579 are higher by `sumalco`.
# • Breakdown of mismatches:
#     • conso: Non-drinker / sumalco: Light (n = 259)
# • conso: Non-drinker / sumalco: Moderate (n = 284)
# • conso: Non-drinker / sumalco: Heavy (n = 512)
# • conso: Light / sumalco: Non-drinker (n = 160)
# • conso: Light / sumalco: Moderate (n = 457)
# • conso: Light / sumalco: Heavy (n = 1755)
# • conso: Moderate / sumalco: Non-drinker (n = 71)
# • conso: Moderate / sumalco: Light (n = 21)
# • conso: Moderate / sumalco: Heavy (n = 2312)
# • conso: Heavy / sumalco: Non-drinker (n = 61)
# • conso: Heavy / sumalco: Light (n = 7)
# • conso: Heavy / sumalco: Moderate (n = 16)

# =============================================================================
# R/derive_colaus_alcohol.R
# =============================================================================
# Derives alcohol_category from conso_hebdo (units/week) or sumalco (g ethanol/day).
#
# conso_hebdo is the primary source. sumalco is used as fallback where
# conso_hebdo is missing.
#
# Thresholds are applied on a per-weekly basis:
#   0 units/week (0 g/day)              -> 0 = Non-drinker
#   ≥1 to 3 units/week (>0–6 g/day)     -> 1 = Light
#   ≥4 to 7 units/week (>6–12 g/day)    -> 2 = Moderate
#   >7 units/week (>12 g/day)           -> 3 = Heavy
#
# The g/day thresholds are derived from the Swiss national guidelines defining a standard drink
# as containing approximately 10-12 g of pure ethanol.
# The conversion from units/week to g/week uses a standard value of 10 g/unit, the lower end of the typical range.
# =============================================================================

#' Derive alcohol_category for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with alcohol_units_week (numeric) and alcohol_category (ordered factor) added.
derive_alcohol <- function(df, g_per_unit = 10) {
    
    # ── Ensure source columns are present ------------------------------------
    required_cols <- c("conso_hebdo", "sumalco")
    actual_cols <- names(df)
    missing_cols <- setdiff(required_cols, actual_cols)
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_alcohol: missing required columns: {.val {missing_cols}}.
        Alcohol category will not be derived."
        )
        return(df)
    }
    
    # ── Ensure Lazy State -------------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    # ── Calculate Units and Categories --------------------------------
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
                levels = 0:3,
                labels = c("Non-drinker", "Light", "Moderate", "Heavy"),
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
                levels = 0:3,
                labels = c("Non-drinker", "Light", "Moderate", "Heavy"),
                ordered = TRUE
            )
        )
    
    # ── Check Agreement -------------------------------------------------
    df <- df |>
        dplyr::mutate(
            alcohol_agreement = dplyr::case_when(
                is.na(alcohol_category_conso) | is.na(alcohol_category_sumalco) ~ NA_character_,
                alcohol_category_conso == alcohol_category_sumalco ~ "Agree",
                alcohol_category_conso > alcohol_category_sumalco  ~ "Conso higher",
                alcohol_category_conso < alcohol_category_sumalco  ~ "Sumalco higher"
            ) |> factor(levels = c("Agree", "Conso higher", "Sumalco higher"))
        )
    
    
    
    # ── Diagnostics --------------------------------
    
    # Calculate general agreement stats
    diag <- df |>
        dplyr::filter(!is.na(alcohol_agreement)) |>
        dplyr::summarise(
            total            = dplyr::n(),
            n_disagree       = sum(alcohol_agreement != "Agree", na.rm = TRUE),
            n_conso_higher   = sum(alcohol_agreement == "Conso higher", na.rm = TRUE),
            n_sumalco_higher = sum(alcohol_agreement == "Sumalco higher", na.rm = TRUE)
        ) |>
        dplyr::as_tibble()
    
    if (diag$n_disagree > 0) {
        # Calculate specific combinations of disagreement
        pairings <- df |>
            dplyr::filter(!is.na(alcohol_agreement), alcohol_agreement != "Agree") |>
            dplyr::count(alcohol_category_conso, alcohol_category_sumalco) |>
            dplyr::as_tibble() |>
            dplyr::mutate(
                msg = glue::glue("conso: {alcohol_category_conso} / sumalco: {alcohol_category_sumalco} (n = {n})")
            )
        
        p_disagree <- round((diag$n_disagree / diag$total) * 100, 2)
        
        cli::cli_h2("Derive Alcohol Category")
        cli::cli_inform(c(
            "derive_alcohol: {diag$n_disagree} row(s) ({p_disagree}%) have disagreement.",
            "i" = "{diag$n_conso_higher} are higher by {.col conso_hebdo}; {diag$n_sumalco_higher} are higher by {.col sumalco}.",
            "*" = "Breakdown of mismatches:"
        ))
        
        # Print each combination found
        cli::cli_li(pairings$msg)
    }
    
    # ── Final Category --------------------------------
    # Priority: Conso > Sumalco
    df <- df |>
        dplyr::mutate(
            alcohol_category = dplyr::case_when(
                !is.na(alcohol_category_conso) ~ as.character(alcohol_category_conso),
                !is.na(alcohol_category_sumalco) ~ as.character(alcohol_category_sumalco),
                TRUE ~ NA_character_
            ) |> factor(levels = c("Non-drinker", "Light", "Moderate", "Heavy"), ordered = TRUE)
        ) |>
        # --- Relocate all alcohol-related columns together ---
        dplyr::relocate(
            conso_hebdo, 
            sumalco, 
            sumalco_units, 
            alcohol_category_conso, 
            alcohol_category_sumalco, 
            alcohol_agreement, 
            alcohol_category,
            .after = dplyr::last_col() # or use .before = 1 to put them at the start
        ) |>
        # collect as tibble 
        dplyr::as_tibble()
    
 
    
    return(df)
}
