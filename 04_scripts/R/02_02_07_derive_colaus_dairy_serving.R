# =============================================================================
# R/derive_colaus_dairy_serving.R
# =============================================================================
# Derives dairy serving counts and Swiss-guideline compliance from FFQ
# frequency and portion-size columns.
#
# SWISS GUIDELINE REFERENCE
# ─────────────────────────
# Swiss Society for Nutrition (SGE/SSN): 2–3 portions of dairy products per
# day are recommended. One portion corresponds to:
#   • 2 dl (200 ml) milk
#   • 150–200 g yoghurt, quark, cottage cheese, or blanc battu
#   •  30 g semi-hard or hard cheese (e.g. Gruyère, Emmental)
#   •  60 g soft cheese (e.g. Camembert, Brie)
# The guideline is considered met when a participant consumes 2 portions on
# some days and 3 portions on others across the week; i.e., the average is
# between 2 and 3 portions/day. We operationalise compliance as >= 2 servings
# per day (>= 14 servings/week) as the lower bound of the recommendation.
#
# FFQ ITEM SELECTION
# ──────────────────
# Items included (FFQ numbers 1–8, 85, 86) are those that represent clearly
# countable dairy portions matching the Swiss guideline unit definitions above:
#
#   FFQ1   Plain yoghurt              → ~150–200 g per serving  ✓
#   FFQ2   Low-fat yoghurt            → ~150–200 g per serving  ✓
#   FFQ3   Fruit yoghurt              → ~150–200 g per serving  ✓
#   FFQ4   Cottage cheese 0%          → ~150–200 g per serving  ✓
#   FFQ5   Cottage cheese / ricotta   → ~150–200 g per serving  ✓
#   FFQ6   Feta / mozzarella          → ~60 g (soft cheese)     ✓
#   FFQ7   Gruyère / tomme / Camembert→ ~30 g (hard/semi-hard)  ✓
#   FFQ8   Cheese fondue              → ~30 g cheese equivalent ✓
#   FFQ85  Milk drink 0%              → ~200 ml per serving     ✓
#   FFQ86  Milk drink non-0%          → ~200 ml per serving     ✓
#
# Items EXCLUDED and rationale:
#   FFQ52  Butter                     → condiment, not a dairy portion
#   FFQ53  Cream 35%                  → condiment/ingredient, not a portion
#   FFQ63  Cream tart/cake            → composite dish, dairy content unclear
#   FFQ68  Ice cream / sorbet         → high sugar, no clear dairy portion size
#   FFQ71  Butter for cooking         → condiment, not a dairy portion
#   FFQ82  Milk in coffee 0%          → small volume, not a full serving
#   FFQ83  Milk in coffee non-0%      → small volume, not a full serving
#   FFQ84  Coffee creamer             → small volume, not a full serving
#
# TWO CALCULATION METHODS
# ───────────────────────
# Method 1 — Frequency sum (dairy_freq_total):
#   Sums the raw FFQ frequency scores (servings/day equivalent) across the
#   selected items. Assumes each consumption occasion equals exactly one
#   standard portion. Quick and reproducible.
#
# Method 2 — Portion-adjusted sum (dairy_portion_total):
#   Weights each frequency score by the participant's self-reported portion
#   size (FFQp columns: "Less" = 0.5, "Equal" = 1.0, "More" = 1.5).
#   Better reflects actual intake volume but introduces additional measurement
#   error from the portion-size question.
#
# Compliance threshold: >= 2 servings/day (lower bound of Swiss recommendation).
# =============================================================================

#' Derive dairy serving count and Swiss-guideline compliance for CoLaus.
#'
#' Computes two serving-count estimates (frequency-based and portion-adjusted)
#' and classifies each row against the Swiss dairy guideline (>= 2 servings/day).
#' Also reports agreement between the two methods.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with dairy_freq_total, dairy_portion_total, dairy_guidelines_freq, and
#'   dairy_guidelines_port added. Intermediate diff columns are dropped.
derive_dairy_servings <- function(df) {
    
    # ── Check required columns -----------------------------------------------
    .FREQ_COLS <- paste0("freqFFQ", c(1:8, 85, 86))
    .PORT_COLS <- paste0("FFQp",    c(1:8, 85, 86))
    
    required_cols <- c(.FREQ_COLS, .PORT_COLS)
    missing_cols  <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_dairy_servings: missing required columns: {.val {missing_cols}}. \\
             Dairy servings will not be derived."
        )
        return(df)
    }
    
    n_rows <- nrow(df)
    
    # ── Derivation -----------------------------------------------------------
    df <- df |>
        dplyr::mutate(
            # Method 1: simple frequency sum (servings/day)
            # NA is returned when ALL frequency columns are NA for that row
            # (FFQ not completed); na.rm = TRUE would silently return 0 otherwise.
            dairy_freq_total = dplyr::if_else(
                rowSums(!is.na(dplyr::across(dplyr::all_of(.FREQ_COLS)))) == 0L,
                NA_real_,
                round(rowSums(dplyr::across(dplyr::all_of(.FREQ_COLS)), na.rm = TRUE), 3)
            ),
            
            # Method 2: portion-adjusted sum
            # Each frequency score is multiplied by its portion-size multiplier.
            # NA is returned when ALL frequency columns are NA for that row.
            dairy_portion_total = dplyr::if_else(
                rowSums(!is.na(dplyr::across(dplyr::all_of(.FREQ_COLS)))) == 0L,
                NA_real_,
                round(
                    rowSums(
                        dplyr::across(dplyr::all_of(.FREQ_COLS)) *
                            dplyr::across(dplyr::all_of(.PORT_COLS), ~ dplyr::case_when(
                                .x == "Less"  ~ 0.5,
                                .x == "Equal" ~ 1.0,
                                .x == "More"  ~ 1.5,
                                TRUE          ~ NA_real_
                            )),
                        na.rm = TRUE
                    ),
                    3
                )
            ),
            
            # Swiss guideline compliance: >= 2 servings/day
            dairy_guidelines_freq = factor(
                dplyr::if_else(dairy_freq_total >= 2, ">= 2 servings/day", "< 2 servings/day"),
                levels = c("< 2 servings/day", ">= 2 servings/day")
            ),
            dairy_guidelines_port = factor(
                dplyr::if_else(dairy_portion_total >= 2, ">= 2 servings/day", "< 2 servings/day"),
                levels = c("< 2 servings/day", ">= 2 servings/day")
            )
        ) |>
        dplyr::relocate(
            dairy_freq_total, dairy_portion_total, dairy_guidelines_freq, dairy_guidelines_port,
            .after = dplyr::last_col()
        )
    
    # ── Summary --------------------------------------------------------------
    cli::cli_h2("Derive Dairy Servings")
    
    # Coverage
    n_freq_derived <- sum(!is.na(df$dairy_freq_total))
    n_port_derived <- sum(!is.na(df$dairy_portion_total))
    
    cli::cli_inform(c(
        "i" = "Total rows: {n_rows}",
        "*" = "dairy_freq_total derived    : {n_freq_derived} ({round(n_freq_derived / n_rows * 100, 1)}%)",
        "*" = "dairy_portion_total derived : {n_port_derived} ({round(n_port_derived / n_rows * 100, 1)}%)"
    ))
    
    # Compliance rates per method
    for (var in c("dairy_guidelines_freq", "dairy_guidelines_port")) {
        dist <- df |>
            dplyr::count(!!rlang::sym(var), .drop = FALSE) |>
            dplyr::mutate(pct = round(n / sum(n) * 100, 1))
        cli::cli_inform(c("i" = "Distribution of {.col {var}}:"))
        cli::cli_inform(paste(capture.output(print(dist, n = Inf)), collapse = "\n"))
    }
    
    # Agreement between methods
    both_present <- !is.na(df$dairy_guidelines_freq) & !is.na(df$dairy_guidelines_port)
    n_both    <- sum(both_present)
    n_agree   <- sum(both_present & df$dairy_guidelines_freq == df$dairy_guidelines_port)
    n_differ  <- n_both - n_agree
    
    cli::cli_inform(c(
        "i" = "Method agreement (rows with both non-missing, n = {n_both}):",
        "*" = "Agree   : {n_agree}  ({round(n_agree  / n_both * 100, 1)}%)",
        "*" = "Disagree: {n_differ} ({round(n_differ / n_both * 100, 1)}%)"
    ))
    
    if (n_differ > 0) {
        crosstab <- df |>
            dplyr::filter(both_present & dairy_guidelines_freq != dairy_guidelines_port) |>
            dplyr::count(dairy_guidelines_freq, dairy_guidelines_port)
        
        cli::cli_inform("  Cross-tabulation of disagreements (freq → portion):")
        cli::cli_inform(paste(capture.output(print(crosstab, n = Inf)), collapse = "\n"))
        
        # Per-visit disagreement rate
        visit_agree <- df |>
            dplyr::filter(both_present) |>
            dplyr::group_by(.visit) |>
            dplyr::summarise(
                n          = dplyr::n(),
                n_differ   = sum(dairy_guidelines_freq != dairy_guidelines_port),
                pct_differ = round(n_differ / n * 100, 1),
                .groups    = "drop"
            )
        
        cli::cli_inform("  Disagreement rate by visit:")
        cli::cli_inform(paste(capture.output(print(visit_agree, n = Inf)), collapse = "\n"))
    }
    
    return(df)
}