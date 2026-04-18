# TODO: think about this missing data handling strategy
# Missing FFQ item handling:
#   If ALL dairy items are NA -> sub-category = NA (FFQ not completed).
#   If at least one item is non-NA -> missing items treated as 0.
# TODO: FFQ4 cottage cheese 0% is more consumed in women than men? is this correct? Are these values coming from Bernstein et al?
# =============================================================================
# R/derive_colaus_dairy.R
# =============================================================================
# Derives dairy intake sub-categories by summing FFQ amount columns.
#
# FFQ amount columns are in grams/day as output from the dietary analysis.
# Each FFQ item contributes to one or more sub-categories as specified in the
# data dictionary.
#
# Sub-category definitions
# ─────────────────────────
#   dairy_total_gday         all dairy items
#   dairy_fermented_gday     fermented dairy (yoghurt, cheese)
#   dairy_non_fermented_gday non-fermented dairy (milk products)
#   dairy_lowfat_gday        low-fat dairy items
#   dairy_highfat_gday       high-fat dairy items
#
# Item -> sub-category mapping:
#   FFQ1amount  -> total, fermented, high-fat      plain yogurt
#   FFQ2amount  -> total, fermented, low-fat         low-fat yogurt
#   FFQ3amount  -> total, fermented, high-fat        fruit yogurt
#   FFQ4amount  -> total, fermented, low-fat         cottage cheese 0%
#   FFQ5amount  -> total, fermented, high-fat        cottage cheese/ricotta
#   FFQ6amount  -> total, fermented, high-fat        feta/mozzarella
#   FFQ7amount  -> total, fermented, high-fat        gruyere/tomme/camembert
#   FFQ8amount  -> total, fermented, high-fat        cheese fondue
#   FFQ52amount -> total, non-fermented, high-fat    butter
#   FFQ53amount -> total, non-fermented, high-fat    Cream 35% (g/day)
#   FFQ63amount -> total, non-fermented, high-fat    cream tart/cake
#   FFQ68amount -> total, non-fermented, high-fat    ice cream/sorbet (assumed dairy)
#   FFQ71amount -> total, non-fermented, high-fat    butter for cooking
#   FFQ82amount -> total, non-fermented, low-fat     milk in coffee 0%
#   FFQ83amount -> total, non-fermented, high-fat    milk in coffee non-0%
#   FFQ84amount -> total, non-fermented, high-fat    coffee creamer
#   FFQ85amount -> total, non-fermented, lowf-at     milk drink 0%
#   FFQ86amount -> total, non-fermented, high-fat    milk drink non-0%
# =============================================================================

#' Derive dairy sub-category intakes (g/day) for a CoLaus long tibble.
#' 
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with dairy_total_gday, dairy_fermented_gday,
#'   dairy_non_fermented_gday, dairy_lowfat_gday, dairy_highfat_gday
#'   (all numeric, g/day) added.
derive_dairy <- function(df) {
    
    # Item membership per sub-category (base column names, no wave prefix)
    .DAIRY_TOTAL        <- paste0("FFQ", c(1:8, 52, 53, 63, 68, 71, 82:86), "amount")
    .DAIRY_FERMENTED    <- paste0("FFQ", c(1:8), "amount")
    .DAIRY_NON_FERM     <- paste0("FFQ", c(52, 53, 63, 68, 71, 82:86), "amount")
    .DAIRY_LOWFAT       <- paste0("FFQ", c(2, 4, 82, 85), "amount")
    .DAIRY_HIGHFAT      <- paste0("FFQ", c(1, 3, 5, 6, 7, 8,52, 53, 63, 68, 71, 83, 84, 86), "amount")
    
    
    # ── Column Check ----------------------------------------------
    required_cols <- .DAIRY_TOTAL
    actual_cols <- names(df)
    missing_cols <- setdiff(required_cols, actual_cols)
    
    if (length(missing_cols) > 0) {
        cli::cli_warn(c(
            "x" = "derive_dairy: Missing required FFQ amount columns.",
            "i" = "Missing: {.val {missing_cols}}",
            "!" = "Dairy categories cannot be calculated without these sources."
        ))
        return(df)
    }
    
    
    # ── Ensure Lazy State -------------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    
    # ──  Main Derivation ----------------------------------------------
    df <- df |>
        dplyr::mutate(
            dairy_total_gday = !!.build_dairy_sum_expr(names(df), .DAIRY_TOTAL),
            dairy_total_gday         = !!.build_dairy_sum_expr(names(df), .DAIRY_TOTAL),
            dairy_fermented_gday     = !!.build_dairy_sum_expr(names(df), .DAIRY_FERMENTED),
            dairy_non_fermented_gday = !!.build_dairy_sum_expr(names(df), .DAIRY_NON_FERM),
            dairy_lowfat_gday        = !!.build_dairy_sum_expr(names(df), .DAIRY_LOWFAT),
            dairy_highfat_gday       = !!.build_dairy_sum_expr(names(df), .DAIRY_HIGHFAT)
        ) |>
        dplyr::as_tibble()
    
    # ── Eager Summary & Sanity Check ----------------------------------------------
    stats <- df |>
        dplyr::summarise(
            n_exceed = sum(
                (dairy_fermented_gday + dairy_non_fermented_gday) > (dairy_total_gday + 0.01),
                na.rm = TRUE
            ),
            valid_total      = sum(!is.na(dairy_total_gday)),
            valid_fermented  = sum(!is.na(dairy_fermented_gday)),
            valid_non_ferm   = sum(!is.na(dairy_non_fermented_gday)),
            valid_lowfat     = sum(!is.na(dairy_lowfat_gday)),
            valid_highfat    = sum(!is.na(dairy_highfat_gday))
        ) |>
        dplyr::as_tibble()
    
    # ── Reporting ----------------------------------------------
    if (stats$n_exceed > 0) {
        cli::cli_warn("derive_dairy: {stats$n_exceed} row(s) where fermented + non_fermented exceeds total.")
    }
    
    cli::cli_h2("Dairy Intake Sub-Categories")
    cli::cli_inform(c(
        "v" = "derive_dairy: derived variables added.",
        " " = "Summary of non-missing values:",
        " " = " - total: {stats$valid_total}",
        " " = " - fermented: {stats$valid_fermented}",
        " " = " - non-fermented: {stats$valid_non_ferm}",
        " " = " - low-fat: {stats$valid_lowfat}",
        " " = " - high-fat: {stats$valid_highfat}"
    ))
    
    
    
    return(df)
}



# Internal helper: sum a set of FFQ amount columns, returning NA if all are NA.
.build_dairy_sum_expr <- function(col_names, cols) {
    present <- intersect(cols, col_names)
    
    if (length(present) == 0) return(rlang::expr(NA_real_))
    
    # Logic: If all present columns are NA, return NA. Else sum them (NA = 0).
    # We build a chain of is.na(col1) & is.na(col2)...
    all_na_condition <- present |>
        lapply(function(x) rlang::expr(is.na(.data[[!!x]]))) |>
        purrr::reduce(function(a, b) rlang::expr(!!a & !!b))
    
    # Build the row-sum expression (base rowSums is translated by dtplyr)
    rlang::expr(dplyr::case_when(
        !!all_na_condition ~ NA_real_,
        TRUE ~ rowSums(dplyr::across(dplyr::all_of(!!present)), na.rm = TRUE)
    ))
}




    
