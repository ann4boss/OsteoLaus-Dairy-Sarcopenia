# =============================================================================
# R/02_02_16_derive_colaus_food_groups.R
# =============================================================================
# Sums FFQ item amounts (g/day) into food-group totals for a CoLaus long
# tibble. Defines one function: derive_food_groups().
#
# Food groups and their constituent FFQ<n>amount columns are defined by the
# .ANIMAL_PROTEIN / .PLANT_PROTEIN / .VEG / .FRU / .GRAINS / .FATS /
# .PROCESSED / .ALCOHOL item-index vectors inside the function.
#
# TODO check if NA values are not recorded as 0
# =============================================================================

# -----------------------------------------------------------------------------
# derive_food_groups()
# -----------------------------------------------------------------------------
#' Derive food-group intake totals (g/day) for a CoLaus long tibble.
#'
#' Sums the FFQ<n>amount columns belonging to each food group (animal
#' protein, plant protein, vegetables, fruits, grains, fats/oils,
#' processed/ultra-processed foods, alcohol). A row's group total is NA if
#' any constituent item is NA, and NA if every constituent item is NA
#' cohort-wide (see sum_block()).
#'
#' @param df CoLaus long tibble containing all FFQ<n>amount columns.
#' @return df with animal_protein_gday, plant_protein_gday, veg_gday,
#'   fru_gday, grains_gday, fats_gday, processed_gday, and alcohol_gday added.
derive_food_groups <- function(df) {
    
    # ── ANIMAL PROTEIN ──────────────────────────────────────────────────────
    .ANIMAL_PROTEIN <- paste0("FFQ", c(
        # meat
        14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
        # fish / seafood
        25, 26, 27, 28, 29,
        # eggs
        49
    ), "amount")
    
    # ── PLANT PROTEIN ───────────────────────────────────────────────────────
    .PLANT_PROTEIN <- paste0("FFQ", c(
        50 # tofu
    ), "amount")
    
    # ── VEGETABLES ──────────────────────────────────────────────────────────
    .VEG <- paste0("FFQ", c(
        30, 31, 32, 33, 34, 36,
        37, 38, 39, 40, 45
    ), "amount")
    
    # ── FRUITS ───────────────────────────────────────────────────────────────
    .FRU <- paste0("FFQ", c(
        55, 56, 57, 58, 59, 60, 91, 92
    ), "amount")
    
    # ── GRAINS ───────────────────────────────────────────────────────────────
    .GRAINS <- paste0("FFQ", c(
        9, 10, 11, 12, 13,
        42, 43, 44, 46
    ), "amount")
    
    # ── FATS AND OILS ───────────────────────────────────────────────────────
    .FATS <- paste0("FFQ", c(
        35, 51, 54, 72, 73, 74, 75
    ), "amount")
    
    # ── PROCESSED / ULTRA-PROCESSED FOODS ──────────────────────────────────────
    .PROCESSED <- paste0("FFQ", c(
        41, 47, 48, 61, 62, 64, 65, 66, 67, 70,
        90, 91, 92
    ), "amount")
    
    # ── ALCOHOL ──────────────────────────────────────────────────────────────
    .ALCOHOL <- paste0("FFQ", c(
        94, 95, 96, 97
    ), "amount")
    
    # ── ALL REQUIRED COLUMNS ────────────────────────────────────────────────
    .ALL_ITEMS <- unique(c(
        .ANIMAL_PROTEIN,
        .PLANT_PROTEIN,
        .VEG,
        .FRU,
        .GRAINS,
        .FATS,
        .PROCESSED,
        .ALCOHOL
    ))
    
    # ── CHECK MISSING ───────────────────────────────────────────────────────
    missing_cols <- setdiff(.ALL_ITEMS, names(df))
    
    if (length(missing_cols) > 0) {
        cli::cli_warn(c(
            "x" = "derive_food_groups: Missing FFQ columns.",
            "i" = "Missing: {.val {missing_cols}}"
        ))
        return(df)
    }
    
    # ── SUM FUNCTION ─────────────────────────────────────────────────────────
    # Row-sums `cols`; a row is NA if any of its `cols` values is NA, and the
    # whole result is NA if every value in `cols` is NA across all rows.
    sum_block <- function(data, cols) {
        x <- data[, cols, drop = FALSE]
        
        if (all(is.na(x))) {
            return(rep(NA_real_, nrow(data)))
        }
        
        apply(x, 1, function(row) {
            if (any(is.na(row))) return(NA_real_)
            sum(row)
        })
    }
    
    # ── DERIVATION ───────────────────────────────────────────────────────────
    df <- df |>
        dplyr::mutate(
            animal_protein_gday = sum_block(df, .ANIMAL_PROTEIN),
            plant_protein_gday  = sum_block(df, .PLANT_PROTEIN),
            veg_gday            = sum_block(df, .VEG),
            fru_gday            = sum_block(df, .FRU),
            grains_gday         = sum_block(df, .GRAINS),
            fats_gday           = sum_block(df, .FATS),
            processed_gday         = sum_block(df, .PROCESSED),
            alcohol_gday        = sum_block(df, .ALCOHOL)
        ) |>
        dplyr::as_tibble()
    
    # ── SUMMARY ──────────────────────────────────────────────────────────────
    stats <- df |>
        dplyr::summarise(
            animal_protein = sum(!is.na(animal_protein_gday)),
            plant_protein  = sum(!is.na(plant_protein_gday)),
            veg            = sum(!is.na(veg_gday)),
            fru            = sum(!is.na(fru_gday)),
            grains         = sum(!is.na(grains_gday)),
            fats           = sum(!is.na(fats_gday)),
            processed         = sum(!is.na(processed_gday)),
            alcohol        = sum(!is.na(alcohol_gday))
        )
    
    cli::cli_h2("Food group derivation (updated MSM structure)")
    cli::cli_inform(c(
        "v" = "Derived food groups successfully.",
        " " = "animal protein : {stats$animal_protein}",
        " " = "plant protein  : {stats$plant_protein}",
        " " = "vegetables     : {stats$veg}",
        " " = "fruits         : {stats$fru}",
        " " = "grains         : {stats$grains}",
        " " = "fats           : {stats$fats}",
        " " = "processed         : {stats$processed}",
        " " = "alcohol        : {stats$alcohol}"
    ))
    
    return(df)
}