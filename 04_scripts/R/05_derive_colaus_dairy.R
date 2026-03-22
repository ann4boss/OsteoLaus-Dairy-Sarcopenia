# =============================================================================
# R/05_derive_colaus_dairy.R
# =============================================================================
# Derives dairy intake sub-categories by summing FFQ amount columns.
#
# FFQ amount columns are in grams/day as output from the dietary analysis.
# Each FFQ item contributes to one or more sub-categories as specified in the
# data dictionary. All output columns carry the _gday suffix.
#
# Sub-category definitions
# ─────────────────────────
#   dairy_total_gday         all dairy items
#   dairy_fermented_gday     fermented dairy (yoghurt, cheese)
#   dairy_non_fermented_gday non-fermented dairy (milk products)
#   dairy_lowfat_gday        low-fat dairy items
#   dairy_highfat_gday       high-fat dairy items
#
# Item -> sub-category mapping (from data dictionary):
#   FFQ1amount  -> total, highfat, fermented        plain yogurt
#   FFQ2amount  -> total, fermented, lowfat         low-fat yogurt
#   FFQ3amount  -> total, fermented, highfat        fruit yogurt
#   FFQ4amount  -> total, fermented, lowfat         cottage cheese 0%
#   FFQ5amount  -> total, fermented, highfat        cottage cheese/ricotta
#   FFQ6amount  -> total, fermented, highfat        feta/mozzarella
#   FFQ7amount  -> total, fermented, highfat        gruyere/tomme/camembert
#   FFQ8amount  -> total, fermented, highfat        cheese fondue
#   FFQ52amount -> total                            butter
#   FFQ53amount -> total                            cream 35%
#   FFQ63amount -> total                            cream tart/cake
#   FFQ68amount -> total                            ice cream/sorbet (assumed dairy)
#   FFQ71amount -> total                            butter for cooking
#   FFQ82amount -> total, non_fermented, lowfat     milk in coffee 0%
#   FFQ83amount -> total, non_fermented, highfat    milk in coffee non-0%
#   FFQ84amount -> total                            coffee creamer
#   FFQ85amount -> total, non_fermented, lowfat     milk drink 0%
#   FFQ86amount -> total, non_fermented, highfat    milk drink non-0%
#
# Items FFQ52, 53, 63, 68, 71, 84 contribute to total only (not fermented or
# non-fermented). This means dairy_fermented + dairy_non_fermented < dairy_total
# by design; the sanity check tests the opposite direction (exceeding total),
# not for equality.
#
# Missing FFQ item handling:
#   If ALL dairy items are NA -> sub-category = NA (FFQ not completed).
#   If at least one item is non-NA -> missing items treated as 0.
#
# Depends on: nothing
# =============================================================================

# Item membership per sub-category (base column names, no wave prefix)
.DAIRY_TOTAL        <- paste0("FFQ", c(1:8, 52, 53, 63, 68, 71, 82:86), "amount")
.DAIRY_FERMENTED    <- paste0("FFQ", c(1:8), "amount")
.DAIRY_NON_FERM     <- paste0("FFQ", c(82, 83, 85, 86), "amount")
.DAIRY_LOWFAT       <- paste0("FFQ", c(2, 4, 82, 85), "amount")
.DAIRY_HIGHFAT      <- paste0("FFQ", c(1, 3, 5, 6, 7, 8, 83, 86), "amount")

# Internal helper: sum a set of FFQ amount columns, returning NA if all are NA.
.dairy_sum <- function(df, cols) {
    present <- intersect(cols, names(df))
    if (length(present) == 0) return(rep(NA_real_, nrow(df)))
    
    mat    <- as.matrix(dplyr::select(df, dplyr::all_of(present)))
    all_na <- apply(is.na(mat), 1, all)
    out    <- rowSums(mat, na.rm = TRUE)
    out[all_na] <- NA_real_
    out
}

#' Derive dairy sub-category intakes (g/day) for a CoLaus long tibble.
#'
#' All output columns carry the _gday suffix to be consistent with
#' build_exposures() column expectations. The sub-category items that
#' contribute to total but not to fermented or non-fermented (butter, cream,
#' ice cream, coffee creamer) mean that fermented + non_fermented < total
#' is expected; this is NOT a data error.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with dairy_total_gday, dairy_fermented_gday,
#'   dairy_non_fermented_gday, dairy_lowfat_gday, dairy_highfat_gday
#'   (all numeric, g/day) added.
derive_dairy <- function(df) {
    
    df <- dplyr::mutate(df,
                        dairy_total_gday         = .dairy_sum(df, .DAIRY_TOTAL),
                        dairy_fermented_gday     = .dairy_sum(df, .DAIRY_FERMENTED),
                        dairy_non_fermented_gday = .dairy_sum(df, .DAIRY_NON_FERM),
                        dairy_lowfat_gday        = .dairy_sum(df, .DAIRY_LOWFAT),
                        dairy_highfat_gday       = .dairy_sum(df, .DAIRY_HIGHFAT)
    )
    
    # ── Sanity check: fermented + non_fermented must not exceed total ----------
    # Items FFQ52/53/63/68/71/84 go into total only, so the sum of fermented
    # and non_fermented is always expected to be <= total. A violation indicates
    # a mapping error, not an expected condition.
    n_exceed <- sum(
        !is.na(df$dairy_total_gday) &
            !is.na(df$dairy_fermented_gday) &
            !is.na(df$dairy_non_fermented_gday) &
            (df$dairy_fermented_gday + df$dairy_non_fermented_gday) >
            df$dairy_total_gday + 0.01,
        na.rm = TRUE
    )
    if (n_exceed > 0)
        cli::cli_warn(
            "derive_dairy: {n_exceed} row(s) where fermented + non_fermented \
       exceeds dairy_total_gday. Check FFQ item-to-category mapping."
        )
    
    return(df)
}