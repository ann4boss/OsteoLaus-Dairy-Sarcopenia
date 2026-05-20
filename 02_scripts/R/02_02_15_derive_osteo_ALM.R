# =============================================================================
# R/derive_osteolaus_alm.R
# =============================================================================
# Derives ALM-based body composition indices from DXA measurements.
#
# RAW INDICES (from the instrument-native ALM measurement)
# --------------------------------------------------------
#   ALM       Appendicular Lean Mass          sum of four limb lean masses  [g]
#   ALM_HT2   ALM index                       ALM / (Height / 100)^2       [kg/m2]
#   ALM_BMI   ALM relative to BMI             ALM / BMI                    [kg/(kg/m2)]
#   ALM_WT    ALM relative to body weight     ALM / Weight                 [unitless]
#
# CROSS-CALIBRATED INDICES (Lunar converted to Hologic scale)
# -----------------------------------------------------------
# OsteoLaus used two DXA platforms across visits:
#   Visits Baseline–V2  → Hologic 
#   Visits V3–V5        → Lunar
#
# Because Lunar and Hologic scanners produce systematically different ALM
# readings, analyses that span visits or compare absolute ALM values across
# the cohort require cross-calibration. The calibration equation converts
# Lunar measurements to the Hologic scale:
#
#   ALM_harmonised [g] = 381.8 + 1.034 × ALM_Hologic_equivalent
#
# Applied per row:
#   DXA_method == "Lunar"    →  ALM_harmonised = 381.8 + 1.034 × ALM
#   DXA_method == "Hologic"  →  ALM_harmonised = ALM   (no conversion needed)
#   DXA_method == NA         →  ALM_harmonised = NA
#
# Reference: manufacturer/publication cross-calibration coefficients as
# specified in the study protocol.
#
# After computing ALM_harmonised, three indices are derived from it:
#   ALM_HT2_harmonised   ALM_harmonised / (Height / 100)^2   [kg/m2]
#   ALM_BMI_harmonised   ALM_harmonised / BMI                [kg/(kg/m2)]
#   ALM_WT_harmonised    ALM_harmonised / Weight             [unitless]
#
# PRE-EXISTING COLUMNS
# --------------------
# Any pre-existing column with a name matching a derived output is renamed to
# *_source before the new value is written, preserving the original for audit.
#
# FUNCTION OVERVIEW
# -----------------
#   derive_alm_indices()     Main derivation function.
#   split_alm_by_method()    Adds method-stratified variants (Hologic / Lunar)
#                            for every derived index.
# =============================================================================


# ── Calibration constants (Lunar → Hologic scale) ----------------------------
# ALM_harmonised = .ALM_CAL_INTERCEPT + .ALM_CAL_SLOPE × ALM_Lunar
# Hologic measurements are already on the target scale (identity transform).
.ALM_CAL_INTERCEPT <- -369.3   # grams
.ALM_CAL_SLOPE     <-   0.967  # dimensionless

# =============================================================================
# Main derivation function
# =============================================================================

#' Derive ALM-based body composition indices for an OsteoLaus long tibble.
#'
#' Produces two parallel sets of indices:
#'   Raw           — from ALM as measured by the native DXA platform.
#'   Harmonised    — from ALM_harmonised, where Lunar values are converted
#'                   to the Hologic scale before indexing.
#'
#' @param df OsteoLaus long tibble after harmonisation and stacking. Must
#'   contain LARM_LEAN_MASS, RARM_LEAN_MASS, LLEG_LEAN_MASS, RLEG_LEAN_MASS,
#'   Height, BMI, Weight, and DXA_method.
#' @return df with ALM, ALM_harmonised, and all derived index columns added.
derive_alm_indices <- function(df) {
    
    required_cols <- c(
        "LARM_LEAN_MASS", "RARM_LEAN_MASS",
        "LLEG_LEAN_MASS", "RLEG_LEAN_MASS",
        "Height", "BMI", "Weight",
        "DXA_method"
    )
    
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0L) {
        cli::cli_warn(c(
            "derive_alm_indices(): missing required columns: {.val {missing_cols}}.",
            "i" = "ALM indices were not derived."
        ))
        return(df)
    }
    
    # ── Columns that will be written ----------------------------------------
    # Raw set
    raw_derived <- c("ALM", "ALM_HT2", "ALM_BMI", "ALM_WT")
    # Harmonised set
    harm_derived <- c("ALM_harmonised",
                      "ALM_HT2_harmonised", "ALM_BMI_harmonised", "ALM_WT_harmonised")
    all_derived  <- c(raw_derived, harm_derived)
    
    # ── Preserve any pre-existing columns by renaming to *_source -----------
    existing_derived <- intersect(all_derived, names(df))
    
    if (length(existing_derived) > 0L) {
        rename_to <- paste0(existing_derived, "_source")
        source_collision <- intersect(rename_to, names(df))
        
        if (length(source_collision) > 0L) {
            cli::cli_abort(c(
                "derive_alm_indices() cannot preserve existing derived columns.",
                "x" = "Source backup columns already exist: {.val {source_collision}}",
                "i" = "Rename or remove those columns before deriving ALM indices."
            ))
        }
        
        df <- dplyr::rename(df, !!!stats::setNames(existing_derived, rename_to))
    }
    
    limb_cols <- c("LARM_LEAN_MASS", "RARM_LEAN_MASS",
                   "LLEG_LEAN_MASS", "RLEG_LEAN_MASS")
    
    # ── Step 1: Raw ALM (native platform, grams) ----------------------------
    df <- df |>
        dplyr::mutate(
            ALM = dplyr::if_else(
                stats::complete.cases(dplyr::pick(dplyr::all_of(limb_cols))),
                round(LARM_LEAN_MASS + RARM_LEAN_MASS +
                          LLEG_LEAN_MASS + RLEG_LEAN_MASS, 4L),
                NA_real_
            )
        )
    
    # ── Step 2: Cross-calibrated ALM (Lunar → Hologic scale, grams) --------
    # Hologic rows: ALM_harmonised = ALM          (already on target scale)
    # Lunar rows:   ALM_harmonised = 381.8 + 1.034 × ALM
    # Unknown/NA:   ALM_harmonised = NA
    df <- df |>
        dplyr::mutate(
            ALM_harmonised = dplyr::case_when(
                is.na(ALM)                    ~ NA_real_,
                DXA_method == "Hologic"       ~ ALM,
                DXA_method == "Lunar"         ~
                    .ALM_CAL_INTERCEPT + .ALM_CAL_SLOPE * ALM,
                TRUE                          ~ NA_real_   # unknown method
            )
        )
    
    # ── Step 3: Raw indices from ALM ----------------------------------------
    df <- df |>
        dplyr::mutate(
            ALM_HT2 = dplyr::if_else(
                !is.na(ALM) & !is.na(Height) & Height > 0,
                round((ALM / 1000) / (Height / 100)^2, 4L),
                NA_real_
            ),
            ALM_BMI = dplyr::if_else(
                !is.na(ALM) & !is.na(BMI) & BMI > 0,
                round((ALM / 1000) / BMI, 4L),
                NA_real_
            ),
            ALM_WT = dplyr::if_else(
                !is.na(ALM) & !is.na(Weight) & Weight > 0,
                round((ALM / 1000) / Weight, 4L),
                NA_real_
            )
        )
    
    # ── Step 4: Harmonised indices from ALM_harmonised ----------------------
    df <- df |>
        dplyr::mutate(
            ALM_HT2_harmonised = dplyr::if_else(
                !is.na(ALM_harmonised) & !is.na(Height) & Height > 0,
                round((ALM_harmonised / 1000) / (Height / 100)^2, 4L),
                NA_real_
            ),
            ALM_BMI_harmonised = dplyr::if_else(
                !is.na(ALM_harmonised) & !is.na(BMI) & BMI > 0,
                round((ALM_harmonised / 1000) / BMI, 4L),
                NA_real_
            ),
            ALM_WT_harmonised = dplyr::if_else(
                !is.na(ALM_harmonised) & !is.na(Weight) & Weight > 0,
                round((ALM_harmonised / 1000) / Weight, 4L),
                NA_real_
            )
        )
    
    # ── Reporting -----------------------------------------------------------
    cli::cli_h2("Derive ALM Indices")
    
    pct      <- function(n, d) if (d == 0L) NA_real_ else round(n / d * 100, 1)
    fmt_num  <- function(x, digits = 3L) {
        if (length(x) == 0L || all(is.na(x))) "NA"
        else as.character(round(x, digits))
    }
    
    n_rows <- nrow(df)
    
    # Per-column summary helper
    summarise_col <- function(col) {
        vals <- df[[col]]
        obs  <- vals[!is.na(vals)]
        n_d  <- length(obs)
        cli::cli_inform(c(
            "v" = "{.col {col}}: {n_d} derived, {n_rows - n_d} missing",
            "i" = "Range {fmt_num(min(obs), 3)} – {fmt_num(max(obs), 3)} | ",
            " " = "Mean \u00b1 SD: {fmt_num(mean(obs), 3)} \u00b1 {fmt_num(stats::sd(obs), 3)}"
        ))
    }
    
    cli::cli_h3("Raw indices (native DXA platform)")
    purrr::walk(raw_derived, summarise_col)
    
    cli::cli_h3("Harmonised indices (Lunar converted to Hologic scale)")
    purrr::walk(harm_derived, summarise_col)
    
    # Cross-method difference for ALM_harmonised vs ALM (Hologic rows only,
    # where both are identical, as a sanity check)
    hologic_rows <- !is.na(df$DXA_method) & df$DXA_method == "Hologic" &
        !is.na(df$ALM) & !is.na(df$ALM_harmonised)
    n_hol <- sum(hologic_rows)
    
    if (n_hol > 0L) {
        max_diff <- max(abs(df$ALM[hologic_rows] - df$ALM_harmonised[hologic_rows]),
                        na.rm = TRUE)
        if (max_diff > 0.001) {
            cli::cli_warn(c(
                "!" = "Hologic rows: ALM_harmonised differs from ALM by up to ",
                " " = "{fmt_num(max_diff, 4)} g. Expected difference = 0."
            ))
        } else {
            cli::cli_inform(c("v" = "Sanity check passed: ALM_harmonised == ALM for all {n_hol} Hologic rows."))
        }
    }
    
    # Agreement with pre-existing source columns (if any were renamed)
    source_cols <- paste0(raw_derived, "_source")
    purrr::walk2(raw_derived, source_cols, function(derived_col, source_col) {
        
        if (!source_col %in% names(df)) return(invisible(NULL))
        
        both   <- !is.na(df[[derived_col]]) & !is.na(df[[source_col]])
        n_both <- sum(both)
        if (n_both == 0L) {
            cli::cli_inform("Agreement with {.col {source_col}}: no overlapping values.")
            return(invisible(NULL))
        }
        
        diff     <- abs(df[[derived_col]][both] - df[[source_col]][both])
        n_exact  <- sum(diff < 0.001)
        n_close  <- sum(diff >= 0.001 & diff < 0.1)
        n_differ <- sum(diff >= 0.1)
        
        cli::cli_inform(c(
            "i" = "Agreement {.col {derived_col}} vs {.col {source_col}} (n = {n_both}):",
            "*" = "< 0.001: {n_exact} ({pct(n_exact, n_both)}%)",
            "*" = "0.001 to 0.1: {n_close} ({pct(n_close, n_both)}%)",
            "*" = "\u2265 0.1: {n_differ} ({pct(n_differ, n_both)}%)",
            "i" = "Median |diff|: {fmt_num(stats::median(diff), 4)} | Max: {fmt_num(max(diff), 4)}"
        ))
        
        if (n_differ > 0L)
            cli::cli_warn("{n_differ} row(s) differ by \u2265 0.1 between derived and source.")
        
        invisible(NULL)
    })
    
    return(df)
}


# =============================================================================
# Split ALM-derived variables by DXA method
# =============================================================================

#' Create method-specific ALM columns based on DXA_method.
#'
#' @param df Data frame containing ALM-derived variables and DXA_method.
#' @return df with method-specific ALM columns added.
split_alm_by_method <- function(df) {
    
    required_cols <- c("DXA_method", "ALM", "ALM_HT2", "ALM_BMI", "ALM_WT")
    
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0L) {
        cli::cli_warn(c(
            "split_alm_by_method(): missing required columns: {.val {missing_cols}}.",
            "i" = "Method-specific ALM columns were not created."
        ))
        return(df)
    }
    
    derive_method_col <- function(var, method) {
        dplyr::if_else(
            df$DXA_method == method,
            df[[var]],
            NA_real_
        )
    }
    
    df <- df |>
        dplyr::mutate(
            ALM_Hologic    = derive_method_col("ALM", "Hologic"),
            ALM_Lunar     = derive_method_col("ALM", "Lunar"),
            
            ALM_HT2_Hologic = derive_method_col("ALM_HT2", "Hologic"),
            ALM_HT2_Lunar  = derive_method_col("ALM_HT2", "Lunar"),
            
            ALM_BMI_Hologic = derive_method_col("ALM_BMI", "Hologic"),
            ALM_BMI_Lunar  = derive_method_col("ALM_BMI", "Lunar"),
            
            ALM_WT_Hologic  = derive_method_col("ALM_WT", "Hologic"),
            ALM_WT_Lunar   = derive_method_col("ALM_WT", "Lunar")
        )
    
    cli::cli_h2("Split ALM by DXA method")
    cli::cli_inform(c(
        "v" = "Method-specific ALM columns created:",
        "*" = "Hologic and Lunar splits for ALM, ALM_HT2, ALM_BMI, ALM_WT"
    ))
    
    return(df)
}