# =============================================================================
# R/select_analysis_columns.R
# =============================================================================
# Selects the final set of columns required for analysis from the fully
# derived CoLaus and OsteoLaus datasets.
#
# Public interface
# ----------------
#   select_analysis_columns(df)    -- plain data frame (complete-case route)
#   select_analysis_columns(mids)  -- mids object (MICE route)
#
# MICE route
# ----------
# A mids object is converted to long format via:
#
#   mice::complete(mids_obj, action = "long", include = TRUE)
#
# Column selection is applied to the long tibble (including the .imp == 0
# observed-data slice), and the result is converted back with mice::as.mids().
#
# @return
#   plain df  -> tibble with only the analysis columns
#   mids      -> mids with only the analysis columns
# =============================================================================

.ANALYSIS_COLS <- c(
    # Identifiers
    "pt", "exam_date_iso", ".visit", ".cohort",
    # imputation
    ".imp", ".id",
    # Anthropometry
    "Age", "Height", "Weight", "BMI", "BMI_category",
    # Sociodemographics
    "mrtsts2", "education_level",
    # Lifestyle & clinical
    "smoking_status", "smoking_impute_source",
    "alcohol_category_conso", "alcohol_category_sumalco",
    "pa_levels_tertile_f1", "pa_levels_who_f1",
    "diabetes_status", "htn_status",
    # Medications
    "hrt_status", "hypolip_drug_status", "corticoids_status", "vitD_status",
    "calcium_status", "benzo_status", "bisphosphonate_status",
    # Diet
    "sumtot1",
    #"sumprot1", "sumgluc1", "sumlipi1",
    "dairy_total_gday", "dairy_fermented_gday", "dairy_non_fermented_gday",
    "dairy_lowfat_gday", "dairy_highfat_gday",
    "dairy_guidelines_port",
    "dairy_quartile_baseline",
    "dairy_total_gday_cumavg", "dairy_fermented_gday_cumavg",
    "dairy_non_fermented_gday_cumavg",
    "dairy_lowfat_gday_cumavg", "dairy_highfat_gday_cumavg",
    #"animal_protein_gday","plant_protein_gday","veg_gday","fru_gday",
    #"grains_gday","fats_gday","processed_gday",
    
    # Outcomes
    "HGS_MAX", "gait_speed", "ALM_HT2_harmonised", "ALM_BMI_harmonised"
    #,"ALM_harmonised","ALM_WT_harmonised",
    #"ALM_Lunar","ALM_Hologic","ALM_HT2_Lunar","ALM_HT2_Hologic",
    #"ALM","ALM_HT2","ALM_BMI","ALM_WT","DXA_method"
)


#' Select analysis columns from a derived data frame or mids object.
#'
#' Keeps only the columns listed in `.ANALYSIS_COLS`. Columns absent from the
#' input are silently skipped and reported in a summary message.
#'
#' @param df Either:
#'   * A fully derived tibble / data frame (complete-case route), or
#'   * A `mids` object (MICE route) — converted to long format internally via
#'     `mice::complete(..., include = TRUE)`, column selection applied to the
#'     long tibble, then converted back with `mice::as.mids()`.
#'
#' @return
#'   * Plain input -> tibble with only the analysis columns.
#'   * `mids` input -> `mids` with only the analysis columns.
select_analysis_columns <- function(df) {
    
    # ── MICE route: mids object ──────────────────────────────────────────────
    if (inherits(df, "mids")) {
        m    <- df$m
        long <- mice::complete(df, action = "long", include = TRUE) |>
            tibble::as_tibble()
        
        cli::cli_h2("Select Analysis Columns (MICE, m = {m})")
        selected_long <- .select_cols_from_df(long)
        
        return(mice::as.mids(selected_long))
    }
    
    # ── Complete-case route: plain data frame ────────────────────────────────
    .select_cols_from_df(df)
}


# ── Internal helper ───────────────────────────────────────────────────────────

# Apply .ANALYSIS_COLS selection to a plain data frame or long tibble.
# .imp and .id are kept when present — they are required by mice::as.mids().
.select_cols_from_df <- function(df) {
    cohort <- unique(df$.cohort)
    cohort <- cohort[!is.na(cohort)]
    
    cli::cli_h2("Select Analysis Columns ({paste(cohort, collapse = ', ')})")
    
    present <- intersect(.ANALYSIS_COLS, names(df))
    absent  <- setdiff(.ANALYSIS_COLS, names(df))
    
    if (length(absent) > 0) {
        cli::cli_inform(c(
            "i" = "{length(absent)} column(s) not present in \\
                   {paste(cohort, collapse='/')} data \\
                   (expected for cohort-specific variables):",
            "*" = "{.val {absent}}"
        ))
    }
    
    out <- dplyr::select(df, dplyr::all_of(present))
    
    cli::cli_inform(c(
        "v" = "Selected {length(present)} / {length(.ANALYSIS_COLS)} columns.",
        "i" = "{nrow(out)} rows retained."
    ))
    
    out
}