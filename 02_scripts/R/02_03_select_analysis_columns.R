# =============================================================================
# R/select_analysis_columns.R
# =============================================================================
# Selects the final set of columns required for analysis from the fully
# derived CoLaus and OsteoLaus tibbles. Columns not present in a given
# cohort are silently ignored (e.g. DXA columns absent from CoLaus).
# =============================================================================

# All required columns in a single named vector.
# Names are used in the missing-column report; values are the actual column names.
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
    "diabetes_status",  "htn_status",
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
    "dairy_total_gday_cumavg","dairy_fermented_gday_cumavg", "dairy_non_fermented_gday_cumavg",
    "dairy_lowfat_gday_cumavg", "dairy_highfat_gday_cumavg",
    #"animal_protein_gday","plant_protein_gday" ,"veg_gday", "fru_gday", "grains_gday", "fats_gday",
    #"processed_gday",
    
    # Outcomes
    "HGS_MAX", "gait_speed", "ALM_HT2_harmonised", "ALM_BMI_harmonised"
    #,"ALM_harmonised", "ALM_WT_harmonised",
    #"ALM_Lunar", "ALM_Hologic", "ALM_HT2_Lunar", "ALM_HT2_Hologic",
    #"ALM", "ALM_HT2", "ALM_BMI", "ALM_WT", "DXA_method"
)


#' Select analysis columns from a fully derived CoLaus or OsteoLaus tibble.
#'
#' Keeps only the columns listed in .ANALYSIS_COLS. Columns absent from the
#' input (expected for cohort-specific variables) are silently skipped and
#' reported in a summary message so omissions are transparent.
#'
#' @param df Fully derived tibble (output of the derivation pipeline).
#' @return Tibble containing only the available analysis columns, in the
#'   order defined by .ANALYSIS_COLS.
select_analysis_columns <- function(df) {
    
    cohort <- unique(df$.cohort)
    
    cli::cli_h2("Select Analysis Columns ({cohort})")
    
    present <- intersect(.ANALYSIS_COLS, names(df))
    absent  <- setdiff(.ANALYSIS_COLS, names(df))
    
    if (length(absent) > 0) {
        cli::cli_inform(c(
            "i" = "{length(absent)} column(s) not present in {cohort} data \\
                   (expected for cohort-specific variables):",
            "*" = "{.val {absent}}"
        ))
    }
    
    out <- dplyr::select(df, dplyr::all_of(present))
    
    cli::cli_inform(c(
        "v" = "Selected {length(present)} / {length(.ANALYSIS_COLS)} columns.",
        "i" = "{nrow(out)} rows retained."
    ))
    
    return(out)
}
