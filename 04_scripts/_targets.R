# =============================================================================
# _targets.R
# CoLaus / OsteoLaus sarcopenia & dairy intake pipeline
# =============================================================================
#
# Pipeline stages
#
#   SHARED (both routes)
#   ─────────────────────────────────────────────────────────────────────────
#   00  File paths        format = "file", rebuilds when CSV changes on disk
#   01  Import            raw CSV → all-character tibble
#   02  Harmonise         type coercion, sentinel recoding, factor levels, dates
#   03  QC (data)         cross-wave integrity checks → qc_tbl
#   04  Stack             row-bind harmonised visits → colaus_long, osteo_long
#   05  QC (variables)    variable-level range/plausibility checks
#
#   COMPLETE-CASE (CC) ROUTE
#   ─────────────────────────────────────────────────────────────────────────
#     Derive CoLaus     computed analytical variables → colaus_derived
#    Wave match        nearest-date merge → merged_cc
#     Derive combined   BMI_category + sarcopenia staging → analysis_cc
#    CC exclusions     10_exclusion.R helpers via 09_cc_prepare.R
#                           → cc_result$annotated  — full dataset + excl_* flags
#                           → cc_result$excluded   — flagged rows for comparison
#                           → cc_result$flow_log   — CONSORT flow tibble
#
#   MICE ROUTE
#   ─────────────────────────────────────────────────────────────────────────
#     MICE imputation   impute raw CoLaus columns (pre-derivation)
#                           → mice_result$mids, $long
#     MICE derive       derive_colaus() on each imputed dataset
#                           → mice_derived_long  (.imp × colaus rows)
#     MICE merge+derive merge_closest_exams() + derive_combined() per .imp
#                           → mice_analysis_long
#     MICE exclusions   apply_exclusions() per .imp → mice_excl
#                           $annotated_long, $excluded, $flow_log_list, $summary
#
# =============================================================================

library(targets)
library(tarchetypes)

tar_option_set(
    packages = c(
        "readr", "dplyr", "dtplyr", "data.table", "tidyr", "stringr",
        "lubridate", "forcats", "purrr", "glue", "tibble", "cli",
        "mice",
        "gtsummary", "ggplot2", "patchwork", "scales",
        "lme4", "lmerTest", "broom.mixed", "broom",
        "survival", "gt",
        "sessioninfo"
    )
)

tar_source("04_scripts/R")


# =============================================================================
# 00. FILE PATHS
# =============================================================================

path_targets <- list(
    
    tar_target(f_colaus_baseline,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_base.csv",
               format = "file"),
    tar_target(f_colaus_f1,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_FU1.csv",
               format = "file"),
    tar_target(f_colaus_f2,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_FU2.csv",
               format = "file"),
    tar_target(f_colaus_f3,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_FU3.csv",
               format = "file"),
    
    tar_target(f_osteo_baseline,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_OstBas.csv",
               format = "file"),
    tar_target(f_osteo_v2,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_OstV2.csv",
               format = "file"),
    tar_target(f_osteo_v3,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_OstV3.csv",
               format = "file"),
    tar_target(f_osteo_v4,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_OstV4.csv",
               format = "file"),
    tar_target(f_osteo_v5,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_OstV5.csv",
               format = "file")
)


# =============================================================================
# 01. Prepare Core
# =============================================================================

prep_core <- tar_target(
    core,
    prepare_core(
        f_colaus_baseline, f_colaus_f1, f_colaus_f2, f_colaus_f3,
        f_osteo_baseline,  f_osteo_v2,  f_osteo_v3,  f_osteo_v4,  f_osteo_v5
    )
)


# =============================================================================
# ── COMPLETE-CASE (CC) ROUTE ──────────────────────────────────────────────────
# =============================================================================

cc_analysis <- tar_target(cc_route, build_analysis_dataset(core$colaus_long, core$osteo_long))




# ------------------------------------------------------------------------------
cc_exclusion_targets <- list(
    tar_target(
        cc_analysis, apply_exclusions(data = merged_cc_derived,
                                      qc_table = qc_tbl,
                                      covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                            "mrtsts2", "education_level",
                                                            "smoking_status","alcohol_category",
                                                            "pa_levels_tertile_f1",
                                                            "diabetes_status", "hrt_status", "htn_status",
                                                            "sumtot1"
                                                         ),
                                      exposure = "dairy_quartile_baseline",
                                      outcome = "HGS_MAX",
                                      visit_min = 2L,
                                      pt_col = "pt",
                                      visit_col = ".visit_osteo",
                                      impute = FALSE,
                                      imp_col = ".imp",
                                      return_tracking = TRUE)
    )
)
        

# =============================================================================
# ── MICE ROUTE ────────────────────────────────────────────────────────────────
# =============================================================================

mice_analysis <- tar_target(mice_route, build_analysis_dataset(core$colaus_long, core$osteo_long,
                                                               imputed = TRUE, m = 5L, maxit = 20L, seed = 2024L))




# MICE. Apply exclusions across all m imputed datasets

# ------------------------------------------------------------------------------
mice_exclusion_targets <-  list(
    tar_target(
        mice_analysis, apply_exclusions(data = mice_analysis_long,
                                      qc_table = qc_tbl,
                                      covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                         "mrtsts2", "education_level",
                                                         "smoking_status","alcohol_category",
                                                         "pa_levels_tertile_f1",
                                                         "diabetes_status", "hrt_status", "htn_status",
                                                         "sumtot1"
                                      ),
                                      exposure = "dairy_quartile_baseline",
                                      outcome = "HGS_MAX",
                                      visit_min = 2L,
                                      pt_col = "pt",
                                      visit_col = ".visit_osteo",
                                      impute = TRUE,
                                      imp_col = ".imp",
                                      return_tracking = TRUE)
    )
)




# =============================================================================
# ASSEMBLE
# =============================================================================

c(
    # ── Shared steps ──────────────────────────────────────────────────
    path_targets,
    prep_core,
    # ── Complete-case route ───────────────────────────────────────────────────
    cc_analysis,
    # ── MICE route ────────────────────────────────────────────────────────────
    mice_analysis

)