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
#   07  Derive CoLaus     computed analytical variables → colaus_derived
#   08  Wave match        nearest-date merge → merged_cc
#   09  Derive combined   BMI_category + sarcopenia staging → analysis_cc
#   10  CC exclusions     10_exclusion.R helpers via 09_cc_prepare.R
#                           → cc_result$annotated  — full dataset + excl_* flags
#                           → cc_result$excluded   — flagged rows for comparison
#                           → cc_result$flow_log   — CONSORT flow tibble
#
#   MICE ROUTE
#   ─────────────────────────────────────────────────────────────────────────
#   06  MICE imputation   impute raw CoLaus columns (pre-derivation)
#                           → mice_result$mids, $long
#   07  MICE derive       derive_colaus() on each imputed dataset
#                           → mice_derived_long  (.imp × colaus rows)
#   08  MICE merge+derive merge_closest_exams() + derive_combined() per .imp
#                           → mice_analysis_long
#   10  MICE exclusions   apply_exclusions() per .imp → mice_excl
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
# 01. IMPORT
# =============================================================================

import_targets <- list(
    
    tar_target(colaus_bsl_raw, import_visit(f_colaus_baseline, "CoLaus", "Baseline")),
    tar_target(colaus_f1_raw,  import_visit(f_colaus_f1,       "CoLaus", "F1")),
    tar_target(colaus_f2_raw,  import_visit(f_colaus_f2,       "CoLaus", "F2")),
    tar_target(colaus_f3_raw,  import_visit(f_colaus_f3,       "CoLaus", "F3")),
    
    tar_target(osteo_bsl_raw,  import_visit(f_osteo_baseline, "OsteoLaus", "Baseline")),
    tar_target(osteo_v2_raw,   import_visit(f_osteo_v2,       "OsteoLaus", "V2")),
    tar_target(osteo_v3_raw,   import_visit(f_osteo_v3,       "OsteoLaus", "V3")),
    tar_target(osteo_v4_raw,   import_visit(f_osteo_v4,       "OsteoLaus", "V4")),
    tar_target(osteo_v5_raw,   import_visit(f_osteo_v5,       "OsteoLaus", "V5"))
)


# =============================================================================
# 02. HARMONISE
# =============================================================================

harmonise_targets <- list(
    
    tar_target(colaus_bsl_harm, harmonise_colaus(colaus_bsl_raw)),
    tar_target(colaus_f1_harm,  harmonise_colaus(colaus_f1_raw)),
    tar_target(colaus_f2_harm,  harmonise_colaus(colaus_f2_raw)),
    tar_target(colaus_f3_harm,  harmonise_colaus(colaus_f3_raw)),
    
    tar_target(osteo_bsl_harm,  harmonise_osteo(osteo_bsl_raw)),
    tar_target(osteo_v2_harm,   harmonise_osteo(osteo_v2_raw)),
    tar_target(osteo_v3_harm,   harmonise_osteo(osteo_v3_raw)),
    tar_target(osteo_v4_harm,   harmonise_osteo(osteo_v4_raw)),
    tar_target(osteo_v5_harm,   harmonise_osteo(osteo_v5_raw))
)


# =============================================================================
# 03. QC (data integrity)
# =============================================================================

qc_targets <- list(
    
    tar_target(qc_out, qc(list(
        colaus_bsl_harm, colaus_f1_harm, colaus_f2_harm, colaus_f3_harm,
        osteo_bsl_harm,  osteo_v2_harm,  osteo_v3_harm,  osteo_v4_harm,  osteo_v5_harm
    ))),
    
    tar_target(qc_tbl,     qc_out$tbl),
    tar_target(qc_summary, qc_out$summary)
)


# =============================================================================
# 04a. STACK visits
# =============================================================================

stack_targets <- list(
    
    tar_target(colaus_long,
               stack_visits(
                   colaus_bsl_harm, colaus_f1_harm,
                   colaus_f2_harm,  colaus_f3_harm
               )),
    
    tar_target(osteo_long,
               stack_visits(
                   osteo_bsl_harm, osteo_v2_harm, osteo_v3_harm,
                   osteo_v4_harm,  osteo_v5_harm
               ))
)


# =============================================================================
# 04b. QC (variable-level)
# =============================================================================

qc_variable_targets <- list(
    tar_target(qc_variables_colaus, qc_variables(colaus_long,  "colaus")),
    tar_target(qc_variables_osteo,  qc_variables(osteo_long,   "osteo"))
)


# =============================================================================
# ── COMPLETE-CASE (CC) ROUTE ──────────────────────────────────────────────────
# =============================================================================

# 07 CC. Derive variables + Select columns
# ------------------------------------------------------------------------------
cc_derive_targets <- list(
    tar_target(colaus_derived, derive_colaus(colaus_long)),
    tar_target(osteo_derived, derive_osteo(osteo_long))
)


cc_select_targets <- list(
    tar_target(colaus_selected, select_analysis_columns(colaus_derived)),
    tar_target(osteo_selected, select_analysis_columns(osteo_derived))
)


# 08 CC. Wave match — nearest-date merge of CoLaus into OsteoLaus backbone
# ------------------------------------------------------------------------------
cc_merge_targets <- list(
    
    tar_target(merged_cc,       merge_closest_exams(colaus_selected, osteo_selected)),
    tar_target(merged_cc_data,  merged_cc$data),
    tar_target(merged_cc_qc,    merged_cc$qc)
)


# 09 CC. Derive combined variables (sarcopenia staging)
# ------------------------------------------------------------------------------
cc_derive_combined_targets <- list(
    tar_target(merged_cc_derived, derive_combined(merged_cc_data))
)


# 10 CC. Apply exclusions — produces annotated dataset + excluded companion
# ------------------------------------------------------------------------------
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

# 06 MICE. Impute missing values
# ------------------------------------------------------------------------------
mice_impute_targets <- list(
    
    tar_target(colaus_imp,
               impute_mice_colaus(colaus_long,
                                  m     = 5L,
                                  maxit = 20L,
                                  seed  = 2024L
        )
    ),

    tar_target(osteo_imp,
               impute_mice_osteo(osteo_long,
                                 m     = 5L,
                                 maxit = 20L,
                                 seed  = 2024L
        )
    ),
    
    tar_target(colaus_imp_check, post_imputation_checks(colaus_imp, "06_outputs/imputation_colaus")),
    tar_target(osteo_imp_check, post_imputation_checks(osteo_imp, "06_outputs/imputation_osteo"))
    
)


# 07 MICE. Derive variables for each imputed dataset
# ------------------------------------------------------------------------------
mice_derive_targets <- list(
    
    tar_target(
        colaus_imp_derived,
        mice_derive_colaus(colaus_imp)
    ),

    tar_target(
        osteo_imp_derived,
        mice_derive_osteo(osteo_imp)
    )
)



# 08 MICE. Delete columns that are not needed
# ------------------------------------------------------------------------------

mice_targets_selected <- list(
    tar_target(
        mice_selected_colaus,
        select_analysis_columns(colaus_imp_derived)
    ),
    tar_target(
        mice_selected_osteo,
        select_analysis_columns(osteo_imp_derived)
    )
    
)

# 09 MICE. Merge with OsteoLaus + derive combined variables (per .imp)
# ------------------------------------------------------------------------------
mice_merge_targets <- list(
    
    tar_target(
        mice_analysis_long,
        mice_merge_derive(
            df_colaus = mice_selected_colaus,
            df_osteo  = mice_selected_osteo
        )
    )
)



# 10 MICE. Apply exclusions across all m imputed datasets
# ------------------------------------------------------------------------------
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
# Descriptives
# =============================================================================

comp_by_visit <- list(
    tar_target(cc_descriptive_completeness, completeness_by_visit(merged_cc$data))
)


# =============================================================================
# ASSEMBLE
# =============================================================================

c(
    # ── Shared steps (01–04) ──────────────────────────────────────────────────
    path_targets,
    import_targets,
    harmonise_targets,
    qc_targets,
    stack_targets,
    #qc_variable_targets,
    
    # # ── Complete-case route ───────────────────────────────────────────────────
    cc_derive_targets,
    cc_select_targets,
    cc_merge_targets,
    cc_derive_combined_targets,
    cc_exclusion_targets,
    # comp_by_visit
    
    # ── MICE route ────────────────────────────────────────────────────────────
    mice_impute_targets,
    mice_derive_targets,
    mice_targets_selected,
    mice_merge_targets,
    mice_exclusion_targets
)