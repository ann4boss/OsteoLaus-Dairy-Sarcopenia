# =============================================================================
# _targets.R
# CoLaus / OsteoLaus sarcopenia & dairy intake pipeline
# =============================================================================
#
# Pipeline stages
# TODO add description
# =============================================================================

library(targets)
library(tarchetypes)

tar_option_set(
    packages = c(
        "dplyr", "tidyverse", "readr", "dtplyr", "data.table", "tidyr",
        "stringr", "magrittr", "lubridate", "forcats", "purrr", "glue",
        "tibble", "cli",
        "mice", "here",
        "gtsummary", "ggplot2", "patchwork", "scales", "ggridges", "ggalluvial",
        "RColorBrewer",
        "lme4", "lmerTest", "broom.mixed", "broom", "rms", "splines",
        "survival", "survminer", "gt", "survey",
        "mgcv", "car",
        "sessioninfo",
        "WeightIt", "cobalt", "geepack"
    )
)

tar_source("02_scripts/R")


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
               format = "file"),
    
    tar_target(f_colaus_baseline_add_food,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Baseline_additionalFood.csv",
               format = "file"),
    tar_target(f_colaus_f1_add_food,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/FU1_additionalFood.csv",
               format = "file"),
    tar_target(f_colaus_f2_add_food,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/FU2_additionalFood.csv",
               format = "file"),
    tar_target(f_colaus_f3_add_food,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/FU3_additionalFood.csv",
               format = "file"),
    
    tar_target(f_colaus_death,
               "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Deaths.csv",
               format = "file")
)


# =============================================================================
# 01. Prepare Core
# =============================================================================

prep_core <- tar_target(
    core,
    prepare_core(
        f_colaus_baseline, f_colaus_f1, f_colaus_f2, f_colaus_f3,
        f_osteo_baseline,  f_osteo_v2,  f_osteo_v3,  f_osteo_v4, f_osteo_v5,
        f_colaus_baseline_add_food, f_colaus_f1_add_food,
        f_colaus_f2_add_food, f_colaus_f3_add_food,
        f_colaus_death
    )
)


# =============================================================================
# Shared: covariate sets and exclusion arguments
# (defined once, used by both CC and MICE exclusion targets)
# =============================================================================

.OUTCOMES <- c(
    "ewgsop2_sarcopenia_stage",
    "fnih_sarcopenia",
    "gait_speed",
    "ALM_HT2_harmonised",
    "HGS_MAX"
)

.COVARIATES <- list(
    HGS_MAX = c(
        "Age", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status", "sumtot1"
    ),
    gait_speed = c(
        "Age", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status"
    ),
    ALM_HT2_harmonised = c(
        "Age", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status", "sumtot1"
    ),
    ewgsop2_sarcopenia_stage = c(
        "Age", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status"
    ),
    fnih_sarcopenia = c(
        "Age", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status"
    )
)


# =============================================================================
# ── COMPLETE-CASE (CC) ROUTE ──────────────────────────────────────────────────
# =============================================================================
#
# Stage 1 → core (shared)
# Stage 2 → cc_colaus_derived, cc_osteo_derived   [derive()]
# Stage 3 → cc_colaus_selected, cc_osteo_selected [select_analysis_columns()]
# Stage 4 → cc_merged                             [merge_visit_pairs()]
# Stage 5 → cc_merged_derived                     [derive_combined()]
# Stage 6 → cc_analysis                           [apply_exclusions()]

cc_prep_targets <- list(
    
    tar_target(
        cc_colaus_derived,
        derive(core$colaus_long)
    ),
    
    tar_target(
        cc_osteo_derived,
        derive(core$osteo_long)
    ),
    
    tar_target(
        cc_colaus_selected,
        select_analysis_columns(cc_colaus_derived)
    ),
    
    tar_target(
        cc_osteo_selected,
        select_analysis_columns(cc_osteo_derived)
    ),
    
    tar_target(
        cc_merged,
        merge_visit_pairs(cc_colaus_selected, cc_osteo_selected)
    ),
    
    tar_target(
        cc_merged_derived,
        derive_combined(cc_merged$data)
    )
)

cc_exclusion <- tar_target(
    cc_analysis,
    apply_exclusions(
        data       = cc_merged_derived,
        qc_table   = core$qc_tbl,
        outcomes   = .OUTCOMES,
        covariates = .COVARIATES,
        visit_min  = 2L,
        pt_col     = "pt",
        visit_col  = "time_point",
        exposure   = "dairy_total_gday_cumavg"
    )
)


# =============================================================================
# ── MICE ROUTE ────────────────────────────────────────────────────────────────
# =============================================================================
#
# Stage 1 → core (shared)
# Stage 2 → mice_colaus_imp, mice_osteo_imp       [impute_mice_*()]
# Stage 3 → mice_colaus_derived, mice_osteo_derived [derive()]          mids→mids
# Stage 4 → mice_colaus_selected, mice_osteo_selected [select_analysis_columns()] mids→mids
# Stage 5 → mice_merged                           [merge_visit_pairs()] mids×mids→list(mids,qc)
# Stage 6 → mice_merged_derived                   [derive_combined()]   mids→mids
# Stage 7 → mice_analysis                         [apply_exclusions()]

mice_prep_targets <- list(
    
    # ── Stage 2: Imputation ──────────────────────────────────────────────────
    tar_target(
        mice_colaus_imp,
        impute_mice_colaus(
            core$colaus_long,
            m     = 5L,
            maxit = 30L,
            seed  = 2024L
        )
    ),
    
    tar_target(
        mice_osteo_imp,
        impute_mice_osteo(
            core$osteo_long,
            m     = 5L,
            maxit = 30L,
            seed  = 2024L
        )
    ),
    
    # ── Stage 3: Derive ──────────────────────────────────────────────────────
    # derive() accepts a mids and returns a mids with derived columns.
    tar_target(
        mice_colaus_derived,
        derive(mice_colaus_imp$mids)
    ),
    
    tar_target(
        mice_osteo_derived,
        derive(mice_osteo_imp$mids)
    ),
    
    # ── Stage 4: Select analysis columns ─────────────────────────────────────
    # select_analysis_columns() accepts a mids and returns a mids.
    tar_target(
        mice_colaus_selected,
        select_analysis_columns(mice_colaus_derived$mids)
    ),
    
    tar_target(
        mice_osteo_selected,
        select_analysis_columns(mice_osteo_derived$mids)
    ),
    
    # ── Stage 5: Merge visit pairs ───────────────────────────────────────────
    # merge_visit_pairs() accepts two mids and returns list(mids, qc).
    tar_target(
        mice_merged,
        merge_visit_pairs(mice_colaus_selected, mice_osteo_selected)
    ),
    
    # ── Stage 6: Derive combined variables ───────────────────────────────────
    # derive_combined() accepts a mids and returns a mids.
    tar_target(
        mice_merged_derived,
        derive_combined(mice_merged$mids)
    )
)

mice_exclusion <- tar_target(
    mice_analysis,
    apply_exclusions(
        data       = mice_merged_derived,
        qc_table   = core$qc_tbl,
        outcomes   = .OUTCOMES,
        covariates = .COVARIATES,
        visit_min  = 2L,
        pt_col     = "pt",
        visit_col  = "time_point",
        exposure   = "dairy_total_gday_cumavg"
    )
)


# =============================================================================
# LMM TARGETS
# =============================================================================

covariate_sets_hgs <- list(
    minimal = c(
        "age_at_baseline", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status", "sumtot1"
    ),
    other_PA = c(
        "age_at_baseline", "BMI_category", "education_level", "smoking_status",
        "pa_levels_who_f1", "diabetes_status", "sumtot1"
    )
)

covariate_sets_alm <- list(
    minimal = c(
        "age_at_baseline", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status", "sumtot1"
    ),
    other_PA = c(
        "age_at_baseline", "BMI_category", "education_level", "smoking_status",
        "pa_levels_who_f1", "diabetes_status", "sumtot1"
    )
)

covariate_sets_gait <- list(
    minimal = c(
        "age_at_baseline", "BMI_category_lag", "education_level_lag",
        "smoking_status_lag", "pa_levels_tertile_f1_lag", "diabetes_status_lag"
    ),
    other_PA = c(
        "age_at_baseline", "BMI_category_lag", "education_level_lag",
        "smoking_status_lag", "pa_levels_who_f1_lag", "diabetes_status_lag"
    )
)

LLM_targets_HGS <- list(
    tar_target(
        lmm_model_grid_hgs,
        create_model_grid(
            outcomes             = c("HGS_MAX"),
            exposure_definitions = exposure_definitions,
            datasets             = c("cc", "mice"),
            interactions         = c(TRUE, FALSE),
            random_slopes        = c(FALSE, TRUE),
            cov_sets             = c("minimal", "other_PA")
        )
    ),
    tar_target(
        lmm_results_hgs,
        run_lmm_model(
            config         = lmm_model_grid_hgs,
            cc_data        = cc_analysis$data$HGS_MAX,
            mice_data      = mice_analysis$data$HGS_MAX,
            covariate_sets = covariate_sets_hgs,
            id_var         = "pt",
            time_var       = "time_since_baseline"
        ),
        pattern = map(lmm_model_grid_hgs)
    ),
    tar_target(
        lmm_exports_hgs,
        export_lmm_results(lmm_results_hgs),
        pattern = map(lmm_results_hgs),
        format  = "file"
    ),
    tar_target(
        lmm_diagnostics_hgs,
        run_lmm_diagnostics(lmm_results_hgs),
        pattern = map(lmm_results_hgs)
    )
)

LLM_targets_ALM <- list(
    tar_target(
        lmm_model_grid_alm,
        create_model_grid(
            outcomes             = c("ALM_HT2_harmonised"),
            exposure_definitions = exposure_definitions,
            datasets             = c("cc", "mice"),
            interactions         = c(TRUE, FALSE),
            random_slopes        = c(FALSE, TRUE),
            cov_sets             = c("minimal", "other_PA")
        )
    ),
    tar_target(
        lmm_results_alm,
        run_lmm_model(
            config         = lmm_model_grid_alm,
            cc_data        = cc_analysis$data$ALM_HT2_harmonised,
            mice_data      = mice_analysis$data$ALM_HT2_harmonised,
            covariate_sets = covariate_sets_alm,
            id_var         = "pt",
            time_var       = "time_since_baseline"
        ),
        pattern = map(lmm_model_grid_alm)
    ),
    tar_target(
        lmm_exports_alm,
        export_lmm_results(lmm_results_alm),
        pattern = map(lmm_results_alm),
        format  = "file"
    ),
    tar_target(
        lmm_diagnostics_alm,
        run_lmm_diagnostics(lmm_results_alm),
        pattern = map(lmm_results_alm)
    )
)

LLM_targets_gait <- list(
    tar_target(
        lmm_model_grid_gait,
        create_model_grid(
            outcomes             = c("gait_speed"),
            exposure_definitions = exposure_definitions_gait,
            datasets             = c("cc", "mice"),
            interactions         = c(TRUE, FALSE),
            random_slopes        = c(FALSE, TRUE),
            cov_sets             = c("minimal", "other_PA")
        )
    ),
    tar_target(
        lmm_results_gait,
        run_lmm_model(
            config         = lmm_model_grid_gait,
            cc_data        = cc_analysis$data$gait_speed,
            mice_data      = mice_analysis$data$gait_speed,
            covariate_sets = covariate_sets_gait,
            id_var         = "pt",
            time_var       = "time_since_baseline"
        ),
        pattern = map(lmm_model_grid_gait)
    ),
    tar_target(
        lmm_exports_gait,
        export_lmm_results(lmm_results_gait),
        pattern = map(lmm_results_gait),
        format  = "file"
    ),
    tar_target(
        lmm_diagnostics_gait,
        run_lmm_diagnostics(lmm_results_gait),
        pattern = map(lmm_results_gait)
    )
)


# =============================================================================
# COX
# =============================================================================

cox_targets <- list(
    
    tar_target(
        cc_ewgsop2_fixed_cont,
        run_cox_sarcopenia(
            data           = cc_analysis$data$ewgsop2_sarcopenia_stage,
            sarcopenia_def = "ewgsop2",
            covariate_type = "fixed",
            dairy_type     = "continuous",
            dairy_col      = "dairy_total_gday_cumavg",
            analysis_route = "cc"
        )
    ),
    tar_target(cox_outliers_cc,     cc_ewgsop2_fixed_cont$outlier_flagged),
    tar_target(cox_influential_cc,  cc_ewgsop2_fixed_cont$dfbeta_flag_detail),
    tar_target(cox_results_cc_cont, cc_ewgsop2_fixed_cont$results_adj),
    
    tar_target(
        mice_ewgsop2_fixed_cat,
        run_cox_sarcopenia(
            data           = mice_analysis$data$ewgsop2_sarcopenia_stage,
            sarcopenia_def = "ewgsop2",
            covariate_type = "fixed",
            dairy_type     = "categorical",
            dairy_cat_col  = "dairy_quartile_baseline",
            analysis_route = "mice"
        )
    ),
    tar_target(cox_outliers_mice_cat,    mice_ewgsop2_fixed_cat$outlier_flagged),
    tar_target(cox_influential_mice_cat, mice_ewgsop2_fixed_cat$dfbeta_flag_detail),
    tar_target(cox_results_mice_cat,     mice_ewgsop2_fixed_cat$results_adj),
    tar_target(cox_km_mice_cat,          mice_ewgsop2_fixed_cat$km_plot)
)


# =============================================================================
# Descriptive
# =============================================================================




# =============================================================================
# ASSEMBLE
# =============================================================================

c(
    # ── Shared ────────────────────────────────────────────────────────────────
    path_targets,
    prep_core,
    
    # ── Complete-case route ───────────────────────────────────────────────────
    cc_prep_targets,
    cc_exclusion,
    # cc_descriptives,
    # table_one,
    
    # ── MICE route ────────────────────────────────────────────────────────────
    mice_prep_targets,
    mice_exclusion
    # mice_table_one,
    
    # ── Models ────────────────────────────────────────────────────────────────
    # LLM_targets_HGS,
    # LLM_targets_ALM,
    # LLM_targets_gait,
    # cox_targets,
    
    # ── Descriptives ──────────────────────────────────────────────────────────
    # consort
)