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
        "gtsummary", "ggplot2", "patchwork", "gridExtra", "scales", "ggridges", "ggalluvial",
        "RColorBrewer",
        "nlme",
        "lme4", "lmerTest", "broom.mixed", "broom", "rms", "splines", "clubSandwich",
        "survival", "survminer", "gt", "survey",
        "mgcv", "car",
        "sessioninfo",
        "WeightIt", "cobalt", "geepack",
        "DiagrammeR", "DiagrammeRsvg", "rsvg",
        "performance"
        
    )
)



tar_source("02_scripts/R")


# =============================================================================
# 00. FILE PATHS
# =============================================================================
#TODO make paths relative for last GitHub update
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
# Covariate sets and exclusion arguments
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
        "Age_lag", "BMI_category_lag", "education_level_lag", "smoking_status_lag",
        "pa_levels_tertile_f1_lag", "diabetes_status_lag"
    ),
    ALM_HT2_harmonised = c(
        "Age", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status","sumtot1"
    ),
    ewgsop2_sarcopenia_stage = c(
        "Age", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status"
    ),
    fnih_sarcopenia = c(
        "Age", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status")
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
        derive(core$colaus_long,
               log_pdf = "03_outputs/logs/derive_cc_colaus.pdf")
    ),

    tar_target(
        cc_osteo_derived,
        derive(core$osteo_long,
               log_pdf = "03_outputs/logs/derive_cc_osteo.pdf")
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
        derive_combined(cc_merged$data,
                        log_pdf = "03_outputs/logs/derive_combined_cc.pdf")
    )
)

cc_exclusion <- tar_target(
    cc_analysis,
    {
        res      <- run_exclusions(
            data       = cc_merged_derived,
            qc_tbl     = core$qc_tbl,
            outcomes   = .OUTCOMES,
            covariates = .COVARIATES,
            shared_covariates = c("Age", "BMI_category", "education_level", "smoking_status",
                                  "pa_levels_tertile_f1", "diabetes_status"),
            min_visit  = 2L,
            pt_col     = "pt",
            visit_col  = "time_point",
            exposure   = "dairy_total_gday_cumavg"
        )
        # $data and $mids aliases keep downstream targets unchanged.
        # For the CC route $data_outcome holds plain data frames; mids is NULL.
        res$data <- res$data_outcome
        res$mids <- stats::setNames(
            vector("list", length(.OUTCOMES)), .OUTCOMES
        )
        res
    }
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
            m     = 20L,
            maxit = 20L,
            seed  = 2024L
        )
    ),
    
    tar_target(
        mice_osteo_imp,
        impute_mice_osteo(
            core$osteo_long,
            m     = 20L,
            maxit = 20L,
            seed  = 2024L
        )
    ),
    
    # ── Stage 3: Derive ──────────────────────────────────────────────────────
    # derive() accepts a mids and returns a mids with derived columns.
    tar_target(
        mice_colaus_derived,
        derive(mice_colaus_imp$mids,
               log_pdf = "03_outputs/logs/derive_mice_colaus.pdf")
    ),

    tar_target(
        mice_osteo_derived,
        derive(mice_osteo_imp$mids,
               log_pdf = "03_outputs/logs/derive_mice_osteo.pdf")
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
        derive_combined(mice_merged$mids,
                        log_pdf = "03_outputs/logs/derive_combined_mice.pdf")
    )
)

mice_exclusion <- tar_target(
    mice_analysis,
    {
        res      <- run_exclusions(
            data       = mice_merged_derived,
            qc_tbl     = core$qc_tbl,
            outcomes   = .OUTCOMES,
            covariates = .COVARIATES,
            shared_covariates = c("Age", "BMI_category", "education_level", "smoking_status",
                                  "pa_levels_tertile_f1", "diabetes_status"),
            min_visit  = 2L,
            pt_col     = "pt",
            visit_col  = "time_point",
            exposure   = "dairy_total_gday_cumavg"
        )
        # $data_outcome holds mids objects for the MICE route.
        # $data and $mids aliases keep all downstream targets unchanged.
        res$data <- res$data_outcome
        res$mids <- res$data_outcome
        res
    }
)


# =============================================================================
# Main LMM TARGETS
# =============================================================================

# ── Covariate sets ────────────────────────────────────
covariate_sets_hgs <- list(
    main = c(
        "age_at_baseline_scaled", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status", "sumtot1_scaled"
    ),
    other_PA = c(
        "age_at_baseline_scaled", "BMI_category", "education_level", "smoking_status",
        "pa_levels_who_f1", "diabetes_status", "sumtot1_scaled"
    )
)

covariate_sets_alm <- list(
    main = c(
        "age_at_baseline_scaled", "BMI_category", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status", "sumtot1_scaled"
    ),
    other_PA = c(
        "age_at_baseline_scaled", "BMI_category", "education_level", "smoking_status",
        "pa_levels_who_f1", "diabetes_status", "sumtot1_scaled"
    )
)

covariate_sets_alm_bmi <- list(
    other_outcome = c(
        "age_at_baseline_scaled", "education_level", "smoking_status",
        "pa_levels_tertile_f1", "diabetes_status", "sumtot1_scaled"
    )
)

covariate_sets_gait <- list(
    main = c(
        "age_at_baseline_scaled_lag", "BMI_category_lag", "education_level_lag", "smoking_status_lag",
        "pa_levels_tertile_f1_lag", "diabetes_status_lag"
    ),
    other_PA = c(
        "age_at_baseline_scaled_lag", "BMI_category_lag", "education_level_lag", "smoking_status_lag",
        "pa_levels_who_f1_lag", "diabetes_status_lag"
    )
)

# ── Exposure definitions ────────────────────────────────────
exposure_definitions <- tibble::tribble(
    ~exposure,                  ~exposure_type, ~ref_level,
    
    "dairy_100g",         "linear",       NA,
    "fermented_100g",         "linear",       NA,
    "nonfermented_100g",         "linear",       NA,
    "highfat_100g",         "linear",       NA,
    "lowfat_100g",         "linear",       NA,
    
    "dairy_quartile_baseline",  "categorical",  "Q1",
    
    "dairy_guidelines_port",    "categorical",  "< 2 servings/day"
)

exposure_definitions_gait <- tibble::tribble(
    ~exposure,                  ~exposure_type, ~ref_level,
    
    "dairy_100g_lag",         "linear",       NA,
    "fermented_100g_lag",         "linear",       NA,
    "nonfermented_100g_lag",         "linear",       NA,
    "highfat_100g_lag",         "linear",       NA,
    "lowfat_100g_lag",         "linear",       NA,
    
    "dairy_quartile_baseline_lag",  "categorical",  "Q1",
    
    "dairy_guidelines_port_lag",    "categorical",  "< 2 servings/day"
)


# ── Main LMM ────────────────────────────────────
LMM_targets_HGS <- tar_target(
    lmm_report_hgs,
    run_lmm_report(
        mids_object    = mice_analysis$mids$HGS_MAX,
        outcome        = "HGS_MAX",
        outcome_fn     = identity,
        exposures      = exposure_definitions,
        covariate_sets = covariate_sets_hgs,
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/HGS"
    ),
    format = "file")


LMM_targets_ALM <- tar_target(
    lmm_report_alm,
    run_lmm_report(
        mids_object    = mice_analysis$mids$ALM_HT2_harmonised,
        outcome        = "ALM_HT2_harmonised",
        outcome_fn     = identity,
        exposures      = exposure_definitions,
        covariate_sets = covariate_sets_alm,
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/ALMI"
    ),
    format = "file")

LMM_targets_ALM_BMI <- tar_target(
    lmm_report_alm_bmi,
    run_lmm_report(
        mids_object    = mice_analysis$mids$ALM_HT2_harmonised,
        outcome        = "ALM_BMI_harmonised",
        outcome_fn     = identity,
        exposures      = exposure_definitions,
        covariate_sets = covariate_sets_alm_bmi,
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/ALMI_BMI"
    ),
    format = "file")


LMM_targets_gait <- tar_target(
    lmm_report_gait,
    run_lmm_report_gait(
        mids_object    = mice_analysis$mids$gait_speed,
        outcome        = "gait_speed",
        outcome_fn     = identity,
        exposures      = exposure_definitions_gait,
        covariate_sets = covariate_sets_gait,
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/gait_speed"
    ),
    format = "file")

# =============================================================================
# Further LMM TARGETS
# =============================================================================
# DAIRY PROTEIN CONTENT — exposures & covariate sets
# 
# Does the muscle-outcome association depend on the protein content of the
# dairy product, not just its weight (100g/day)?
#
#   1. exposure_definitions_ffq_items      — one row per individual dairy FFQ
#                                             item (17), to see whether some
#                                             products drive the association
#                                             more than others.
#   2. covariate_sets_*_adj_nondairy_protein — main covariate set + non-dairy
#                                             protein intake, to check the
#                                             dairy association is not simply
#                                             explained by total protein.
#   3. exposure_definitions_dairy_protein  — dairy protein content
#                                             (prot_content_dairy_100g) used in
#                                             place of dairy_100g as exposure.
# =============================================================================

exposure_definitions_ffq_items <- tibble::tribble(
    ~exposure,                     ~exposure_type, ~ref_level,
    
    "FFQ1amount_cumavg_100g",      "linear",       NA,
    "FFQ2amount_cumavg_100g",      "linear",       NA,
    "FFQ3amount_cumavg_100g",      "linear",       NA,
    "FFQ4amount_cumavg_100g",      "linear",       NA,
    "FFQ5amount_cumavg_100g",      "linear",       NA,
    "FFQ6amount_cumavg_100g",      "linear",       NA,
    "FFQ7amount_cumavg_100g",      "linear",       NA,
    "FFQ8amount_cumavg_100g",      "linear",       NA,
    "FFQ52amount_cumavg_100g",     "linear",       NA,
    "FFQ53amount_cumavg_100g",     "linear",       NA,
    "FFQ63amount_cumavg_100g",     "linear",       NA,
    "FFQ68amount_cumavg_100g",     "linear",       NA,
    "FFQ71amount_cumavg_100g",     "linear",       NA,
    "FFQ82amount_cumavg_100g",     "linear",       NA,
    "FFQ83amount_cumavg_100g",     "linear",       NA,
    "FFQ84amount_cumavg_100g",     "linear",       NA,
    "FFQ85amount_cumavg_100g",     "linear",       NA,
    "FFQ86amount_cumavg_100g",     "linear",       NA
)

exposure_definitions_dairy_protein <- tibble::tribble(
    ~exposure,                  ~exposure_type, ~ref_level,
    
    "prot_content_dairy_100g",  "linear",       NA
)

# Total dairy intake excluding FFQ8 (cheese fondue) — sensitivity exposure to
# check the dairy_100g association isn't driven by fondue.
exposure_definitions_dairy_nofondue <- tibble::tribble(
    ~exposure,               ~exposure_type, ~ref_level,
    
    "dairy_nofondue_100g",   "linear",       NA
)

# main covariate set with total calorie intake (sumtot1_scaled) swapped out
# for non-dairy protein intake — adjusting for both would double up on the
# same dietary signal (total calories are largely driven by total protein).
covariate_sets_hgs_adj_nondairy_protein <- list(
    main_adj_nondairy_protein = c(
        setdiff(covariate_sets_hgs$main, "sumtot1_scaled"),
        "prot_content_nondairy_10g"
    )
)

covariate_sets_alm_adj_nondairy_protein <- list(
    main_adj_nondairy_protein = c(
        setdiff(covariate_sets_alm$main, "sumtot1_scaled"),
        "prot_content_nondairy_10g"
    )
)

# ── Individual dairy FFQ items: does one product drive the association? ─────
LMM_targets_HGS_ffq_items <- tar_target(
    lmm_report_hgs_ffq_items,
    run_lmm_report(
        mids_object    = mice_analysis$mids$HGS_MAX,
        outcome        = "HGS_MAX",
        outcome_fn     = identity,
        exposures      = exposure_definitions_ffq_items,
        covariate_sets = list(main = covariate_sets_hgs$main),
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/HGS_FFQ_items"
    ),
    format = "file")

LMM_targets_ALM_ffq_items <- tar_target(
    lmm_report_alm_ffq_items,
    run_lmm_report(
        mids_object    = mice_analysis$mids$ALM_HT2_harmonised,
        outcome        = "ALM_HT2_harmonised",
        outcome_fn     = identity,
        exposures      = exposure_definitions_ffq_items,
        covariate_sets = list(main = covariate_sets_alm$main),
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/ALMI_FFQ_items"
    ),
    format = "file")

# ── Adjusted for non-dairy protein intake ────────────────────────────────────
LMM_targets_HGS_adj_nondairy_protein <- tar_target(
    lmm_report_hgs_adj_nondairy_protein,
    run_lmm_report(
        mids_object    = mice_analysis$mids$HGS_MAX,
        outcome        = "HGS_MAX",
        outcome_fn     = identity,
        exposures      = exposure_definitions,
        covariate_sets = covariate_sets_hgs_adj_nondairy_protein,
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/HGS_adj_nondairy_protein"
    ),
    format = "file")

LMM_targets_ALM_adj_nondairy_protein <- tar_target(
    lmm_report_alm_adj_nondairy_protein,
    run_lmm_report(
        mids_object    = mice_analysis$mids$ALM_HT2_harmonised,
        outcome        = "ALM_HT2_harmonised",
        outcome_fn     = identity,
        exposures      = exposure_definitions,
        covariate_sets = covariate_sets_alm_adj_nondairy_protein,
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/ALMI_adj_nondairy_protein"
    ),
    format = "file")

# ── Dairy protein content as exposure (instead of 100g dairy weight) ────────
LMM_targets_HGS_dairy_protein_exposure <- tar_target(
    lmm_report_hgs_dairy_protein_exposure,
    run_lmm_report(
        mids_object    = mice_analysis$mids$HGS_MAX,
        outcome        = "HGS_MAX",
        outcome_fn     = identity,
        exposures      = exposure_definitions_dairy_protein,
        covariate_sets = covariate_sets_hgs,
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/HGS_dairy_protein_exposure"
    ),
    format = "file")

LMM_targets_ALM_dairy_protein_exposure <- tar_target(
    lmm_report_alm_dairy_protein_exposure,
    run_lmm_report(
        mids_object    = mice_analysis$mids$ALM_HT2_harmonised,
        outcome        = "ALM_HT2_harmonised",
        outcome_fn     = identity,
        exposures      = exposure_definitions_dairy_protein,
        covariate_sets = covariate_sets_alm,
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/ALMI_dairy_protein_exposure"
    ),
    format = "file")

# ── Total dairy intake excluding FFQ8 (cheese fondue) ────────────────────────
LMM_targets_HGS_dairy_nofondue <- tar_target(
    lmm_report_hgs_dairy_nofondue,
    run_lmm_report(
        mids_object    = mice_analysis$mids$HGS_MAX,
        outcome        = "HGS_MAX",
        outcome_fn     = identity,
        exposures      = exposure_definitions_dairy_nofondue,
        covariate_sets = covariate_sets_hgs,
        random_slope   = FALSE,
        interaction    = FALSE,
        out_dir        = "03_outputs/LMM_exploratory/HGS_dairy_nofondue"
    ),
    format = "file")




# =============================================================================
# MODEL SPECIFICATION SENSITIVITY TARGETS
# =============================================================================

LMM_modspec_HGS <- tar_target(
    modspec_hgs,
    run_model_specification_sensitivity(
        mids_object  = mice_analysis$mids$HGS_MAX,
        outcome      = "HGS_MAX",
        outcome_fn   = identity,
        covariates   = covariate_sets_hgs$main,
        random_slope = TRUE,
        out_dir      = "03_outputs/model_specification_sensitivity/HGS"
    ),
    format = "file"
)

LMM_modspec_ALM <- tar_target(
    modspec_alm,
    run_model_specification_sensitivity(
        mids_object  = mice_analysis$mids$ALM_HT2_harmonised,
        outcome      = "ALM_HT2_harmonised",
        outcome_fn   = log,
        covariates   = covariate_sets_alm$main,
        random_slope = TRUE,
        out_dir      = "03_outputs/model_specification_sensitivity/ALMI"
    ),
    format = "file"
)

LMM_modspec_gait <- tar_target(
    modspec_gait,
    run_model_specification_sensitivity_gait(
        mids_object  = mice_analysis$mids$gait_speed,
        outcome      = "gait_speed",
        outcome_fn   = identity,
        covariates   = covariate_sets_gait$main,
        random_slope = FALSE,
        out_dir      = "03_outputs/model_specification_sensitivity/gait_speed"
    ),
    format = "file"
)



# =============================================================================
# COX MODEL
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
        mice_ewgsop2_fixed_continuous,
        run_cox_sarcopenia(
            data           = mice_analysis$data$ewgsop2_sarcopenia_stage,
            sarcopenia_def = "ewgsop2",
            covariate_type = "fixed",
            dairy_type     = "continuous",
            dairy_cat_col  = "dairy_total_gday_cumavg",
            analysis_route = "mice"
        )
    ),
    tar_target(cox_outliers_mice_cat,    mice_ewgsop2_fixed_cat$outlier_flagged),
    tar_target(cox_influential_mice_cat, mice_ewgsop2_fixed_cat$dfbeta_flag_detail),
    tar_target(cox_results_mice_cat,     mice_ewgsop2_fixed_cat$results_adj),
    tar_target(cox_km_mice_cat,          mice_ewgsop2_fixed_cat$km_plot),
    
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
    tar_target(
        mice_ewgsop2_timedep_cat,
        run_cox_sarcopenia(
            data           = mice_analysis$data$ewgsop2_sarcopenia_stage,
            sarcopenia_def = "ewgsop2",
            covariate_type = "time_dependent",
            dairy_type     = "categorical",
            dairy_cat_col  = "dairy_quartile_baseline",
            analysis_route = "mice"
        )
    ),
    tar_target(
        mice_ewgsop2_timedep_con,
        run_cox_sarcopenia(
            data           = mice_analysis$data$ewgsop2_sarcopenia_stage,
            sarcopenia_def = "ewgsop2",
            covariate_type = "time_dependent",
            dairy_type     = "continuous",
            dairy_cat_col  = "dairy_total_gday_cumavg",
            analysis_route = "mice"
        )
    ),
    tar_target(
        mice_ewgsop2_fixed_cont_spline,
        run_cox_sarcopenia(
            data           = mice_analysis$data$ewgsop2_sarcopenia_stage,
            sarcopenia_def = "ewgsop2",
            covariate_type = "fixed",
            dairy_type     = "continuous",
            analysis_route = "mice",
            spline_df      = 3 
        )
    ),
    tar_target(
        mice_ewgsop2_timdep_cont_spline,
        run_cox_sarcopenia(
            data           = mice_analysis$data$ewgsop2_sarcopenia_stage,
            sarcopenia_def = "ewgsop2",
            covariate_type = "time_dependent",
            dairy_type     = "continuous",
            analysis_route = "mice",
            spline_df      = 3 
        )
    )
)

cox_targets_fnih <- list(
    
    tar_target(
        cc_fnih_fixed_cont,
        run_cox_sarcopenia(
            data           = cc_analysis$data$fnih_sarcopenia,
            sarcopenia_def = "fnih",
            covariate_type = "fixed",
            dairy_type     = "continuous",
            dairy_col      = "dairy_total_gday_cumavg",
            analysis_route = "cc"
        )
    ),
    tar_target(
        mice_fnih_fixed_cat,
        run_cox_sarcopenia(
            data           = mice_analysis$data$fnih_sarcopenia,
            sarcopenia_def = "fnih",
            covariate_type = "fixed",
            dairy_type     = "continuous",
            dairy_cat_col  = "dairy_total_gday_cumavg",
            analysis_route = "mice"
        )
    ),
 
    tar_target(
        mice_fnih_fixed_cat_quartile,
        run_cox_sarcopenia(
            data           = mice_analysis$data$fnih_sarcopenia,
            sarcopenia_def = "fnih",
            covariate_type = "fixed",
            dairy_type     = "categorical",
            dairy_cat_col  = "dairy_quartile_baseline",
            analysis_route = "mice"
        )
    ),
    tar_target(
        mice_fnih_timedep,
        run_cox_sarcopenia(
            data           = mice_analysis$data$fnih_sarcopenia,
            sarcopenia_def = "fnih",
            covariate_type = "time_dependent",
            dairy_type     = "categorical",
            dairy_cat_col  = "dairy_quartile_baseline",
            analysis_route = "mice"
        )
    ),
    tar_target(
        mice_fnih_timedep_continuous,
        run_cox_sarcopenia(
            data           = mice_analysis$data$fnih_sarcopenia,
            sarcopenia_def = "fnih",
            covariate_type = "time_dependent",
            dairy_type     = "continuous",
            dairy_cat_col  = "dairy_total_gday_cumavg",
            analysis_route = "mice"
        )
    )
)


# =============================================================================
# Descriptive
# =============================================================================

consort <- tar_target(consort_flowchart, create_consort_flowchart())


#------------------------------------------------------------------------------


tableOne_targets_1 <-
    tar_target(tableOne_quartile,
        make_table_one_by_exposure(mids_to_long(mice_analysis$data_shared),
            by    = "dairy_quartile_baseline",
            visit = "time_point"
        )
    )

    # Analysis populations as columns (HGS vs ALM vs gait speed).
    # Each dataset's baseline is its minimum time_point (HGS/ALM → T1,
    # gait speed → T3 after lagging). All needed columns are present in
    # the analysis datasets.
tableOne_targets_2 <- tar_target(tableOne_datasets,
        make_table_one_by_dataset(
            list(
                HGS          = mids_to_long(mice_analysis$mids$HGS_MAX),
                ALM          = mids_to_long(mice_analysis$mids$ALM_HT2_harmonised),
                `Gait speed` = mids_to_long(mice_analysis$mids$gait_speed),
                Shared       = mids_to_long(mice_analysis$data_shared)
            ),
            visit = "time_point"
        )
    )

tableOne_targets_compare <- tar_target(tableOne_datasets_excluded,
                                 make_table_one_by_dataset(
                                     list(
                                         Shared       = mice_analysis$data_shared,
                                         Excluded    = mice_analysis$data_excluded_shared
                                     ),
                                     visit = "time_point"
                                 )
)

# Included vs excluded participants (with p-values).
# full_data = first imputation of pre-exclusion MICE data (all participants).
# included_ids = unique pt IDs from the post-exclusion shared MICE dataset.
tableOne_incl_vs_excl <- tar_target(
    tableOne_included_vs_excluded,
    make_table_one_included_vs_excluded(
        full_data    = mice::complete(mice_merged_derived, 1),
        included_ids = unique(mids_to_long(mice_analysis$data_shared)[["pt"]]),
        id           = "pt",
        visit        = "time_point",
        caption      = "**Table S2.** Comparison of included and excluded participants (MICE, first imputation)"
    )
)

tableOne_save <- list(
    tar_target(
        tableOne_quartile_files,
        save_gtsummary_table(tableOne_quartile, "03_outputs/TableOne/mice/quartile"),
        format = "file"
    ),
    tar_target(
        tableOne_datasets_files,
        save_gtsummary_table(tableOne_datasets, "03_outputs/TableOne/mice/datasets"),
        format = "file"
    ),
    tar_target(
        tableOne_datasets_files_excluded,
        save_gtsummary_table(tableOne_datasets_excluded, "03_outputs/TableOne/mice/datasets_compare"),
        format = "file"
    ),
    tar_target(
        tableOne_incl_vs_excl_files,
        save_gtsummary_table(tableOne_included_vs_excluded, "03_outputs/TableOne/mice/included_vs_excluded"),
        format = "file"
    )
)

#------------------------------------------------------------------------------
# VISIT DESCRIPTIVES


visit_descriptives_targets <- list(
    
    # ── Visit structure counts ─────────────────────────────────────────────────
    tar_target(
        visit_structure_cc,
        analyze_visits_structure(cc_analysis$data_shared)
    ),
    
    tar_target(
        visit_structure_mice,
        analyze_visits_structure(mice_analysis$data_shared)
    ),

    # ── Visit structure counts, per outcome ────────────────────────────────────
    tar_target(
        visit_structure_mice_HGS,
        analyze_visits_structure(mice_analysis$data$HGS_MAX)
    ),

    tar_target(
        visit_structure_mice_ALM,
        analyze_visits_structure(mice_analysis$data$ALM_HT2_harmonised)
    ),

    tar_target(
        visit_structure_mice_gait,
        analyze_visits_structure(mice_analysis$data$gait_speed)
    ),

    tar_target(
        visit_structure_mice_sarcopenia,
        analyze_visits_structure(mice_analysis$data$ewgsop2_sarcopenia_stage)
    ),

    # ── Timing violin ────────────────────────────────────────────
    tar_target(
        violin_timing_cc,
        {
            out <- plot_timing_violin(cc_analysis$data_shared, timing_var = "days_colaus_minus_osteo", y_label = "Difference in examination date (CoLaus - OsteoLaus)")
            dir.create("03_outputs/descriptives/visits", recursive = TRUE, showWarnings = FALSE)
            f_png <- "03_outputs/descriptives/visits/cc_timing_violin.png"
            f_csv <- "03_outputs/descriptives/visits/cc_timing_summary.csv"
            ggplot2::ggsave(f_png, out$plot, width = 8, height = 5, dpi = 180)
            readr::write_csv(out$summary, f_csv)
            c(f_png, f_csv)
        },
        format = "file"
    ),
    
    tar_target(
        violin_timing_mice,
        {
            out <- plot_timing_violin(mice_analysis$data_shared, timing_var = "days_colaus_minus_osteo",y_label = "Difference in examination date (CoLaus - OsteoLaus)")
            dir.create("03_outputs/descriptives/visits", recursive = TRUE, showWarnings = FALSE)
            f_png <- "03_outputs/descriptives/visits/mice_timing_violin.png"
            f_csv <- "03_outputs/descriptives/visits/mice_timing_summary.csv"
            ggplot2::ggsave(f_png, out$plot, width = 8, height = 5, dpi = 180)
            readr::write_csv(out$summary, f_csv)
            c(f_png, f_csv)
        },
        format = "file"
    ),
    
    # ── Patient coverage bar chart ─────────────────────────────────────────────
    tar_target(
        coverage_by_outcome,
        {
            f <- "03_outputs/descriptives/visits/coverage_by_outcome.png"
            dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
            ggplot2::ggsave(f,
                            plot_patient_coverage(
                                datasets_list = list(
                                    "HGS"        = mice_analysis$mids$HGS_MAX,
                                    "ALMI"       = mice_analysis$mids$ALM_HT2_harmonised,
                                    "Gait speed" = mice_analysis$mids$gait_speed,
                                    "Sarcopenia" = mice_analysis$mids$ewgsop2_sarcopenia_stage
                                )
                            )$plot,
                            width = 8, height = 5, dpi = 180)
            f
        },
        format = "file"
    )
)

#------------------------------------------------------------------------------
# MISSINGNESS ANALYSIS (pre-exclusion)

missingness_target <- tar_target(
    missingness_report,
    {
        describe_missingness(
            data     = mice_merged_derived,
            compare  = list(
                "After MICE" = mice::complete(mice_merged_derived, 1)
            ),
            comparison_vars = c(
                "dairy_total_gday_cumavg",
                "HGS_MAX",
                "ALM_HT2_harmonised",
                "ALM_BMI_harmonised",
                "gait_speed",
                "ewgsop2_sarcopenia_stage",
                "fnih_sarcopenia",
                "BMI",
                "education_level",
                "smoking_status",
                "pa_levels_tertile_f1",
                "diabetes_status",
                "sumtot1"
            ),
            time_col = "time_point",
            out_dir  = "03_outputs/descriptives/missingness"
        )
        list.files("03_outputs/descriptives/missingness",
                   full.names = TRUE, recursive = TRUE)
    },
    format = "file"
)



#------------------------------------------------------------------------------
# VARIABLE DESCRIPTIVES

variable_descriptives_target <- tar_target(
    variable_descriptives,
    {
        out_dir <- "03_outputs/descriptives/variables/mice"
        describe_variables(
            data             = mice_analysis$data_shared,
            time_col         = "time_point",
            continuous_vars  = CONTINUOUS_VARS,
            categorical_vars = CATEGORICAL_VARS,
            scatter_pairs    = list(
                hgs = list(
                    data = mice_analysis$mids$HGS_MAX,
                    x    = c("dairy_total_gday_cumavg", "dairy_fermented_gday_cumavg",
                             "dairy_non_fermented_gday_cumavg", "dairy_highfat_gday_cumavg",
                             "dairy_lowfat_gday_cumavg"),
                    y    = "HGS_MAX"
                ),
                alm = list(
                    data = mice_analysis$mids$ALM_HT2_harmonised,
                    x    = c("dairy_total_gday_cumavg", "dairy_fermented_gday_cumavg",
                             "dairy_non_fermented_gday_cumavg", "dairy_highfat_gday_cumavg",
                             "dairy_lowfat_gday_cumavg"),
                    y    = "ALM_HT2_harmonised"
                ),
                gait = list(
                    data = mice_analysis$mids$gait_speed,
                    x    = c("dairy_total_gday_cumavg_lag", "dairy_fermented_gday_cumavg_lag",
                             "dairy_non_fermented_gday_cumavg_lag", "dairy_highfat_gday_cumavg_lag",
                             "dairy_lowfat_gday_cumavg_lag"),
                    y    = "gait_speed"
                )
            ),
            loess_span           = 0.75,
            quartile_cuts_file   = dairy_quartile_cuts,
            quartile_dataset     = "MICE",
            out_dir              = out_dir,
            width                = 8,
            height               = 5,
            dpi                  = 180
        )
        list.files(out_dir, full.names = TRUE)
    },
    format = "file"
)

#------
# Age trajectory

age_trajectories_target <- tar_target(
    age_trajectories,
    {
        out_dir <- "03_outputs/descriptives/age_trajectories/mice"
        plot_age_trajectories(mice_analysis, out_dir = out_dir)
        list.files(out_dir, full.names = TRUE)
    },
    format = "file"
)

age_trajectories_target_cc <- tar_target(
    age_trajectories_cc,
    {
        out_dir <- "03_outputs/descriptives/age_trajectories/cc"
        plot_age_trajectories(cc_analysis, out_dir = out_dir)
        list.files(out_dir, full.names = TRUE)
    },
    format = "file"
)

#------
# Baseline age group trajectories (outcome vs. Age, coloured by 5-year
# baseline-age group, with per-group lm fit)

baseline_age_group_trajectories_target <- tar_target(
    baseline_age_group_trajectories,
    {
        out_dir <- "03_outputs/descriptives/baseline_age_group_trajectories/mice"
        plot_baseline_age_group_trajectories(mice_analysis, out_dir = out_dir)
        list.files(out_dir, full.names = TRUE)
    },
    format = "file"
)

baseline_age_group_trajectories_target_cc <- tar_target(
    baseline_age_group_trajectories_cc,
    {
        out_dir <- "03_outputs/descriptives/baseline_age_group_trajectories/cc"
        plot_baseline_age_group_trajectories(cc_analysis, out_dir = out_dir)
        list.files(out_dir, full.names = TRUE)
    },
    format = "file"
)



# FOLLOW-UP TIME
# ----------

followup_targets <- list(

    # ── MICE ──────────────────────────────────────────────────────────────────
    tar_target(
        followup_mice_shared,
        summarise_followup(mice_analysis$data_shared)
    ),
    tar_target(
        followup_mice_HGS,
        summarise_followup(mice_analysis$data$HGS_MAX)
    ),
    tar_target(
        followup_mice_ALM,
        summarise_followup(mice_analysis$data$ALM_HT2_harmonised)
    ),
    tar_target(
        followup_mice_gait,
        summarise_followup(mice_analysis$data$gait_speed)
    ),
    tar_target(
        followup_mice_sarcopenia,
        summarise_followup(mice_analysis$data$ewgsop2_sarcopenia_stage)
    ),

    # ── CC ────────────────────────────────────────────────────────────────────
    tar_target(
        followup_cc_shared,
        summarise_followup(cc_analysis$data_shared)
    ),
    tar_target(
        followup_cc_HGS,
        summarise_followup(cc_analysis$data$HGS_MAX)
    ),
    tar_target(
        followup_cc_ALM,
        summarise_followup(cc_analysis$data$ALM_HT2_harmonised)
    ),
    tar_target(
        followup_cc_gait,
        summarise_followup(cc_analysis$data$gait_speed)
    ),

    # ── Save ──────────────────────────────────────────────────────────────────
    tar_target(
        followup_mice_files,
        {
            out_dir <- "03_outputs/descriptives/followup"
            dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
            combined <- dplyr::bind_rows(
                dplyr::mutate(followup_mice_shared, dataset = "Shared"),
                dplyr::mutate(followup_mice_HGS,    dataset = "HGS_MAX"),
                dplyr::mutate(followup_mice_ALM,    dataset = "ALM_HT2_harmonised"),
                dplyr::mutate(followup_mice_gait,   dataset = "gait_speed"),
                dplyr::mutate(followup_mice_sarcopenia,   dataset = "Sarcopenia EWGSOP2"),
            ) |> dplyr::relocate(dataset)
            f <- file.path(out_dir, "followup_mice.csv")
            readr::write_csv(combined, f)
            f
        },
        format = "file"
    ),

    tar_target(
        followup_cc_files,
        {
            out_dir <- "03_outputs/descriptives/followup"
            dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
            combined <- dplyr::bind_rows(
                dplyr::mutate(followup_cc_shared, dataset = "Shared"),
                dplyr::mutate(followup_cc_HGS,    dataset = "HGS_MAX"),
                dplyr::mutate(followup_cc_ALM,    dataset = "ALM_HT2_harmonised"),
                dplyr::mutate(followup_cc_gait,   dataset = "gait_speed")
            ) |> dplyr::relocate(dataset)
            f <- file.path(out_dir, "followup_cc.csv")
            readr::write_csv(combined, f)
            f
        },
        format = "file"
    )
)


variable_descriptives_target_cc <- tar_target(
    variable_descriptives_cc,
    {
        out_dir <- "03_outputs/descriptives/variables/cc"
        describe_variables(
            data             = cc_analysis$data_shared,
            time_col         = "time_point",
            continuous_vars  = CONTINUOUS_VARS,
            categorical_vars = CATEGORICAL_VARS,
            scatter_pairs    = list(
                hgs = list(
                    data = cc_analysis$data$HGS_MAX,
                    x    = c("dairy_total_gday_cumavg", "dairy_fermented_gday_cumavg",
                             "dairy_non_fermented_gday_cumavg", "dairy_highfat_gday_cumavg",
                             "dairy_lowfat_gday_cumavg"),
                    y    = "HGS_MAX"
                ),
                alm = list(
                    data = cc_analysis$data$ALM_HT2_harmonised,
                    x    = c("dairy_total_gday_cumavg", "dairy_fermented_gday_cumavg",
                             "dairy_non_fermented_gday_cumavg", "dairy_highfat_gday_cumavg",
                             "dairy_lowfat_gday_cumavg"),
                    y    = "ALM_HT2_harmonised"
                ),
                gait = list(
                    data = cc_analysis$data$gait_speed,
                    x    = c("dairy_total_gday_cumavg_lag", "dairy_fermented_gday_cumavg_lag",
                             "dairy_non_fermented_gday_cumavg_lag", "dairy_highfat_gday_cumavg_lag",
                             "dairy_lowfat_gday_cumavg_lag"),
                    y    = "gait_speed"
                )
            ),
            loess_span           = 0.75,
            quartile_cuts_file   = dairy_quartile_cuts,
            quartile_dataset     = "CC",
            out_dir              = out_dir,
            width                = 8,
            height               = 5,
            dpi                  = 180
        )
        list.files(out_dir, full.names = TRUE)
    },
    format = "file"
)

dairy_quartile_cuts_target <- tar_target(
    dairy_quartile_cuts,
    {
        out_file <- "03_outputs/descriptives/dairy_quartile_cuts.csv"
        export_dairy_quartile_cuts(
            data_list = list(
                "CC"   = cc_merged_derived,
                "MICE" = mice_merged_derived
            ),
            out_file       = out_file,
            visit_col      = "time_point",
            baseline_visit = "T1"
        )
    },
    format = "file"
)

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
    
    # ── MICE route ────────────────────────────────────────────────────────────
    mice_prep_targets,
    mice_exclusion,

    # ── Models ────────────────────────────────────────────────────────────────
    # LMM_targets_HGS,
    # LMM_targets_ALM,
    # LMM_targets_ALM_BMI,
    # LMM_targets_gait,
    # cox_targets,
    cox_targets_fnih,

    # ── Dairy protein content sensitivity analyses ───────────────────────────
    #LMM_targets_HGS_ffq_items,
    #LMM_targets_ALM_ffq_items
    # LMM_targets_HGS_adj_nondairy_protein,
    # LMM_targets_ALM_adj_nondairy_protein,
    # LMM_targets_HGS_dairy_protein_exposure,
    # LMM_targets_ALM_dairy_protein_exposure,
    # LMM_targets_HGS_dairy_nofondue

    # ── Model specification sensitivity ──────────────────────────────────────
    # LMM_modspec_HGS,
    # LMM_modspec_ALM,
    # LMM_modspec_gait
    
    # ── Descriptives ──────────────────────────────────────────────────────────
    #consort
    # tableOne_targets_1,
    # tableOne_targets_2,
    # tableOne_targets_compare,
    # tableOne_incl_vs_excl,
    # tableOne_save,
    visit_descriptives_targets
    # missingness_target,
    # dairy_quartile_cuts_target,
    # variable_descriptives_target,
    # variable_descriptives_target_cc,
    # age_trajectories_target,
    # age_trajectories_target_cc,
    # baseline_age_group_trajectories_target,
    # baseline_age_group_trajectories_target_cc,
    # followup_targets

)
