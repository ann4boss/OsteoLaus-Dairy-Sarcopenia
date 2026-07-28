# =============================================================================
# Gait Speed LMM Report
# Calls run_lmm_report() from R/04_01_LMM_report.R
# =============================================================================

# Gait speed uses lagged exposures and covariates (_lag suffix).
# No sumtot1 in the model (not a confounder for gait speed).
# Only random intercept (no random slope — two visits only).

# -----------------------------------------------------------------------------
# 1. EXPOSURE DEFINITIONS
# -----------------------------------------------------------------------------

exposures_gait <- tibble::tribble(
    ~exposure,                              ~exposure_type, ~ref_level,

    "dairy_100g_lag",                       "linear",       NA,
    "dairy_100g_lag",                       "rcs",          NA,

    "fermented_100g_lag",                   "linear",       NA,
    "nonfermented_100g_lag",                "linear",       NA,

    "dairy_quartile_baseline_lag",          "categorical",  "Q1",
    "dairy_guidelines_port_lag",            "categorical",  "< 2 servings/day"
)


# -----------------------------------------------------------------------------
# 2. COVARIATE SETS
# -----------------------------------------------------------------------------

covariate_sets_gait_report <- list(
    minimal = c(
        "age_decades",
        "BMI_category_lag",
        "education_level_lag",
        "smoking_status_lag",
        "pa_levels_tertile_f1_lag",
        "diabetes_status_lag"
    ),
    other_PA = c(
        "age_decades",
        "BMI_category_lag",
        "education_level_lag",
        "smoking_status_lag",
        "pa_levels_who_f1_lag",
        "diabetes_status_lag"
    )
)


# -----------------------------------------------------------------------------
# 3. RUN REPORT
# -----------------------------------------------------------------------------

run_lmm_report_gait(
    mids_object    = mice_analysis$mids$gait_speed,
    outcome        = "gait_speed",
    outcome_fn     = identity,
    exposures      = exposures_gait,
    covariate_sets = covariate_sets_gait_report,
    random_slope   = FALSE,
    interaction    = FALSE,
    id_var         = "pt",
    time_var       = "time_since_baseline",
    out_dir        = "03_outputs/LMM_exploratory"
)
