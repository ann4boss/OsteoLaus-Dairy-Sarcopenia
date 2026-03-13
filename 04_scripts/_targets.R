# =============================================================================
# _targets.R
# CoLaus / OsteoLaus sarcopenia dairy intake pipeline
# =============================================================================
# Run the pipeline:   targets::tar_make()
# Inspect the graph:  targets::tar_visnetwork()
# Load a target:      targets::tar_read(target_name)
#                     targets::tar_load(target_name)
#
# Pipeline stages:
#   1. File paths      — tracked with format = "file" so changes trigger rebuild
#   2. Import          — raw CSV → all-character tibble, one target per wave
#   3. QC: uniqueness  — pt unique within each wave file
#   4. Harmonise       — type coercion, factor levels, date parsing
#   5. QC: identity    — pt refers to same person across files and cohorts
#   6. Stack           — row-bind waves into long tibbles per cohort
#   7. QC: overlap     — OsteoLaus pts found in CoLaus
#   8. QC: gate        — hard stop if any FAIL before derivation
#   9. Merge           — OsteoLaus backbone + nearest CoLaus wave attached
#  10. Clean           — range checks, implausible values, carry-forward
#  11. Derive          — computed variables (sarcopenia stage, alcohol cat, …)
#  12. Freeze          — final inclusion/exclusion, column selection
#  13. Descriptives    — Table 1, flow chart, missing data, plots
#
# This file ONLY wires targets together. No computation lives here.
# All functions are defined in R/functions_*.R and sourced by tar_source().
#


library(targets)
library(tarchetypes)   # tar_file(), tar_render() etc.

# Load all pipeline functions
tar_option_set(
    packages = c(
        "readr", "dplyr", "tidyr", "stringr", "lubridate",
        "forcats", "purrr", "glue", "tibble",
        "gtsummary", "ggplot2", "patchwork", "scales"
    )
)

# Load all custom functions from R/
scripts <- list.files("04_scripts/R", full.names = TRUE)
lapply(scripts, source)


# =============================================================================
# PIPELINE
# =============================================================================

# =============================================================================
# 1. FILE PATHS
# format = "file" means targets rebuilds downstream when the file changes.
# Adjust paths to match your actual data directory structure.
# =============================================================================

path_targets <- list(
    
    # CoLaus raw CSVs
    tar_target(f_colaus_baseline,   "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_base.csv",format = "file"),
    tar_target(f_colaus_f1,         "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_FU1.csv", format = "file"),
    tar_target(f_colaus_f2,         "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_FU2.csv", format = "file"),
    tar_target(f_colaus_f3,         "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_FU3.csv", format = "file"),
    
    # OsteoLaus raw CSVs
    tar_target(f_osteo_baseline,    "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_OstBas.csv", format = "file"),
    tar_target(f_osteo_v2,          "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_OstV2.csv",       format = "file"),
    tar_target(f_osteo_v3,          "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_OstV3.csv",       format = "file"),
    tar_target(f_osteo_v4,          "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_OstV4.csv",       format = "file"),
    tar_target(f_osteo_v5,          "/Users/annaboss/Library/CloudStorage/OneDrive-UniversitaetBern/Dairy_sarcopenia_data/Dairy_sarcopenia_OstV5.csv",       format = "file")
    )

# =============================================================================
# 2. IMPORT — raw CSV → all-character tibble
# import_wave() attaches .cohort, .wave, .wave_num and fast-fails on missing
# pt column or malformed date column.
# =============================================================================

import_targets <- list(
    
    # CoLaus
    tar_target(colaus_bsl_raw, import_wave(f_colaus_baseline, "CoLaus", "Baseline", 0L)),
    tar_target(colaus_f1_raw,  import_wave(f_colaus_f1,       "CoLaus", "F1",       1L)),
    tar_target(colaus_f2_raw,  import_wave(f_colaus_f2,       "CoLaus", "F2",       2L)),
    tar_target(colaus_f3_raw,  import_wave(f_colaus_f3,       "CoLaus", "F3",       3L)),
    
    # OsteoLaus
    tar_target(osteo_bsl_raw,  import_wave(f_osteo_baseline, "OsteoLaus", "Baseline", 1L)),
    tar_target(osteo_v2_raw,   import_wave(f_osteo_v2,       "OsteoLaus", "V2",       2L)),
    tar_target(osteo_v3_raw,   import_wave(f_osteo_v3,       "OsteoLaus", "V3",       3L)),
    tar_target(osteo_v4_raw,   import_wave(f_osteo_v4,       "OsteoLaus", "V4",       4L)),
    tar_target(osteo_v5_raw,   import_wave(f_osteo_v5,       "OsteoLaus", "V5",       5L))
)

# =============================================================================
# 3. QC: pt UNIQUENESS within each raw wave file
# Must pass before harmonisation. Any FAIL = duplicate rows in a source file,
# which must be resolved upstream (data extraction error).
# =============================================================================

qc_uniqueness_targets <- list(
    
    tar_target(qc_pt_uniqueness,
               check_pt_uniqueness(list(
                   "CoLaus Baseline"   = colaus_bsl_raw,
                   "CoLaus F1"         = colaus_f1_raw,
                   "CoLaus F2"         = colaus_f2_raw,
                   "CoLaus F3"         = colaus_f3_raw,
                   "OsteoLaus Baseline"= osteo_bsl_raw,
                   "OsteoLaus V2"      = osteo_v2_raw,
                   "OsteoLaus V3"      = osteo_v3_raw,
                   "OsteoLaus V4"      = osteo_v4_raw,
                   "OsteoLaus V5"      = osteo_v5_raw
               ))
    )
)

# =============================================================================
# 4. HARMONISE — type coercion and sentinel recoding, factor levels, date parsing
# harmonise_wave() dispatches on .cohort to harmonise_colaus() or
# harmonise_osteo().
# =============================================================================

harmonise_targets <- list(
    
    # CoLaus
    tar_target(colaus_bsl_harm, harmonise_wave(colaus_bsl_raw)),
    tar_target(colaus_f1_harm,  harmonise_wave(colaus_f1_raw)),
    tar_target(colaus_f2_harm,  harmonise_wave(colaus_f2_raw)),
    tar_target(colaus_f3_harm,  harmonise_wave(colaus_f3_raw)),
    
    # OsteoLaus
    tar_target(osteo_bsl_harm,  harmonise_wave(osteo_bsl_raw)),
    tar_target(osteo_v2_harm,   harmonise_wave(osteo_v2_raw)),
    tar_target(osteo_v3_harm,   harmonise_wave(osteo_v3_raw)),
    tar_target(osteo_v4_harm,   harmonise_wave(osteo_v4_raw)),
    tar_target(osteo_v5_harm,   harmonise_wave(osteo_v5_raw))
)

# =============================================================================
# 5. QC: pt IDENTITY — same person across all files and both cohorts
# Runs on harmonised data (so age, height, sex, ethnicity are typed correctly).
# Anchors used:
#   sex            CoLaus only    — FAIL if changes across CoLaus waves
#   ethnicity      Both, Baseline — FAIL if disagrees cross-cohort
#   height         Both           — WARN if >3 cm variation (within or cross)
#   age trajectory Both           — FAIL if backwards; WARN if implausible rate
# =============================================================================

qc_identity_targets <- list(
    
    tar_target(qc_pt_identity,
               check_pt_identity(
                   colaus_waves = list(
                       colaus_bsl_harm, colaus_f1_harm, colaus_f2_harm,
                       colaus_f3_harm
                   ),
                   osteo_waves = list(
                       osteo_bsl_harm, osteo_v2_harm, osteo_v3_harm,
                       osteo_v4_harm,  osteo_v5_harm
                   ),
                   ht_tol  = 3,    # cm — adjust if measurement protocol differs
                   age_tol = 2     # yr — extra tolerance on top of calendar gap
               )
    )
)

# =============================================================================
# 6. STACK — row-bind harmonised waves into one long tibble per cohort
# stack_waves() is a thin wrapper around dplyr::bind_rows().
# Kept as explicit targets so they are inspectable with tar_read().
# =============================================================================

stack_targets <- list(
    
    tar_target(colaus_long,
               stack_waves(
                   colaus_bsl_harm, colaus_f1_harm, colaus_f2_harm,
                   colaus_f3_harm
               )
    ),
    
    tar_target(osteo_long,
               stack_waves(
                   osteo_bsl_harm, osteo_v2_harm, osteo_v3_harm,
                   osteo_v4_harm,  osteo_v5_harm
               )
    )
)

# =============================================================================
# 7. QC: pt OVERLAP between cohorts
# OsteoLaus is a sub-cohort of CoLaus so virtually all OsteoLaus pts should
# appear in CoLaus. WARN if <90% overlap.
# =============================================================================

qc_overlap_targets <- list(
    
    tar_target(qc_pt_overlap,
               check_pt_overlap(colaus_long, osteo_long)
    )
)

# =============================================================================
# 8. QC GATE — hard stop if any FAIL before derivation
# assert_no_failures() collects all QC results and stops the pipeline with a
# formatted error listing every FAIL. Warnings are printed but do not stop.
# All downstream targets depend on qc_all_pass so they cannot run until QC
# is clean.
# =============================================================================

qc_gate_targets <- list(
    
    tar_target(qc_all_pass,
               assert_no_failures(
                   qc_pt_uniqueness,
                   qc_pt_identity,
                   qc_pt_overlap
               )
    )
)

# =============================================================================
# 9. MERGE — OsteoLaus backbone + nearest CoLaus wave (F1-F4) attached
# merge_cohorts():
#   - Excludes CoLaus Baseline from matching (collected 2003-2008, predates
#     all OsteoLaus visits by >=2 years)
#   - Matches by smallest absolute date gap (before OR after)
#   - Prefixes shared column names: osteolaus_* / colaus_*
#   - Adds audit columns: .colaus_wave, .gap_days
# =============================================================================

merge_targets <- list(
    
    tar_target(merged_long,
               {
                   # Gate: only runs after QC passes
                   # force(qc_all_pass)
                   merge_cohorts(colaus_long, osteo_long, gap_threshold = 912)
               }
    )
)

# =============================================================================
# 10. CLEAN — range checks, implausible values, carry-forward
# =============================================================================

clean_targets <- list(
    
    tar_target(cleaned_long, clean_data(merged_long))
)

# =============================================================================
# 11. DERIVE — computed analytical variables
# derive_variables() applies sub-derivations in order:
#   alcohol_category, diabetes_status, cdv_event, hrt_status,
#   BMI_category, dairy sub-categories, ewgsop2_sarcopenia_stage
# =============================================================================

derive_targets <- list(
    
    tar_target(derived_list, derive_variables(cleaned_long))
)

# =============================================================================
# 12. FREEZE — final inclusion/exclusion criteria, select analytical columns
#
# ============================================================================

freeze_targets <- list(
    
    tar_target(analysis_long, freeze_dataset(derived_list))
)

# =============================================================================
# 13. DESCRIPTIVES
# =============================================================================

descriptive_targets <- list(
    
    tar_target(table_one,
               make_table_one(analysis_long)
    ),
    
    tar_target(missing_summary,
               make_missing_summary(analysis_long)
    ),
    
    tar_target(cohort_flow,
               make_cohort_flow(derived_list, analysis_long)
    ),
    
    tar_target(wave_summary,
               make_wave_summary(analysis_long)
    ),
    
    tar_target(exposure_plots,
               make_exposure_plots(analysis_long)
    ),
    
    tar_target(outcome_plots,
               make_outcome_plots(analysis_long)
    )
)

tarchetypes::tar_quarto(
    descriptive_report,
    path = "06_outputs/reports/descriptive_analysis.qmd"
)

# =============================================================================
# ASSEMBLE — combine all target lists
# =============================================================================

c(
    path_targets,
    import_targets,
    # qc_uniqueness_targets,
    harmonise_targets,
    # qc_identity_targets,
    stack_targets,
    # qc_overlap_targets,
    # qc_gate_targets,
    merge_targets,
    clean_targets,
    derive_targets,
    freeze_targets,
    descriptive_targets
)

