# =============================================================================
# _targets.R
# CoLaus / OsteoLaus sarcopenia dairy intake pipeline
# =============================================================================
#
# Pipeline stages
#   01  File paths        format = "file", rebuilds when CSV changes on disk
#   02  Import            raw CSV -> all-character tibble; validate_wave() runs here
#   03  Harmonise         type coercion, sentinel recoding, factor levels, dates
#   04  Stack             row-bind harmonised waves into one long tibble per cohort
#   05  QC                identity anchors (on stacked tibbles), overlap check
#   QC GATE               assert_no_failures() — hard stop on OsteoLaus FAILs
#   06  Derive            computed analytical variables per cohort
#   06b Combined derive   handgrip_max_all + sarcopenia staging (cross-cohort)
#   07  Build tables      participants / visits / exposures (normalised grain)
#   08  Freeze            hard + outcome-specific exclusions -> analysis_long
#   08b Flow diagrams     CONSORT graphs for hard and per-outcome exclusions
#   09  Descriptives      Table 1, missing summary, wave summary, plots, report
#
# Exclusion model (two tiers):
#   Hard exclusions  — criteria 0-6; participants assigned inclusion_status "No"
#   Partial          — pass hard criteria but ineligible for >= 1 outcome;
#                      inclusion_status "Partial"; eligible_* columns in analysis_long
#   Full inclusion   — pass all criteria; inclusion_status "Yes"
#
# This file ONLY wires targets together. No computation lives here.
# All functions are loaded via tar_source().
# =============================================================================

library(targets)
library(tarchetypes)

tar_option_set(
    packages = c(
        "readr", "dplyr", "tidyr", "stringr", "lubridate",
        "forcats", "purrr", "glue", "tibble", "cli",
        "gtsummary", "ggplot2", "patchwork", "scales",
        "lme4", "lmerTest", "broom.mixed",
        "sessioninfo"
    )
)

tar_source("04_scripts/R")


# =============================================================================
# 01. FILE PATHS
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
# 02. IMPORT
# =============================================================================

import_targets <- list(
    
    tar_target(colaus_bsl_raw, import_wave(f_colaus_baseline, "CoLaus", "Baseline")),
    tar_target(colaus_f1_raw,  import_wave(f_colaus_f1,       "CoLaus", "F1")),
    tar_target(colaus_f2_raw,  import_wave(f_colaus_f2,       "CoLaus", "F2")),
    tar_target(colaus_f3_raw,  import_wave(f_colaus_f3,       "CoLaus", "F3")),
    
    tar_target(osteo_bsl_raw,  import_wave(f_osteo_baseline, "OsteoLaus", "Baseline")),
    tar_target(osteo_v2_raw,   import_wave(f_osteo_v2,       "OsteoLaus", "V2")),
    tar_target(osteo_v3_raw,   import_wave(f_osteo_v3,       "OsteoLaus", "V3")),
    tar_target(osteo_v4_raw,   import_wave(f_osteo_v4,       "OsteoLaus", "V4")),
    tar_target(osteo_v5_raw,   import_wave(f_osteo_v5,       "OsteoLaus", "V5"))
)


# =============================================================================
# 03. HARMONISE
# =============================================================================

harmonise_targets <- list(
    
    tar_target(colaus_bsl_harm, harmonise_wave(colaus_bsl_raw)),
    tar_target(colaus_f1_harm,  harmonise_wave(colaus_f1_raw)),
    tar_target(colaus_f2_harm,  harmonise_wave(colaus_f2_raw)),
    tar_target(colaus_f3_harm,  harmonise_wave(colaus_f3_raw)),
    
    tar_target(osteo_bsl_harm,  harmonise_wave(osteo_bsl_raw)),
    tar_target(osteo_v2_harm,   harmonise_wave(osteo_v2_raw)),
    tar_target(osteo_v3_harm,   harmonise_wave(osteo_v3_raw)),
    tar_target(osteo_v4_harm,   harmonise_wave(osteo_v4_raw)),
    tar_target(osteo_v5_harm,   harmonise_wave(osteo_v5_raw))
)


# =============================================================================
# 04. STACK
# =============================================================================

stack_targets <- list(
    
    tar_target(colaus_long,
               stack_waves(
                   colaus_bsl_harm, colaus_f1_harm,
                   colaus_f2_harm,  colaus_f3_harm
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
# 05. QC + GATE
# Runs on stacked pre-derive data. qc_all_pass stops the pipeline on any
# FAIL involving an OsteoLaus participant.
# =============================================================================

qc_targets <- list(
    
    tar_target(qc_pt_identity,
               check_pt_identity(
                   colaus_long = colaus_long,
                   osteo_long  = osteo_long,
                   ht_tol      = 3,
                   age_tol     = 2
               )
    ),
    
    tar_target(qc_pt_overlap,
               check_pt_overlap(colaus_long, osteo_long)
    ),
    
    tar_target(qc_all_pass,
               assert_no_failures(qc_pt_identity, qc_pt_overlap)
    )
)


# =============================================================================
# 06. DERIVE — per-cohort
# Both derive targets run after QC. qc_exclusions from qc_all_pass flows
# into freeze_dataset() so QC FAILs propagate as exclusions, not aborts.
# There is no longer a separate derive_combined step — sarcopenia staging
# using the combined handgrip_max_all is done in build_sarcopenia() (07b)
# after both visits and exposures are available.
# =============================================================================

derive_targets <- list(
    
    tar_target(colaus_derived, derive_colaus(colaus_long)),
    
    tar_target(osteo_derived,      derive_osteo(osteo_long))
)


# =============================================================================
# 06b. WAVE MATCH
# Computed once, reused by build_exposures() and build_sarcopenia().
# =============================================================================

wave_match_targets <- list(
    
    tar_target(wave_match, {
        colaus_dates <- colaus_derived |>
            dplyr::filter(.wave != "Baseline") |>
            dplyr::select(pt, colaus_wave = .wave, colaus_date = exam_date_iso)
        
        osteo_visits <- osteo_derived |>
            dplyr::select(pt, osteo_wave = .wave, osteo_date = exam_date_iso)
        
        .match_waves_by_date(osteo_visits, colaus_dates)
    })
)


# =============================================================================
# 07. BUILD NORMALISED TABLES
# =============================================================================

build_targets <- list(
    
    tar_target(participants_base,
               build_participants(colaus_derived, osteo_derived)
    ),
    
    tar_target(visits,
               build_visits(osteo_derived, participants_base)
    ),
    
    tar_target(exposures,
               build_exposures(colaus_derived, visits, wave_match = wave_match)
    ),
    
    # Step 07b: assemble handgrip_max_all and run definitive sarcopenia staging.
    # Runs after both visits and exposures are available, breaking the circular
    # dependency. The output replaces visits as the input to freeze_dataset().
    tar_target(visits_staged,
               build_sarcopenia(
                   visits         = visits,
                   colaus_derived = colaus_derived,
                   wave_match     = wave_match
               )
    )
)


# =============================================================================
# 08. FREEZE
# frozen$data carries eligible_* flag columns for each outcome so downstream
# models can filter without joining a separate table.
# =============================================================================

freeze_targets <- list(
    
    tar_target(frozen,
               freeze_dataset(
                   participants   = participants_base,
                   visits         = visits_staged,
                   exposures      = exposures,
                   qc_exclusions  = qc_all_pass,
                   min_visits     = 2L
               )
    ),
    
    # Unpack frozen outputs.
    # participants is REPLACED here with the enriched version that carries
    # inclusion_status, inclusion_reason, and eligible_* columns.
    # This overwrites the build_participants() output so all downstream code
    # (descriptives, models) can use tar_read(participants) and get the full
    # flagged table without needing to know about participants_flagged.
    tar_target(participants,      frozen$participants_flagged),
    tar_target(analysis_long,     frozen$data),
    tar_target(flow_log_hard,     frozen$flow_log_hard),
    tar_target(flow_log_outcomes, frozen$flow_log_outcomes)
)


# =============================================================================
# 08b. CONSORT FLOW DIAGRAMS
# =============================================================================

flow_graph_targets <- list(
    
    # Hard exclusion CONSORT diagram
    # Requires DiagrammeR: install.packages("DiagrammeR") + renv::snapshot()
    tar_target(flow_graph_hard, {
        library(DiagrammeR)
        make_hard_exclusion_graph(
            flow_log_hard,
            title = "CoLaus/OsteoLaus: Participant Flow - Hard Exclusions"
        )
    }),
    
    # Per-outcome eligibility diagram
    tar_target(flow_graph_outcomes, {
        library(DiagrammeR)
        make_outcome_exclusion_graph(
            flow_log_outcomes,
            title = "CoLaus/OsteoLaus: Outcome-Specific Eligibility"
        )
    }),
    
    # Export to PNG
    # Also requires DiagrammeRsvg and rsvg:
    #   install.packages(c("DiagrammeRsvg", "rsvg")); renv::snapshot()
    tar_target(flow_graph_hard_file, {
        library(DiagrammeR)
        path <- "05_outputs/figures/flow_hard_exclusions.png"
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        export_exclusion_graph(flow_graph_hard, path,
                               width = 900L, height = 800L)
        path
    }, format = "file"),
    
    tar_target(flow_graph_outcomes_file, {
        library(DiagrammeR)
        path <- "05_outputs/figures/flow_outcome_eligibility.png"
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        export_exclusion_graph(flow_graph_outcomes, path,
                               width = 700L, height = 1400L)
        path
    }, format = "file")
    
)


# =============================================================================
# 09. DESCRIPTIVES
# =============================================================================

descriptive_targets <- list(
    
    # All descriptive functions take analysis_long as their sole input.
    tar_target(table_one,
               make_table_one(analysis_long)
    ),
    
    tar_target(table_one_by_quartile,
               make_table_one_by_quartile(analysis_long)
    ),
    
    tar_target(missing_summary,
               make_missing_summary(analysis_long)
    ),
    
    tar_target(wave_summary,
               make_wave_summary(analysis_long)
    ),
    
    tar_target(exposure_plots,
               make_exposure_plots(analysis_long)
    ),
    
    tar_target(outcome_plots,
               make_outcome_plots(analysis_long)
    ),
    
    tar_target(cumavg_trajectory_plot,
               make_cumavg_trajectory_plot(analysis_long)
    ),
    
    tar_target(dairy_quartile_flow,
               make_dairy_quartile_flow(analysis_long)
    ),
    
    tar_target(smoking_change_plot,
               make_smoking_change_plot(analysis_long)
    ),
    
    tar_target(session_info, {
        si <- sessioninfo::session_info()
        dir.create("06_outputs", recursive = TRUE, showWarnings = FALSE)
        saveRDS(si, "06_outputs/session_info.rds")
        si
    })
)

# =============================================================================
# 10. MODELS
# =============================================================================

model_targets <- list(
    
    # ── Grip strength model ───────────────────────────────────────────────────
    tar_target(grip_model_data,
               build_grip_model_data(analysis_long)
    ),
    tar_target(grip_model_fit,
               fit_grip_model(grip_model_data)
    ),
    tar_target(grip_model_table,
               make_grip_model_table(grip_model_fit)
    ),
    tar_target(grip_model_plots,
               make_grip_model_plots(grip_model_fit, grip_model_data)
    ),
    
    # ── Appendicular lean mass model ──────────────────────────────────────────
    tar_target(alm_model_data,
               build_alm_model_data(analysis_long)
    ),
    tar_target(alm_model_fit,
               fit_alm_model(alm_model_data)
    ),
    tar_target(alm_model_table,
               make_alm_model_table(alm_model_fit)
    ),
    tar_target(alm_model_plots,
               make_alm_model_plots(alm_model_fit, alm_model_data)
    ),
    
    # ── Gait speed model (random intercept only — max 2 obs per pt) ──────────
    tar_target(gait_model_data,
               build_gait_model_data(analysis_long)
    ),
    tar_target(gait_model_fit,
               fit_gait_model(gait_model_data)
    ),
    tar_target(gait_model_table,
               make_gait_model_table(gait_model_fit)
    ),
    tar_target(gait_model_plots,
               make_gait_model_plots(gait_model_fit, gait_model_data)
    ),
    
    # ── Cox model: incident sarcopenia ────────────────────────────────────────
    tar_target(cox_model_data,
               build_cox_model_data(analysis_long)
    ),
    tar_target(cox_quartile_fit,
               fit_cox_quartile(cox_model_data)
    ),
    tar_target(cox_continuous_fit,
               fit_cox_continuous(cox_model_data)
    ),
    tar_target(cox_quartile_table,
               make_cox_quartile_table(cox_quartile_fit)
    ),
    tar_target(cox_continuous_table,
               make_cox_continuous_table(cox_continuous_fit)
    ),
    tar_target(cox_model_plots,
               make_cox_model_plots(cox_model_data,
                                    cox_quartile_fit,
                                    cox_continuous_fit)
)


# =============================================================================
# ASSEMBLE
# =============================================================================

c(
    path_targets,
    import_targets,
    harmonise_targets,
    stack_targets,
    qc_targets,
    derive_targets,
    wave_match_targets,
    build_targets,
    freeze_targets,
    flow_graph_targets,
    descriptive_targets,
    model_targets,
    list(
        tarchetypes::tar_quarto(
            descriptive_report,
            path = "06_outputs/reports/descriptive_analysis.qmd"
        )
    )
)
