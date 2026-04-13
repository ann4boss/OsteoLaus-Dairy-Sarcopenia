# =============================================================================
# _targets.R
# CoLaus / OsteoLaus sarcopenia dairy intake pipeline
# =============================================================================
#
# Pipeline stages
#   00  File paths        format = "file", rebuilds when CSV changes on disk
#   01  Import            raw CSV -> all-character tibble; validate_wave() runs here
#   02  Harmonise         type coercion, sentinel recoding, factor levels, dates
#   03  QC                
#   04  Stack             row-bind harmonised waves into one long tibble per cohort
#   05  Derive            computed analytical variables per cohort
#   07  Build tables      participants / visits / exposures (normalised grain)
#   08a  Freeze            hard + outcome-specific exclusions -> analysis_long
#   08b Flow diagrams     CONSORT graphs for hard and per-outcome exclusions
#   09  Descriptives      Table 1, missing summary, wave summary, plots, report
#   10  Models            LME (grip, ALM, gait) and Cox (incident sarcopenia)
#                         Four-tier hierarchy M0–M3 per model.
#                         Exposure sensitivity (lag-1, concurrent).
#                         Exclusion sensitivity (min_visits = 3).
#
# =============================================================================

library(targets)
library(tarchetypes)

tar_option_set(
    # TODO check which packages are actually still used
    packages = c(
        "readr", "dtplyr", "data.table", "tidyr", "stringr", "lubridate",
        "forcats", "purrr", "glue", "tibble", "cli",
        "gtsummary", "ggplot2", "patchwork", "scales",
        "lme4", "lmerTest", "broom.mixed", "broom",
        "survival",
        "gt",
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
# 02. HARMONISE
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
# 03. QC
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
# 05. DERIVE — per-cohort (only colaus)
# =============================================================================

derive_targets <- list(
    tar_target(colaus_derived, derive_colaus(colaus_long)))


# =============================================================================
# 06. WAVE MATCH
# =============================================================================

wave_match_targets <- list(tar_target(merged_table, merge_closest_exams(colaus_derived, osteo_long))
)


# =============================================================================
# 07. Derive values for both cohorts
# =============================================================================

derive_targets_both <- tar_target(merged_table_derived, derive_combined(merged_table))


# =============================================================================
# 08. FREEZE
# =============================================================================

freeze_targets <- list(
    tar_target(excl_out,      apply_exclusions(merged_table_derived, qc_tbl, qc_summary)),
    tar_target(analysis_data, excl_out$data),
    tar_target(consort_dot,   excl_out$consort),   # now a plain character string
    tar_target(excl_counts,   excl_out$counts),
    
    tar_target(consort_html, {
        widget <- DiagrammeR::grViz(consort_dot)
        path   <- "06_outputs/consort.html"
        htmlwidgets::saveWidget(widget, path, selfcontained = TRUE)
        path
    }, format = "file")
)

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
        path <- "06_outputs/figures/flow_hard_exclusions.png"
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        export_exclusion_graph(flow_graph_hard, path,
                               width = 900L, height = 800L)
        path
    }, format = "file"),
    
    tar_target(flow_graph_outcomes_file, {
        library(DiagrammeR)
        path <- "06_outputs/figures/flow_outcome_eligibility.png"
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
# 09b. Smoking behaviour
# =============================================================================

smoking_descriptives <- list(
    tar_target(
        smoking_prevalence_table,
        make_smoking_prevalence_table(analysis_long)
    ),
    
    tar_target(
        smoking_transition_matrix,
        make_smoking_transition_matrix(analysis_long)
    ),
    
    tar_target(
        smoking_transition_heatmap,
        make_smoking_transition_heatmap(analysis_long)
    ),
    
    tar_target(
        smoking_trajectory_plot,
        make_smoking_trajectory_plot(analysis_long)
    ),
    
    tar_target(
        smoking_sankey_consecutive,
        make_smoking_sankey_consecutive(analysis_long)
    ),
    
    # -------------------------------------------------------------------------
    # Participant-level change summary
    # Tibble: n_changes, direction (quit/relapsed), trajectory string, group
    # Useful as an input for further analyses (e.g. sensitivity models)
    # -------------------------------------------------------------------------
    tar_target(
        smoking_change_summary,
        make_smoking_change_summary(analysis_long)
    ),
    
    # -------------------------------------------------------------------------
    # Direction-of-change plot
    # Two-panel: absolute counts + proportions by trajectory group × baseline
    # status
    # -------------------------------------------------------------------------
    tar_target(
        smoking_change_plot_detail,
        make_smoking_change_plot_detail(analysis_long)
    ),
    
    # -------------------------------------------------------------------------
    # Stability summary table (gtsummary)
    # Rows: no change / changed once / changed ≥2 times
    # -------------------------------------------------------------------------
    tar_target(
        smoking_stability_table,
        make_smoking_stability_table(analysis_long)
    )
    
)

# =============================================================================
# 9c. wave matching overview
# =============================================================================
wave_descriptives <- tar_target(gap_days_plots, analyse_gap_distribution(df        = analysis_long,
                                                                         wave_var  = osteo_wave,
                                                                         match_var = .colaus_wave,
                                                                         gap_var   = .gap_days
)
                               )


# =============================================================================
# 10. MODELS
# =============================================================================
#
# NOTE: fit_*_model() functions return a named list, not a bare model object:
#   $fit_reml  — REML fit   (used for reported coefficients / tables / plots)
#   $fit_ml    — ML fit     (used for LRT / AIC comparisons between tiers)
#   $formula   — formula used
#   $tier      — covariate tier label ("M0"–"M3")
#   $exposure  — exposure column name
#   $add_interaction — logical
#
# fit_cox_*() return the same structure with $fit instead of $fit_reml/$fit_ml.
#
# All tiers and sensitivity models share the same analytical sample built once
# by build_*_model_data(). Coefficient changes across tiers therefore reflect
# adjustment only, not sample composition differences.
# =============================================================================

model_targets <- list(
    
    # =========================================================================
    # GRIP STRENGTH  (LME, random intercept + slope)
    # =========================================================================
    
    tar_target(grip_model_data,
               build_grip_model_data(analysis_long)
    ),
    
    # ── Primary model ─────────────────────────────────────────────────────────
    tar_target(grip_model_fit_M3,
               fit_grip_model(grip_model_data,
                              tier            = "M3",
                              exposure        = "dairy_cumavg",
                              add_interaction = FALSE)
    ),
    
    # ── Covariate tier progression (robustness / supplementary Table S2) ──────
    tar_target(grip_model_fit_M0,
               fit_grip_model(grip_model_data, tier = "M0")
    ),
    tar_target(grip_model_fit_M1,
               fit_grip_model(grip_model_data, tier = "M1")
    ),
    tar_target(grip_model_fit_M2,
               fit_grip_model(grip_model_data, tier = "M2")
    ),
    
    # Stability table: dairy beta across M0-M3 in one tibble
    tar_target(grip_stability_table,
               make_grip_stability_table(
                   fits = list(M0 = grip_model_fit_M0,
                               M1 = grip_model_fit_M1,
                               M2 = grip_model_fit_M2,
                               M3 = grip_model_fit_M3),
                   exposure = "dairy_cumavg"
               )
    ),
    
    # # ── Exposure-metric sensitivity (M3 covariates, different dairy column) ───
    # # S1: one-wave lag (tests whether prior intake predicts current outcome)
    # tar_target(grip_model_fit_S1,
    #            fit_grip_model(grip_model_data,
    #                           tier     = "M3",
    #                           exposure = "dairy_total_lag1")
    # ),
    # # S2: concurrent instantaneous dairy (no accumulation assumption)
    # tar_target(grip_model_fit_S2,
    #            fit_grip_model(grip_model_data,
    #                           tier     = "M3",
    #                           exposure = "dairy_total_gday")
    # ),
    
    # ── Trajectory-modification model (secondary hypothesis) ─────────────────
    # Tests whether higher dairy intake attenuates the rate of grip decline.
    # Reported in supplementary; NOT the primary model.
    tar_target(grip_model_fit_interaction,
               fit_grip_model(grip_model_data,
                              tier            = "M2",
                              add_interaction = TRUE)
    ),
    
    # ── Outputs for primary model ─────────────────────────────────────────────
    tar_target(grip_model_table,
               make_grip_model_table(grip_model_fit_M2)
    ),
    tar_target(grip_model_plots,
               make_grip_model_plots(grip_model_fit_M2, grip_model_data)
    ),
    
    
    # =========================================================================
    # APPENDICULAR LEAN MASS INDEX  (LME, random intercept + slope)
    # Height replaces BMI (collinearity — see model_specs.R)
    # =========================================================================
    
    tar_target(alm_model_data,
               build_alm_model_data(analysis_long)
    ),
    
    # ── Primary model ─────────────────────────────────────────────────────────
    tar_target(alm_model_fit_M3,
               fit_alm_model(alm_model_data,
                             tier            = "M3",
                             exposure        = "dairy_cumavg",
                             add_interaction = FALSE)
    ),
    
    # ── Covariate tier progression ────────────────────────────────────────────
    tar_target(alm_model_fit_M0,
               fit_alm_model(alm_model_data, tier = "M0")
    ),
    tar_target(alm_model_fit_M1,
               fit_alm_model(alm_model_data, tier = "M1")
    ),
    tar_target(alm_model_fit_M2,
               fit_alm_model(alm_model_data, tier = "M2")
    ),
    
    tar_target(alm_stability_table,
               make_alm_stability_table(
                   fits = list(M0 = alm_model_fit_M0,
                               M1 = alm_model_fit_M1,
                               M2 = alm_model_fit_M2,
                               M3 = alm_model_fit_M3),
                   exposure = "dairy_cumavg"
               )
    ),
    
    # # ── Exposure sensitivity ──────────────────────────────────────────────────
    # tar_target(alm_model_fit_S1,
    #            fit_alm_model(alm_model_data,
    #                          tier     = "M3",
    #                          exposure = "dairy_total_lag1")
    # ),
    # tar_target(alm_model_fit_S2,
    #            fit_alm_model(alm_model_data,
    #                          tier     = "M3",
    #                          exposure = "dairy_total_gday")
    # ),
    
    # ── Trajectory-modification model (secondary) ─────────────────────────────
    tar_target(alm_model_fit_interaction,
               fit_alm_model(alm_model_data,
                             tier            = "M3",
                             add_interaction = TRUE)
    ),
    
    # ── Outputs ───────────────────────────────────────────────────────────────
    tar_target(alm_model_table,
               make_alm_model_table(alm_model_fit_M3)
    ),
    tar_target(alm_model_plots,
               make_alm_model_plots(alm_model_fit_M3, alm_model_data)
    ),
    
    
    # =========================================================================
    # GAIT SPEED  (LME, random intercept ONLY — max 2 obs per participant)
    # =========================================================================
    
    tar_target(gait_model_data,
               build_gait_model_data(analysis_long)
    ),
    
    # ── Primary model ─────────────────────────────────────────────────────────
    tar_target(gait_model_fit_M3,
               fit_gait_model(gait_model_data,
                              tier            = "M3",
                              exposure        = "dairy_cumavg",
                              add_interaction = FALSE)
    ),
    
    # ── Covariate tier progression ────────────────────────────────────────────
    tar_target(gait_model_fit_M0,
               fit_gait_model(gait_model_data, tier = "M0")
    ),
    tar_target(gait_model_fit_M1,
               fit_gait_model(gait_model_data, tier = "M1")
    ),
    tar_target(gait_model_fit_M2,
               fit_gait_model(gait_model_data, tier = "M2")
    ),
    
    tar_target(gait_stability_table,
               make_gait_stability_table(
                   fits = list(M0 = gait_model_fit_M0,
                               M1 = gait_model_fit_M1,
                               M2 = gait_model_fit_M2,
                               M3 = gait_model_fit_M3),
                   exposure = "dairy_cumavg"
               )
    ),
    
    # # ── Exposure sensitivity ──────────────────────────────────────────────────
    # tar_target(gait_model_fit_S1,
    #            fit_gait_model(gait_model_data,
    #                           tier     = "M3",
    #                           exposure = "dairy_total_lag1")
    # ),
    # tar_target(gait_model_fit_S2,
    #            fit_gait_model(gait_model_data,
    #                           tier     = "M3",
    #                           exposure = "dairy_total_gday")
    # ),
    
    # ── Outputs ───────────────────────────────────────────────────────────────
    tar_target(gait_model_table,
               make_gait_model_table(gait_model_fit_M3)
    ),
    tar_target(gait_model_plots,
               make_gait_model_plots(gait_model_fit_M3, gait_model_data)
    ),
    
    
    # =========================================================================
    # COX MODEL: INCIDENT SARCOPENIA
    # =========================================================================
    
    tar_target(cox_model_data,
               build_cox_model_data(analysis_long)
    ),
    
    # ── Primary models (M3) ───────────────────────────────────────────────────
    tar_target(cox_quartile_M3,
               fit_cox_quartile(cox_model_data, tier = "M3")
    ),
    tar_target(cox_continuous_M3,
               fit_cox_continuous(cox_model_data,
                                  tier     = "M3",
                                  exposure = "dairy_cumavg")
    ),
    
    # ── Covariate tier progression ────────────────────────────────────────────
    tar_target(cox_quartile_M0,
               fit_cox_quartile(cox_model_data, tier = "M0")
    ),
    tar_target(cox_quartile_M1,
               fit_cox_quartile(cox_model_data, tier = "M1")
    ),
    tar_target(cox_quartile_M2,
               fit_cox_quartile(cox_model_data, tier = "M2")
    ),
    
    tar_target(cox_stability_table,
               make_cox_stability_table(
                   fits = list(M0 = cox_quartile_M0,
                               M1 = cox_quartile_M1,
                               M2 = cox_quartile_M2,
                               M3 = cox_quartile_M3)
               )
    ),
    
    # # ── Exposure sensitivity (continuous model) ───────────────────────────────
    # tar_target(cox_continuous_S1,
    #            fit_cox_continuous(cox_model_data,
    #                               tier     = "M3",
    #                               exposure = "dairy_total_lag1")
    # ),
    # tar_target(cox_continuous_S2,
    #            fit_cox_continuous(cox_model_data,
    #                               tier     = "M3",
    #                               exposure = "dairy_total_gday")
    # ),
    # 
    # ── Outputs ───────────────────────────────────────────────────────────────
    # Note: argument order is (quartile_fit, continuous_fit, data)
    tar_target(cox_quartile_table,
               make_cox_quartile_table(cox_quartile_M3)
    ),
    tar_target(cox_continuous_table,
               make_cox_continuous_table(cox_continuous_M3)
    ),
    tar_target(cox_model_plots,
               make_cox_model_plots(cox_quartile_M3,
                                    cox_continuous_M3,
                                    cox_model_data)
    ),
    
    
    # =========================================================================
    # EXCLUSION SENSITIVITY — min_visits = 3
    # Refits primary M3 models on a stricter sample (analysis_long_3v).
    # If the dairy signal persists, it is not driven by participants with
    # sparse follow-up. Uses the same build_*_model_data() and fit_*_model()
    # functions; only the input dataset changes.
    # =========================================================================
    
    tar_target(grip_model_data_3v,
               build_grip_model_data(analysis_long_3v)
    ),
    tar_target(grip_model_fit_M3_3v,
               fit_grip_model(grip_model_data_3v, tier = "M3")
    ),
    
    tar_target(alm_model_data_3v,
               build_alm_model_data(analysis_long_3v)
    ),
    tar_target(alm_model_fit_M3_3v,
               fit_alm_model(alm_model_data_3v, tier = "M3")
    ),
    
    tar_target(gait_model_data_3v,
               build_gait_model_data(analysis_long_3v)
    ),
    tar_target(gait_model_fit_M3_3v,
               fit_gait_model(gait_model_data_3v, tier = "M3")
    ),
    
    tar_target(cox_model_data_3v,
               build_cox_model_data(analysis_long_3v)
    ),
    tar_target(cox_quartile_M3_3v,
               fit_cox_quartile(cox_model_data_3v, tier = "M3")
    )
)

# =============================================================================
# 12 minimal models for exploration
# =============================================================================

# ── Exploration / minimal models ──────────────────────────────────────────────
minimal_models <-list(
    tar_target(exploration_results,
           run_exploration(analysis_long)
    ),

    tar_target(minimal_coef_table,
           exploration_results$coef_table
    )
)

# =============================================================================
# 13 subtype exploration
# =============================================================================


subtypes <- list(tar_target(subtype_results,
                            run_subtype_analysis(analysis_long)
))


# =============================================================================
# 14 Simple Cox model
# =============================================================================
simple_cox <- list(tar_target(cox_simple_results,
                              run_cox_simple(analysis_long)
))




# =============================================================================
# 15 models with splines and age groups
# =============================================================================

splines <- list(
        
        
        # Fit minimal spline models (stratified by age group)
        tar_target(
            grip_spline_fit,
            fit_minimal_grip_spline(analysis_long, age_groups = c(50, 60, 70, 80), random_slope = TRUE)
        ),
        
        tar_target(
            alm_spline_fit,
            fit_minimal_alm_spline(analysis_long, age_groups = c(50, 60, 70, 80), random_slope = TRUE)
        ),
        
        tar_target(
            gait_spline_fit,
            fit_minimal_gait_spline(analysis_long, age_groups = c(50, 60, 70, 80))
        ),
        
        # Optional: coefficient summary table
        tar_target(
            minimal_coef_table_spline,
            make_minimal_coef_table(grip_spline_fit, alm_spline_fit, gait_spline_fit)
        ),
        
        # Optional: diagnostic plots
        tar_target(
            grip_diag_plot_spline,
            plot_minimal_diagnostics(grip_spline_fit, title = "Grip strength spline model")
        ),
        
        tar_target(
            alm_diag_plot_spline,
            plot_minimal_diagnostics(alm_spline_fit, title = "ALMI spline model")
        ),
        
        tar_target(
            gait_diag_plot_spline,
            plot_minimal_diagnostics(gait_spline_fit, title = "Gait speed spline model")
        )
    )



# =============================================================================
# ASSEMBLE
# =============================================================================

c(
    path_targets,
    import_targets,
    harmonise_targets,
    qc_targets,
    stack_targets,
    derive_targets,
    wave_match_targets,
    derive_targets_both,
    freeze_targets
    # flow_graph_targets,
    # descriptive_targets,
    # smoking_descriptives,
    # wave_descriptives
    # model_targets,
    # minimal_models,
    # subtypes,
    # simple_cox,
    # splines
    # list(
    #     tarchetypes::tar_render(
    #         descriptive_report,
    #         path = "06_outputs/reports/descriptive_analysis.qmd"
    #     ),
    #     tarchetypes::tar_render(
    #         results_report,
    #         path = "06_outputs/reports/results.qmd"
    #     )
    # )
)
