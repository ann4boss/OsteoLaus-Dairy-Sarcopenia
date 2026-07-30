# =============================================================================
# export_manuscript_outputs.R
# Collects the specific figures/tables cited in the manuscript from
# 03_outputs/ into 03_outputs/manuscript/, under stable Figure/Table names.
#
# Run from the project root (working directory = repo root), e.g.:
#   source("02_scripts/export_manuscript_outputs.R")
# or as a plain script:
#   Rscript 02_scripts/export_manuscript_outputs.R
#

# Some Cox model outputs live in run folders whose name ends in a
# date/time stamp (e.g. mice_fnih_fixed_categorical_27072026_1813) that
# changes every time 04_03_cox.R is rerun. For those, the source is given
# as a glob pattern ("..._categorical_*") and resolve_path() picks the
# most recently modified matching folder, so this script keeps working
# without edits across reruns.
#
# Figures/tables with source = NA are assembled by hand (schematics, DAGs,
# multi-panel diagnostic composites) and are not produced by the pipeline;
# they are still listed here so the manifest documents every item in the
# manuscript's figure/table list.
# =============================================================================

library(tools)

out_dir <- "03_outputs/manuscript"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------

# Resolves a source path, expanding "*" globs (used for timestamped Cox
# run folders) and picking the most recently modified match.
resolve_path <- function(pattern) {
    matches <- Sys.glob(pattern)
    if (length(matches) == 0) {
        return(NA_character_)
    }
    if (length(matches) > 1) {
        mtime <- file.info(matches)$mtime
        matches <- matches[order(mtime, decreasing = TRUE)]
        message(sprintf(
            "  multiple matches for '%s' -- using most recent: %s",
            pattern, matches[1]
        ))
    }
    matches[1]
}

# Copies a single source file to out_dir/<dest_name>.<original extension>.
export_single <- function(dest_name, source_pattern) {
    if (is.na(source_pattern)) {
        message(sprintf("[MANUAL ] %s -- assembled by hand, not exported", dest_name))
        return(NA_character_)
    }
    src <- resolve_path(source_pattern)
    if (is.na(src)) {
        warning(sprintf("[MISSING] %s -- no file found for: %s", dest_name, source_pattern), call. = FALSE)
        return(NA_character_)
    }
    dest <- file.path(out_dir, paste0(dest_name, ".", file_ext(src)))
    file.copy(src, dest, overwrite = TRUE)
    message(sprintf("[OK     ] %s -> %s", dest_name, dest))
    dest
}

# Row-binds several *_main_coefs.csv files (one per alternative exposure)
# into a single table, tagging each block of rows with the source model
# so the combined table stays traceable back to its exposure-specific fit.
export_combined_csv <- function(dest_name, source_patterns) {
    srcs <- vapply(source_patterns, resolve_path, character(1))
    missing <- is.na(srcs)
    if (any(missing)) {
        warning(sprintf(
            "[MISSING] %s -- %d of %d source files not found:\n%s",
            dest_name, sum(missing), length(srcs),
            paste("   ", source_patterns[missing], collapse = "\n")
        ), call. = FALSE)
    }
    srcs <- srcs[!missing]
    if (length(srcs) == 0) {
        return(NA_character_)
    }

    combined <- do.call(rbind, lapply(srcs, function(f) {
        df <- read.csv(f, stringsAsFactors = FALSE)
        cbind(exposure_model = file_path_sans_ext(basename(f)), df)
    }))

    dest <- file.path(out_dir, paste0(dest_name, ".csv"))
    write.csv(combined, dest, row.names = FALSE)
    message(sprintf("[OK     ] %s -> %s (combined %d files)", dest_name, dest, length(srcs)))
    dest
}

# -----------------------------------------------------------------------
# Manifest: one entry per figure/table in the manuscript
# Each entry: id, title, source (single path, glob pattern, character
# vector of paths to combine, or NA for hand-assembled items)
# -----------------------------------------------------------------------

manifest <- list(
    list(id = "Figure01", title = "Overview_of_study_timeline_and_measurements",
         source = NA),
    list(id = "Figure02", title = "Flow_diagram_of_participant_selection",
         source = "03_outputs/descriptives/consort_flow.png"),
    list(id = "Figure03", title = "Days_between_matched_CoLaus_OsteoLaus_exam_dates_by_visit",
         source = "03_outputs/descriptives/visits/mice_timing_violin.png"),
    list(id = "Figure04", title = "Missing_data_before_and_after_imputation",
         source = "03_outputs/descriptives/missingness/missing_comparison.png"),
    list(id = "Figure05", title = "Stability_of_dairy_intake_quartile_membership",
         source = "03_outputs/descriptives/variables/mice/alluvial_dairy_quartile_baseline.png"),
    list(id = "Figure06", title = "Mean_total_dairy_intake_by_age",
         source = "03_outputs/descriptives/age_trajectories/mice/trajectory_dairy_total_gday_cumavg.png"),
    list(id = "Figure07", title = "Mean_handgrip_strength_by_age",
         source = "03_outputs/descriptives/age_trajectories/mice/trajectory_HGS_MAX.png"),
    list(id = "Figure08", title = "Mean_gait_speed_by_age",
         source = "03_outputs/descriptives/age_trajectories/mice/trajectory_gait_speed.png"),
    list(id = "Figure09", title = "Mean_appendicular_lean_mass_index_by_age",
         source = "03_outputs/descriptives/age_trajectories/mice/trajectory_ALM_HT2_harmonised.png"),
    list(id = "Figure10", title = "Cumulative_dairy_intake_and_handgrip_strength_by_visit",
         source = "03_outputs/descriptives/variables/mice/scatter_hgs_HGS_MAX vs dairy_total_gday_cumavg.png"),
    list(id = "Figure11", title = "Cumulative_dairy_intake_and_ALMI_by_visit",
         source = "03_outputs/descriptives/variables/mice/scatter_alm_ALM_HT2_harmonised vs dairy_total_gday_cumavg.png"),
    list(id = "Figure12", title = "Cumulative_dairy_intake_and_gait_speed_by_visit",
         source = "03_outputs/descriptives/variables/mice/scatter_gait_gait_speed vs dairy_total_gday_cumavg_lag.png"),

    list(id = "FigureS01", title = "DAG_handgrip_strength_analysis", source = NA),
    list(id = "FigureS02", title = "DAG_ALMI_analysis", source = NA),
    list(id = "FigureS03", title = "DAG_gait_speed_analysis", source = NA),
    list(id = "FigureS04", title = "DAG_sarcopenia_incidence_analysis", source = NA),

    list(id = "FigureS05", title = "Convergence_imputed_left_arm_lean_mass_baseline_OsteoLaus", source = NA),
    list(id = "FigureS06", title = "Density_observed_vs_imputed_left_arm_lean_mass_baseline_OsteoLaus", source = NA),
    list(id = "FigureS07", title = "Distribution_observed_vs_imputed_left_arm_lean_mass_baseline_OsteoLaus", source = NA),
    list(id = "FigureS08", title = "Convergence_imputed_vigorous_PA_followup1_CoLaus", source = NA),
    list(id = "FigureS09", title = "Density_observed_vs_imputed_vigorous_PA_followup1_CoLaus", source = NA),
    list(id = "FigureS10", title = "Distribution_observed_vs_imputed_vigorous_PA_followup1_CoLaus", source = NA),

    list(id = "FigureS11", title = "Change_in_BMI_category_across_visits",
         source = "03_outputs/descriptives/variables/mice/alluvial_BMI_category.png"),
    list(id = "FigureS12", title = "Change_in_smoking_status_across_visits",
         source = "03_outputs/descriptives/variables/mice/alluvial_smoking_status.png"),
    list(id = "FigureS13", title = "Change_in_diabetes_status_across_visits",
         source = "03_outputs/descriptives/variables/mice/alluvial_diabetes_status.png"),

    list(id = "FigureS14", title = "Diagnostics_handgrip_strength_LMM",
         source = "03_outputs/LMM_exploratory/HGS/HGS_MAX___dairy_100g__linear______main_main_diagnostics.png"),
    list(id = "FigureS15", title = "Diagnostics_ALMI_LMM",
         source = "03_outputs/LMM_exploratory/ALMI/ALM_HT2_harmonised___dairy_100g__linear______main_main_diagnostics.png"),
    list(id = "FigureS16", title = "Diagnostics_gait_speed_LMM",
         source = "03_outputs/LMM_exploratory/gait_speed/gait_speed___dairy_100g_lag__linear______main_main_diagnostics.png"),

    # Cox run folder name carries a date/time stamp that changes on rerun
    # (04_03_cox.R) -- glob on the stable prefix and take the newest match.
    list(id = "FigureS17", title = "Dose_response_dairy_intake_incident_sarcopenia",
         source = "03_outputs/Cox/mice_ewgsop2_fixed_continuous_spline3_*/mice_spline_hr.png"),

    list(id = "Table01", title = "Dairy_food_items_and_portion_sizes_FFQ", source = NA),
    list(id = "Table02", title = "Baseline_characteristics_by_dairy_intake_quartile",
         source = "03_outputs/TableOne/mice/quartile.html"),
    list(id = "Table03", title = "Adjusted_association_dairy_intake_handgrip_strength",
         source = "03_outputs/LMM_exploratory/HGS/HGS_MAX___dairy_100g__linear______main_coefs.csv"),
    list(id = "Table04", title = "Adjusted_associations_alternative_dairy_exposures_handgrip_strength",
         source = c(
             "03_outputs/LMM_exploratory/HGS/HGS_MAX___dairy_guidelines_port__categorical______main_coefs.csv",
             "03_outputs/LMM_exploratory/HGS/HGS_MAX___dairy_quartile_baseline__categorical______main_coefs.csv",
             "03_outputs/LMM_exploratory/HGS/HGS_MAX___fermented_100g__linear______main_coefs.csv",
             "03_outputs/LMM_exploratory/HGS/HGS_MAX___highfat_100g__linear______main_coefs.csv",
             "03_outputs/LMM_exploratory/HGS/HGS_MAX___lowfat_100g__linear______main_coefs.csv",
             "03_outputs/LMM_exploratory/HGS/HGS_MAX___nonfermented_100g__linear______main_coefs.csv"
         )),
    list(id = "Table05", title = "Adjusted_association_dairy_intake_ALMI",
         source = "03_outputs/LMM_exploratory/ALMI/ALM_HT2_harmonised___dairy_100g__linear______main_coefs.csv"),
    list(id = "Table06", title = "Adjusted_associations_alternative_dairy_exposures_ALMI",
         source = c(
             "03_outputs/LMM_exploratory/ALMI/ALM_HT2_harmonised___dairy_guidelines_port__categorical______main_coefs.csv",
             "03_outputs/LMM_exploratory/ALMI/ALM_HT2_harmonised___dairy_quartile_baseline__categorical______main_coefs.csv",
             "03_outputs/LMM_exploratory/ALMI/ALM_HT2_harmonised___fermented_100g__linear______main_coefs.csv",
             "03_outputs/LMM_exploratory/ALMI/ALM_HT2_harmonised___highfat_100g__linear______main_coefs.csv",
             "03_outputs/LMM_exploratory/ALMI/ALM_HT2_harmonised___lowfat_100g__linear______main_coefs.csv",
             "03_outputs/LMM_exploratory/ALMI/ALM_HT2_harmonised___nonfermented_100g__linear______main_coefs.csv"
         )),
    list(id = "Table07", title = "Adjusted_association_dairy_intake_gait_speed",
         source = "03_outputs/LMM_exploratory/gait_speed/gait_speed___dairy_100g_lag__linear______main_coefs.csv"),
    list(id = "Table08", title = "Adjusted_associations_alternative_dairy_exposures_gait_speed",
         source = c(
             "03_outputs/LMM_exploratory/gait_speed/gait_speed___dairy_guidelines_port_lag__categorical______main_coefs.csv",
             "03_outputs/LMM_exploratory/gait_speed/gait_speed___dairy_quartile_baseline_lag__categorical______main_coefs.csv",
             "03_outputs/LMM_exploratory/gait_speed/gait_speed___fermented_100g_lag__linear______main_coefs.csv",
             "03_outputs/LMM_exploratory/gait_speed/gait_speed___highfat_100g_lag__linear______main_coefs.csv",
             "03_outputs/LMM_exploratory/gait_speed/gait_speed___lowfat_100g_lag__linear______main_coefs.csv",
             "03_outputs/LMM_exploratory/gait_speed/gait_speed___nonfermented_100g_lag__linear______main_coefs.csv"
         )),

    # Cox run folders below are timestamped and change on rerun -- globbed
    # on the stable prefix, newest match wins (see FigureS17 note above).
    list(id = "Table09", title = "Incident_EWGSOP2_sarcopenia_by_dairy_quartile",
         source = "03_outputs/Cox/mice_ewgsop2_time_dependent_categorical_*/mice_incidence_by_quartile.csv"),
    list(id = "Table10", title = "Cox_regression_dairy_quartiles_incident_EWGSOP2_sarcopenia",
         source = "03_outputs/Cox/mice_ewgsop2_time_dependent_categorical_*/mice_model_results.csv"),
    list(id = "Table11", title = "Cox_regression_dairy_quartiles_incident_FNIH_sarcopenia",
         source = "03_outputs/Cox/mice_fnih_fixed_categorical_*/mice_incidence_by_quartile.csv"),

    list(id = "TableS01", title = "FFQ_dairy_item_to_category_mapping", source = NA),
    list(id = "TableS02", title = "Baseline_characteristics_included_vs_excluded",
         source = "03_outputs/TableOne/mice/included_vs_excluded.html"),
    list(id = "TableS03", title = "Spline_Cox_model_dairy_intake_incident_sarcopenia",
         source = "03_outputs/Cox/mice_ewgsop2_time_dependent_continuous_spline3_*/mice_model_results.csv")
)

# -----------------------------------------------------------------------
# Run export + write manifest
# -----------------------------------------------------------------------

results <- lapply(manifest, function(item) {
    dest_name <- paste(item$id, item$title, sep = "_")
    dest <- if (length(item$source) > 1) {
        export_combined_csv(dest_name, item$source)
    } else {
        export_single(dest_name, item$source)
    }
    status <- if (length(item$source) == 1 && is.na(item$source)) {
        "manual"
    } else if (is.na(dest)) {
        "missing"
    } else {
        "exported"
    }
    data.frame(
        id = item$id,
        title = item$title,
        status = status,
        source = paste(item$source, collapse = " | "),
        dest_file = ifelse(is.na(dest), NA, basename(dest)),
        stringsAsFactors = FALSE
    )
})

manifest_df <- do.call(rbind, results)
write.csv(manifest_df, file.path(out_dir, "manifest.csv"), row.names = FALSE)

message(sprintf(
    "\nDone: %d exported, %d manual, %d missing. See %s/manifest.csv",
    sum(manifest_df$status == "exported"),
    sum(manifest_df$status == "manual"),
    sum(manifest_df$status == "missing"),
    out_dir
))
