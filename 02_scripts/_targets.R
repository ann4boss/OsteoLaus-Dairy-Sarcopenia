# =============================================================================
# _targets.R
# CoLaus / OsteoLaus sarcopenia & dairy intake pipeline
# =============================================================================
#
# Pipeline stages
#TODO add description
# =============================================================================

library(targets)
library(tarchetypes)

tar_option_set(
    packages = c(
        "tidyverse","readr", "dplyr", "dtplyr", "data.table", "tidyr", "stringr",
        "lubridate", "forcats", "purrr", "glue", "tibble", "cli",
        "mice",
        "gtsummary", "ggplot2", "patchwork", "scales", "ggridges", "ggalluvial", "RColorBrewer", "patchwork", 
        "lme4", "lmerTest", "broom.mixed", "broom","rms","splines",
        "survival", "survminer", "gt", "survey",
        "mgcv", "car",
        "sessioninfo"
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

cc_analysis_prep <- tar_target(cc_route, build_analysis_dataset(core$colaus_long, core$osteo_long))


# ------------------------------------------------------------------------------
# Exclusion
# ------------------------------------------------------------------------------
cc_exclusion_targets <- list(
    tar_target(
        cc_analysis_general, apply_exclusions(data = cc_route$merged_derived,
                                      qc_table = core$qc_tbl,
                                      covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                         "mrtsts2", "education_level", "smoking_status",
                                                         "pa_levels_tertile_f1", "alcohol_category_conso",
                                                         "diabetes_status", "hrt_status", "htn_status",
                                                         "hypolip_drug_status", "corticoids_status", "vitD_status",
                                                         "calcium_status", "benzo_status", "bisphosphonate_status",
                                                         "sumtot1" 
                                                         ),
                                      exposure = "dairy_total_gday",
                                      outcome = NULL,
                                      visit_min = 2L,
                                      pt_col = "pt",
                                      visit_col = ".visit_osteo",
                                      impute = FALSE,
                                      imp_col = ".imp",
                                      return_tracking = TRUE)
    ),
    tar_target(
        cc_analysis_HGS, apply_exclusions(data = cc_route$merged_derived,
                                              qc_table = core$qc_tbl,
                                              covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                                 "mrtsts2", "education_level", "smoking_status",
                                                                 "pa_levels_tertile_f1", "alcohol_category_conso",
                                                                 "diabetes_status", "hrt_status", "htn_status",
                                                                 "hypolip_drug_status", "corticoids_status", "vitD_status",
                                                                 "calcium_status", "benzo_status", "bisphosphonate_status",
                                                                 "sumtot1" 
                                              ),
                                              exposure = "dairy_total_gday",
                                              outcome = "HGS_MAX",
                                              visit_min = 3L,
                                              pt_col = "pt",
                                              visit_col = ".visit_osteo",
                                              impute = FALSE,
                                              imp_col = ".imp",
                                              return_tracking = TRUE)
    ),
    tar_target(
        cc_analysis_ALM, apply_exclusions(data = cc_route$merged_derived,
                                              qc_table = core$qc_tbl,
                                              covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                                 "mrtsts2", "education_level", "smoking_status",
                                                                 "pa_levels_tertile_f1", "alcohol_category_conso",
                                                                 "diabetes_status", "hrt_status", "htn_status",
                                                                 "hypolip_drug_status", "corticoids_status", "vitD_status",
                                                                 "calcium_status", "benzo_status", "bisphosphonate_status",
                                                                 "sumtot1" 
                                              ),
                                              exposure = "dairy_total_gday",
                                              outcome = "ALM_HT2",
                                              visit_min = 2L,
                                              pt_col = "pt",
                                              visit_col = ".visit_osteo",
                                              impute = FALSE,
                                              imp_col = ".imp",
                                              return_tracking = TRUE)
    ),
    tar_target(
        cc_analysis_ALM_Lunar, apply_exclusions(data = cc_route$merged_derived,
                                          qc_table = core$qc_tbl,
                                          covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                             "mrtsts2", "education_level", "smoking_status",
                                                             "pa_levels_tertile_f1", "alcohol_category_conso",
                                                             "diabetes_status", "hrt_status", "htn_status",
                                                             "hypolip_drug_status", "corticoids_status", "vitD_status",
                                                             "calcium_status", "benzo_status", "bisphosphonate_status",
                                                             "sumtot1" 
                                          ),
                                          exposure = "dairy_total_gday",
                                          outcome = "ALM_HT2_Lunar",
                                          visit_min = 2L,
                                          pt_col = "pt",
                                          visit_col = ".visit_osteo",
                                          impute = FALSE,
                                          imp_col = ".imp",
                                          return_tracking = TRUE)
    ),
    tar_target(
        cc_analysis_gait, apply_exclusions(data = cc_route$merged_derived,
                                              qc_table = core$qc_tbl,
                                              covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                                 "mrtsts2", "education_level", "smoking_status",
                                                                 "pa_levels_tertile_f1", "alcohol_category_conso",
                                                                 "diabetes_status", "hrt_status", "htn_status",
                                                                 "hypolip_drug_status", "corticoids_status", "vitD_status",
                                                                 "calcium_status", "benzo_status", "bisphosphonate_status",
                                                                 "sumtot1" 
                                              ),
                                              exposure = "dairy_total_gday",
                                              outcome = "gait_speed",
                                              visit_min = 2L,
                                              pt_col = "pt",
                                              visit_col = ".visit_osteo",
                                              impute = FALSE,
                                              imp_col = ".imp",
                                              return_tracking = TRUE)
    ),
    
    tar_target(
        cc_analysis_sarcopenia, apply_exclusions(data = cc_route$merged_derived,
                                           qc_table = core$qc_tbl,
                                           covariant_list = c("Age", "BMI", 
                                                              "pa_levels_tertile_f1",
                                                              "sumtot1" 
                                           ),
                                           exposure = "dairy_total_gday",
                                           outcome = "ewgsop2_sarcopenia_stage",
                                           visit_min = 2L,
                                           pt_col = "pt",
                                           visit_col = ".visit_osteo",
                                           impute = FALSE,
                                           imp_col = ".imp",
                                           return_tracking = TRUE)
    )
)

# ------------------------------------------------------------------------------
# Table 1
# ------------------------------------------------------------------------------
cc_table_one <- list(
    tar_target(
        cc_table_one_outputs,
        save_table_one_outputs(
            analysis_long = cc_analysis_general$data,
            output_root = "03_outputs/TableOne",
            by = "dairy_quartile_baseline"
        )
    ),
    tar_target(
        cc_table_one_files,
        unlist(cc_table_one_outputs$files),
        format = "file"
    )
)

# ------------------------------------------------------------------------------
# Descriptives
# ------------------------------------------------------------------------------


purrr::walk(
    c(
        "03_outputs/descriptive",
        "03_outputs/descriptive/continuous",
        "03_outputs/descriptive/categorical",
        "03_outputs/descriptive/alluvial"
    ),
    ~dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)


cc_descriptives <-list(
    # Input dataset
    tar_target(
        descriptive_input,
        cc_analysis$data
    ),
    
    # Config
     tar_target(
        config,
        descriptive_config
    ),
    
    # Prepare data
     tar_target(
        descriptive_data,
        prepare_descriptive_data(
            descriptive_input,
            config
        )
    ),
    
    # Missingness
    tar_target(
        missingness_table,
        compute_missingness(
            data = descriptive_data$missingness_data,
            vars = c(
                config$continuous_vars,
                config$categorical_vars
            ),
            visit_var = config$visit_var
        )
    ),
    
    tar_target(
        missingness_heatmap,
        plot_missingness_heatmap(
            missingness_table
        )
    ),
    
    tar_target(
        save_missingness_heatmap,
        save_plot(
            missingness_heatmap,
            "03_outputs/descriptive/missingness_heatmap.png"
        ),
        format = "file"
    ),
    
    tar_target(
        
        missingness_summary_table,
        
        make_missing_summary(
            
            data = descriptive_data$analysis_data,
            
            vars = c(
                config$continuous_vars,
                config$categorical_vars
            ),
            
            visit_var = config$visit_var
        )
    ),
    
    tar_target(
        
        save_missingness_summary_table,
        
        save_table(
            
            missingness_summary_table,
            
            "03_outputs/descriptive/missingness_summary.csv"
        ),
        
        format = "file"
    ),
    
    # visit summary table
    tar_target(
        
        visit_summary_table,
        
        make_visit_summary(
            
            data = descriptive_data$analysis_data,
            
            visit_var = config$visit_var,
            
            id_var = config$id_var
        )
    ),
    
    tar_target(
        
        save_visit_summary_table,
        
        save_table(
            
            visit_summary_table,
            
            "03_outputs/descriptive/visit_summary.csv"
        ),
        
        format = "file"
    ),
    
    # Timing analysis
    tar_target(
        timing_summary,
        compute_timing_summary(
            descriptive_data$analysis_data,
            timing_var = "days_colaus_minus_osteo",
            visit_var = config$visit_var
        )
    ),
    
    tar_target(
        timing_violin,
        plot_timing_violin(
            descriptive_data$analysis_data,
            timing_var = "days_colaus_minus_osteo",
            visit_var = config$visit_var
        )
    ),
    
    tar_target(
        save_timing_violin,
        save_plot(
            timing_violin,
            "03_outputs/descriptive/timing_violin.png"
        ),
        format = "file"
    ),
    # Continuous variables
    tar_target(
        continuous_plots,
        make_continuous_plots(
            data = descriptive_data$analysis_data,
            vars = config$continuous_vars,
            visit_var = config$visit_var
        )
    ),
    
    tar_target(
        save_continuous_plots,
        {
            dir.create(
                "03_outputs/descriptive/continuous",
                recursive = TRUE,
                showWarnings = FALSE
            )
            
            paths <- purrr::imap_chr(
                continuous_plots,
                function(p, nm) {
                    
                    file <- file.path(
                        "03_outputs/descriptive/continuous",
                        paste0(nm, ".png")
                    )
                    
                    save_plot(
                        plot = p,
                        file = file
                    )
                    
                    file
                }
            )
            
            paths
        },
        format = "file"
    ),
    
    # Categorical variables
    tar_target(
        categorical_plots,
        purrr::map(
            config$categorical_vars,
            ~plot_categorical_over_time(
                data = descriptive_data$analysis_data,
                variable = .x,
                visit_var = config$visit_var
            )
        ) |>
            rlang::set_names(config$categorical_vars)
    ),
    
    tar_target(
        save_categorical_plots,
        {
            dir.create(
                "03_outputs/descriptive/categorical",
                recursive = TRUE,
                showWarnings = FALSE
            )
            
            paths <- purrr::imap_chr(
                categorical_plots,
                function(p, nm) {
                    
                    file <- file.path(
                        "03_outputs/descriptive/categorical",
                        paste0(nm, ".png")
                    )
                    
                    save_plot(
                        plot = p,
                        file = file
                    )
                    
                    file
                }
            )
            
            paths
        },
        format = "file"
    ),
    
    # Alluvial plots
    tar_target(
        
        alluvial_plots,
        
        purrr::map(
            
            config$categorical_vars,
            
            ~plot_categorical_alluvial(
                
                data = descriptive_data$analysis_data,
                
                variable = .x,
                
                visit_var = config$visit_var,
                
                id_var = config$id_var
                
            )
            
        ) |>
            rlang::set_names(
                config$categorical_vars
            )
    ),
    
    tar_target(
        
        save_alluvial_plots,
        
        {
            
            dir.create(
                "03_outputs/descriptive/alluvial",
                recursive = TRUE,
                showWarnings = FALSE
            )
            
            paths <- purrr::imap_chr(
                
                alluvial_plots,
                
                function(p, nm) {
                    
                    file <- file.path(
                        
                        "03_outputs/descriptive/alluvial",
                        
                        paste0(
                            nm,
                            "_alluvial.png"
                        )
                    )
                    
                    save_plot(
                        p,
                        file,
                        width = 10,
                        height = 6
                    )
                    
                    file
                }
            )
            
            paths
        },
        
        format = "file"
    ),
    
     # Table 1 summaries
    tar_target(
        pooled_continuous_table1,
        purrr::map_dfr(
            config$continuous_vars,
            function(v) {
                
                pool_continuous(
                    descriptive_data$analysis_data,
                    variable = v
                ) |>
                    dplyr::mutate(variable = v)
            }
        )
    ),
    
    tar_target(
        pooled_categorical_table1,
        purrr::map(
            config$categorical_vars,
            function(v) {
                
                pool_categorical(
                    descriptive_data$analysis_data,
                    variable = v
                ) |>
                    dplyr::mutate(variable_name = v)
            }
        )
    ),
    
    tar_target(
        save_table1_continuous,
        save_table(
            pooled_continuous_table1,
            "03_outputs/descriptive/table1_continuous.csv"
        ),
        format = "file"
    ),
    
    tar_target(
        save_table1_categorical,
        {
            combined <- dplyr::bind_rows(
                pooled_categorical_table1
            )
            
            save_table(
                combined,
                "03_outputs/descriptive/table1_categorical.csv"
            )
        },
        format = "file"
    )
)


        

# =============================================================================
# ── MICE ROUTE ────────────────────────────────────────────────────────────────
# =============================================================================

mice_analysis_prep <- tar_target(mice_route, build_analysis_dataset(core$colaus_long, core$osteo_long,
                                                               imputed = TRUE, m = 5L, maxit = 20L, seed = 2024L))

# ------------------------------------------------------------------------------
# Exclusion
# ------------------------------------------------------------------------------
mice_exclusion_targets <- list(
    tar_target(
        mice_analysis_general, apply_exclusions(data = mice_route$merged_derived,
                                              qc_table = core$qc_tbl,
                                              covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                                 "mrtsts2", "education_level", "smoking_status",
                                                                 "pa_levels_tertile_f1", "alcohol_category_conso",
                                                                 "diabetes_status", "hrt_status", "htn_status",
                                                                 "hypolip_drug_status", "corticoids_status", "vitD_status",
                                                                 "calcium_status", "benzo_status", "bisphosphonate_status",
                                                                 "sumtot1" 
                                              ),
                                              exposure = "dairy_total_gday",
                                              outcome = NULL,
                                              visit_min = 2L,
                                              pt_col = "pt",
                                              visit_col = ".visit_osteo",
                                              impute = FALSE,
                                              imp_col = ".imp",
                                              return_tracking = TRUE)
    ),
    tar_target(
        mice_analysis_HGS, apply_exclusions(data = mice_route$merged_derived,
                                          qc_table = core$qc_tbl,
                                          covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                             "mrtsts2", "education_level", "smoking_status",
                                                             "pa_levels_tertile_f1", "alcohol_category_conso",
                                                             "diabetes_status", "hrt_status", "htn_status",
                                                             "hypolip_drug_status", "corticoids_status", "vitD_status",
                                                             "calcium_status", "benzo_status", "bisphosphonate_status",
                                                             "sumtot1" 
                                          ),
                                          exposure = "dairy_total_gday",
                                          outcome = "HGS_MAX",
                                          visit_min = 3L,
                                          pt_col = "pt",
                                          visit_col = ".visit_osteo",
                                          impute = FALSE,
                                          imp_col = ".imp",
                                          return_tracking = TRUE)
    ),
    tar_target(
        mice_analysis_ALM, apply_exclusions(data = mice_route$merged_derived,
                                                 qc_table = core$qc_tbl,
                                                 covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                                    "mrtsts2", "education_level", "smoking_status",
                                                                    "pa_levels_tertile_f1", "alcohol_category_conso",
                                                                    "diabetes_status", "hrt_status", "htn_status",
                                                                    "hypolip_drug_status", "corticoids_status", "vitD_status",
                                                                    "calcium_status", "benzo_status", "bisphosphonate_status",
                                                                    "sumtot1" 
                                                 ),
                                                 exposure = "dairy_total_gday",
                                                 outcome = "ALM_HT2",
                                                 visit_min = 2L,
                                                 pt_col = "pt",
                                                 visit_col = ".visit_osteo",
                                                 impute = FALSE,
                                                 imp_col = ".imp",
                                                 return_tracking = TRUE)
    ),
    tar_target(
        mice_analysis_ALM_Lunar, apply_exclusions(data = mice_route$merged_derived,
                                            qc_table = core$qc_tbl,
                                            covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                               "mrtsts2", "education_level", "smoking_status",
                                                               "pa_levels_tertile_f1", "alcohol_category_conso",
                                                               "diabetes_status", "hrt_status", "htn_status",
                                                               "hypolip_drug_status", "corticoids_status", "vitD_status",
                                                               "calcium_status", "benzo_status", "bisphosphonate_status",
                                                               "sumtot1" 
                                            ),
                                            exposure = "dairy_total_gday",
                                            outcome = "ALM_HT2_Lunar",
                                            visit_min = 2L,
                                            pt_col = "pt",
                                            visit_col = ".visit_osteo",
                                            impute = FALSE,
                                            imp_col = ".imp",
                                            return_tracking = TRUE)
    ),
    tar_target(
        mice_analysis_gait, apply_exclusions(data = mice_route$merged_derived,
                                           qc_table = core$qc_tbl,
                                           covariant_list = c("Age", "Height", "Weight", "BMI", "BMI_category",
                                                              "mrtsts2", "education_level", "smoking_status",
                                                              "pa_levels_tertile_f1", "alcohol_category_conso",
                                                              "diabetes_status", "hrt_status", "htn_status",
                                                              "hypolip_drug_status", "corticoids_status", "vitD_status",
                                                              "calcium_status", "benzo_status", "bisphosphonate_status",
                                                              "sumtot1" 
                                           ),
                                           exposure = "dairy_total_gday",
                                           outcome = "gait_speed",
                                           visit_min = 2L,
                                           pt_col = "pt",
                                           visit_col = ".visit_osteo",
                                           impute = FALSE,
                                           imp_col = ".imp",
                                           return_tracking = TRUE)
    ),
    tar_target(
        mice_analysis_sarcopenia, apply_exclusions(data = mice_route$merged_derived,
                                                 qc_table = core$qc_tbl,
                                                 covariant_list = c("Age", "BMI", 
                                                                    "pa_levels_tertile_f1",
                                                                    "sumtot1"  
                                                 ),
                                                 exposure = "dairy_total_gday",
                                                 outcome = "ewgsop2_sarcopenia_stage",
                                                 visit_min = 2L,
                                                 pt_col = "pt",
                                                 visit_col = ".visit_osteo",
                                                 impute = FALSE,
                                                 imp_col = ".imp",
                                                 return_tracking = TRUE)
    )
)

# ------------------------------------------------------------------------------
# Table 1
# ------------------------------------------------------------------------------
mice_table_one <- list(
    tar_target(
        mice_table_one_outputs,
        save_table_one_outputs(
            analysis_long = mice_analysis_general$data,
            output_root = "03_outputs/TableOne",
            by = "dairy_quartile_baseline"
        )
        
    ),
    
    tar_target(
        mice_table_one_files,
        unlist(mice_table_one_outputs$files),
        format = "file"
    )
)



# =============================================================================
# LMM TARGETS
# =============================================================================
covariate_sets_hgs <- list(
    
    minimal = c(
        "age_at_baseline",
        "BMI"
    ),
    
    full_alcohol_conso = c(
        "alcohol_category_conso",
        "age_at_baseline", "BMI",
        "mrtsts2", "education_level",
        "smoking_status",
        "pa_levels_tertile_f1",
        "diabetes_status",
        "hrt_status", "htn_status",
        "hypolip_drug_status", "corticoids_status", "vitD_status", "calcium_status",
        "benzo_status", "bisphosphonate_status", 
        "sumtot1"
    ),
    
    full_alcohol_sumalco = c(
        "alcohol_category_sumalco",
        "age_at_baseline", "BMI",
        "mrtsts2", "education_level",
        "smoking_status",
        "pa_levels_tertile_f1",
        "diabetes_status",
        "hrt_status", "htn_status",
        "hypolip_drug_status", "corticoids_status", "vitD_status", "calcium_status",
        "benzo_status", "bisphosphonate_status", 
        "sumtot1"
    )
)

covariate_sets_alm <- list(
    
    minimal = c(
        "age_at_baseline"
    ),
    
    full_alcohol_conso = c(
        "alcohol_category_conso",
        "age_at_baseline", 
        "mrtsts2", "education_level",
        "smoking_status",
        "pa_levels_tertile_f1",
        "diabetes_status",
        "hrt_status", "htn_status",
        "hypolip_drug_status", "corticoids_status", "vitD_status", "calcium_status",
        "benzo_status", "bisphosphonate_status", 
        "sumtot1"
    ),
    
    full_alcohol_sumalco = c(
        "alcohol_category_sumalco",
        "age_at_baseline",
        "mrtsts2", "education_level",
        "smoking_status",
        "pa_levels_tertile_f1",
        "diabetes_status",
        "hrt_status", "htn_status",
        "hypolip_drug_status", "corticoids_status", "vitD_status", "calcium_status",
        "benzo_status", "bisphosphonate_status", 
        "sumtot1"
    )
)



LLM_targets_HGS <- list(
    tar_target(
        lmm_model_grid_hgs,
        create_model_grid(
            outcomes = c("HGS_MAX"),
            exposure_definitions = exposure_definitions,
            datasets = c("cc", "mice"),
            interactions = c(TRUE, FALSE),
            random_slopes = c(FALSE, TRUE),
            cov_sets = c(
                "minimal",
                "full_alcohol_conso",
                "full_alcohol_sumalco"
            )
        )
    ),
    tar_target(
        lmm_results_hgs,
        run_lmm_model(
            config = lmm_model_grid_hgs,
            cc_data = cc_analysis_HGS$data,
            mice_data = mice_analysis_HGS$data,
            covariate_sets = covariate_sets_hgs,
            id_var = "pt",
            time_var = "time_since_baseline"
        ),
        pattern = map(lmm_model_grid_hgs)
    ),
    
    tar_target(
        lmm_exports_hgs,
        export_lmm_results(lmm_results_hgs),
        pattern = map(lmm_results_hgs),
        format = "file"
    ),
    
    tar_target(
        lmm_diagnostics_hgs,
        run_lmm_diagnostics(
            lmm_results_hgs
        ),
        pattern = map(lmm_results_hgs)
    )
)


LLM_targets_ALM <- list(
    tar_target(
        lmm_model_grid_alm,
        create_model_grid(
            outcomes = c("ALM_HT2_harmonised"),
            exposure_definitions = exposure_definitions,
            datasets = c("cc", "mice"),
            interactions = c(TRUE, FALSE),
            random_slopes = c(FALSE, TRUE),
            cov_sets = c(
                "minimal",
                "full_alcohol_conso",
                "full_alcohol_sumalco"
            )
        )
    ),
    tar_target(
        lmm_results_alm,
        run_lmm_model(
            config = lmm_model_grid_alm,
            cc_data = cc_analysis_ALM$data,
            mice_data = mice_analysis_ALM$data,
            covariate_sets = covariate_sets_alm,
            id_var = "pt",
            time_var = "time_since_baseline"
        ),
        pattern = map(lmm_model_grid_alm)
    ),
    tar_target(
        lmm_exports_alm,
        export_lmm_results(lmm_results_alm),
        pattern = map(lmm_results_alm),
        format = "file"
    ),
    tar_target(
        lmm_diagnostics_alm,
        run_lmm_diagnostics(
            lmm_results_alm
        ),
        pattern = map(lmm_results_alm)
    )
)

LLM_targets_ALM_Lunar <- list(
    tar_target(
        lmm_model_grid_alm_lunar,
        create_model_grid(
            outcomes = c("ALM_HT2_Lunar"),
            exposure_definitions = exposure_definitions,
            datasets = c("cc", "mice"),
            interactions = c(TRUE, FALSE),
            random_slopes = c(FALSE, TRUE),
            cov_sets = c(
                "minimal",
                "full_alcohol_conso",
                "full_alcohol_sumalco"
            )
        )
    ),
    tar_target(
        lmm_results_alm_lunar,
        run_lmm_model(
            config = lmm_model_grid_alm_lunar,
            cc_data = cc_analysis_ALM_Lunar$data,
            mice_data = mice_analysis_ALM_Lunar$data,
            covariate_sets = covariate_sets_alm,
            id_var = "pt",
            time_var = "time_since_baseline"
        ),
        pattern = map(lmm_model_grid_alm_lunar)
    ),
    tar_target(
        lmm_exports_alm_lunar,
        export_lmm_results(lmm_results_alm_lunar),
        pattern = map(lmm_results_alm_lunar),
        format = "file"
    ),
    tar_target(
        lmm_diagnostics_alm_lunar,
        run_lmm_diagnostics(
            lmm_results_alm_lunar
        ),
        pattern = map(lmm_results_alm_lunar)
    )
)



# =============================================================================
# GAMM TARGETS
# =============================================================================
GAMM_targets_HGS <- list(
    
    # 1. MODEL GRID
    tar_target(
        gamm_model_grid_hgs,
        create_gamm_grid(
            outcomes             = c("HGS_MAX"),
            gamm_exposure_definitions = gamm_exposure_definitions,
            datasets             = c("cc", "mice"),
            interactions         = c(TRUE, FALSE),
            cov_sets             = c(
                "minimal",
                "full_alcohol_conso",
                "full_alcohol_sumalco"
            ),
            k_smooth = 4L
        )
    ),
    
    # 2. FIT
    tar_target(
        gamm_results_hgs,
        run_gamm_model(
            config         = gamm_model_grid_hgs,
            cc_data        = cc_analysis_HGS$data,
            mice_data      = mice_analysis_HGS$data,
            gamm_covariate_sets = gamm_covariate_sets,
            id_var         = "pt",
            time_var       = "time_since_baseline"
        ),
        pattern = map(gamm_model_grid_hgs)
    ),
    
    # 3. EXPORT COEFFICIENTS
    tar_target(
        gamm_exports_hgs,
        export_gamm_results(gamm_results_hgs),
        pattern = map(gamm_results_hgs),
        format  = "file"
    ),
    
    # 4. SMOOTH PLOTS  (skips silently for non-smooth exposure types)
    tar_target(
        gamm_smooth_plots_hgs,
        save_smooth_plots(
            result      = gamm_results_hgs,
            data        = cc_analysis_HGS$data,
            outcome_lab = "HGS Max",
            exposure_lab = "Dairy intake (g/day)"
        ),
        pattern = map(gamm_results_hgs),
        format  = "file"
    ),
    
    #  5. CC DIAGNOSTICS
    tar_target(
        gamm_diagnostics_hgs,
        run_gamm_diagnostics(gamm_results_hgs),
        pattern = map(gamm_results_hgs)
    ),
    
    # 6. MICE DIAGNOSTICS
    tar_target(
        gamm_mice_diagnostics_hgs,
        run_mice_gamm_diagnostics_full(gamm_results_hgs),
        pattern = map(gamm_results_hgs)
    ),
    
    tar_target(
        gamm_mice_diag_export_hgs,
        export_mice_gamm_diagnostics(gamm_mice_diagnostics_hgs),
        pattern = map(gamm_mice_diagnostics_hgs),
        format  = "file"
    )
)

# =============================================================================
# COX
# =============================================================================

cox_targets <- list(
    # # ── Example 1: CC, EWGSOP2, fixed covariates, continuous dairy ----------
    # tar_target(cc_ewgsop2_fixed_cont, run_cox_sarcopenia(
    #     data           = cc_analysis_sarcopenia$data,
    #     sarcopenia_def = "ewgsop2",
    #     covariate_type = "fixed",
    #     dairy_type     = "continuous",
    #     dairy_col        = "dairy_total_gday",
    #     analysis_route = "cc"
    # )
    # ),
    # 
    # # ── Example 2: CC, EWGSOP2, fixed covariates, categorical dairy ----------
    # tar_target(cc_ewgsop2_fixed_cat, run_cox_sarcopenia(
    #     data             = cc_analysis_sarcopenia$data,
    #     sarcopenia_def = "ewgsop2",
    #     covariate_type   = "fixed",
    #     dairy_type       = "categorical",
    #     dairy_cat_col    = "dairy_quartile_baseline",
    #     analysis_route = "cc"
    # )
    # ),
    
    # # ── Example 3: CC, EWGSOP2, time_dependent covariates, categorical dairy ----------
    # tar_target(cc_ewgsop2_timedep_cat, run_cox_sarcopenia(
    #     data             = cc_analysis_sarcopenia$data,
    #     sarcopenia_def = "ewgsop2",
    #     covariate_type   = "time_dependent",
    #     dairy_type       = "categorical",
    #     dairy_cat_col    = "dairy_quartile_baseline",
    #     analysis_route = "cc"
    # )
    # ),
    # # ── Example 4: MICE, EWGSOP2, fixed covariates, categorical dairy ----------
    # tar_target(mice_ewgsop2_fixed_cat, run_cox_sarcopenia(
    #     data             = mice_analysis_sarcopenia$data,
    #     sarcopenia_def = "ewgsop2",
    #     covariate_type   = "fixed",
    #     dairy_type       = "categorical",
    #     dairy_cat_col    = "dairy_quartile_baseline",
    #     analysis_route = "mice"
    # )
    # ),
    # # ── Example 5: MICE, FNIH, fixed covariates, categorical dairy ----------
    # tar_target(mice_fnih_fixed_cat, run_cox_sarcopenia(
    #     data             = mice_analysis_sarcopenia$data,
    #     sarcopenia_def = "fnih",
    #     covariate_type   = "fixed",
    #     dairy_type       = "categorical",
    #     dairy_cat_col    = "dairy_quartile_baseline",
    #     analysis_route = "mice"
    # )
    # ),
    # ── Example 5: MICE, EWGSOP2, fixed covariates, categorical dairy with interaction ----------
    tar_target(mice_ewgsop2_fixed_cat_int, run_cox_sarcopenia(
        data             = mice_analysis_sarcopenia$data,
        sarcopenia_def = "ewgsop2",
        covariate_type   = "fixed",
        dairy_type       = "categorical",
        dairy_cat_col    = "dairy_quartile_baseline",
        analysis_route = "mice",
        interaction_var = "pa_levels_tertile_f1"
    )
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
    cc_analysis_prep,
    cc_exclusion_targets,
    # cc_descriptives,
    #cc_table_one,
    # ── MICE route ────────────────────────────────────────────────────────────
    mice_analysis_prep,
    mice_exclusion_targets,
    #mice_table_one,
    
    # ── Model ──────────────────────────────
    LLM_targets_HGS,
    LLM_targets_ALM,
    LLM_targets_ALM_Lunar
    # GAMM_targets_HGS
    # cox_targets

)
