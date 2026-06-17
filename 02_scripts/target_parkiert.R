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


# ------------------------------------------------------------------------------
# Manuscript result numbers and flow diagram
# ------------------------------------------------------------------------------
result_section_targets <- list(
    tar_target(
        mice_result_summary,
        make_result_numbers(
            general_analysis = mice_analysis_general,
            outcome_analyses = list(
                HGS = mice_analysis_HGS,
                ALM = mice_analysis_ALM,
                ALM_Lunar = mice_analysis_ALM_Lunar,
                gait = mice_analysis_gait,
                sarcopenia = mice_analysis_sarcopenia
            )
        )
    ),
    
    tar_target(
        mice_flow_diagram,
        plot_analysis_flow(mice_result_summary)
    ),
    
    tar_target(
        mice_flow_diagram_file,
        save_analysis_flow(mice_flow_diagram),
        format = "file"
    ),
    
    tar_target(
        table1_cc_vs_mice,
        make_table_one_cc_vs_mice(
            cc_analysis_long = cc_analysis_general$data,
            mice_analysis_long = mice_analysis_general$data
        )
    ),
    
    tar_target(
        table1_included_vs_excluded_mice,
        make_table_one_included_vs_excluded_mice(
            full_analysis_long = mice_route$merged_derived,
            included_analysis_long = mice_analysis_general$data
        )
    ),
    
    tar_target(
        table1_supplement_files,
        {
            dir.create("03_outputs/TableOne/supplement", recursive = TRUE, showWarnings = FALSE)
            c(
                save_gtsummary_table(
                    table1_cc_vs_mice,
                    "03_outputs/TableOne/supplement/Table1_cc_vs_mice"
                ),
                save_gtsummary_table(
                    table1_included_vs_excluded_mice,
                    "03_outputs/TableOne/supplement/Table1_included_vs_excluded_mice"
                )
            )
        },
        format = "file"
    )
)


# ------------------------------------------------------------------------------
# Table 1
# ------------------------------------------------------------------------------

table_one <- list(
    tar_target(
        table1_overall_cc,
        make_table_one(
            cc_analysis_general$data,
            id    = "pt",
            visit = ".visit_osteo"
        )
    ),
    
    tar_target(
        table1_overall_cc_files,
        save_table_one_outputs_targets(
            overall = table1_overall_cc,
            by_exposure = NULL,
            method = "cc"
        ),
        format = "file"
    ),
    
    tar_target(
        table1_overall_mice,
        make_table_one(
            mice_analysis_general$data,
            id      = "pt",
            visit   = ".visit_osteo",
            imp_col = ".imp"
        )
    ),
    
    tar_target(
        table1_overall_mice_files,
        save_table_one_outputs_targets(
            overall = table1_overall_mice,
            by_exposure = NULL,
            method = "mice"
        ),
        format = "file"
    )
)



# ------------------------------------------------------------------------------
# Descriptives
# ------------------------------------------------------------------------------


histograms <- list(
    tar_target(histograms_colaus, qc_histograms(core$colaus_long, "03_outputs/descriptive/histograms/colaus", plot_type = "facet")),
    tar_target(histograms_osteo, qc_histograms(core$osteo_long, "03_outputs/descriptive/histograms/osteo", plot_type = "facet"))
)

variable_quality_control <- list(
    tar_target(qc_variable_colaus, qc_variables(core$colaus_long, cohort = "colaus", "03_outputs/descriptives/histograms/colaus")),
    tar_target(qc_variable_osteo, qc_variables(core$osteo_long, cohort, "osteo", "03_outputs/descriptives/histograms/osteo"))
)


plot_scatter_all <- list(tar_target(scatter_plots, 
                                    plot_scatter(cc_analysis_general$data, 
                                                 x= c("dairy_total_gday",
                                                      "dairy_quartile_baseline",
                                                      "dairy_fermented_gday",
                                                      "dairy_highfat_gday",
                                                      "dairy_guidelines_port"), 
                                                 y = c(
                                                     "HGS_MAX",
                                                     "ALM_HT2_harmonised",
                                                     "gait_speed",
                                                     "ewgsop2_low_perf",
                                                     "ewgsop2_sarcopenia_stage"),
                                                 by_visit      = TRUE,
                                                 add_smooth = TRUE,
                                                 smooth_method = "lm"
                                    )
)
)


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
        cc_analysis_general$data
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