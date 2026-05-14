# ============================================================================
# descriptive_config
# ============================================================================

descriptive_config <- list(
    
    id_var = "pt",
    visit_var = ".visit_osteo",
    
    continuous_vars = c(
        "Age", "Height", "Weight", "BMI",
        "HGS_MAX", "ALM_HT2", "gait_speed",
        "sumtot1",
        "dairy_total_gday",
        "dairy_fermented_gday",
        "dairy_non_fermented_gday",
        "dairy_lowfat_gday",
        "dairy_highfat_gday",
        "days_colaus_minus_osteo"
    ),
    
    categorical_vars = c(
        "BMI_category",
        "smoking_status",
        "alcohol_category_conso",
        "alcohol_category_sumalco",
        "pa_levels_tertile_f1",
        "pa_levels_tertile_f2",
        "pa_levels_who_f1",
        "pa_levels_who_f2",
        "diabetes_status",
        "htn_status",
        "hrt_status",
        "hypolip_drug_status",
        "corticoids_status",
        "vitD_status",
        "calcium_status",
        "benzo_status",
        "mrtsts2",
        "ewgsop2_sarcopenia_stage",
        "fnih_sarcopenia"
    ),
    
    labels = c(
        Age = "Age (yr)",
        Height = "Height (cm)",
        Weight = "Weight (kg)",
        BMI = "BMI (kg/m²)",
        HGS_MAX = "Grip strength (kg)",
        ALM_HT2 = "ALMI (kg/m²)",
        gait_speed = "Gait speed (m/s)"
    )
)


# ============================================================================
# descriptive_helpers 
# ============================================================================

is_imputed <- function(data) {
    ".imp" %in% names(data) &&
        length(unique(data$.imp)) > 1
}

get_analysis_data <- function(data) {
    
    if (!is_imputed(data)) {
        return(data)
    }
    
    data |>
        dplyr::filter(.imp > 0)
}

get_missingness_data <- function(data,
                                 id_var,
                                 visit_var) {
    
    if (!is_imputed(data)) {
        return(data)
    }
    
    vars <- setdiff(
        names(data),
        c(id_var, visit_var, ".imp")
    )
    
    data |>
        dplyr::group_by(
            dplyr::across(
                dplyr::all_of(c(id_var, visit_var))
            )
        ) |>
        dplyr::summarise(
            dplyr::across(
                dplyr::all_of(vars),
                ~ all(is.na(.x))
            ),
            .groups = "drop"
        )
}

prepare_descriptive_data <- function(data,
                                     config) {
    
    list(
        
        analysis_data = get_analysis_data(data),
        
        missingness_data = get_missingness_data(
            data,
            id_var = config$id_var,
            visit_var = config$visit_var
        )
    )
}

# ============================================================================
# descriptive_missingness 
# ============================================================================

compute_missingness <- function(data,
                                vars,
                                visit_var) {
    
    purrr::map_dfr(vars, function(v) {
        
        data |>
            dplyr::group_by(.data[[visit_var]]) |>
            dplyr::summarise(
                
                n_total = dplyr::n(),
                
                n_miss = sum(is.na(.data[[v]])),
                
                pct_miss = 100 * n_miss / n_total,
                
                .groups = "drop"
                
            ) |>
            dplyr::mutate(variable = v)
    })
}

plot_missingness_heatmap <- function(miss_df) {
    
    ggplot2::ggplot(
        miss_df,
        ggplot2::aes(
            x = .data$`.visit_osteo`,
            y = .data$variable,
            fill = .data$pct_miss
        )
    ) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::theme_minimal()
}

# ============================================================================
# descriptive_missingness_tables
# ============================================================================

make_missing_summary <- function(data,
                                 vars,
                                 visit_var,
                                 visit_order = NULL) {
    
    # --------------------------------------------------------------------------
    # visit sample sizes
    # --------------------------------------------------------------------------
    
    visit_n <- data |>
        dplyr::group_by(.data[[visit_var]]) |>
        dplyr::summarise(
            n = dplyr::n(),
            .groups = "drop"
        )
    
    if (!is.null(visit_order)) {
        
        visit_n[[visit_var]] <- factor(
            visit_n[[visit_var]],
            levels = visit_order
        )
        
        visit_n <- visit_n |>
            dplyr::arrange(.data[[visit_var]])
    }
    
    # --------------------------------------------------------------------------
    # Missingness per variable per visit
    # --------------------------------------------------------------------------
    
    miss_long <- data |>
        dplyr::group_by(.data[[visit_var]]) |>
        dplyr::summarise(
            
            dplyr::across(
                dplyr::all_of(vars),
                ~ round(mean(is.na(.x)) * 100, 1)
            ),
            
            .groups = "drop"
        ) |>
        tidyr::pivot_longer(
            
            cols = dplyr::all_of(vars),
            
            names_to = "variable",
            
            values_to = "pct_missing"
        ) |>
        dplyr::left_join(
            visit_n,
            by = visit_var
        ) |>
        dplyr::mutate(
            
            visit_label = glue::glue(
                "{.data[[visit_var]]}\n(n = {scales::comma(n)})"
            )
        )
    
    # --------------------------------------------------------------------------
    # Wide format
    # --------------------------------------------------------------------------
    
    miss_long |>
        dplyr::select(
            variable,
            visit_label,
            pct_missing
        ) |>
        tidyr::pivot_wider(
            
            names_from = visit_label,
            
            values_from = pct_missing
            
        )
}

# ============================================================================
# descriptive_visit_summary 
# ============================================================================

make_visit_summary <- function(data,
                              visit_var,
                              id_var,
                              outcome_vars = c(
                                  "ewgsop2_sarcopenia_stage",
                                  "HGS_MAX",
                                  "ALM_HT2",
                                  "gait_speed"
                              ),
                              exposure_var = "dairy_total_gday") {
    
    data |>
        dplyr::group_by(.data[[visit_var]]) |>
        dplyr::summarise(
            
            n_participants =
                dplyr::n_distinct(.data[[id_var]]),
            
            across(
                dplyr::all_of(outcome_vars),
                
                ~ round(
                    mean(!is.na(.x)) * 100,
                    1
                ),
                
                .names = "pct_complete_{.col}"
            ),
            
            pct_ffq_complete =
                round(
                    mean(!is.na(.data[[exposure_var]])) * 100,
                    1
                ),
            
            median_dairy_gday =
                round(
                    median(
                        .data[[exposure_var]],
                        na.rm = TRUE
                    ),
                    1
                ),
            
            .groups = "drop"
        )
}


# ============================================================================
# descriptive_timing
# ============================================================================

compute_timing_summary <- function(data,
                                   timing_var,
                                   visit_var) {
    
    data |>
        dplyr::group_by(.data[[visit_var]]) |>
        dplyr::summarise(
            
            n = sum(!is.na(.data[[timing_var]])),
            
            mean = mean(.data[[timing_var]], na.rm = TRUE),
            
            sd = sd(.data[[timing_var]], na.rm = TRUE),
            
            median = median(.data[[timing_var]], na.rm = TRUE),
            
            p25 = quantile(.data[[timing_var]], 0.25, na.rm = TRUE),
            
            p75 = quantile(.data[[timing_var]], 0.75, na.rm = TRUE),
            
            .groups = "drop"
        )
}

plot_timing_violin <- function(data,
                               timing_var,
                               visit_var) {
    
    ggplot2::ggplot(
        data,
        ggplot2::aes(
            x = .data[[visit_var]],
            y = .data[[timing_var]],
            fill = .data[[visit_var]]
        )
    ) +
        ggplot2::geom_violin() +
        ggplot2::geom_boxplot(width = 0.15) +
        ggplot2::theme_minimal()
}

# ============================================================================
# descriptive_continuous
# ============================================================================

plot_continuous_by_visit <- function(data,
                                     variable,
                                     visit_var) {
    
    ggplot2::ggplot(
        data,
        ggplot2::aes(
            x = .data[[variable]],
            colour = .data[[visit_var]],
            fill = .data[[visit_var]]
        )
    ) +
        ggplot2::geom_density(alpha = 0.2) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = variable
        )
}

make_continuous_plots <- function(data,
                                  vars,
                                  visit_var) {
    
    purrr::map(
        vars,
        ~plot_continuous_by_visit(
            data,
            variable = .x,
            visit_var = visit_var
        )
    ) |>
        rlang::set_names(vars)
}

# ============================================================================
# FILE: R/descriptive_categorical.R
# ============================================================================

plot_categorical_over_time <- function(data,
                                       variable,
                                       visit_var) {
    
    prop_df <- data |>
        dplyr::filter(!is.na(.data[[variable]])) |>
        dplyr::count(
            .data[[visit_var]],
            .data[[variable]]
        ) |>
        dplyr::group_by(.data[[visit_var]]) |>
        dplyr::mutate(
            pct = 100 * n / sum(n)
        )
    
    ggplot2::ggplot(
        prop_df,
        ggplot2::aes(
            x = .data[[visit_var]],
            y = .data$pct,
            fill = .data[[variable]]
        )
    ) +
        ggplot2::geom_col() +
        ggplot2::theme_minimal()
}

# ============================================================================
# FILE: R/descriptive_alluvial.R
# ============================================================================

plot_categorical_alluvial <- function(data,
                                      variable,
                                      visit_var,
                                      id_var,
                                      min_visits = 2) {
    
    if (!requireNamespace("ggalluvial", quietly = TRUE)) {
        
        stop(
            "Package 'ggalluvial' is required."
        )
    }
    
    # --------------------------------------------------------------------------
    # Keep required columns
    # --------------------------------------------------------------------------
    
    df <- data |>
        dplyr::select(
            dplyr::all_of(
                c(id_var, visit_var, variable)
            )
        ) |>
        dplyr::filter(
            !is.na(.data[[visit_var]])
        )
    
    # --------------------------------------------------------------------------
    # Remove duplicate participant-visit rows
    # --------------------------------------------------------------------------
    
    dup_check <- df |>
        dplyr::count(
            .data[[id_var]],
            .data[[visit_var]]
        ) |>
        dplyr::filter(n > 1)
    
    if (nrow(dup_check) > 0) {
        
        warning(
            "Duplicate participant-visit rows detected for ",
            variable,
            ". Keeping first observation."
        )
        
        df <- df |>
            dplyr::group_by(
                .data[[id_var]],
                .data[[visit_var]]
            ) |>
            dplyr::slice(1) |>
            dplyr::ungroup()
    }
    
    # --------------------------------------------------------------------------
    # Keep participants with >= min_visits
    # --------------------------------------------------------------------------
    
    keep_ids <- df |>
        dplyr::filter(
            !is.na(.data[[variable]])
        ) |>
        dplyr::count(.data[[id_var]]) |>
        dplyr::filter(n >= min_visits) |>
        dplyr::pull(.data[[id_var]])
    
    df <- df |>
        dplyr::filter(
            .data[[id_var]] %in% keep_ids
        )
    
    # --------------------------------------------------------------------------
    # Prepare factors
    # --------------------------------------------------------------------------
    
    df[[visit_var]] <- factor(
        df[[visit_var]],
        levels = unique(df[[visit_var]])
    )
    
    df[[variable]] <- as.factor(
        df[[variable]]
    )
    
    # --------------------------------------------------------------------------
    # Plot
    # --------------------------------------------------------------------------
    
    ggplot2::ggplot(
        
        df,
        
        ggplot2::aes(
            
            x = .data[[visit_var]],
            
            stratum = .data[[variable]],
            
            alluvium = .data[[id_var]],
            
            fill = .data[[variable]],
            
            label = .data[[variable]]
        )
        
    ) +
        
        ggalluvial::geom_flow(
            alpha = 0.5
        ) +
        
        ggalluvial::geom_stratum(
            alpha = 0.9
        ) +
        
        ggplot2::geom_text(
            stat = "stratum",
            size = 3
        ) +
        
        ggplot2::labs(
            
            title = paste(
                variable,
                "over time"
            ),
            
            x = "Visit",
            
            y = "Participants"
            
        ) +
        
        ggplot2::theme_minimal()
}

# ============================================================================
# FILE: R/descriptive_table1.R
# ============================================================================

pool_continuous <- function(data,
                            variable) {
    
    # --------------------------------------------------------------------------
    # Non-imputed dataset
    # --------------------------------------------------------------------------
    
    if (!".imp" %in% names(data)) {
        
        return(
            tibble::tibble(
                
                pooled_mean =
                    mean(data[[variable]], na.rm = TRUE),
                
                pooled_sd =
                    sd(data[[variable]], na.rm = TRUE)
                
            )
        )
    }
    
    # --------------------------------------------------------------------------
    # Multiply imputed dataset
    # --------------------------------------------------------------------------
    
    data |>
        dplyr::filter(.imp > 0) |>
        dplyr::group_by(.imp) |>
        dplyr::summarise(
            
            mean =
                mean(.data[[variable]], na.rm = TRUE),
            
            sd =
                sd(.data[[variable]], na.rm = TRUE),
            
            .groups = "drop"
            
        ) |>
        dplyr::summarise(
            
            pooled_mean = mean(mean),
            
            pooled_sd = mean(sd)
            
        )
}

pool_categorical <- function(data,
                             variable) {
    
    # --------------------------------------------------------------------------
    # Non-imputed dataset
    # --------------------------------------------------------------------------
    
    if (!".imp" %in% names(data)) {
        
        return(
            
            data |>
                dplyr::filter(!is.na(.data[[variable]])) |>
                dplyr::count(.data[[variable]]) |>
                dplyr::mutate(
                    
                    pooled_prop = n / sum(n)
                    
                )
        )
    }
    
    # --------------------------------------------------------------------------
    # Multiply imputed dataset
    # --------------------------------------------------------------------------
    
    data |>
        dplyr::filter(.imp > 0) |>
        dplyr::group_by(.imp) |>
        dplyr::count(.data[[variable]]) |>
        dplyr::group_by(.imp) |>
        dplyr::mutate(
            
            prop = n / sum(n)
            
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(.data[[variable]]) |>
        dplyr::summarise(
            
            pooled_prop = mean(prop),
            
            .groups = "drop"
            
        )
}


# ============================================================================
# FILE: R/descriptive_save.R
# ============================================================================

save_plot <- function(plot,
                      file,
                      width = 8,
                      height = 6) {
    
    ggplot2::ggsave(
        filename = file,
        plot = plot,
        width = width,
        height = height
    )
    
    file
}

save_table <- function(table,
                       file) {
    
    readr::write_csv(table, file)
    
    file
}

