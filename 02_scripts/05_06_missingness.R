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


make_missing_summary(cc_route$merged_derived, 
                     vars = c("age_at_baseline","time_since_baseline", "BMI_category", "education_level", "smoking_status",
                             "pa_levels_who_f1","pa_levels_who_f1", "diabetes_status", "sumtot1", "HGS_MAX", "gait_speed", 
                            "ALM_HT2_harmonised", "dairy_quartile_baseline" ,"dairy_total_gday_cumavg","dairy_guidelines_port", 
                            "ewgsop2_sarcopenia_stage", "fnih_sarcopenia"),
                     visit_var = "time_point")