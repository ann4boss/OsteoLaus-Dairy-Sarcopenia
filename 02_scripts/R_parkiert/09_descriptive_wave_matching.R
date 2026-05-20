# =============================================================================
# R/analyse_gap_distribution.R
# =============================================================================
# Generic analysis of time gaps and wave matching
# =============================================================================

#' Analyse gap distribution and wave matching
#'
#' @param df Data frame
#' @param wave_var Character, name of wave column (e.g. "osteo_wave")
#' @param match_var Character, name of matching wave column (e.g. ".colaus_wave")
#' @param gap_var Character, name of numeric gap column (e.g. ".gap_days")
#'
#' @return List with summary tables and ggplots
analyse_gap_distribution <- function(df,
                                     wave_var,
                                     match_var,
                                     gap_var) {
    
    # NSE setup
    wave_var  <- rlang::ensym(wave_var)
    match_var <- rlang::ensym(match_var)
    gap_var   <- rlang::ensym(gap_var)
    
    # -------------------------------------------------------------------------
    # 1. Summary statistics per wave
    # -------------------------------------------------------------------------
    summary_tbl <- df |>
        dplyr::group_by(!!wave_var) |>
        dplyr::summarise(
            n            = dplyr::n(),
            mean_gap     = mean(!!gap_var, na.rm = TRUE),
            sd_gap       = sd(!!gap_var, na.rm = TRUE),
            median_gap   = median(!!gap_var, na.rm = TRUE),
            p25_gap      = quantile(!!gap_var, 0.25, na.rm = TRUE),
            p75_gap      = quantile(!!gap_var, 0.75, na.rm = TRUE),
            min_gap      = min(!!gap_var, na.rm = TRUE),
            max_gap      = max(!!gap_var, na.rm = TRUE),
            .groups = "drop"
        )
    
    # -------------------------------------------------------------------------
    # 2. Distribution plot per wave
    # -------------------------------------------------------------------------
    plot_hist <- ggplot2::ggplot(df, ggplot2::aes(x = !!gap_var)) +
        ggplot2::geom_histogram(bins = 30) +
        ggplot2::facet_wrap(ggplot2::vars(!!wave_var), scales = "free_y") +
        ggplot2::labs(
            title = "Distribution of gap days per wave",
            x = "Gap (days)",
            y = "Count"
        )
    
    # -------------------------------------------------------------------------
    # 3. Boxplot per wave
    # -------------------------------------------------------------------------
    plot_box <- ggplot2::ggplot(df,
                                ggplot2::aes(x = !!wave_var, y = !!gap_var)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(
            title = "Gap days by wave",
            x = "Wave",
            y = "Gap (days)"
        )
    
    # -------------------------------------------------------------------------
    # 4. Matching table between waves
    # -------------------------------------------------------------------------
    match_tbl <- df |>
        dplyr::count(!!wave_var, !!match_var, name = "n") |>
        dplyr::group_by(!!wave_var) |>
        dplyr::mutate(p = n / sum(n)) |>
        dplyr::ungroup()
    
    # -------------------------------------------------------------------------
    # 5. Matching heatmap
    # -------------------------------------------------------------------------
    plot_match <- ggplot2::ggplot(
        match_tbl,
        ggplot2::aes(x = !!match_var, y = !!wave_var, fill = p)
    ) +
        ggplot2::geom_tile() +
        ggplot2::labs(
            title = "Wave matching proportions",
            x = "Matched wave",
            y = "Reference wave",
            fill = "Proportion"
        )
    
    # -------------------------------------------------------------------------
    # Return everything
    # -------------------------------------------------------------------------
    return(list(
        summary      = summary_tbl,
        match_table  = match_tbl,
        plot_hist    = plot_hist,
        plot_box     = plot_box,
        plot_match   = plot_match
    ))
}