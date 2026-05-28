#' Save composite plots per numeric variable (histogram + density)
#'
#' @param df        Stacked long tibble.
#' @param out_dir   Directory to write PNG files into.
#' @param plot_type Type of plot: "overlay" (all visits on one plot) or 
#'                  "facet" (separate panel per visit). Default "overlay".
#' @param width,height  Plot dimensions in inches.
#' @param ncol_facet Number of columns for faceted plots (default 2).
qc_histograms <- function(df, out_dir, 
                          plot_type = c("overlay", "facet"),
                          width = 10, height = 6, 
                          ncol_facet = 2) {
    
    # Match plot_type argument
    plot_type <- match.arg(plot_type)
    
    # Create output directory
    hist_dir <- file.path(out_dir)
    dir.create(hist_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Collect data
    df <- dplyr::collect(df)
    
    # Identify numeric columns (exclude internal columns)
    num_cols <- names(df)[sapply(df, is.numeric)]
    num_cols <- setdiff(num_cols, c("pt", "visit_num"))
    
    # Get unique cohorts
    cohorts <- unique(as.character(df$.cohort))
    
    # Calculate total plots for reporting
    n_plots <- length(cohorts) * length(num_cols)
    cli::cli_inform("Saving up to {n_plots} {plot_type} plot(s) to {.path {hist_dir}} 
                    ({length(cohorts)} cohort(s) × {length(num_cols)} variable(s))")
    
    # Loop through cohorts and variables
    for (cohort in cohorts) {
        
        df_cohort <- df |> dplyr::filter(.cohort == cohort)
        
        for (col in num_cols) {
            
            # Skip variables not present for this cohort
            if (!col %in% names(df_cohort)) next
            
            # Prepare data for plotting
            plot_df <- df_cohort |>
                dplyr::select(dplyr::all_of(c(".cohort", ".visit", col))) |>
                dplyr::filter(!is.na(.data[[col]])) |>
                dplyr::mutate(visit_label = as.character(.visit))
            
            if (nrow(plot_df) == 0) next
            
            # Get implausible rules for this variable if they exist
            rule <- IMPLAUSIBLE_RULES |> dplyr::filter(variable == col)
            rule <- if (nrow(rule) > 0) rule else NULL
            
            # Create plot based on type
            if (plot_type == "overlay") {
                p <- .create_continuous_composite_plot(
                    data = plot_df,
                    variable = col,
                    group_var = "visit_label",
                    rule = rule,
                    cohort = cohort
                )
            } else {  # facet plot
                p <- .create_faceted_continuous_plot_wrapper(
                    data = plot_df,
                    variable = col,
                    facet_var = "visit_label",
                    rule = rule,
                    cohort = cohort,
                    ncol = ncol_facet
                )
            }
            
            # Save plot
            fname <- file.path(hist_dir, paste0(cohort, "_", col, "_", plot_type, ".png"))
            ggplot2::ggsave(fname, plot = p, width = width, height = height, dpi = 150)
            
            # Optional: Also save faceted version if overlay was requested
            if (plot_type == "overlay" && n_distinct(plot_df$visit_label) > 3) {
                cli::cli_alert_info(paste0(
                    "Consider using plot_type = 'facet' for {col} in {cohort} ",
                    "({n_distinct(plot_df$visit_label)} visits detected)"
                ))
            }
        }
    }
    
    cli::cli_inform(c("v" = "All {plot_type} plots saved to {.path {hist_dir}}"))
}

#' Internal wrapper for faceted continuous plots
#'
#' @param data Filtered data for one cohort/variable combination
#' @param variable Column name to plot
#' @param facet_var Variable to facet by (typically visit)
#' @param rule Implausible rules (tibble with min/max)
#' @param cohort Cohort name (for title)
#' @param ncol Number of facet columns
#' @return ggplot2 object
.create_faceted_continuous_plot_wrapper <- function(data,
                                                    variable,
                                                    facet_var,
                                                    rule = NULL,
                                                    cohort = NULL,
                                                    ncol = 2) {
    
    p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = .data[[variable]])
    ) +
        ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            bins = 50,
            fill = "#4e79a7",
            colour = "white",
            alpha = 0.6,
            linewidth = 0.2
        ) +
        ggplot2::geom_density(
            fill = "#e15759",
            colour = "#e15759",
            alpha = 0.3,
            linewidth = 0.8
        ) +
        ggplot2::facet_wrap(~ .data[[facet_var]], 
                            scales = "free_y", 
                            ncol = ncol) +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(
            strip.background = ggplot2::element_rect(fill = "#dce9f5"),
            strip.text = ggplot2::element_text(face = "bold"),
            plot.title = ggplot2::element_text(face = "bold", size = 14),
            plot.subtitle = ggplot2::element_text(colour = "grey40", size = 10),
            panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::labs(
            title = paste0(variable, if (!is.null(cohort)) paste0("  [", cohort, "]") else ""),
            subtitle = paste0("Distribution by ", gsub("_label$", "", facet_var)),
            x = variable,
            y = "Density"
        )
    
    # Add implausible-range bounds
    if (!is.null(rule) && nrow(rule) > 0) {
        if (!is.na(rule$min)) {
            p <- p + ggplot2::geom_vline(
                xintercept = rule$min,
                colour = "#2c3e50",
                linetype = "dashed",
                linewidth = 0.7,
                alpha = 0.8
            )
        }
        if (!is.na(rule$max)) {
            p <- p + ggplot2::geom_vline(
                xintercept = rule$max,
                colour = "#2c3e50",
                linetype = "dashed",
                linewidth = 0.7,
                alpha = 0.8
            )
        }
        
        # Add caption with range info
        min_text <- if (!is.na(rule$min)) sprintf("%.1f", rule$min) else "NA"
        max_text <- if (!is.na(rule$max)) sprintf("%.1f", rule$max) else "NA"
        p <- p + ggplot2::labs(
            caption = paste0("Dashed lines: plausible range [", min_text, ", ", max_text, "]")
        )
    }
    
    return(p)
}

#' Original overlay plot function (kept for reference)
.create_continuous_composite_plot <- function(data,
                                              variable,
                                              group_var,
                                              rule = NULL,
                                              cohort = NULL) {
    
    # Calculate binwidth using Freedman-Diaconis rule if possible
    x <- data[[variable]]
    binwidth <- 2 * stats::IQR(x) / (length(x)^(1/3))
    if (is.na(binwidth) | binwidth == 0) binwidth <- diff(range(x)) / 30
    
    # Base plot with histogram and density
    p <- ggplot2::ggplot(
        data,
        ggplot2::aes(
            x = .data[[variable]],
            colour = .data[[group_var]],
            fill = .data[[group_var]]
        )
    ) +
        ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            bins = 50,
            alpha = 0.3,
            position = "identity",
            colour = "white",
            linewidth = 0.2
        ) +
        ggplot2::geom_density(alpha = 0.4, linewidth = 0.8) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position = "bottom",
            legend.title = ggplot2::element_text(face = "bold"),
            plot.title = ggplot2::element_text(face = "bold", size = 14),
            plot.subtitle = ggplot2::element_text(colour = "grey40", size = 10),
            panel.grid.minor = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(fill = NA, colour = "grey70", linewidth = 0.5)
        ) +
        ggplot2::labs(
            title = paste0(variable, if (!is.null(cohort)) paste0("  [", cohort, "]") else ""),
            subtitle = paste0("Distribution by ", gsub("_label$", "", group_var)),
            x = variable,
            y = "Density",
            colour = "Visit",
            fill = "Visit"
        ) +
        ggplot2::scale_fill_brewer(palette = "Set2") +
        ggplot2::scale_colour_brewer(palette = "Set2")
    
    # Add statistical annotations (mean/median lines)
    stats_by_group <- data |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
            mean = mean(.data[[variable]], na.rm = TRUE),
            median = stats::median(.data[[variable]], na.rm = TRUE),
            .groups = "drop"
        )
    
    p <- p +
        ggplot2::geom_vline(
            data = stats_by_group,
            ggplot2::aes(xintercept = mean, colour = .data[[group_var]]),
            linetype = "dashed", linewidth = 0.5, alpha = 0.7
        ) +
        ggplot2::geom_vline(
            data = stats_by_group,
            ggplot2::aes(xintercept = median, colour = .data[[group_var]]),
            linetype = "dotted", linewidth = 0.5, alpha = 0.7
        )
    
    # Overlay plausible-range bounds
    if (!is.null(rule) && nrow(rule) > 0) {
        if (!is.na(rule$min)) {
            p <- p + ggplot2::geom_vline(
                xintercept = rule$min,
                colour = "#d95f02",
                linetype = "dashed",
                linewidth = 0.9,
                alpha = 0.8
            )
        }
        if (!is.na(rule$max)) {
            p <- p + ggplot2::geom_vline(
                xintercept = rule$max,
                colour = "#d95f02",
                linetype = "dashed",
                linewidth = 0.9,
                alpha = 0.8
            )
        }
        
        min_text <- if (!is.na(rule$min)) sprintf("min = %.1f", rule$min) else "min = NA"
        max_text <- if (!is.na(rule$max)) sprintf("max = %.1f", rule$max) else "max = NA"
        p <- p + ggplot2::labs(
            caption = paste0("Red dashed lines: plausible range [", min_text, ", ", max_text, "]\n",
                             "Dashed = mean, Dotted = median")
        )
    } else {
        p <- p + ggplot2::labs(
            caption = "Dashed = mean, Dotted = median"
        )
    }
    
    return(p)
}