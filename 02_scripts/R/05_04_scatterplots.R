plot_scatter <- function(data,
                             x,
                             y,
                             by_visit      = FALSE,
                             visit_var     = ".visit_osteo",
                             output_dir    = "03_outputs/descriptive",
                             point_alpha   = 0.7,
                             point_size    = 2.5,
                             add_smooth    = FALSE,
                             smooth_method = "lm",
                             smooth_span   = 0.75,
                             use_jitter    = TRUE,
                             jitter_width  = 0.15,
                             width         = 6,
                             height        = 5,
                             dpi           = 300,
                             save_plots    = TRUE,
                             return_list   = TRUE) {
    
    require(ggplot2)
    require(dplyr)
    
    x <- as.character(x)
    y <- as.character(y)
    
    # ── timestamped output folder ───────────────────────────────────────────
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    
    plot_dir <- file.path(
        output_dir,
        paste0("scatterplot_", timestamp)
    )
    
    if (save_plots) {
        dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # ── combinations ---------------------------------------------------------
    combinations <- expand.grid(
        x = x,
        y = y,
        stringsAsFactors = FALSE
    )
    
    plots <- list()
    
    # ── loop over x/y --------------------------------------------------------
    for (i in seq_len(nrow(combinations))) {
        
        x_var <- combinations$x[i]
        y_var <- combinations$y[i]
        
        is_cat_x <- is.factor(data[[x_var]]) || is.character(data[[x_var]])
        is_num_y <- is.numeric(data[[y_var]])
        
        p <- ggplot(
            data = data,
            aes(x = .data[[x_var]], y = .data[[y_var]])
        )
        
        # points / jitter
        if (use_jitter && is_cat_x) {
            
            p <- p +
                geom_jitter(
                    width = jitter_width,
                    alpha = point_alpha,
                    size  = point_size
                )
            
        } else {
            
            p <- p +
                geom_point(
                    alpha = point_alpha,
                    size  = point_size
                )
        }
        
        # smoother
        if (isTRUE(add_smooth) && !is_cat_x && is_num_y) {
            
            p <- p +
                geom_smooth(
                    method = smooth_method,
                    se = TRUE,
                    span = if (smooth_method == "loess") smooth_span else NULL,
                    linewidth = 0.8
                )
        }
        
        # ── facet by visit (single combined file) ────────────────────────────
        if (by_visit) {
            
            if (!visit_var %in% names(data)) {
                stop("visit_var not found in data")
            }
            
            p <- p +
                facet_wrap(stats::as.formula(paste("~", visit_var)))
        }
        
        # ── theme -------------------------------------------------------------
        p <- p +
            labs(
                x = x_var,
                y = y_var,
                title = paste0(y_var, " vs ", x_var)
            ) +
            theme_bw(base_size = 12) +
            theme(
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(linewidth = 0.2),
                panel.border     = element_rect(linewidth = 0.8),
                axis.title       = element_text(face = "bold"),
                axis.text        = element_text(color = "black"),
                plot.title       = element_text(hjust = 0.5, face = "bold"),
                strip.background = element_rect(fill = "grey90"),
                strip.text       = element_text(face = "bold"),
                legend.position  = "none"
            )
        
        # ── name --------------------------------------------------------------
        plot_name <- paste0(
            y_var,
            "_vs_",
            x_var,
            if (by_visit) "_by_visit" else ""
        )
        
        plots[[plot_name]] <- p
        
        # ── save --------------------------------------------------------------
        if (save_plots) {
            
            ggsave(
                filename = file.path(plot_dir, paste0(plot_name, ".png")),
                plot     = p,
                width    = if (by_visit) 10 else width,
                height   = if (by_visit) 7 else height,
                dpi      = dpi
            )
        }
    }
    
    # ── summary --------------------------------------------------------------
    if (save_plots) {
        cli::cli_h2("Scatter plot export")
        cli::cli_inform(c(
            "v" = "{length(plots)} plot(s) saved",
            "i" = "Directory: {.file {plot_dir}}"
        ))
    }
    
    if (return_list) return(plots)
    
    invisible(plots)
}