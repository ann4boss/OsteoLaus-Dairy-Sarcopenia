# Load required libraries
library(ggplot2)
library(paletteer)

# Filter data (T1, T2, T3 only)
df_filtered <- subset(df, time_point %in% c("T1", "T2", "T3"))

# Create publication-ready violin plot
p <- ggplot(df_filtered, aes(x = time_point, y = days_colaus_minus_osteo, fill = time_point)) +
    geom_violin(trim = FALSE, alpha = 0.7, linewidth = 0.3) +
    geom_boxplot(width = 0.15, fill = "white", alpha = 0.7, linewidth = 0.3,
                 outlier.size = 0.8, outlier.alpha = 0.5, outlier.linewidth = 0.2) +
    stat_summary(fun = "median", geom = "point", size = 1.5, shape = 21, fill = "black") +
    scale_fill_manual(values = paletteer_d("MetBrewer::Hiroshige", n = 3)) +
    scale_x_discrete(limits = c("T1", "T2", "T3")) +  # Ensure correct order
    labs(
        title = NULL,  # Title goes in caption, not on figure
        x = "Time Point",
        y = "Days (Colaus - Osteo)"
    ) +
    theme_classic(base_size = 10) +
    theme(
        legend.position = "none",
        axis.title = element_text(size = 10, face = "plain"),
        axis.text = element_text(size = 8, color = "black"),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        axis.ticks = element_line(linewidth = 0.3, color = "black"),
        axis.ticks.length = unit(1.5, "pt"),
        plot.margin = margin(5, 5, 5, 5, "pt")
    )

# Save with appropriate settings
ggsave("violin_plot.tiff", plot = p, width = 3.5, height = 4, 
       dpi = 600, compression = "lzw")
ggsave("violin_plot.pdf", plot = p, width = 3.5, height = 4, 
       device = "pdf")