#TODO does not work
plot_dairy_distributions <- function(df_result) {
    # 1. Collect from dtplyr and filter carefully
    # Using backticks because of the dot prefix
    wave_var <- ".wave"
    
    plot_data <- df_result %>% 
        filter(wave_var == "F1") 
    
    plot_data %>% 
        as_tibble()
    
    # 2. Check if data is empty
    if(nrow(plot_data) == 0) {
        cli::cli_alert_danger("No data found for F1 in column '.wave'")
        return(NULL)
    }
    
    
    # 3. Density Plots
    p1 <- ggplot(plot_data, aes(x = dairy_total_gday, fill = Dairy_OK)) +
        geom_density(alpha = 0.5) +
        labs(title = "Original Dairy_OK", fill = "Category")
    
    p2 <- ggplot(plot_data, aes(x = dairy_total_gday, fill = Dairy_OK_port)) +
        geom_density(alpha = 0.5) +
        labs(title = "Portion-Adjusted Dairy_OK", fill = "Category")
    
    print(p1 / p2)
    
    # 4. Table Comparison (Accessing columns directly via $)
    overlap_table <- table(plot_data$Dairy_OK, plot_data$Dairy_OK_port)
    
    return(overlap_table)
}