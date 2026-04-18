#TODO add description
# =============================================================================
# R/derive_colaus_servings.R
# =============================================================================
derive_dairy_servings <- function(df) {
    
    # ── Ensure source columns are present -------------------------------------------------
    .FREQ_COLS <- paste0("freqFFQ", c(1:8, 85, 86))
    .PORT_COLS <- paste0("FFQp", c(1:8, 85, 86))
    
    required_cols <- c(.FREQ_COLS, .PORT_COLS)
    actual_cols <- names(df)
    missing_cols <- setdiff(required_cols, actual_cols)
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_dairy_servings: missing required columns: {.val {missing_cols}}.
        Dairy Servings category will not be derived."
        )
        return(df)
    }
    
    # ── Ensure Lazy State -------------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    
    # 1. Calculate Simple Frequency Sum (Dairy_freq_total)
    # 2. Calculate Portion Adjusted Sum (Dairy_portion_total)
    # 3. compare to precalculated value Dairy
    # Logic for FFQp: "Less" -> 0.5, "Equal" -> 1, "More" -> 1.5
    
    df <- df |>
        dplyr::mutate(
            # Simple frequency sum and round to 3 decimal places
            dairy_freq_total = round(rowSums(across(all_of(.FREQ_COLS)), na.rm = TRUE), 3),
            
            # Adjusted frequency based on portion size
            dairy_portion_total = rowSums(across(all_of(.FREQ_COLS)) * across(all_of(.PORT_COLS), ~case_when(
                .x == "Less"  ~ 0.5,
                .x == "More"  ~ 1.5,
                .x == "Equal" ~ 1.0,
                TRUE          ~ NA
            )), na.rm = TRUE),
            
            # Round the portion-adjusted total to 3 decimal places
            dairy_portion_total = round(dairy_portion_total, 3),
            
            # Compare to original Dairy to the newly calculated and derive a new column with the difference
            diff_freq = round(Dairy - dairy_freq_total, 3),
            diff_port = round(Dairy - dairy_portion_total, 3),
            
            
            # Define New Binary Columns as factor columns with levels "< 3 servings" and ">= 3 servings"
            Dairy_OK_freq = factor(if_else(dairy_freq_total >= 3, ">= 3 servings", "< 3 servings"), levels = c("< 3 servings", ">= 3 servings")),
            Dairy_OK_port = factor(if_else(dairy_portion_total >= 3, ">= 3 servings", "< 3 servings"), levels = c("< 3 servings", ">= 3 servings")
                                   )
        )
    
    # place the new columns next to Dairy and Dairy_ok & collection
    df <- df |>
        dplyr::relocate(dairy_freq_total, dairy_portion_total, Dairy_OK_freq, Dairy_OK_port, diff_freq, diff_port, .after = Dairy_OK) |>
        dplyr::as_tibble()
    
    # ──  Statistics- --------------------------------
    # sum of diff_freq that are not zero
    not_zero_freq <- sum(df$diff_freq != 0, na.rm = TRUE) 
    percentage_not_zero_freq <- round(not_zero_freq / nrow(df) * 100,2)
    not_zero_port <- sum(df$diff_port != 0, na.rm = TRUE)
    
    # show the number of cases where the new classifications differ from the original Dairy_OK
    cli::cli_h2("Dairy Serving Comparison")
    cli::cli_inform(c(
        "i" = "Differences vs Original Dairy_OK:",
        "*" = "Using Frequency sum: {not_zero_freq} cases ({percentage_not_zero_freq}%) of total",
        "*" = "Using Portion-adjusted sum: {not_zero_port} cases"
    ))
    
    
   
    
    return(df)
}