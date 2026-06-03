add_ffq_columns <- function(base_df, add_df, timepoint) {
    
    exclude_nums <- c(1:8, 52, 53, 63, 68, 71, 82:86)
    
    # match both patterns:
    ffq_cols_tp <- grep(
        paste0("^", timepoint, "(freq)?FFQ"),
        names(add_df),
        value = TRUE
    )
    
    # filter excluded indices
    ffq_cols_tp <- ffq_cols_tp[
        !as.integer(gsub(".*FFQ([0-9]+).*", "\\1", ffq_cols_tp)) %in% exclude_nums
    ]
    
    extra_cols <- c(
        "cmp", "hdv", "chf", "artm", "cad", "angn", "miac",
        "strk", "vslg", "ccth", "cabg", "datquest"
    )
    
    extra_cols_tp <- paste0(timepoint, extra_cols)
    
    cols_to_add <- c("pt", intersect(c(ffq_cols_tp, extra_cols_tp), names(add_df)))
    
    add_subset <- add_df[, cols_to_add, drop = FALSE]
    
    # convert datquest to Date if present
    datquest_col <- paste0(timepoint, "datquest")
    
    if (datquest_col %in% names(add_subset)) {
        add_subset[[datquest_col]] <- parse_exam_date(add_subset[[datquest_col]])
    }
    
    merge(base_df, add_subset, by = "pt", all.x = TRUE)

}

add_death_date <- function(baseline_df, death_df) {
    
    # ----------------------------
    # Death date
    # ----------------------------
    death_subset <- death_df[, c("pt", "datdeath"), drop = FALSE]
    death_subset$datdeath <- parse_exam_date(death_subset$datdeath)
    death_subset <- death_subset[!duplicated(death_subset$pt), ]
    
    merged <- merge(baseline_df, death_subset, by = "pt", all.x = TRUE)
    
    
    return(merged)
}

add_birth_date <- function(baseline_df,birth_df) {

    
    # ----------------------------
    # Birth date
    # ----------------------------
    birth_subset <- birth_df[, c("pt", "datbirth"), drop = FALSE]
    birth_subset$datbirth <- parse_exam_date(birth_subset$datbirth)
    birth_subset <- birth_subset[!duplicated(birth_subset$pt), ]
    
    merged <- merge(baseline_df, birth_subset, by = "pt", all.x = TRUE)
    
    return(merged)
}