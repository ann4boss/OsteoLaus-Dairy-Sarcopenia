# =============================================================================
# Wave matching function: CoLaus → OsteoLaus (backbone = OsteoLaus)
# =============================================================================
#' Match CoLaus waves to OsteoLaus waves by nearest exam date
#'
#' Uses OsteoLaus as the backbone dataset. Each OsteoLaus visit is enriched
#' with the closest CoLaus visit (within participant `pt`) using a rolling join.
#'
#' CoLaus visits may be reused across multiple OsteoLaus visits.
#'
#' Additionally:
#'   - Overlapping CoLaus columns are suffixed with "_colaus"
#'   - Matching quality is quantified via absolute date difference (days)
#'   - CoLaus visits that are never matched are retained and assigned to
#'     hypothetical ("IMPUTED") OsteoLaus rows
#'
#' @param colaus_long data.table or data.frame
#' @param osteo_long  data.table or data.frame
#' @param roll_direction character: "nearest", "past", "future"
#'
#' @return data.table with matched and unmatched rows
# =============================================================================

merge_closest_exams <- function(
        colaus, 
        osteolaus, 
        colaus_cols = c("pt", ".wave", "exam_date_iso", "Dairy", "Dairy_OK", "ethori_self", "mrtsts2", "HGS_MAX", "smoking_status", "cdv_event", "hrt_status",
                        "pa_levels", "dairy_total_gday", "dairy_fermented_gday", "dairy_non_fermented_gday", "dairy_lowfat_gday", "dairy_highfat_gday",
                        "hypolip_drug_status", "corticoids_status", "vitD_status", "calcium_status", "benzo_status", "bisphosphonate_status", "education_level",
                        "HTN_status","Age", "Height", "Weight", "BMI", "alcohol_category", "diabetes_status", "sumtot1"),
        osteo_cols = c("pt", ".wave", "exam_date_iso", "Height", "Weight", "BMI", "Age",
                       "HGS_MAX", "ALM","ALM_HT2", "ALM_BMI", "DXA_method", "gait_speed")
) {
    
    # ── Check if all the required columns exist  -----------------------------------------
    missing_osteo <- setdiff(osteo_cols, names(osteolaus))
    missing_colaus <- setdiff(colaus_cols, names(colaus))
    if (length(missing_osteo) > 0) {
        cli::cli_abort(
            "OsteoLaus is missing required columns: {.val {missing_osteo}}. \\
       Please check the input data and column specifications."
        )
    }
    if (length(missing_colaus) > 0) {
        cli::cli_abort(
            "CoLaus is missing required columns: {.val {missing_colaus}}.
       Please check the input data and column specifications."
        )
    }
    
    
    # ── Subset & convert to data.table -----------------------------------------
    osteo_dt <- as.data.table(osteolaus)[, .SD, .SDcols = intersect(osteo_cols, names(osteolaus))]
    col_dt   <- as.data.table(colaus)   [, .SD, .SDcols = intersect(colaus_cols, names(colaus))]
    
    # ── Restrict colaus to patients that exist in osteolaus -------------------
    pts_in_osteo <- unique(osteo_dt$pt)
    col_dt       <- col_dt[pt %in% pts_in_osteo]
    
    
    # ── Rename shared columns to prevent collision ----------------
    to_rename <- intersect(names(osteo_dt), names(col_dt))
    to_rename <- setdiff(to_rename, c("pt", "exam_date_iso")) # Don't rename join keys
    
    setnames(osteo_dt, to_rename, paste0(to_rename, "_osteo"))
    setnames(col_dt, to_rename, paste0(to_rename, "_colaus"))
    
    # ── Ensure Date class -----------------------------------------
    osteo_dt[, exam_date_iso := as.Date(exam_date_iso)]
    col_dt  [, exam_date_iso := as.Date(exam_date_iso)]
    
    # ── Rename columns with the same name in both columns ---------------------
    # rename overlapping columns in col_dt with a suffix to avoid conflicts during the join
    overlapping_cols <- intersect(names(col_dt), names(osteo_dt))
    overlapping_cols <- setdiff(overlapping_cols, c("pt", "exam_date_iso"))
    if (length(overlapping_cols) > 0) {
        new_names <- paste0(overlapping_cols, "_colaus")
        setnames(col_dt, old = overlapping_cols, new = new_names)
    }
    
    # ── Setup join keys -----------------------------------------
    # Create a copy so the colaus date survives the join
    col_dt[, colaus_exam_date := exam_date_iso]
    setnames(osteo_dt, "exam_date_iso", "osteo_exam_date")
    
    setkey(col_dt, pt, exam_date_iso)
    setkey(osteo_dt, pt, osteo_exam_date)
    
    # ── Rolling-nearest join -----------------------------------------
    matched <- col_dt[osteo_dt, roll = "nearest", on = .(pt, exam_date_iso = osteo_exam_date)]
    
    # Rename the join key back to osteo_exam_date
    setnames(matched, "exam_date_iso", "osteo_exam_date")
    
    # ── Signed day difference -----------------------------------------
    matched[, days_colaus_minus_osteo := as.integer(colaus_exam_date - osteo_exam_date)]
    
    # ── Unmatched colaus rows -----------------------------------------
    matched_keys <- unique(matched[!is.na(colaus_exam_date), .(pt, colaus_exam_date)])
    unmatched_col <- col_dt[!matched_keys, on = .(pt, colaus_exam_date)]
    
    # ── Stack matched + unmatched -----------------------------------------
    result <- rbind(matched, unmatched_col, fill = TRUE)
    
    # ── Create Unified Exam Date & HGS_MAX logic -----------------------------------------
    # Date logic
    result[, final_exam_date := fcoalesce(osteo_exam_date, colaus_exam_date)]
    
    # HGS_max Logic: Priority Colaus, but V5 priority Osteo
    result[, HGS_MAX := HGS_MAX_colaus]
    result[.wave_osteo == "V5", HGS_MAX := HGS_MAX_osteo]
    result[is.na(HGS_MAX), HGS_MAX := fcoalesce(HGS_MAX_colaus, HGS_MAX_osteo)]
    
    # Age, Height, BMI Logic: Priority Osteo, fill with Colaus
    # This works for the matched rows (prefers osteo) and unmatched rows (takes colaus)
    result[, Age    := fcoalesce(Age_osteo, Age_colaus)]
    result[, Height := fcoalesce(Height_osteo, Height_colaus)]
    result[, BMI    := fcoalesce(BMI_osteo, BMI_colaus)]
    
    
    # discard the intermediate columns
    result[, c("osteo_exam_date", "colaus_exam_date", "exam_date_iso") := NULL]
    result[, c("HGS_MAX_osteo", "HGS_MAX_colaus") := NULL]
    result[, c("Age_osteo", "Age_colaus", "Height_osteo", "Height_colaus", "BMI_osteo", "BMI_colaus", "Weight_colaus", "Weight_osteo") := NULL]
    
    
    # ── 10. Tidy up -----------------------------------------
    
    # Define the preferred lead columns
    desired_lead <- c("pt", "final_exam_date", ".wave_osteo",".wave_colaus", "osteo_exam_date", 
                      "colaus_exam_date", "days_colaus_minus_osteo","Age", "Weight", "BMI", "Heigth","HGS_MAX")
    
    # Identify which of those actually exist in the result
    present_lead <- intersect(desired_lead, names(result))
    
    # Identify all other columns currently in the table (the measurements)
    other_cols <- setdiff(names(result), present_lead)
    
    # Combine them to create a complete list of current columns
    final_column_set <- c(present_lead, other_cols)
    
    # Apply the order
    setcolorder(result, final_column_set)
    
    # Final sort by patient and the unified timeline
    setorder(result, pt, final_exam_date)
    
    return(result[])
}