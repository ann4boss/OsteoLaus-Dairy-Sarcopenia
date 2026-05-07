# =============================================================================
# visit matching function: CoLaus → OsteoLaus (backbone = OsteoLaus)
# =============================================================================
#' Match CoLaus visits to OsteoLaus visits by nearest exam date
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
        osteolaus
) {
    
    # ── Check if all the required columns exist  -----------------------------------------
    # TODO add cols exam date and pt check to make sure they exist
    
    
    # ── Subset & convert to data.table -----------------------------------------
    osteo_dt <- data.table::as.data.table(osteolaus)
    col_dt   <- data.table::as.data.table(colaus)
    
    
    # ── Ensure Date class -----------------------------------------------------
    osteo_dt[, exam_date_iso := as.Date(exam_date_iso)]
    col_dt  [, exam_date_iso := as.Date(exam_date_iso)]
    
    
    # ── Remove rows with missing dates ---------------------------------
    n_osteo_removed <- sum(is.na(osteo_dt$exam_date_iso))
    n_col_removed   <- sum(is.na(col_dt$exam_date_iso))
    
    n_osteo_pt_removed <- osteolaus |>
        dplyr::filter(is.na(exam_date_iso)) |>
        dplyr::distinct(pt) |>
        nrow()
    
    n_col_pt_removed <- colaus |>
        dplyr::filter(is.na(exam_date_iso)) |>
        dplyr::distinct(pt) |>
        nrow()
    
    if (n_osteo_removed > 0)
        cli::cli_inform(
            "Removed {n_osteo_removed} OsteoLaus rows with missing exam_date_iso ({n_osteo_pt_removed} unique patients)"
        )
    
    if (n_col_removed > 0)
        cli::cli_inform(
            "Removed {n_col_removed} CoLaus rows with missing exam_date_iso ({n_col_pt_removed} unique patients)"
        )
    
    # Remove rows with missing exam_date_iso before the join, as they cannot be matched
    osteo_dt <- osteo_dt[!is.na(exam_date_iso)]
    col_dt   <- col_dt[!is.na(exam_date_iso)]
    
    # ── Restrict colaus to patients that exist in osteolaus -------------------
    col_dt <- col_dt[pt %in% unique(osteo_dt$pt)]
    
    
    # ── Rename shared columns to prevent collision ----------------
    overlap <- intersect(names(col_dt), names(osteo_dt))
    overlap <- setdiff(overlap, c("pt", "exam_date_iso"))
    
    data.table::setnames(col_dt, overlap, paste0(overlap, "_colaus"))
    data.table::setnames(osteo_dt, overlap, paste0(overlap, "_osteo"))
    
    
    # ── Setup join keys -----------------------------------------
    col_dt[, colaus_exam_date := exam_date_iso]
    osteo_dt[, osteo_exam_date := exam_date_iso]
    
    data.table::setkey(col_dt, pt, exam_date_iso)
    data.table::setkey(osteo_dt, pt, osteo_exam_date)
    
    
    # ── Rolling-nearest join -----------------------------------------
    matched <- col_dt[osteo_dt, roll = "nearest",
                      on = .(pt, exam_date_iso = osteo_exam_date)]
    # Rename the join key back to osteo_exam_date
    data.table::setnames(matched, "exam_date_iso", "osteo_exam_date")
    
    
    # ── Signed day difference -----------------------------------------
    matched[, days_colaus_minus_osteo :=
                as.integer(colaus_exam_date - osteo_exam_date)]
    
    # ── Unmatched colaus rows -----------------------------------------
    matched_keys <- unique(matched[!is.na(colaus_exam_date),
                                   .(pt, colaus_exam_date)])
    
    unmatched_col <- col_dt[!matched_keys, on = .(pt, colaus_exam_date)]
    
    
    
    # ── Stack matched + unmatched -----------------------------------------
    result <- rbind(matched, unmatched_col, fill = TRUE)
    
    # ── Create Unified Exam Date & HGS_MAX logic -----------------------------------------
    # Date logic
    result[, final_exam_date := fcoalesce(osteo_exam_date, colaus_exam_date)]
    
    # HGS_max Logic: Priority Colaus, but V5 priority Osteo
    result[, HGS_MAX := HGS_MAX_colaus]
    result[.visit_osteo == "V5", HGS_MAX := HGS_MAX_osteo]
    result[is.na(HGS_MAX), HGS_MAX := fcoalesce(HGS_MAX_colaus, HGS_MAX_osteo)]
    
    # Age, Height, BMI Logic: Priority Osteo, fill with Colaus
    # This works for the matched rows (prefers osteo) and unmatched rows (takes colaus)
    result[, Age    := fcoalesce(Age_osteo, Age_colaus)]
    result[, Height := fcoalesce(Height_osteo, Height_colaus)]
    result[, Weight := fcoalesce(Weight_osteo, Weight_colaus)]
    result[, BMI    := fcoalesce(BMI_osteo, BMI_colaus)]
    result[, BMI_category    := fcoalesce(BMI_category_osteo, BMI_category_colaus)]
    
    
    # discard the intermediate columns
    result[, c("osteo_exam_date", "colaus_exam_date", "exam_date_iso", "i.exam_date_iso") := NULL]
    result[, c("HGS_MAX_osteo", "HGS_MAX_colaus") := NULL]
    result[, c("Age_osteo", "Age_colaus", "Height_osteo", "Height_colaus", "BMI_osteo", "BMI_colaus", "Weight_colaus", "Weight_osteo",
               ".cohort_osteo", ".cohort_colaus", "BMI_category_osteo", "BMI_category_colaus") := NULL]
    
    
    # ── Tidy up -----------------------------------------
    
    # Define the preferred lead columns
    desired_lead <- c("pt", "final_exam_date", ".visit_osteo",".visit_colaus", "osteo_exam_date", 
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
    
    return(list(
        data = result[],
        qc = list(
            n_osteo_removed = n_osteo_removed,
            n_col_removed   = n_col_removed,
            n_osteo_pt_removed = n_osteo_pt_removed,
            n_col_pt_removed   = n_col_pt_removed
        )
    ))
}