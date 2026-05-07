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
#'     hypothetical ("imputedD") OsteoLaus rows
#'
#' @param colaus_long data.table or data.frame
#' @param osteo_long  data.table or data.frame
#' TODO: should this be implemented?
#' @param roll_direction character: "nearest", "past", "future"
#'
#' @return data.table with matched and unmatched rows
# =============================================================================

merge_closest_exams <- function(
        colaus,
        osteolaus,
        imputed = NULL
) {
    
    if (!is.null(imputed) && !isTRUE(imputed) && !isFALSE(imputed)) {
        cli::cli_abort("{.arg imputed} must be {.code TRUE}, {.code FALSE}, or {.code NULL}.")
    }
    
    required_cols <- c("pt", "exam_date_iso")
    missing_colaus <- setdiff(required_cols, names(colaus))
    missing_osteo  <- setdiff(required_cols, names(osteolaus))
    
    if (length(missing_colaus) > 0L || length(missing_osteo) > 0L) {
        cli::cli_abort(c(
            "merge_closest_exams() is missing required columns.",
            "x" = "CoLaus missing: {.val {missing_colaus}}",
            "x" = "OsteoLaus missing: {.val {missing_osteo}}"
        ))
    }
    
    colaus_has_imp <- ".imp" %in% names(colaus)
    osteo_has_imp  <- ".imp" %in% names(osteolaus)
    is_imputedd     <- if (is.null(imputed)) {
        colaus_has_imp || osteo_has_imp
    } else {
        isTRUE(imputed)
    }
    
    remove_existing_cols <- function(df, cols) {
        cols <- intersect(cols, names(df))
        if (length(cols) > 0L) {
            df <- as.data.frame(df)
            df[, setdiff(names(df), cols), drop = FALSE]
        } else {
            df
        }
    }
    
    merge_one_slice <- function(colaus_slice, osteo_slice) {
        
        # -- Subset & convert to data.table ------------------------------------
        osteo_dt <- data.table::as.data.table(osteo_slice)
        col_dt   <- data.table::as.data.table(colaus_slice)
        
        # -- Ensure Date class -------------------------------------------------
        osteo_dt[, exam_date_iso := as.Date(exam_date_iso)]
        col_dt[, exam_date_iso := as.Date(exam_date_iso)]
        
        # -- Remove rows with missing dates ------------------------------------
        n_osteo_removed <- sum(is.na(osteo_dt$exam_date_iso))
        n_col_removed   <- sum(is.na(col_dt$exam_date_iso))
        
        n_osteo_pt_removed <- osteo_dt[is.na(exam_date_iso), data.table::uniqueN(pt)]
        n_col_pt_removed   <- col_dt[is.na(exam_date_iso), data.table::uniqueN(pt)]
        
        if (n_osteo_removed > 0L) {
            cli::cli_inform(
                "Removed {n_osteo_removed} OsteoLaus rows with missing exam_date_iso ({n_osteo_pt_removed} unique patients)"
            )
        }
        
        if (n_col_removed > 0L) {
            cli::cli_inform(
                "Removed {n_col_removed} CoLaus rows with missing exam_date_iso ({n_col_pt_removed} unique patients)"
            )
        }
        
        osteo_dt <- osteo_dt[!is.na(exam_date_iso)]
        col_dt   <- col_dt[!is.na(exam_date_iso)]
        
        # -- Restrict colaus to patients that exist in osteolaus ---------------
        col_dt <- col_dt[pt %in% unique(osteo_dt$pt)]
        
        # -- Rename shared columns to prevent collision ------------------------
        join_cols <- c("pt", "exam_date_iso", ".imp")
        overlap <- setdiff(intersect(names(col_dt), names(osteo_dt)), join_cols)
        
        if (length(overlap) > 0L) {
            data.table::setnames(col_dt, overlap, paste0(overlap, "_colaus"))
            data.table::setnames(osteo_dt, overlap, paste0(overlap, "_osteo"))
        }
        
        # -- Setup join keys ---------------------------------------------------
        col_dt[, colaus_exam_date := exam_date_iso]
        osteo_dt[, osteo_exam_date := exam_date_iso]
        
        data.table::setkey(col_dt, pt, exam_date_iso)
        data.table::setkey(osteo_dt, pt, osteo_exam_date)
        
        # -- Rolling-nearest join ---------------------------------------------
        #TODO here I can define joining style
        matched <- col_dt[
            osteo_dt,
            roll = "nearest",
            on = .(pt, exam_date_iso = osteo_exam_date)
        ]
        
        data.table::setnames(matched, "exam_date_iso", "osteo_exam_date")
        
        # -- Signed day difference --------------------------------------------
        matched[, days_colaus_minus_osteo :=
                    as.integer(colaus_exam_date - osteo_exam_date)]
        
        # -- Unmatched colaus rows --------------------------------------------
        matched_keys <- unique(matched[!is.na(colaus_exam_date),
                                       .(pt, colaus_exam_date)])
        
        unmatched_col <- col_dt[!matched_keys, on = .(pt, colaus_exam_date)]
        
        # -- Stack matched + unmatched ----------------------------------------
        result <- data.table::rbindlist(list(matched, unmatched_col), fill = TRUE)
        
        add_missing_cols <- function(dt, cols) {
            missing_cols <- setdiff(cols, names(dt))
            for (col in missing_cols) {
                dt[, (col) := NA]
            }
            invisible(dt)
        }
        
        drop_existing_cols <- function(dt, cols) {
            cols <- intersect(cols, names(dt))
            if (length(cols) > 0L) {
                dt[, (cols) := NULL]
            }
            invisible(dt)
        }
        
        # -- Create unified exam date and derived source-priority fields --------
        result[, final_exam_date := data.table::fcoalesce(
            osteo_exam_date,
            colaus_exam_date
        )]
        
        add_missing_cols(result, c(
            "HGS_MAX_colaus", "HGS_MAX_osteo", ".visit_osteo",
            "Age_osteo", "Age_colaus",
            "Height_osteo", "Height_colaus",
            "Weight_osteo", "Weight_colaus",
            "BMI_osteo", "BMI_colaus",
            "BMI_category_osteo", "BMI_category_colaus"
        ))
        
        # HGS_MAX logic: priority CoLaus, but V5 priority Osteo.
        result[, HGS_MAX := HGS_MAX_colaus]
        result[!is.na(.visit_osteo) & .visit_osteo == "V5", HGS_MAX := HGS_MAX_osteo]
        result[is.na(HGS_MAX), HGS_MAX := data.table::fcoalesce(
            HGS_MAX_colaus,
            HGS_MAX_osteo
        )]
        
        # Age, Height, Weight, BMI logic: priority Osteo, fill with CoLaus.
        result[, Age := data.table::fcoalesce(Age_osteo, Age_colaus)]
        result[, Height := data.table::fcoalesce(Height_osteo, Height_colaus)]
        result[, Weight := data.table::fcoalesce(Weight_osteo, Weight_colaus)]
        result[, BMI := data.table::fcoalesce(BMI_osteo, BMI_colaus)]
        result[, BMI_category := data.table::fcoalesce(
            BMI_category_osteo,
            BMI_category_colaus
        )]
        
        # -- Discard intermediate columns --------------------------------------
        drop_existing_cols(result, c(
            "osteo_exam_date", "colaus_exam_date",
            "exam_date_iso", "i.exam_date_iso",
            "HGS_MAX_osteo", "HGS_MAX_colaus",
            "Age_osteo", "Age_colaus",
            "Height_osteo", "Height_colaus",
            "BMI_osteo", "BMI_colaus",
            "Weight_colaus", "Weight_osteo",
            ".cohort_osteo", ".cohort_colaus",
            "BMI_category_osteo", "BMI_category_colaus"
        ))
        
        # -- Tidy up -----------------------------------------------------------
        desired_lead <- c(
            ".imp", "pt", "final_exam_date", ".visit_osteo", ".visit_colaus",
            "days_colaus_minus_osteo", "Age", "Height", "Weight", "BMI",
            "BMI_category", "HGS_MAX"
        )
        
        present_lead <- intersect(desired_lead, names(result))
        other_cols <- setdiff(names(result), present_lead)
        data.table::setcolorder(result, c(present_lead, other_cols))
        
        sort_cols <- intersect(c(".imp", "pt", "final_exam_date"), names(result))
        data.table::setorderv(result, sort_cols)
        
        list(
            data = result[],
            qc = list(
                n_osteo_removed = n_osteo_removed,
                n_col_removed = n_col_removed,
                n_osteo_pt_removed = n_osteo_pt_removed,
                n_col_pt_removed = n_col_pt_removed
            )
        )
    }
    
    if (!is_imputedd) {
        colaus <- remove_existing_cols(colaus, ".imp")
        osteolaus <- remove_existing_cols(osteolaus, c(".imp", ".id"))
        return(merge_one_slice(colaus, osteolaus))
    }
    
    cli::cli_h1("MICE: Merge Closest Exams (all imputedd datasets)")
    
    if (!colaus_has_imp) {
        cli::cli_abort("df_colaus must contain a {.col .imp} column.")
    }
    
    if (!osteo_has_imp) {
        cli::cli_abort("df_osteo must contain a {.col .imp} column.")
    }
    
    colaus_imp_ids <- sort(setdiff(unique(colaus$.imp), 0L))
    osteo_imp_ids  <- sort(setdiff(unique(osteolaus$.imp), 0L))
    
    if (!identical(colaus_imp_ids, osteo_imp_ids)) {
        cli::cli_abort(c(
            "x" = "Mismatch between CoLaus and OsteoLaus .imp indices.",
            "i" = "CoLaus: {.val {colaus_imp_ids}}",
            "i" = "OsteoLaus: {.val {osteo_imp_ids}}"
        ))
    }
    
    imp_ids <- colaus_imp_ids
    m <- length(imp_ids)
    
    if (m == 0L) {
        cli::cli_abort("No imputedd datasets found after removing {.code .imp == 0}.")
    }
    
    cli::cli_inform(c("i" = "Processing {m} imputedd datasets."))
    
    by_imp <- lapply(imp_ids, function(imp_id) {
        cli::cli_h2("imputedd dataset .imp = {imp_id} / {m}")
        
        colaus_slice <- colaus[colaus$.imp == imp_id, , drop = FALSE]
        osteo_slice  <- osteolaus[osteolaus$.imp == imp_id, , drop = FALSE]
        
        colaus_slice <- remove_existing_cols(colaus_slice, ".imp")
        osteo_slice  <- remove_existing_cols(osteo_slice, c(".imp", ".id"))
        
        merged <- merge_one_slice(colaus_slice, osteo_slice)
        qc_i <- merged$qc
        
        cli::cli_inform(c(
            " " = paste0(
                "  Merge QC (.imp = {imp_id}): ",
                "OsteoLaus rows removed = {qc_i$n_osteo_removed}, ",
                "CoLaus rows removed = {qc_i$n_col_removed}"
            )
        ))
        
        merged$data[, .imp := imp_id]
        data.table::setcolorder(merged$data, c(".imp", setdiff(names(merged$data), ".imp")))
        
        merged$qc$.imp <- imp_id
        merged
    })
    
    data <- data.table::rbindlist(lapply(by_imp, `[[`, "data"), fill = TRUE)
    qc_by_imp <- data.table::rbindlist(lapply(by_imp, `[[`, "qc"), fill = TRUE)
    
    data.table::setorderv(data, intersect(c(".imp", "pt", "final_exam_date"), names(data)))
    
    cli::cli_inform(c(
        "v" = "merge_closest_exams() complete.",
        "i" = "{nrow(data)} rows across {m} imputedd datasets ({nrow(data) / m} rows each)."
    ))
    
    list(
        data = data[],
        qc = list(
            by_imp = qc_by_imp[],
            n_osteo_removed = sum(qc_by_imp$n_osteo_removed),
            n_col_removed = sum(qc_by_imp$n_col_removed),
            n_osteo_pt_removed = sum(qc_by_imp$n_osteo_pt_removed),
            n_col_pt_removed = sum(qc_by_imp$n_col_pt_removed)
        )
    )
}

