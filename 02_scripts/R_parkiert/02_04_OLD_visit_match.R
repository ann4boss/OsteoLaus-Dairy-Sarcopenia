# =============================================================================
# visit matching function: CoLaus → OsteoLaus (backbone = OsteoLaus)
# =============================================================================
#' Match CoLaus visits to OsteoLaus visits by nearest exam date
#'
#' Uses OsteoLaus as the backbone. Each OsteoLaus visit is enriched with the
#' closest CoLaus visit (within participant `pt`) via a rolling nearest join.
#' CoLaus visits may be reused across multiple OsteoLaus visits. Unmatched
#' CoLaus rows are appended with OsteoLaus columns set to NA.
#'
#' Priority rules applied after matching:
#'   HGS_MAX   : CoLaus wins except at OsteoLaus V5, where OsteoLaus wins.
#'   Age, Height, Weight, BMI, BMI_category : OsteoLaus wins, CoLaus fills NA.
#'
#' @param colaus    data.frame or data.table. Must contain pt, exam_date_iso.
#' @param osteolaus data.frame or data.table. Must contain pt, exam_date_iso.
#' @param imputed   TRUE  — treat both inputs as MICE long format (requires .imp).
#'                  FALSE/NULL — complete-case; .imp columns are stripped if present.
#'
#' @return list(data = data.table, qc = list(...))
# =============================================================================

merge_closest_exams <- function(colaus, osteolaus, imputed = NULL) {
    
    # ── Validate arguments ─────────────────────────────────────────────────
    if (!is.null(imputed) && !isTRUE(imputed) && !isFALSE(imputed))
        cli::cli_abort("{.arg imputed} must be TRUE, FALSE, or NULL.")
    
    .check_required_cols(colaus,    c("pt", "exam_date_iso"), "CoLaus")
    .check_required_cols(osteolaus, c("pt", "exam_date_iso"), "OsteoLaus")
    
    # ── Detect MICE mode ───────────────────────────────────────────────────
    colaus_has_imp <- ".imp" %in% names(colaus)
    osteo_has_imp  <- ".imp" %in% names(osteolaus)
    is_mice        <- if (is.null(imputed)) colaus_has_imp || osteo_has_imp
    else isTRUE(imputed)
    
    if (!is_mice) {
        colaus    <- .drop_cols(colaus,    c(".imp", ".id"))
        osteolaus <- .drop_cols(osteolaus, c(".imp", ".id"))
        return(.merge_slice(colaus, osteolaus))
    }
    
    # ── MICE mode: validate and split by .imp ──────────────────────────────
    if (!colaus_has_imp) cli::cli_abort("CoLaus must contain a {.col .imp} column.")
    if (!osteo_has_imp)  cli::cli_abort("OsteoLaus must contain a {.col .imp} column.")
    
    imp_ids <- .validate_imp_ids(colaus$.imp, osteolaus$.imp)
    m       <- length(imp_ids)
    cli::cli_h1("MICE: Merge Closest Exams ({m} imputed datasets)")
    
    by_imp <- lapply(imp_ids, function(imp_id) {
        cli::cli_h2("Processing .imp = {imp_id} / {m}")
        
        col_slice   <- .drop_cols(colaus[colaus$.imp == imp_id, ],    c(".imp", ".id"))
        osteo_slice <- .drop_cols(osteolaus[osteolaus$.imp == imp_id, ], c(".imp", ".id"))
        
        result      <- .merge_slice(col_slice, osteo_slice)
        
        # Re-attach .imp label
        result$data[, .imp := imp_id]
        data.table::setcolorder(result$data, c(".imp", setdiff(names(result$data), ".imp")))
        result$qc$.imp <- imp_id
        
        cli::cli_inform("QC: OsteoLaus removed = {result$qc$n_osteo_removed}, CoLaus removed = {result$qc$n_col_removed}")
        result
    })
    
    .combine_mice_results(by_imp, m)
}


# =============================================================================
# Internal helpers
# =============================================================================

# ── Column guards ──────────────────────────────────────────────────────────────

.check_required_cols <- function(df, cols, label) {
    missing <- setdiff(cols, names(df))
    if (length(missing) > 0L)
        cli::cli_abort("{label} is missing required columns: {.val {missing}}")
}

.drop_cols <- function(df, cols) {
    to_drop <- intersect(cols, names(df))
    if (length(to_drop) == 0L) return(df)
    df <- as.data.frame(df)
    df[, setdiff(names(df), to_drop), drop = FALSE]
}


# ── MICE imputation ID validation ─────────────────────────────────────────────

.validate_imp_ids <- function(colaus_imp, osteo_imp) {
    col_ids  <- sort(setdiff(unique(colaus_imp), 0L))
    osteo_ids <- sort(setdiff(unique(osteo_imp),  0L))
    
    if (!identical(col_ids, osteo_ids))
        cli::cli_abort(c(
            "x" = "Mismatch between CoLaus and OsteoLaus .imp indices.",
            "i" = "CoLaus: {.val {col_ids}}",
            "i" = "OsteoLaus: {.val {osteo_ids}}"
        ))
    
    if (length(col_ids) == 0L)
        cli::cli_abort("No imputed datasets found after removing .imp == 0.")
    
    col_ids
}


# ── Core merge for one slice (no .imp column present) ─────────────────────────

.merge_slice <- function(colaus_slice, osteo_slice) {
    
    col_dt   <- .prepare_dt(colaus_slice)
    osteo_dt <- .prepare_dt(osteo_slice)
    
    qc <- .remove_missing_dates(col_dt, osteo_dt)  # mutates both in place
    col_dt   <- qc$col_dt
    osteo_dt <- qc$osteo_dt
    
    # Restrict CoLaus to patients present in OsteoLaus
    col_dt <- col_dt[pt %in% unique(osteo_dt$pt)]
    
    matched   <- .rolling_join(col_dt, osteo_dt)
    unmatched <- .unmatched_colaus(col_dt, matched)
    
    result <- data.table::rbindlist(list(matched, unmatched), fill = TRUE)
    result <- .resolve_priority_columns(result)
    result <- .finalise_column_order(result)
    
    list(data = result[], qc = qc[c("n_osteo_removed", "n_col_removed",
                                    "n_osteo_pt_removed", "n_col_pt_removed")])
}


# ── Convert to data.table and coerce exam date ─────────────────────────────────

.prepare_dt <- function(df) {
    dt <- data.table::as.data.table(df)
    dt[, exam_date_iso := as.Date(exam_date_iso)]
    dt
}


# ── Remove rows with missing exam dates; return QC counts ─────────────────────

.remove_missing_dates <- function(col_dt, osteo_dt) {
    n_osteo_removed    <- sum(is.na(osteo_dt$exam_date_iso))
    n_col_removed      <- sum(is.na(col_dt$exam_date_iso))
    n_osteo_pt_removed <- osteo_dt[is.na(exam_date_iso), data.table::uniqueN(pt)]
    n_col_pt_removed   <- col_dt[is.na(exam_date_iso),   data.table::uniqueN(pt)]
    
    if (n_osteo_removed > 0L)
        cli::cli_inform("Removed {n_osteo_removed} OsteoLaus rows with missing exam_date_iso ({n_osteo_pt_removed} patients)")
    if (n_col_removed > 0L)
        cli::cli_inform("Removed {n_col_removed} CoLaus rows with missing exam_date_iso ({n_col_pt_removed} patients)")
    
    list(
        col_dt             = col_dt[!is.na(exam_date_iso)],
        osteo_dt           = osteo_dt[!is.na(exam_date_iso)],
        n_osteo_removed    = n_osteo_removed,
        n_col_removed      = n_col_removed,
        n_osteo_pt_removed = n_osteo_pt_removed,
        n_col_pt_removed   = n_col_pt_removed
    )
}


# ── Rolling nearest join ───────────────────────────────────────────────────────
#
# Shared columns (excluding join keys) are suffixed: _colaus / _osteo.
# Two date columns are preserved so the signed day difference can be computed.

.rolling_join <- function(col_dt, osteo_dt) {
    
    # Rename shared non-key columns to avoid collision
    join_keys <- c("pt", "exam_date_iso")
    overlap   <- setdiff(intersect(names(col_dt), names(osteo_dt)), join_keys)
    if (length(overlap) > 0L) {
        data.table::setnames(col_dt,   overlap, paste0(overlap, "_colaus"))
        data.table::setnames(osteo_dt, overlap, paste0(overlap, "_osteo"))
    }
    
    # Preserve both raw dates before the join renames exam_date_iso
    col_dt[,   colaus_exam_date := exam_date_iso]
    osteo_dt[, osteo_exam_date  := exam_date_iso]
    
    data.table::setkey(col_dt,   pt, exam_date_iso)
    data.table::setkey(osteo_dt, pt, osteo_exam_date)
    
    matched <- col_dt[osteo_dt, roll = "nearest", on = .(pt, exam_date_iso = osteo_exam_date)]
    data.table::setnames(matched, "exam_date_iso", "osteo_exam_date")
    matched[, days_colaus_minus_osteo := as.integer(colaus_exam_date - osteo_exam_date)]
    
    matched
}


# ── CoLaus rows never matched to any OsteoLaus visit ──────────────────────────

.unmatched_colaus <- function(col_dt, matched) {
    matched_keys  <- unique(matched[!is.na(colaus_exam_date), .(pt, colaus_exam_date)])
    col_dt[!matched_keys, on = .(pt, colaus_exam_date)]
}


# ── Apply source-priority rules and drop intermediate columns ──────────────────
#
# Priority rules (documented in the public function header):
#   HGS_MAX        : CoLaus, except OsteoLaus V5 → OsteoLaus.
#   Age/Height/
#   Weight/BMI/
#   BMI_category   : OsteoLaus, fill with CoLaus.

.resolve_priority_columns <- function(dt) {
    
    # Ensure all split columns exist even if a source had no data
    .add_if_missing <- function(d, cols) {
        for (col in setdiff(cols, names(d))) d[, (col) := NA]
    }
    
    .add_if_missing(dt, c(
        "HGS_MAX_colaus", "HGS_MAX_osteo", ".visit_osteo",
        "Age_osteo",    "Age_colaus",
        "Height_osteo", "Height_colaus",
        "Weight_osteo", "Weight_colaus",
        "BMI_osteo",    "BMI_colaus",
        "BMI_category_osteo", "BMI_category_colaus"
    ))
    
    # HGS_MAX: CoLaus by default; OsteoLaus at V5; fallback fcoalesce
    dt[, HGS_MAX := HGS_MAX_colaus]
    dt[!is.na(.visit_osteo) & .visit_osteo == "V5", HGS_MAX := HGS_MAX_osteo]
    dt[is.na(HGS_MAX), HGS_MAX := data.table::fcoalesce(HGS_MAX_colaus, HGS_MAX_osteo)]
    
    # Osteo-priority columns
    dt[, Age          := data.table::fcoalesce(Age_osteo,    Age_colaus)]
    dt[, Height       := data.table::fcoalesce(Height_osteo, Height_colaus)]
    dt[, Weight       := data.table::fcoalesce(Weight_osteo, Weight_colaus)]
    dt[, BMI          := data.table::fcoalesce(BMI_osteo,    BMI_colaus)]
    dt[, BMI_category := data.table::fcoalesce(BMI_category_osteo, BMI_category_colaus)]
    
    # Unified exam date (OsteoLaus wins, CoLaus fills unmatched rows)
    dt[, final_exam_date := data.table::fcoalesce(osteo_exam_date, colaus_exam_date)]
    
    # Drop all intermediate split/source columns
    .drop_intermediate_cols(dt)
    
    dt
}

.drop_intermediate_cols <- function(dt) {
    to_drop <- c(
        "osteo_exam_date", "colaus_exam_date", "exam_date_iso", "i.exam_date_iso",
        "HGS_MAX_osteo",    "HGS_MAX_colaus",
        "Age_osteo",        "Age_colaus",
        "Height_osteo",     "Height_colaus",
        "Weight_osteo",     "Weight_colaus",
        "BMI_osteo",        "BMI_colaus",
        "BMI_category_osteo", "BMI_category_colaus",
        ".cohort_osteo",    ".cohort_colaus"
    )
    cols <- intersect(to_drop, names(dt))
    if (length(cols) > 0L) dt[, (cols) := NULL]
    invisible(dt)
}


# ── Put key columns first and sort rows ───────────────────────────────────────

.finalise_column_order <- function(dt) {
    lead_cols <- c(
        ".imp", "pt", "final_exam_date", ".visit_osteo", ".visit_colaus",
        "days_colaus_minus_osteo", "Age", "Height", "Weight", "BMI",
        "BMI_category", "HGS_MAX"
    )
    present_lead <- intersect(lead_cols, names(dt))
    data.table::setcolorder(dt, c(present_lead, setdiff(names(dt), present_lead)))
    
    sort_by <- intersect(c(".imp", "pt", "final_exam_date"), names(dt))
    data.table::setorderv(dt, sort_by)
    
    dt
}


# ── Combine results across all imputed datasets ────────────────────────────────

.combine_mice_results <- function(by_imp, m) {
    data     <- data.table::rbindlist(lapply(by_imp, `[[`, "data"), fill = TRUE)
    qc_list  <- data.table::rbindlist(lapply(by_imp, `[[`, "qc"),  fill = TRUE)
    
    data.table::setorderv(data, intersect(c(".imp", "pt", "final_exam_date"), names(data)))
    
    cli::cli_inform(c(
        "v" = "merge_closest_exams() complete.",
        "i" = "{nrow(data)} rows across {m} datasets ({nrow(data) / m} rows each)."
    ))
    
    list(
        data = data[],
        qc   = list(
            by_imp             = qc_list[],
            n_osteo_removed    = sum(qc_list$n_osteo_removed),
            n_col_removed      = sum(qc_list$n_col_removed),
            n_osteo_pt_removed = sum(qc_list$n_osteo_pt_removed),
            n_col_pt_removed   = sum(qc_list$n_col_pt_removed)
        )
    )
}