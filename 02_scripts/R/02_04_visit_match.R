# =============================================================================
# Visit matching function: CoLaus → OsteoLaus (backbone = OsteoLaus)
#
# Fixed visit-pair mapping:
#   OsteoLaus Baseline  ↔  CoLaus F1   → time_point = "T1"
#   OsteoLaus V2        →  DISCARDED
#   OsteoLaus V3        ↔  CoLaus F2   → time_point = "T2"
#   OsteoLaus V4        ↔  CoLaus F3   → time_point = "T3"
#   OsteoLaus V5        ↔  (no CoLaus) → time_point = "T4"  (OsteoLaus-only)
#
# Each participant has at most one visit per time point in each study, so
# matching is a plain left join on pt — no date proximity logic is needed.
#
# Column strategy:
#   All non-key columns are prefixed with colaus_ / osteo_ upfront.
#   After the join, shared variables: CoLaus wins (colaus_ value if not NA,
#   else osteo_ fallback). OsteoLaus-only columns (e.g. ALM): kept as-is.
#   .visit and .cohort are dropped from the final dataset.
#
# @param colaus    data.frame / data.table. Required: pt, exam_date_iso, .visit
#                  (.visit values: "F1", "F2", "F3").
# @param osteolaus data.frame / data.table. Required: pt, exam_date_iso, .visit
#                  (.visit values: "Baseline", "V2", "V3", "V4", "V5").
# @param imputed   TRUE  — MICE long format (requires .imp in both inputs).
#                  FALSE / NULL — complete-case; .imp stripped if present.
#
# @return list(data = data.table, qc = list(...))
# =============================================================================

merge_visit_pairs <- function(colaus, osteolaus, imputed = NULL) {
    
    # ── Validate arguments ─────────────────────────────────────────────────
    if (!is.null(imputed) && !isTRUE(imputed) && !isFALSE(imputed))
        cli::cli_abort("{.arg imputed} must be TRUE, FALSE, or NULL.")
    
    .check_required_cols(colaus,    c("pt", "exam_date_iso", ".visit"), "CoLaus")
    .check_required_cols(osteolaus, c("pt", "exam_date_iso", ".visit"), "OsteoLaus")
    
    # ── Detect MICE mode ───────────────────────────────────────────────────
    colaus_has_imp <- ".imp" %in% names(colaus)
    osteo_has_imp  <- ".imp" %in% names(osteolaus)
    is_mice        <- if (is.null(imputed)) colaus_has_imp || osteo_has_imp
    else isTRUE(imputed)
    
    if (!is_mice) {
        colaus    <- .drop_cols(colaus,    c(".imp", ".id"))
        osteolaus <- .drop_cols(osteolaus, c(".imp", ".id"))
        return(.merge_pairs_slice(colaus, osteolaus))
    }
    
    # ── MICE mode ──────────────────────────────────────────────────────────
    if (!colaus_has_imp) cli::cli_abort("CoLaus must contain a {.col .imp} column.")
    if (!osteo_has_imp)  cli::cli_abort("OsteoLaus must contain a {.col .imp} column.")
    
    imp_ids <- .validate_imp_ids(colaus$.imp, osteolaus$.imp)
    m       <- length(imp_ids)
    cli::cli_h1("MICE: Merge Visit Pairs ({m} imputed datasets)")
    
    by_imp <- lapply(imp_ids, function(imp_id) {
        cli::cli_h2("Processing .imp = {imp_id} / {m}")
        
        col_slice   <- .drop_cols(colaus[colaus$.imp == imp_id, ],       c(".imp", ".id"))
        osteo_slice <- .drop_cols(osteolaus[osteolaus$.imp == imp_id, ], c(".imp", ".id"))
        
        result <- .merge_pairs_slice(col_slice, osteo_slice)
        
        result$data[, .imp := imp_id]
        data.table::setcolorder(result$data, c(".imp", setdiff(names(result$data), ".imp")))
        result$qc$.imp <- imp_id
        
        cli::cli_inform(
            "QC: OsteoLaus removed = {result$qc$n_osteo_removed}, CoLaus removed = {result$qc$n_col_removed}"
        )
        result
    })
    
    .combine_mice_results(by_imp, m)
}


# =============================================================================
# Internal helpers
# =============================================================================

# Fixed visit-pair map ─────────────────────────────────────────────────────────
.VISIT_MAP <- list(
    "Baseline" = list(colaus_visit = "F1", time_point = "T1"),
    "V2"       = NULL,                      # discarded
    "V3"       = list(colaus_visit = "F2", time_point = "T2"),
    "V4"       = list(colaus_visit = "F3", time_point = "T3"),
    "V5"       = list(colaus_visit = NA,   time_point = "T4")  # OsteoLaus-only
)

# Columns that are never prefixed (join keys + MICE bookkeeping)
.JOIN_KEYS <- c("pt", ".imp", ".id")


# ── Prefix all non-key columns with a study label ─────────────────────────────
#
# Called once per dataset before any joining. After this, every column is
# unambiguous regardless of which time points are present, so T4 (OsteoLaus-only)
# requires no special handling.

.prefix_cols <- function(dt, prefix) {
    to_rename <- setdiff(names(dt), .JOIN_KEYS)
    data.table::setnames(dt, to_rename, paste0(prefix, to_rename))
    invisible(dt)
}


# ── Core merge for one complete-case slice ─────────────────────────────────────

.merge_pairs_slice <- function(colaus_slice, osteo_slice) {
    
    col_dt   <- .prepare_dt(colaus_slice)
    osteo_dt <- .prepare_dt(osteo_slice)
    
    qc <- .remove_missing_dates(col_dt, osteo_dt)
    col_dt   <- qc$col_dt
    osteo_dt <- qc$osteo_dt
    
    # Discard OsteoLaus V2
    n_v2 <- sum(osteo_dt$osteo_.visit == "V2", na.rm = TRUE)
    if (n_v2 > 0L)
        cli::cli_inform("Discarding {n_v2} OsteoLaus V2 rows (as specified).")
    osteo_dt <- osteo_dt[osteo_.visit != "V2" | is.na(osteo_.visit)]
    
    # Warn about unrecognised OsteoLaus visit labels
    known_visits <- names(.VISIT_MAP)[!vapply(.VISIT_MAP, is.null, logical(1))]
    unknown <- setdiff(unique(osteo_dt$osteo_.visit), known_visits)
    if (length(unknown) > 0L)
        cli::cli_warn("Unknown OsteoLaus .visit value(s): {.val {unknown}}")
    
    # Build one merged block per valid OsteoLaus visit
    blocks <- lapply(names(.VISIT_MAP), function(ov) {
        mapping <- .VISIT_MAP[[ov]]
        if (is.null(mapping)) return(NULL)
        
        osteo_block <- osteo_dt[osteo_.visit == ov]
        if (nrow(osteo_block) == 0L) return(NULL)
        
        cv         <- mapping$colaus_visit
        time_point <- mapping$time_point
        
        if (is.na(cv)) {
            # T4 / V5: OsteoLaus-only — all osteo_ columns already present,
            # no join needed, no special handling required.
            osteo_block[, `:=`(
                time_point              = time_point,
                days_colaus_minus_osteo = NA_integer_
            )]
            return(osteo_block)
        }
        
        col_block <- col_dt[colaus_.visit == cv]
        
        if (nrow(col_block) == 0L) {
            cli::cli_warn("No CoLaus {cv} rows found; skipping OsteoLaus {ov} entirely.")
            return(NULL) 
        }
        
        n_before <- nrow(osteo_block)
        joined   <- .join_visit_pair(osteo_block, col_block, time_point)
        n_dropped <- n_before - nrow(joined)
        
        if (n_dropped > 0L)
            cli::cli_inform(
                "{n_dropped} OsteoLaus {ov} row(s) dropped: no matching CoLaus {cv} visit."
            )
        
        joined
    })
    
    result <- data.table::rbindlist(Filter(Negate(is.null), blocks), fill = TRUE)
    result <- .resolve_columns(result)
    result <- .finalise_column_order(result)
    
    qc$n_v2_discarded <- n_v2
    
    list(
        data = result[],
        qc   = qc[c("n_osteo_removed", "n_col_removed",
                    "n_osteo_pt_removed", "n_col_pt_removed",
                    "n_v2_discarded")]
    )
}


# ── Convert to data.table, coerce date, prefix all non-key columns ────────────

.prepare_dt <- function(df, prefix) {
    dt <- data.table::as.data.table(df)
    dt[, exam_date_iso := as.Date(exam_date_iso)]
    dt
}


# ── inner join on pt for one visit pair ────────────────────────────────────────

.join_visit_pair <- function(osteo_block, col_block, time_point) {
    
    data.table::setkey(col_block,   pt)
    data.table::setkey(osteo_block, pt)
    
    # Inner join: only keep participants present in BOTH studies
    matched <- merge(osteo_block, col_block, by = "pt", all = FALSE)
    
    matched[, `:=`(
        days_colaus_minus_osteo = as.integer(colaus_exam_date_iso - osteo_exam_date_iso),
        time_point              = time_point
    )]
    
    matched
}


# ── Resolve prefixed columns into final unprefixed names ──────────────────────
#
# For every variable that exists in both studies (colaus_X + osteo_X):
#   final X = colaus_X if not NA, else osteo_X  (CoLaus wins).
# For OsteoLaus-only variables (osteo_X, no colaus_X): rename to X.
# colaus_-only variables (no osteo_ counterpart): rename to X.
# exam_date_iso → exam_date.
# .visit and .cohort columns are dropped.

.resolve_columns <- function(dt) {
    
    all_cols     <- names(dt)
    colaus_cols  <- grep("^colaus_", all_cols, value = TRUE)
    osteo_cols   <- grep("^osteo_",  all_cols, value = TRUE)
    colaus_bases <- sub("^colaus_", "", colaus_cols)
    osteo_bases  <- sub("^osteo_",  "", osteo_cols)
    
    shared_bases  <- intersect(colaus_bases, osteo_bases)
    colaus_only   <- setdiff(colaus_bases, osteo_bases)
    osteo_only    <- setdiff(osteo_bases,  colaus_bases)
    
    # Shared columns: CoLaus wins
    for (bn in shared_bases) {
        cc      <- paste0("colaus_", bn)
        oc      <- paste0("osteo_",  bn)
        col_vec <- dt[[cc]]
        ost_vec <- dt[[oc]]
        
        if (is.factor(col_vec) || is.factor(ost_vec)) {
            all_levels <- union(levels(col_vec), levels(ost_vec))
            resolved   <- data.table::fcoalesce(as.character(col_vec),
                                                as.character(ost_vec))
            dt[, (bn) := factor(resolved, levels = all_levels)]
        } else {
            dt[, (bn) := data.table::fcoalesce(col_vec, ost_vec)]
        }
    }
    
    # CoLaus-only columns: strip prefix
    for (bn in colaus_only)
        data.table::setnames(dt, paste0("colaus_", bn), bn)
    
    # OsteoLaus-only columns: strip prefix
    for (bn in osteo_only)
        data.table::setnames(dt, paste0("osteo_", bn), bn)
    
    # Tidy up
    if ("exam_date_iso" %in% names(dt))
        data.table::setnames(dt, "exam_date_iso", "exam_date")
    
    # Drop prefixed source columns and unwanted metadata columns
    to_drop <- c(
        grep("^(colaus_|osteo_)", names(dt), value = TRUE),
        grep("^\\.visit",  names(dt), value = TRUE),
        grep("^\\.cohort", names(dt), value = TRUE)
    )
    to_drop <- intersect(unique(to_drop), names(dt))
    if (length(to_drop) > 0L) dt[, (to_drop) := NULL]
    
    dt
}


# ── Put key columns first, sort rows ──────────────────────────────────────────

.finalise_column_order <- function(dt) {
    
    # Ensure ordered factor
    if ("time_point" %in% names(dt)) {
        dt[, time_point := factor(
            time_point,
            levels = c("T1", "T2", "T3", "T4"),
            ordered = TRUE
        )]
    }
    
    lead_cols    <- c(".imp", "pt", "time_point", "exam_date", "Age", "days_colaus_minus_osteo")
    present_lead <- intersect(lead_cols, names(dt))
    data.table::setcolorder(dt, c(present_lead, setdiff(names(dt), present_lead)))
    data.table::setorderv(dt, intersect(c(".imp", "pt", "time_point"), names(dt)))
    dt
}


# =============================================================================
# Shared low-level helpers
# =============================================================================

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

.prepare_dt <- function(df) {
    dt <- data.table::as.data.table(df)
    dt[, exam_date_iso := as.Date(exam_date_iso)]
    dt
}

.remove_missing_dates <- function(col_dt, osteo_dt) {
    # NOTE: called before .prefix_cols, so exam_date_iso is still unprefixed
    n_osteo_removed    <- sum(is.na(osteo_dt$exam_date_iso))
    n_col_removed      <- sum(is.na(col_dt$exam_date_iso))
    n_osteo_pt_removed <- osteo_dt[is.na(exam_date_iso), data.table::uniqueN(pt)]
    n_col_pt_removed   <- col_dt[is.na(exam_date_iso),   data.table::uniqueN(pt)]
    
    if (n_osteo_removed > 0L)
        cli::cli_inform(
            "Removed {n_osteo_removed} OsteoLaus rows with missing exam_date_iso ({n_osteo_pt_removed} patients)"
        )
    if (n_col_removed > 0L)
        cli::cli_inform(
            "Removed {n_col_removed} CoLaus rows with missing exam_date_iso ({n_col_pt_removed} patients)"
        )
    
    # Prefix AFTER removing missing-date rows
    col_clean   <- col_dt[!is.na(exam_date_iso)]
    osteo_clean <- osteo_dt[!is.na(exam_date_iso)]
    .prefix_cols(col_clean,   "colaus_")
    .prefix_cols(osteo_clean, "osteo_")
    
    list(
        col_dt             = col_clean,
        osteo_dt           = osteo_clean,
        n_osteo_removed    = n_osteo_removed,
        n_col_removed      = n_col_removed,
        n_osteo_pt_removed = n_osteo_pt_removed,
        n_col_pt_removed   = n_col_pt_removed
    )
}

.validate_imp_ids <- function(colaus_imp, osteo_imp) {
    col_ids   <- sort(setdiff(unique(colaus_imp), 0L))
    osteo_ids <- sort(setdiff(unique(osteo_imp),  0L))
    
    if (!identical(col_ids, osteo_ids))
        cli::cli_abort(c(
            "x" = "Mismatch between CoLaus and OsteoLaus .imp indices.",
            "i" = "CoLaus:    {.val {col_ids}}",
            "i" = "OsteoLaus: {.val {osteo_ids}}"
        ))
    
    if (length(col_ids) == 0L)
        cli::cli_abort("No imputed datasets found after removing .imp == 0.")
    
    col_ids
}

.combine_mice_results <- function(by_imp, m) {
    data    <- data.table::rbindlist(lapply(by_imp, `[[`, "data"), fill = TRUE)
    qc_list <- data.table::rbindlist(lapply(by_imp, `[[`, "qc"),  fill = TRUE)
    
    data.table::setorderv(data, intersect(c(".imp", "pt", "time_point"), names(data)))
    
    cli::cli_inform(c(
        "v" = "merge_visit_pairs() complete.",
        "i" = "{nrow(data)} rows across {m} datasets ({nrow(data) / m} rows each)."
    ))
    
    list(
        data = data[],
        qc   = list(
            by_imp             = qc_list[],
            n_osteo_removed    = sum(qc_list$n_osteo_removed),
            n_col_removed      = sum(qc_list$n_col_removed),
            n_osteo_pt_removed = sum(qc_list$n_osteo_pt_removed),
            n_col_pt_removed   = sum(qc_list$n_col_pt_removed),
            n_v2_discarded     = sum(qc_list$n_v2_discarded)
        )
    )
}
