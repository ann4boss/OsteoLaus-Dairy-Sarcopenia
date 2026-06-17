# =============================================================================
# R/03_02_qc_exclusion.R
# =============================================================================
# Derives the set of participants that fail QC and should be excluded.
#
# This is always evaluated on the OBSERVED data (.imp == 0 or plain CC data)
# because QC flags are fixed properties of participants, not imputed values.
# The returned participant IDs are then applied uniformly across all imputed
# datasets by the shared-exclusion layer in 03_exclusion_apply.R.
# =============================================================================


#' Identify participants that fail QC checks.
#'
#' Reads the QC flag table and returns a tibble of participants whose flags
#' indicate they were present in OsteoLaus but did not pass all QC criteria.
#'
#' @param qc_table   Data frame with one row per participant x exam, containing
#'   the columns named in `qc_flag_cols` plus `qc_in_osteolaus`.
#' @param pt_col     Name of the participant ID column.
#' @param qc_flag_cols Character vector of column names that must be `TRUE` for
#'   a participant to pass QC.
#'
#' @return Tibble with columns `<pt_col>`, `exclusion_stage`,
#'   `exclusion_reason`, `exclusion_detail`.
qc_exclude_participants <- function(qc_table, pt_col, qc_flag_cols) {
    
    failed <- qc_table |>
        dplyr::filter(qc_in_osteolaus %in% TRUE)
    
    flags <- as.data.frame(lapply(failed[qc_flag_cols], function(x) !x))
    
    failed$.flags <- apply(flags, 1, function(r)
        paste(names(flags)[r], collapse = ";")
    )
    
    failed |>
        dplyr::filter(.flags != "") |>
        dplyr::distinct(.data[[pt_col]]) |>
        dplyr::mutate(
            exclusion_stage  = "qc",
            exclusion_reason = "failed_qc",
            exclusion_detail = "QC flags triggered"
        )
}