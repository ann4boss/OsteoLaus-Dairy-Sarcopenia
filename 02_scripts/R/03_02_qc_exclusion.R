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