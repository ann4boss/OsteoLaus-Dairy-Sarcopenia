# =============================================================================
# R/stack_waves.R
# =============================================================================
# Row-binds harmonised wave tibbles into a single long tibble per cohort.
# No transformation, derivation, or column changes occur here.
#
# Each input tibble must carry .cohort, .wave, and .wave_num (attached by
# import_wave()) so that every row in the stacked output is traceable to its
# source wave.
#
# Depends on: nothing
# =============================================================================

#' Stack harmonised wave tibbles into a single long tibble.
#'
#' Rows are sorted by pt and .wave_num so the output is in participant-wave
#' order, which makes downstream joins and lag operations predictable.
#'
#' @param ... Harmonised wave tibbles in any order. All must share the same
#'   .cohort value; mixing cohorts here is an error.
#' @return Tibble with all rows from every input wave, sorted by pt and
#'   .wave_num.
stack_waves <- function(...) {
    waves <- list(...)
    
    out <- dplyr::bind_rows(waves)
    
    # Guard: all inputs must belong to the same cohort.
    cohorts <- unique(out$.cohort)
    if (length(cohorts) > 1)
        cli::cli_abort(
            "stack_waves() received tibbles from multiple cohorts: {.val {cohorts}}. \\
       Call stack_waves() separately for each cohort."
        )
    # Guard: all inputs must have .wave_num (attached by import_wave()).
    dplyr::arrange(out, pt, .wave_num)
    
    # Test if all columns of input waves are present in the output (i.e. no columns were dropped).
    input_cols <- unique(unlist(lapply(waves, colnames)))
    missing_cols <- setdiff(input_cols, colnames(out))
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "stack_waves() output is missing columns from input waves: {.val {missing_cols}}.
             Check that all input tibbles have the same columns and that no columns were dropped during binding."
        )
    }
    return(out)
}