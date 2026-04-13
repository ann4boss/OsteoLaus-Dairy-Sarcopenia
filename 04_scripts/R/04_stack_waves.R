# =============================================================================
# R/stack_waves.R
# =============================================================================
# Row-binds harmonised wave tibbles into a single long tibble per cohort.
# No transformation, derivation, or column changes occur here.
#
# Each input tibble must carry .cohort and .wave (attached by
# import_wave()) so that every row in the stacked output is traceable to its
# source wave.
# =============================================================================

#' Stack harmonised wave tibbles into a single long tibble.
#'
#' Rows are sorted by pt and .wave so the output is in participant-wave
#' order, which makes downstream joins and lag operations predictable.
#'
#' @param ... Harmonised wave tibbles in any order. All must share the same
#'   .cohort value; mixing cohorts here is an error.
#' @return Tibble with all rows from every input wave, sorted by pt and
#'   .wave.
stack_waves <- function(...) {
    
    waves <- list(...)
    
    # ── Guard: Required columns exist -----------------------------------------
    required_cols <- c("pt", ".cohort", ".wave")
    
    lapply(waves, function(df) {
        missing <- setdiff(required_cols, names(df))
        if (length(missing) > 0) {
            cli::cli_abort("Input wave missing columns: {.val {missing}}")
        }
    })
    
    # ── Guard: Single cohort check --------------------------------------------
    all_cohorts <- unique(unlist(lapply(waves, function(df) df$.cohort)))
    
    if (length(all_cohorts) > 1) {
        cli::cli_abort("stack_waves() received multiple cohorts: {.val {all_cohorts}}")
    }
    
    # ── Bind and Arrange ------------------------------------------------
    out <- dplyr::bind_rows(waves) %>%
        dplyr::arrange(pt, .wave) %>%
        dplyr::relocate(dplyr::all_of(required_cols))
    
    return(out)
}