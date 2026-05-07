# =============================================================================
# R/stack_visits.R
# =============================================================================
# Row-binds harmonised visit tibbles into a single long tibble per cohort.
# No transformation, derivation, or column changes occur here.
#
# Each input tibble must carry .cohort and .visit (attached by
# import_visit()) so that every row in the stacked output is traceable to its
# source visit.
# =============================================================================

#' Stack harmonised visit tibbles into a single long tibble.
#'
#' Rows are sorted by pt and .visit so the output is in participant-visit
#' order, which makes downstream joins and lag operations predictable.
#'
#' @param ... Harmonised visit tibbles in any order. All must share the same
#'   .cohort value; mixing cohorts here is an error.
#' @return Tibble with all rows from every input visit, sorted by pt and
#'   .visit.
stack_visits <- function(...) {
    
    visits <- list(...)
    
    # ── Guard: Required columns exist -----------------------------------------
    required_cols <- c("pt", ".cohort", ".visit")
    
    lapply(visits, function(df) {
        missing <- setdiff(required_cols, names(df))
        if (length(missing) > 0) {
            cli::cli_abort("Input visit missing columns: {.val {missing}}")
        }
    })
    
    # ── Guard: Single cohort check --------------------------------------------
    all_cohorts <- unique(unlist(lapply(visits, function(df) df$.cohort)))
    
    if (length(all_cohorts) > 1) {
        cli::cli_abort("stack_visits() received multiple cohorts: {.val {all_cohorts}}")
    }
    
    # ── Bind and Arrange ------------------------------------------------
    out <- dplyr::bind_rows(visits) |>
        dplyr::arrange(pt, .visit) |>
        dplyr::relocate(dplyr::all_of(required_cols))
    
    return(out)
}