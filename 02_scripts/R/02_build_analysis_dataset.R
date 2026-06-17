# =============================================================================
# R/_build_analysis_dataset.R
# =============================================================================
# One-call wrapper for the complete-case and MICE derivation/merge routes.
#
# Public interface
# ----------------
#   run_derivation_pipeline(core$colaus_long, core$osteo_long)
#   run_derivation_pipeline(core$colaus_long, core$osteo_long,
#                           imputed = TRUE, m = 5L, maxit = 20L, seed = 2024L)
#
# Returns the intermediate OsteoLaus-derived data frame, merge QC, and final
# merged derived data frame.
# =============================================================================

#' Run the derivation, selection, nearest-date merge, and combined derivation.
#'
#' @param colaus_long CoLaus long data frame, usually `core$colaus_long`.
#' @param osteo_long OsteoLaus long data frame, usually `core$osteo_long`.
#' @param imputed `TRUE` to run MICE first. `FALSE` or `NULL` runs the
#'   complete-case route.
#' @param m Number of imputed datasets when `imputed = TRUE`.
#' @param maxit Number of MICE iterations when `imputed = TRUE`.
#' @param seed Random seed used for both CoLaus and OsteoLaus MICE.
#'
#' @return A named list with:
#'   * `osteo_derived`
#'   * `merge_qc`
#'   * `merged_derived`
#'
#' @examples
#' cc <- run_derivation_pipeline(core$colaus_long, core$osteo_long)
#' mice <- run_derivation_pipeline(core$colaus_long, core$osteo_long,
#'                                 imputed = TRUE, m = 5L, maxit = 20L, seed = 2024L)
build_analysis_dataset <- function(colaus_long,
                                   osteo_long,
                                   imputed = FALSE,
                                   m = 20L,
                                   maxit = 20L,
                                   seed = 2024L) {
    
    if (!is.null(imputed) && !isTRUE(imputed) && !isFALSE(imputed)) {
        cli::cli_abort("{.arg imputed} must be {.code TRUE}, {.code FALSE}, or {.code NULL}.")
    }
    
    run_imputed <- isTRUE(imputed)
    
    if (run_imputed) {
        cli::cli_h1("Route: MICE")
        
        colaus <- impute_mice_colaus(colaus_long, m = m, maxit = maxit, seed = seed)
        osteo  <- impute_mice_osteo(osteo_long, m = m, maxit = maxit, seed = seed)
        
        # Keep diagnostic file side-effects securely bounded inside the conditional block
        post_imputation_checks(colaus$mids, out_dir = "03_outputs/mice/colaus")
        post_imputation_checks(osteo$mids,  out_dir = "03_outputs/mice/osteo")
        
        colaus_mids_out <- colaus$mids
        osteo_mids_out  <- osteo$mids
        
        colaus_df_wide <- colaus$df_wide
        osteo_df_wide <- osteo$df_wide
        
    } else {
        cli::cli_h1("Route: Complete Case")
        colaus          <- colaus_long
        osteo           <- osteo_long
        colaus_mids_out <- NULL
        osteo_mids_out  <- NULL
        colaus_df_wide  <- NULL
        osteo_df_wide   <- NULL
    }
    
    # Explicitly separate the pipelines to ensure transparent function tracking
    colaus_derived <- derive(colaus, imputed = run_imputed)
    osteo_derived  <- derive(osteo, imputed = run_imputed)
    
    colaus_selected <- select_analysis_columns(colaus_derived)
    osteo_selected  <- select_analysis_columns(osteo_derived)
    
    merged <- merge_visit_pairs(colaus_selected, osteo_selected, imputed = run_imputed)
    merged_derived <- derive_combined(merged$data, imputed = run_imputed)
    
    list(
        colaus_df_wide  = colaus_df_wide,
        osteo_df_wide   = osteo_df_wide,
        colaus_derived  = colaus_derived,
        osteo_derived   = osteo_derived,
        colaus_selected = colaus_selected,
        osteo_selected  = osteo_selected,
        colaus_mids     = colaus_mids_out,
        osteo_mids      = osteo_mids_out,
        merge_qc        = merged$qc,
        merged_derived  = merged_derived
    )
}
