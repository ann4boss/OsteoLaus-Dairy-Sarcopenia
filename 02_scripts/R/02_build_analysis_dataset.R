# =============================================================================
# R/02_build_analysis_dataset.R
# =============================================================================
# One-call wrapper for the complete-case and MICE derivation/merge routes.
# Defines one function: build_analysis_dataset(). Chains impute_mice_*()
# (R/02_01_mice_impute.R), derive() (R/02_02_derive.R),
# select_analysis_columns() (R/02_03_select_analysis_columns.R),
# merge_visit_pairs() (R/02_04_visit_match.R), and derive_combined()
# (R/02_05_derive_combined_variables.R).
#
# Public interface
# ----------------
#   build_analysis_dataset(core$colaus_long, core$osteo_long)
#   build_analysis_dataset(core$colaus_long, core$osteo_long,
#                          imputed = TRUE, m = 20L, maxit = 20L, seed = 2024L)
#
# Pipeline stages
# ---------------
#   1. impute_mice_*()            MICE route only. Returns list(df_wide, mids, ...).
#   2. derive()                   CC: plain df -> plain df.
#                                 MICE: mids -> mids.
#   3. select_analysis_columns()  CC: plain df -> plain df.
#                                 MICE: mids -> mids.
#   4. merge_visit_pairs()        CC: (df, df) -> list(data, mids=NULL, qc).
#                                 MICE: (mids, mids) -> list(mids, qc).
#   5. derive_combined()          CC: plain df -> plain df.
#                                 MICE: mids -> mids.
#
# Every MICE stage receives and returns a mids object directly.
# The long-format tibble is produced internally within each function via
# mice::complete(..., include = TRUE) and is not surfaced to this wrapper.
# =============================================================================

# -----------------------------------------------------------------------------
# build_analysis_dataset()
# -----------------------------------------------------------------------------
#' Run the full derivation, selection, merge, and combined-derivation pipeline.
#'
#' @param colaus_long CoLaus long data frame (`core$colaus_long`).
#' @param osteo_long  OsteoLaus long data frame (`core$osteo_long`).
#' @param imputed     `TRUE` to run the MICE route. `FALSE` (default) runs the
#'   complete-case route.
#' @param m           Number of imputed datasets (MICE route only).
#' @param maxit       Number of MICE iterations (MICE route only).
#' @param seed        Random seed for both CoLaus and OsteoLaus MICE.
#'
#' @return Named list. Complete-case route entries are plain tibbles; MICE
#'   route entries are mids objects (named `*_mids`).
#'
#'   **Both routes**
#'   * `colaus_derived`     — after `derive()`
#'   * `osteo_derived`      — after `derive()`
#'   * `colaus_selected`    — after `select_analysis_columns()`
#'   * `osteo_selected`     — after `select_analysis_columns()`
#'   * `merge_qc`           — QC summary from `merge_visit_pairs()`
#'   * `merged_derived`     — final analysis dataset (tibble for CC, mids for MICE)
#'
#'   **MICE route only** (`NULL` for complete-case)
#'   * `colaus_df_wide`           — wide data frame passed into mice()
#'   * `osteo_df_wide`            — wide data frame passed into mice()
#'   * `colaus_imputation_mids`   — raw mids from `impute_mice_colaus()`
#'   * `osteo_imputation_mids`    — raw mids from `impute_mice_osteo()`
#'
#' @examples
#' cc      <- build_analysis_dataset(core$colaus_long, core$osteo_long)
#' imputed <- build_analysis_dataset(core$colaus_long, core$osteo_long,
#'                                   imputed = TRUE, m = 20L, seed = 2024L)
#'
#' # Pool a model from the final mids:
#' fit <- mice::with(imputed$merged_derived,
#'                   lm(HGS_MAX ~ dairy_fermented_gday_cumavg + Age + BMI))
#' mice::pool(fit)
build_analysis_dataset <- function(colaus_long,
                                   osteo_long,
                                   imputed = FALSE,
                                   m       = 20L,
                                   maxit   = 20L,
                                   seed    = 2024L) {
    
    if (!is.null(imputed) && !isTRUE(imputed) && !isFALSE(imputed))
        cli::cli_abort(
            "{.arg imputed} must be {.code TRUE}, {.code FALSE}, or {.code NULL}."
        )
    
    run_imputed <- isTRUE(imputed)
    
    # =========================================================================
    # Stage 1 — Imputation (MICE route only)
    # =========================================================================
    
    if (run_imputed) {
        cli::cli_h1("Route: MICE")
        
        colaus_imp <- impute_mice_colaus(colaus_long, m = m, maxit = maxit, seed = seed)
        osteo_imp  <- impute_mice_osteo(osteo_long,  m = m, maxit = maxit, seed = seed)
        
        post_imputation_checks(colaus_imp$mids, out_dir = "03_outputs/mice/colaus")
        post_imputation_checks(osteo_imp$mids,  out_dir = "03_outputs/mice/osteo")
        
        colaus_for_derive      <- colaus_imp$mids
        osteo_for_derive       <- osteo_imp$mids
        colaus_df_wide         <- colaus_imp$df_wide
        osteo_df_wide          <- osteo_imp$df_wide
        colaus_imputation_mids <- colaus_imp$mids
        osteo_imputation_mids  <- osteo_imp$mids
        
    } else {
        cli::cli_h1("Route: Complete Case")
        colaus_for_derive      <- colaus_long
        osteo_for_derive       <- osteo_long
        colaus_df_wide         <- NULL
        osteo_df_wide          <- NULL
        colaus_imputation_mids <- NULL
        osteo_imputation_mids  <- NULL
    }
    
    # =========================================================================
    # Stage 2 — Derive
    # CC:   plain df  -> plain df
    # MICE: mids      -> mids
    # =========================================================================
    
    colaus_derived <- derive(colaus_for_derive)
    osteo_derived  <- derive(osteo_for_derive)
    
    # =========================================================================
    # Stage 3 — Select analysis columns
    # CC:   plain df  -> plain df
    # MICE: mids      -> mids
    # =========================================================================
    
    colaus_selected <- select_analysis_columns(colaus_derived)
    osteo_selected  <- select_analysis_columns(osteo_derived)
    
    # =========================================================================
    # Stage 4 — Merge visit pairs
    # CC:   (df, df)      -> list(data, mids=NULL, qc)
    # MICE: (mids, mids)  -> list(mids, qc)
    # =========================================================================
    
    merged <- merge_visit_pairs(colaus_selected, osteo_selected)
    
    # =========================================================================
    # Stage 5 — Derive combined variables
    # CC:   plain df  -> plain df   (merged$data passed in)
    # MICE: mids      -> mids       (merged$mids passed in)
    # =========================================================================
    
    merged_input   <- if (run_imputed) merged$mids else merged$data
    merged_derived <- derive_combined(merged_input)
    
    # =========================================================================
    # Return
    # =========================================================================
    
    list(
        # Imputation inputs (MICE only)
        colaus_df_wide         = colaus_df_wide,
        osteo_df_wide          = osteo_df_wide,
        colaus_imputation_mids = colaus_imputation_mids,
        osteo_imputation_mids  = osteo_imputation_mids,
        
        # Per-stage outputs
        colaus_derived  = colaus_derived,
        osteo_derived   = osteo_derived,
        colaus_selected = colaus_selected,
        osteo_selected  = osteo_selected,
        
        # Merge QC
        merge_qc = merged$qc,
        
        # Final analysis dataset (tibble for CC, mids for MICE)
        merged_derived = merged_derived
    )
}