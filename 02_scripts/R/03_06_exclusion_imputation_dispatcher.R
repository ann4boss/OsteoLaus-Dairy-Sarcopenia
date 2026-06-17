# =============================================================================
# R/03_06_exclusion_imputation_dispatcher.R
# =============================================================================
# Helpers for per-imputation dispatch and mids reconstruction after exclusions.
#
# Functions
# ---------
#   run_by_imputation(long, qc_table, imp_col, fun)
#       Splits a long-format tibble by .imp (> 0), calls fun() on each slice.
#
#   reconstruct_mids_after_exclusion(long_excluded, original_mids, pt_col)
#       Rebuilds a mids directly from the already-excluded long tibble so that
#       the reconstructed mids matches $data[[outcome]] row-for-row, with no
#       unwanted NAs from rows excluded for missing outcome or covariates.
# =============================================================================


#' Apply a function to each imputed dataset in a long-format tibble.
#'
#' @param long     Long-format tibble with an `.imp` column.
#' @param qc_table QC table passed through to `fun`.
#' @param imp_col  Name of the imputation index column (default `".imp"`).
#' @param fun      `f(slice, qc_table, imp_id)` returning a list with at least
#'   `$data` and `$consort_long`.
#'
#' @return Named list of results; names are imputation indices as strings.
run_by_imputation <- function(long, qc_table, imp_col = ".imp", fun) {
    
    imps <- sort(unique(long[[imp_col]]))
    imps <- imps[imps > 0L]
    
    results <- lapply(imps, function(i) {
        d_i <- dplyr::filter(long, .data[[imp_col]] == i)
        fun(d_i, qc_table, i)
    })
    
    stats::setNames(results, as.character(imps))
}


#' Rebuild a mids object directly from the already-excluded long tibble.
#'
#' Rather than going back to the original mids and re-filtering, this function
#' builds the mids directly from `long_excluded` (the `.imp >= 1` long tibble
#' produced by the exclusion pipeline). This guarantees that `mids$data`
#' reflects exactly the same rows and columns as `$data[[outcome]]` — including
#' row-level exclusions such as missing outcome values — so there are no
#' unwanted NAs in the reconstructed mids.
#'
#' The `.imp == 0` observed-data slot is reconstructed from `long_excluded`
#' by taking the `.imp == 1` slice and setting to NA: (a) columns that mice
#' originally imputed, and (b) derived lag columns (suffix `_lag`) which vary
#' across imputations and must be tracked in `mids$imp` to preserve that
#' variation. Without this, all imputations would receive `.imp == 1` lag
#' values and between-imputation variance would be silently lost. Because the
#' exclusion pipeline already operates on the long-format mids produced by
#' `build_analysis_dataset()`, all outcome- and covariate-level row filters
#' are already reflected in `long_excluded`.
#'
#' @param long_excluded Long-format tibble (`.imp >= 1`) after all exclusions.
#'   Must contain `.imp` and `.id` columns.
#' @param original_mids The mids object from which `long_excluded` was derived.
#'   Used only to identify which columns were imputed (to restore NAs in the
#'   `.imp == 0` slot).
#' @param pt_col        Participant ID column name.
#'
#' @return A `mids` object whose rows match `long_excluded` exactly.
reconstruct_mids_after_exclusion <- function(long_excluded,
                                             original_mids,
                                             pt_col = "pt") {
    
    if (!inherits(original_mids, "mids"))
        cli::cli_abort(
            "{.fn reconstruct_mids_after_exclusion}: \\
             {.arg original_mids} must be a {.cls mids} object."
        )
    
    # ── Reconstruct .imp == 0 from the excluded long data ─────────────────
    # Use .imp == 1 as the row structure, then restore NAs for:
    #   (a) originally imputed columns — so mids$imp tracks their per-imp values
    #   (b) derived lag columns (ending in _lag) — these are computed from
    #       imputed values and therefore vary across imputations. They must
    #       also be NA in .imp == 0 so mice::as.mids() populates mids$imp for
    #       them; otherwise all imputations silently receive the .imp == 1 values
    #       and between-imputation variation is lost.
    imputed_cols <- names(original_mids$method)[original_mids$method != ""]
    lag_cols     <- grep("_lag$", names(long_excluded), value = TRUE)
    na_cols      <- union(imputed_cols, lag_cols)
    
    obs_slice <- long_excluded |>
        dplyr::filter(.imp == 1L) |>
        dplyr::mutate(
            .imp = 0L,
            dplyr::across(
                dplyr::any_of(na_cols),
                ~ .x[NA_integer_]  # NA of the same type — preserves factor levels,
                                   # numeric/character types, so bind_rows doesn't
                                   # coerce factors to character
            )
        )
    
    # ── Bind .imp == 0 with the excluded imputed slices ────────────────────
    long_full <- dplyr::bind_rows(obs_slice, long_excluded)
    
    # ── Reassign .id to be unique within each .imp ─────────────────────────
    long_full <- long_full |>
        dplyr::group_by(.imp) |>
        dplyr::mutate(.id = dplyr::row_number()) |>
        dplyr::ungroup()
    
    # ── Flatten any residual list-columns before as.mids() ─────────────────
    long_full <- dplyr::mutate(
        long_full,
        dplyr::across(where(is.list), ~ unlist(.x, use.names = FALSE))
    )
    
    mice::as.mids(long_full)
}