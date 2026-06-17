# =============================================================================
# R/derive_combined.R
# =============================================================================
# Single entry-point for derivations on the merged OsteoLaus-CoLaus dataset.
#
# Public interface
# ----------------
#   derive_combined(df)    -- plain merged data frame (complete-case route)
#   derive_combined(mids)  -- mids object (MICE route)
#
# Derivation chain
# ----------------
#   1. sarcopenia — staging using unified HGS_MAX, ALM, and gait_speed
#   2. visit_time — time since baseline
#
# MICE route
# ----------
# A mids object is converted to long format via:
#
#   mice::complete(mids_obj, action = "long", include = TRUE)
#
# Derivations are applied to every slice including .imp == 0 so the
# observed-data slot is preserved. The long tibble is then converted back
# with mice::as.mids().
#
# @return
#   plain df  -> tibble with derived variables appended
#   mids      -> mids with derived variables appended
# =============================================================================

# Main derivation chain for merged data.
.derive_chain_combined <- function(df) {
    df |>
        derive_sarcopenia() |>
        derive_visit_time()
}


#' Derive combined variables on the merged dataset.
#'
#' @param data Either:
#'   * A merged tibble / data frame (complete-case route), or
#'   * A `mids` object (MICE route) — converted to long format internally via
#'     `mice::complete(..., include = TRUE)`, derivations applied per slice,
#'     then converted back with `mice::as.mids()`.
#'
#' @return
#'   * Plain input -> tibble with derived variables appended.
#'   * `mids` input -> `mids` with derived variables appended.
#'
#' @examples
#' # Complete-case
#' merged_derived <- derive_combined(merged_df)
#'
#' # MICE — pass the mids from merge_visit_pairs()
#' merged_derived_mids <- derive_combined(merge_result$mids)
#' mice::pool(with(merged_derived_mids, lm(HGS_MAX ~ dairy_fermented_gday_cumavg)))
derive_combined <- function(data) {
    
    # ── MICE route: mids object ──────────────────────────────────────────────
    if (inherits(data, "mids")) {
        m <- data$m
        
        long <- mice::complete(data, action = "long", include = TRUE) |>
            tibble::as_tibble()
        
        cli::cli_h1("Derive Combined Variables: {m} imputed datasets")
        
        imp_ids <- sort(unique(long$.imp))   # includes 0
        
        derived_long <- purrr::map(imp_ids, function(i) {
            label <- if (i == 0L) "observed" else as.character(i)
            cli::cli_inform("  [{label}/{m}] deriving ...")
            long |>
                dplyr::filter(.imp == i) |>
                dplyr::select(-.imp, -dplyr::any_of(".id")) |>
                .derive_chain_combined() |>
                dplyr::mutate(.imp = i, .before = 1L)
        }) |>
            dplyr::bind_rows()
        
        n_imp_rows <- nrow(derived_long[derived_long$.imp > 0L, ])
        cli::cli_inform(c(
            "v" = "derive_combined() complete.",
            "i" = "{n_imp_rows} rows across {m} imputed datasets."
        ))
        
        return(mice::as.mids(derived_long))
    }
    
    # ── Complete-case route: plain data frame ────────────────────────────────
    cli::cli_h1("Deriving Combined Variables")
    .derive_chain_combined(data) |>
        dplyr::as_tibble()
}