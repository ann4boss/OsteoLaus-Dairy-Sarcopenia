# =============================================================================
# R/derive_combined.R
# =============================================================================
# Single entry-point for derivations on the merged OsteoLaus-CoLaus dataset,
# with optional MICE support.
#
# Public interface
# ----------------
#   derive_combined(df)           -- plain merged data frame
#   derive_combined(mice_result)  -- mice result list
#
# Derivation chain
# ----------------
#   1. sarcopenia — staging using unified HGS_MAX, ALM, and gait_speed
#
# =============================================================================

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

# Main derivation chain for merged data
.derive_chain_combined <- function(df) {
    df |>
        derive_sarcopenia()
}

# Detect custom MICE result structure
.is_mice_result <- function(data) {
    is.list(data) &&
        all(c("long", "m") %in% names(data)) &&
        is.data.frame(data$long) &&
        ".imp" %in% names(data$long)
}

# Apply derivation chain to plain data
.derive_combined_single <- function(df) {
    
    .derive_chain_combined(df)
}

# MICE path
.derive_combined_mice <- function(mice_result) {
    
    long_df <- mice_result$long
    
    imp_ids <- sort(setdiff(unique(long_df$.imp), 0L))
    
    cli::cli_h1(
        "Derive Combined Variables: {mice_result$m} imputed datasets"
    )
    
    out <- purrr::map(imp_ids, function(i) {
        
        cli::cli_inform(
            "  [{i}/{mice_result$m}] deriving combined variables ..."
        )
        
        long_df |>
            dplyr::filter(.data$.imp == i) |>
            dplyr::select(-".imp")      |>
            .derive_combined_single()      |>
            dplyr::mutate(.imp = i, .before = 1L)
        
    }) |>
        dplyr::bind_rows()
    
    cli::cli_inform(c(
        "v" = "derive_combined() complete.",
        "i" = "{nrow(out)} rows across {mice_result$m} datasets."
    ))
    
    dplyr::as_tibble(out)
}

# -----------------------------------------------------------------------------
# Public function
# -----------------------------------------------------------------------------

#' Derive variables for merged OsteoLaus-CoLaus datasets,
#' with optional MICE support.
#'
#' @param data Either:
#'   * A merged tibble/data frame, or
#'   * A MICE result list with `$long` and `$m`
#'
#' @param imputed Deprecated. MICE input is detected automatically.
#'
#' @return
#'   * Plain input  -> tibble with derived variables appended.
#'   * MICE input   -> long-format tibble with `.imp` preserved.
#'
#' @examples
#' # Plain
#' merged_derived <- derive_combined(merged_df)
#'
#' # MICE
#' merged_derived_imp <- derive_combined(mice_merged)
derive_combined <- function(data, imputed = NULL) {
    
    is_mice_list <- is.list(data) &&
        all(c("long", "m") %in% names(data)) &&
        is.data.frame(data$long) &&
        ".imp" %in% names(data$long)
    
    long_df <- if (is_mice_list) data$long else data
    has_imp <- ".imp" %in% names(long_df)
    
    is_imputed <- if (is.null(imputed)) {
        is_mice_list || has_imp
    } else {
        isTRUE(imputed)
    }
    
    if (is_imputed && !has_imp) {
        cli::cli_abort("derive_combined(imputed = TRUE) requires a {.col .imp} column or a MICE result list.")
    }
    
    if (!is_imputed) {
        cli::cli_h1("Deriving Combined Variables")
        return(dplyr::as_tibble(derive_sarcopenia(long_df)))
    }
    
    imp_ids <- sort(setdiff(unique(long_df$.imp), 0L))
    m <- length(imp_ids)
    
    cli::cli_h1("Derive Combined Variables: {m} imputed datasets")
    
    out <- purrr::map(imp_ids, function(i) {
        cli::cli_inform("  [{i}/{m}] deriving combined variables ...")
        
        long_df |>
            dplyr::filter(.data$.imp == i) |>
            dplyr::select(-.data$.imp, -dplyr::any_of(".id")) |>
            derive_sarcopenia() |>
            dplyr::mutate(.imp = i, .before = 1L)
    }) |>
        dplyr::bind_rows()
    
    cli::cli_inform(c(
        "v" = "derive_combined() complete.",
        "i" = "{nrow(out)} rows across {m} imputed datasets."
    ))
    
    dplyr::as_tibble(out)
}
