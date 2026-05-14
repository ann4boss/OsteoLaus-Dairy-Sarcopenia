# =============================================================================
# R/derive.R
# =============================================================================
# Single entry-point for all variable derivations, cohort-agnostic and
# imputation-aware.
#
# Public interface
# ----------------
#   derive(df)              -- plain data frame (colaus_long / osteo_long)
#   derive(mice_result)     -- mice result list returned by impute_mice_*()
#
# Cohort is detected automatically from the .cohort column; no argument needed.
#
# Derivation chains
# -----------------
#   CoLaus   : education, alcohol, diabetes, cvd, hrt, pa, dairy_servings,
#              dairy, dairy_quartile, atc, smoking, bmi, bmi_category, htn
#   OsteoLaus: bmi, bmi_category, alm_indices
# =============================================================================

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

.derive_chain_colaus <- function(df) {
    df |>
        derive_education()      |>
        derive_alcohol()        |>
        derive_diabetes()       |>
        derive_cvd()            |>
        derive_hrt()            |>
        derive_pa()             |>
        derive_dairy_servings() |>
        derive_dairy()          |>
        derive_dairy_quartile() |>
        derive_atc()            |>
        derive_smoking()        |>
        derive_bmi()            |>
        derive_bmi_category()   |>
        derive_htn()
}

.derive_chain_osteo <- function(df) {
    df |>
        derive_bmi()          |>
        derive_bmi_category() |>
        derive_alm_indices()  |>
        split_alm_by_method()
}

# Map cohort name to chain function.
.DERIVE_CHAINS <- list(
    CoLaus    = .derive_chain_colaus,
    OsteoLaus = .derive_chain_osteo
)

.is_mice_result <- function(data) {
    is.list(data) &&
        all(c("long", "m") %in% names(data)) &&
        is.data.frame(data$long) &&
        ".imp" %in% names(data$long)
}

.cohort_values <- function(df) {
    if (!".cohort" %in% names(df)) {
        cli::cli_abort("derive() requires a {.field .cohort} column.")
    }
    
    cohorts <- df |>
        dplyr::distinct(.data$.cohort) |>
        dplyr::pull(.data$.cohort)
    
    stats::na.omit(cohorts)
}

# Detect cohort and apply the matching chain to a plain data frame.
.derive_single <- function(df) {
    cohorts <- .cohort_values(df)
    
    if (length(cohorts) == 0L) {
        cli::cli_abort("derive() could not detect a cohort in {.field .cohort}.")
    }
    
    unknown <- setdiff(cohorts, names(.DERIVE_CHAINS))
    if (length(unknown) > 0L) {
        cli::cli_abort(c(
            "derive() does not recognise cohort(s): {.val {unknown}}",
            "i" = "Supported cohorts: {.val {names(.DERIVE_CHAINS)}}"
        ))
    }
    
    if (length(cohorts) > 1L) {
        cli::cli_abort(c(
            "derive() received data from multiple cohorts: {.val {cohorts}}",
            "i" = "Pass each cohort separately."
        ))
    }
    
    .DERIVE_CHAINS[[cohorts]](df)
}

# MICE path: loops over imputed slices, applies derive to each.
.derive_mice <- function(mice_result) {
    long_df <- mice_result$long
    cohorts <- .cohort_values(long_df)
    
    if (length(cohorts) != 1L) {
        cli::cli_abort(c(
            "derive() received a MICE result with invalid cohort data.",
            "i" = "Expected exactly one cohort; found {.val {cohorts}}."
        ))
    }
    
    imp_ids <- sort(setdiff(unique(long_df$.imp), 0L))
    
    cli::cli_h1("Derive: {cohorts} x {mice_result$m} imputed datasets")
    
    out <- purrr::map(imp_ids, function(i) {
        cli::cli_inform("  [{i}/{mice_result$m}] deriving ...")
        long_df |>
            dplyr::filter(.data$.imp == i) |>
            dplyr::select(-.data$.imp)     |>
            .derive_single()               |>
            dplyr::mutate(.imp = i, .before = 1L)
    }) |>
        dplyr::bind_rows()
    
    cli::cli_inform(c(
        "v" = "derive() complete.",
        "i" = "{nrow(out)} rows across {mice_result$m} datasets ({nrow(out) / mice_result$m} rows each)."
    ))
    
    out
}

# -----------------------------------------------------------------------------
# Public function
# -----------------------------------------------------------------------------

#' Derive variables for any cohort, with or without MICE imputation.
#'
#' @param data Either:
#'   * A plain tibble / data frame with a `.cohort` column
#'     (output of `stack_visits()`), **or**
#'   * A MICE result list with `$long` and `$m` elements
#'     (output of `impute_mice_*()`).
#' @param imputed Deprecated. MICE input is detected automatically.
#'
#' @return
#'   * Plain input  -> tibble with derived variables appended.
#'   * MICE input   -> long-format tibble with `.imp` column preserved,
#'                    derived variables appended to every imputed dataset.
#'
#' @examples
#' # Plain
#' colaus_derived <- derive(colaus_long)
#' osteo_derived  <- derive(osteo_long)
#'
#' # MICE
#' colaus_derived_imp <- derive(mice_colaus)
#' osteo_derived_imp  <- derive(mice_osteo)
derive <- function(data, imputed = NULL) {
    is_imputed <- isTRUE(imputed) || .is_mice_result(data)
    
    if (isTRUE(imputed) && !.is_mice_result(data)) {
        cli::cli_abort("derive(imputed = TRUE) requires a MICE result list with {.field long} and {.field m}.")
    }
    
    if (is_imputed) {
        .derive_mice(data)
    } else {
        cohorts <- .cohort_values(data)
        cli::cli_h1("Derive: {cohorts}")
        dplyr::as_tibble(.derive_single(data))
    }
}
