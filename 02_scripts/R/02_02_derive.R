# =============================================================================
# R/derive.R
# =============================================================================
# Single entry-point for all variable derivations, cohort-agnostic and
# imputation-aware.
#
# Public interface
# ----------------
#   derive(df)    -- plain data frame (colaus_long / osteo_long)
#   derive(mids)  -- mids object returned by impute_mice_*()
#
# Cohort is detected automatically from the .cohort column; no argument needed.
#
# Derivation chains
# -----------------
#   CoLaus   : education, alcohol, diabetes, cvd, hrt, pa, dairy_servings,
#              dairy, dairy_quartile, atc, smoking, bmi, bmi_category, htn
#   OsteoLaus: bmi, bmi_category, alm_indices
#
# MICE route
# ----------
# When data is a mids object (output of impute_mice_*()), it is converted to
# long format internally via mice::complete(..., include = TRUE). Derivations
# are applied per imputed dataset (including .imp == 0 so the observed-data
# slot is preserved). The long tibble is then converted back to a mids with
# mice::as.mids(). The return value is list(long, mids, m) so downstream
# callers can use either the long tibble or the mids directly.
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
        derive_dairy_cumavg()   |>
        derive_atc()            |>
        derive_smoking()        |>
        derive_bmi()            |>
        derive_bmi_category()   |>
        derive_age()            |>
        derive_food_groups()    |>
        derive_htn()
}

.derive_chain_osteo <- function(df) {
    df |>
        derive_bmi()          |>
        derive_bmi_category() |>
        derive_alm_indices()  |>
        split_alm_by_method() |>
        derive_age()
}

# Map cohort name to chain function.
.DERIVE_CHAINS <- list(
    CoLaus    = .derive_chain_colaus,
    OsteoLaus = .derive_chain_osteo
)

# The MICE route accepts a raw mids object only.
.is_mice_result <- function(data) inherits(data, "mids")

.cohort_values <- function(df) {
    if (!".cohort" %in% names(df)) {
        cli::cli_abort("derive() requires a {.field .cohort} column.")
    }
    
    cohorts <- df |>
        dplyr::distinct(.cohort) |>
        dplyr::pull(.cohort)
    
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

# MICE path: converts the mids object to long format, applies derivations to
# every slice (including .imp == 0 so the observed-data slot is preserved),
# then converts back to a mids with mice::as.mids().
.derive_mice <- function(mids_obj) {
    
    m <- mids_obj$m
    
    # Convert to long — include = TRUE retains the .imp == 0 observed slice.
    long_df <- mice::complete(mids_obj, action = "long", include = TRUE) |>
        tibble::as_tibble()
    
    cohorts <- .cohort_values(long_df)
    if (length(cohorts) != 1L) {
        cli::cli_abort(c(
            "derive(): mids object contains invalid cohort data.",
            "i" = "Expected exactly one cohort; found {.val {cohorts}}."
        ))
    }
    
    cli::cli_h1("Derive: {cohorts} x {m} imputed datasets")
    
    # All slices including 0; .imp == 0 carries original NAs through.
    imp_ids <- sort(unique(long_df$.imp))
    
    derived_long <- purrr::map(imp_ids, function(i) {
        label <- if (i == 0L) "observed" else as.character(i)
        cli::cli_inform("  [{label}/{m}] deriving ...")
        long_df |>
            dplyr::filter(.imp == i) |>
            dplyr::select(-.imp)     |>
            .derive_single()         |>
            dplyr::mutate(.imp = i, .before = 1L)
    }) |>
        dplyr::bind_rows()
    
    n_imp_rows <- nrow(derived_long[derived_long$.imp > 0L, ])
    cli::cli_inform(c(
        "v" = "derive() complete.",
        "i" = "{n_imp_rows} rows across {m} imputed datasets."
    ))
    
    # Convert back to mids. .imp == 0 is present so $data holds the original
    # NAs, not imputed values.
    derived_mids <- mice::as.mids(derived_long)
    
    list(
        long = derived_long,
        mids = derived_mids,
        m    = m
    )
}

# -----------------------------------------------------------------------------
# Public function
# -----------------------------------------------------------------------------

#' Derive variables for any cohort, with or without MICE imputation.
#'
#' @param data Either:
#'   * A plain tibble / data frame with a `.cohort` column
#'     (output of `stack_visits()`), **or**
#'   * A `mids` object (output of `impute_mice_*()`).
#' @param imputed Deprecated. Input type is detected automatically from
#'   `inherits(data, "mids")`.
#'
#' @return
#'   * Plain input -> tibble with derived variables appended.
#'   * `mids` input -> `list(long, mids, m)`:
#'       - `$long`  long-format tibble (all imputations incl. `.imp == 0`)
#'                  with derived variables appended.
#'       - `$mids`  mids object reconstructed via `mice::as.mids($long)`.
#'       - `$m`     number of imputed datasets.
#'
#' @examples
#' # Plain
#' colaus_derived <- derive(colaus_long)
#' osteo_derived  <- derive(osteo_long)
#'
#' # MICE — pass mids directly; returns list(long, mids, m)
#' colaus_derived_imp <- derive(mice_colaus$mids)
#' osteo_derived_imp  <- derive(mice_osteo$mids)
derive <- function(data, imputed = NULL) {
    
    if (!is.null(imputed)) {
        cli::cli_warn(
            "{.arg imputed} is deprecated in {.fn derive}. \
             Input type is detected automatically from {.cls {class(data)}}."
        )
    }
    
    if (.is_mice_result(data)) {
        .derive_mice(data)
    } else {
        cohorts <- .cohort_values(data)
        cli::cli_h1("Derive: {cohorts}")
        dplyr::as_tibble(.derive_single(data))
    }
}