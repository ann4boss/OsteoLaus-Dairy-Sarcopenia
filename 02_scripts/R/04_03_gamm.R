# =============================================================================
# R/04_03_gamm.R
# =============================================================================
# GAMM (mgcv) — non-linear smooth for exposure and/or time.
# Requires: mgcv, gratia (optional, for tidy output).
#
# Functions:
#   build_gamm_formula() — assembles the mgcv model formula from a config
#   fit_gamm_complete()  — fits one GAMM on a single (complete-case) data frame
#   fit_gamm_mice()      — fits one GAMM per MICE imputation and pools terms
#
# NOTE: normalise_config(), get_covariates(), get_config_value(),
# complete_model_data(), standardize_covariates(), and prepare_exposure() are
# called below but are not defined anywhere in 02_scripts/R/ at the time of
# writing — they are expected to come from the model-config helpers used
# elsewhere in the 04_* analysis scripts (e.g. R/04_02_model_specification_sensitivity.R
# defines a similar config pattern). Confirm they are sourced/available before
# running this script standalone.
# =============================================================================
#install.packages(c("mgcv", "gratia"))

# -----------------------------------------------------------------------------
# build_gamm_formula()
# -----------------------------------------------------------------------------
#' Build the mgcv model formula for a GAMM fit.
#'
#' @param outcome       Outcome variable name (character).
#' @param exposure      Exposure variable name (character).
#' @param exposure_type One of "linear", "categorical", "smooth" — determines
#'   whether the exposure enters as-is, as a factor, or as a `s()` smooth term.
#' @param covariates    Character vector of covariate terms to add as-is.
#' @param id_var        Participant identifier column, used for the random
#'   intercept smooth `s(id_var, bs = 're')`.
#' @param time_var      Time variable column, entered as a `s()` smooth term.
#' @param interaction   If TRUE, adds an exposure x time interaction term
#'   (tensor product smooth `ti()` when `exposure_type == "smooth"`, else a
#'   plain `:` interaction).
#' @return A `formula` object.
build_gamm_formula <- function(outcome, exposure, exposure_type, covariates,
                               id_var, time_var, interaction) {

    # Smooth for time
    time_term <- paste0("s(", time_var, ", bs = 'tp')")
    
    exposure_term <- switch(
        exposure_type,
        linear      = exposure,
        categorical = paste0("factor(", exposure, ")"),
        smooth      = paste0("s(", exposure, ", bs = 'tp', k = 5)"),
        stop("Unknown exposure_type for GAMM: ", exposure_type)
    )
    
    rhs <- c(exposure_term, time_term, covariates)
    
    # Exposure × time interaction as a tensor product smooth
    if (interaction && exposure_type == "smooth") {
        rhs <- c(rhs, paste0("ti(", exposure, ", ", time_var, ", bs = 'tp')"))
    } else if (interaction) {
        rhs <- c(rhs, paste0(exposure_term, ":", time_term))
    }
    
    # Random intercept per subject
    rhs <- c(rhs, paste0("s(", id_var, ", bs = 're')"))
    
    as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
}


# -----------------------------------------------------------------------------
# fit_gamm_complete()
# -----------------------------------------------------------------------------
#' Fit a GAMM on a single (complete-case) data frame.
#'
#' Builds the model formula via `build_gamm_formula()`, prepares the data
#' (complete-case filter, covariate standardization, exposure recoding), and
#' fits with `mgcv::bam()` (faster than `mgcv::gam()` for large longitudinal
#' datasets).
#'
#' @param data           Long-format data frame with one row per participant-visit.
#' @param config         Model config (see `normalise_config()`); provides
#'   outcome, exposure, exposure_type, interaction, and ref_level.
#' @param covariate_sets Named covariate-set lookup passed to `get_covariates()`.
#' @param id_var         Participant identifier column. Default "pt".
#' @param time_var       Time variable column. Default "time_since_baseline".
#' @return List with `model` (the fitted `bam` object), `tidy` (parametric
#'   term estimates), `smooth_summary` (smooth-term summary table),
#'   `formula`, and `config`.
fit_gamm_complete <- function(data, config, covariate_sets,
                              id_var = "pt", time_var = "time_since_baseline") {
    
    config        <- normalise_config(config)
    covariates    <- get_covariates(config, covariate_sets)
    outcome       <- get_config_value(config, "outcome")
    exposure      <- get_config_value(config, "exposure")
    exposure_type <- get_config_value(config, "exposure_type")
    interaction   <- get_config_value(config, "interaction")
    
    data <- complete_model_data(
        data, unique(c(outcome, exposure, covariates, id_var, time_var))
    )
    data <- standardize_covariates(data, covariates)
    data <- prepare_exposure(data,
                             exposure      = exposure,
                             exposure_type = exposure_type,
                             ref_level     = config$ref_level)
    
    formula <- build_gamm_formula(
        outcome       = outcome,
        exposure      = exposure,
        exposure_type = exposure_type,
        covariates    = covariates,
        id_var        = id_var,
        time_var      = time_var,
        interaction   = interaction
    )
    
    # mgcv::bam is faster than gam for large longitudinal datasets
    model <- mgcv::bam(formula, data = data, method = "fREML",
                       discrete = TRUE)
    
    tidy <- if (requireNamespace("gratia", quietly = TRUE)) {
        gratia::tidy_parametric(model, conf.int = TRUE)
    } else {
        as.data.frame(summary(model)$p.table) |>
            tibble::rownames_to_column("term") |>
            tibble::as_tibble()
    }
    
    list(
        model   = model,
        tidy    = tidy,
        smooth_summary = summary(model)$s.table,
        formula = formula,
        config  = config
    )
}


# -----------------------------------------------------------------------------
# fit_gamm_mice()
# -----------------------------------------------------------------------------
#' Fit a GAMM on each MICE imputation and pool the parametric terms.
#'
#' Accepts either a `mids` object or a long data frame with a `.imp` column
#' (imputation 0, if present, is excluded). Fits one `mgcv::bam()` model per
#' imputed dataset using a single shared formula, then pools parametric terms
#' via `mice::pool()` (Rubin's rules) — `mgcv` models expose `coef`/`vcov`
#' methods, so `mice::as.mira()` + `pool()` works for the parametric part of
#' the model. Smooth terms are not pooled.
#'
#' @param mids_object    A `mids` object, or a long data frame with `.imp`.
#' @param config         Model config; see `fit_gamm_complete()`.
#' @param covariate_sets Named covariate-set lookup passed to `get_covariates()`.
#' @param id_var         Participant identifier column. Default "pt".
#' @param time_var       Time variable column. Default "time_since_baseline".
#' @return List with `models` (list of fitted `bam` objects, one per
#'   imputation), `pooled` (the `mice::pool()` result), `tidy` (pooled
#'   parametric-term estimates only), `formula`, and `config`.
fit_gamm_mice <- function(mids_object, config, covariate_sets,
                          id_var = "pt", time_var = "time_since_baseline") {
    
    config        <- normalise_config(config)
    covariates    <- get_covariates(config, covariate_sets)
    outcome       <- get_config_value(config, "outcome")
    exposure      <- get_config_value(config, "exposure")
    exposure_type <- get_config_value(config, "exposure_type")
    interaction   <- get_config_value(config, "interaction")
    
    if (inherits(mids_object, "mids")) {
        imputed_data <- lapply(seq_len(mids_object$m), function(i)
            mice::complete(mids_object, i))
    } else if (is.data.frame(mids_object) && ".imp" %in% names(mids_object)) {
        imp_ids      <- sort(setdiff(unique(mids_object$.imp), 0L))
        imputed_data <- lapply(imp_ids, function(i)
            dplyr::filter(mids_object, .data$.imp == i))
    } else {
        stop("MICE data must be a mids object or a long data frame with .imp.")
    }
    
    formula <- build_gamm_formula(
        outcome       = outcome,
        exposure      = exposure,
        exposure_type = exposure_type,
        covariates    = covariates,
        id_var        = id_var,
        time_var      = time_var,
        interaction   = interaction
    )
    
    models <- lapply(imputed_data, function(df) {
        df <- complete_model_data(
            df, unique(c(outcome, exposure, covariates, id_var, time_var))
        )
        df <- standardize_covariates(df, covariates)
        df <- prepare_exposure(df,
                               exposure      = exposure,
                               exposure_type = exposure_type,
                               ref_level     = config$ref_level)
        mgcv::bam(formula, data = df, method = "fREML", discrete = TRUE)
    })
    
    # Pool parametric terms via Rubin's rules (mice::pool needs a coef/vcov method)
    # mgcv models expose these, so mice::as.mira + pool works for parametric parts
    pooled      <- mice::pool(mice::as.mira(models))
    pooled_tidy <- summary(pooled, conf.int = TRUE) |> tibble::as_tibble()
    
    list(
        models  = models,
        pooled  = pooled,
        tidy    = pooled_tidy,  # parametric terms only
        formula = formula,
        config  = config
    )
}