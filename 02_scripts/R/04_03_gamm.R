# =============================================================================
# GAMM (mgcv) — non-linear smooth for exposure and/or time
# =============================================================================
# Requires: mgcv, gratia (optional, for tidy output)
#install.packages(c("mgcv", "gratia"))

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