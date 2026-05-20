# =============================================================================
# 2. MODEL GRID
# =============================================================================
exposure_definitions <- tibble::tribble(
    ~exposure,                  ~exposure_type, ~ref_level,
    
    "dairy_total_gday",         "linear",       NA,
    "dairy_total_gday",         "rcs",          NA,
    
    "dairy_quartile_baseline",  "categorical",  "Q1",
    
    "dairy_guidelines_port",    "categorical",  "< 2 servings/day"
)


create_model_grid <- function(
        outcomes,
        exposure_definitions,
        datasets = c("cc", "mice"),
        interactions = c(TRUE, FALSE),
        random_slopes = c(FALSE, TRUE),
        cov_sets = names(covariate_sets)
) {
    
    tidyr::crossing(
        outcome = outcomes,
        exposure_definitions,
        dataset = datasets,
        interaction = interactions,
        random_slope = random_slopes,
        cov_set = cov_sets
    ) |>
        dplyr::mutate(
            model_id = dplyr::row_number()
        )
}
# =============================================================================
# 3. HELPERS
# =============================================================================

standardize_covariates <- function(df, covariates) {
    
    missing_covs <- setdiff(covariates, names(df))
    if (length(missing_covs) > 0) {
        stop(
            "Missing covariate(s): ",
            paste(missing_covs, collapse = ", "),
            call. = FALSE
        )
    }
    
    numeric_covs <- covariates[
        sapply(df[covariates], is.numeric)
    ]
    
    df[numeric_covs] <- lapply(
        df[numeric_covs],
        function(x) {
            sx <- stats::sd(x, na.rm = TRUE)
            if (is.na(sx) || sx == 0) {
                return(x)
            }
            as.numeric(scale(x))
        }
    )
    
    df
}


prepare_exposure <- function(df,
                             exposure,
                             exposure_type,
                             ref_level = NULL) {
    
    if (!exposure %in% names(df)) {
        stop("Missing exposure: ", exposure, call. = FALSE)
    }
    
    if (exposure_type == "categorical") {
        
        if (!is.factor(df[[exposure]])) {
            df[[exposure]] <- factor(df[[exposure]])
        }
        
        df[[exposure]] <- droplevels(df[[exposure]])
        
        levs <- levels(df[[exposure]])
        
        if (length(levs) < 2) {
            stop(
                "Categorical exposure ",
                exposure,
                " has fewer than 2 levels.",
                call. = FALSE
            )
        }
        
        if (!is.null(ref_level)) {
            
            if (!ref_level %in% levs) {
                stop(
                    "Reference level ",
                    ref_level,
                    " not found in ",
                    exposure,
                    call. = FALSE
                )
            }
            # TODO can it be ordered = False?
            df[[exposure]] <- stats::relevel(
                factor(df[[exposure]], ordered = FALSE),
                ref = ref_level
            )
        }
    }
    
    df
}



bt <- function(x) {
    paste0("`", gsub("`", "\\\\`", x), "`")
}

get_config_value <- function(config, name) {
    value <- config[[name]]
    if (length(value) != 1) {
        stop("Config field must be length 1: ", name, call. = FALSE)
    }
    value
}

normalise_config <- function(config) {
    if (is.data.frame(config)) {
        config <- as.list(config[1, , drop = FALSE])
    }
    config
}

get_covariates <- function(config, covariate_sets) {
    cov_set <- get_config_value(config, "cov_set")
    covariates <- covariate_sets[[cov_set]]
    
    if (is.null(covariates)) {
        stop(
            "Unknown covariate set: ",
            cov_set,
            ". Available sets: ",
            paste(names(covariate_sets), collapse = ", "),
            call. = FALSE
        )
    }
    
    covariates
}

complete_model_data <- function(data, vars) {
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0) {
        stop(
            "Missing model variable(s): ",
            paste(missing_vars, collapse = ", "),
            call. = FALSE
        )
    }
    
    data |>
        dplyr::filter(dplyr::if_all(dplyr::all_of(vars), ~ !is.na(.x)))
}


build_formula <- function(
        outcome,
        exposure,
        exposure_type,
        covariates,
        id_var,
        time_var,
        interaction,
        random_slope
) {
    
    exposure_term <- switch(
        
        exposure_type,
        
        linear = exposure,
        
        quartile = exposure,
        
        categorical = paste0("factor(", exposure, ")"),
        
        rcs = paste0(
            "rms::rcs(", exposure, ", 4)"
        ),
        
        ns = paste0(
            "splines::ns(", exposure, ", df = 3)"
        ),
        
        stop("Unknown exposure_type: ", exposure_type)
    )
    
    rhs <- c(
        exposure_term,
        time_var,
        covariates
    )
    
    if (interaction) {
        
        rhs <- c(
            rhs,
            paste0(
                exposure_term,
                ":",
                time_var
            )
        )
    }
    
    random_effect <- if (random_slope) {
        
        paste0(
            "(1 + ",
            time_var,
            " | ",
            id_var,
            ")"
        )
        
    } else {
        
        paste0(
            "(1 | ",
            id_var,
            ")"
        )
    }
    
    rhs <- c(
        rhs,
        random_effect
    )
    
    as.formula(
        paste(
            outcome,
            "~",
            paste(rhs, collapse = " + ")
        )
    )
}

# =============================================================================
# 4. COMPLETE CASE MODEL
# =============================================================================

fit_lmm_complete <- function(
        data,
        config,
        covariate_sets,
        id_var = "pt",
        time_var = "time_since_baseline"
) {
    
    config <- normalise_config(config)
    covariates <- get_covariates(config, covariate_sets)
    outcome <- get_config_value(config, "outcome")
    exposure <- get_config_value(config, "exposure")
    exposure_type <- get_config_value(config, "exposure_type")
    interaction <- get_config_value(config, "interaction")
    random_slope <- get_config_value(config, "random_slope")
    
    data <- complete_model_data(
        data,
        unique(c(outcome, exposure, covariates, id_var, time_var))
    )
    
    data <- standardize_covariates(
        data,
        covariates
    )
    
    data <- prepare_exposure(
        data,
        exposure = config$exposure,
        exposure_type = config$exposure_type,
        ref_level = config$ref_level
    )
    
    formula <- build_formula(
        outcome = outcome,
        exposure = exposure,
        exposure_type = exposure_type,
        covariates = covariates,
        id_var = id_var,
        time_var = time_var,
        interaction = interaction,
        random_slope = random_slope
    )
    
    model <- lmerTest::lmer(
        formula,
        data = data,
        REML = FALSE
    )
    
    list(
        
        model = model,
        
        tidy = broom.mixed::tidy(
            model,
            effects = "fixed",
            conf.int = TRUE
        ),
        
        glance = broom.mixed::glance(model),
        
        formula = formula,
        
        config = config
    )
}

# =============================================================================
# 5. MICE MODEL
# =============================================================================

fit_lmm_mice <- function(
        mids_object,
        config,
        covariate_sets,
        id_var = "pt",
        time_var = "time_since_baseline"
) {
    
    config <- normalise_config(config)
    covariates <- get_covariates(config, covariate_sets)
    outcome <- get_config_value(config, "outcome")
    exposure <- get_config_value(config, "exposure")
    exposure_type <- get_config_value(config, "exposure_type")
    interaction <- get_config_value(config, "interaction")
    random_slope <- get_config_value(config, "random_slope")
    
    if (inherits(mids_object, "mids")) {
        imputed_data <- lapply(seq_len(mids_object$m), function(i) {
            mice::complete(mids_object, i)
        })
    } else if (is.data.frame(mids_object) && ".imp" %in% names(mids_object)) {
        imp_ids <- sort(setdiff(unique(mids_object$.imp), 0L))
        imputed_data <- lapply(imp_ids, function(i) {
            dplyr::filter(mids_object, .data$.imp == i)
        })
    } else {
        stop(
            "MICE data must be a mids object or a long data frame with .imp.",
            call. = FALSE
        )
    }
    
    m <- length(imputed_data)
    
    models <- vector(
        "list",
        m
    )
    
    for (i in seq_len(m)) {
        
        df <- complete_model_data(
            imputed_data[[i]],
            unique(c(outcome, exposure, covariates, id_var, time_var))
        )
        
        df <- standardize_covariates(
            df,
            covariates
        )
        
        df <- prepare_exposure(
            df,
            exposure = config$exposure,
            exposure_type = config$exposure_type,
            ref_level = config$ref_level
        )
        
        formula <- build_formula(
            outcome = outcome,
            exposure = exposure,
            exposure_type = exposure_type,
            covariates = covariates,
            id_var = id_var,
            time_var = time_var,
            interaction = interaction,
            random_slope = random_slope
        )
        
        models[[i]] <- lmerTest::lmer(
            formula,
            data = df,
            REML = FALSE
        )
    }
    
    pooled <- mice::pool(
        mice::as.mira(models)
    )
    
    pooled_tidy <- summary(
        pooled,
        conf.int = TRUE
    ) |>
        tibble::as_tibble()
    
    list(
        
        models = models,
        
        pooled = pooled,
        
        tidy = pooled_tidy,
        
        formula = formula,
        
        config = config
    )
}

# =============================================================================
# 6. MAIN WRAPPER
# =============================================================================

run_lmm_model <- function(
        config,
        cc_data,
        mice_data,
        covariate_sets,
        id_var = "pt",
        time_var = "time_since_baseline"
) {
    
    config <- normalise_config(config)
    
    tryCatch(
        {
            if (get_config_value(config, "dataset") == "cc") {
                fit_lmm_complete(
                    data = cc_data,
                    config = config,
                    covariate_sets = covariate_sets,
                    id_var = id_var,
                    time_var = time_var
                )
                
            } else {
                
                fit_lmm_mice(
                    mids_object = mice_data,
                    config = config,
                    covariate_sets = covariate_sets,
                    id_var = id_var,
                    time_var = time_var
                )
            }
        },
        error = function(e) {
            list(
                model = NULL,
                tidy = tibble::tibble(
                    status = "error",
                    message = conditionMessage(e)
                ),
                glance = tibble::tibble(),
                formula = NA,
                config = config
            )
        }
    )
}

run_lmm_models <- function(
        model_grid,
        cc_data,
        mice_data,
        covariate_sets,
        id_var = "pt",
        time_var = "visit_num"
) {
    purrr::pmap(
        model_grid,
        function(...) {
            run_lmm_model(
                config = list(...),
                cc_data = cc_data,
                mice_data = mice_data,
                covariate_sets = covariate_sets,
                id_var = id_var,
                time_var = time_var
            )
        }
    )
}

export_lmm_results <- function(result) {
    
    if (is.list(result) && !is.null(result$tidy)) {
        return(export_one_lmm_result(result))
    }
    
    unlist(
        lapply(result, export_one_lmm_result),
        use.names = FALSE
    )
}

export_one_lmm_result <- function(result) {
    
    cfg <- normalise_config(result$config)
    
    cfg_value <- function(name) {
        as.character(get_config_value(cfg, name))
    }
    
    outdir <- make_output_dir(
        cfg_value("outcome")
    )
    
    filename <- paste(
        cfg_value("model_id"),
        cfg_value("dataset"),
        cfg_value("exposure"),
        cfg_value("exposure_type"),
        cfg_value("cov_set"),
        
        ifelse(
            isTRUE(get_config_value(cfg, "interaction")),
            "interaction",
            "nointeraction"
        ),
        
        ifelse(
            isTRUE(get_config_value(cfg, "random_slope")),
            "rslope",
            "rintercept"
        ),
        
        sep = "_"
    )
    
    out_file <- file.path(
        outdir,
        paste0(
            filename,
            ".csv"
        )
    )
    
    readr::write_csv(
        result$tidy,
        out_file
    )
    
    out_file
}

# =============================================================================
# 7. EXPORT
# =============================================================================

make_output_dir <- function(outcome,
                            prefix = "LMM",
                            subfolder = NULL) {
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    
    base <- file.path(
        "06_outputs",
        paste0(prefix, "_", outcome, "_", timestamp)
    )
    
    if (!is.null(subfolder)) {
        base <- file.path(base, subfolder)
    }
    
    dir.create(base, recursive = TRUE, showWarnings = FALSE)
    
    base
}

# =============================================================================
# 8. DIAGNOSTICS
# =============================================================================


run_lmm_diagnostics <- function(result,
                                base_outdir = NULL) {
    
    cfg <- result$config
    
    # ONLY run for complete-case models
    if (!is.null(cfg$dataset) && cfg$dataset != "cc") {
        return(list(
            model_id = cfg$model_id,
            status = "skipped_non_cc"
        ))
    }
    
    if (is.null(result$model)) {
        return(list(
            model_id = cfg$model_id,
            status = "skipped_no_model"
        ))
    }
    
    if (is.null(base_outdir)) {
        base_outdir <- make_output_dir(cfg$outcome, prefix = "LMM")
    }
    
    diag_dir <- file.path(base_outdir, "diagnostics")
    dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
    
    resid <- resid(result$model)
    fitted <- fitted(result$model)
    
    df_diag <- data.frame(fitted = fitted, residuals = resid)
    
    p_ta <- ggplot2::ggplot(df_diag, ggplot2::aes(fitted, residuals)) +
        ggplot2::geom_point(alpha = 0.4) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::theme_minimal()
    
    p_qq <- ggplot2::ggplot(df_diag, ggplot2::aes(sample = residuals)) +
        ggplot2::stat_qq(alpha = 0.4) +
        ggplot2::stat_qq_line() +
        ggplot2::theme_minimal()
    
    base_name <- paste0(
        "model_", cfg$model_id,
        "_", cfg$dataset
    )
    
    ggplot2::ggsave(file.path(diag_dir, paste0(base_name, "_TA.png")), p_ta)
    ggplot2::ggsave(file.path(diag_dir, paste0(base_name, "_QQ.png")), p_qq)
    
    list(
        model_id = cfg$model_id,
        status = "done",
        outdir = diag_dir
    )
}


# run_mice_diagnostics <- function(mice_result) {
#     
#     models <- mice_result$models
#     cfg <- mice_result$config
#     
#     if (is.null(models) || length(models) == 0) {
#         return(tibble::tibble(status = "no_models"))
#     }
#     
#     diag_list <- lapply(seq_along(models), function(i) {
#         
#         model <- models[[i]]
#         
#         resid <- resid(model)
#         fitted <- fitted(model)
#         
#         data.frame(
#             imp = i,
#             resid_mean = mean(resid, na.rm = TRUE),
#             resid_sd = sd(resid, na.rm = TRUE),
#             fitted_min = min(fitted, na.rm = TRUE),
#             fitted_max = max(fitted, na.rm = TRUE),
#             n = length(resid)
#         )
#     })
#     
#     diag_df <- dplyr::bind_rows(diag_list)
#     
#     summary_df <- diag_df |>
#         dplyr::summarise(
#             across(where(is.numeric), list(
#                 mean = mean,
#                 sd = sd
#             ))
#         )
#     
#     list(
#         per_imputation = diag_df,
#         summary = summary_df,
#         config = cfg
#     )
# }


# check_mice_stability <- function(diag_df) {
#     
#     tibble::tibble(
#         resid_mean_sd = sd(diag_df$resid_mean, na.rm = TRUE),
#         resid_sd_sd = sd(diag_df$resid_sd, na.rm = TRUE),
#         fitted_range_sd = sd(diag_df$fitted_max - diag_df$fitted_min),
#         n_imputations = dplyr::n_distinct(diag_df$imp)
#     ) |>
#         dplyr::mutate(
#             warning = resid_mean_sd > 0.05 | resid_sd_sd > 0.1
#         )
# }
# 
# 
# run_mice_diagnostics_full <- function(mice_result) {
#     
#     diag <- run_mice_diagnostics(mice_result)
#     
#     stability <- check_mice_stability(diag$per_imputation)
#     
#     list(
#         per_imputation = diag$per_imputation,
#         summary = diag$summary,
#         stability = stability,
#         config = mice_result$config
#     )
# }
# 
# 
# export_mice_diagnostics <- function(diag_result,
#                                     base_outdir = NULL) {
#     
#     cfg <- diag_result$config
#     
#     if (is.null(base_outdir)) {
#         base_outdir <- make_output_dir(cfg$outcome, prefix = "LMM")
#     }
#     
#     diag_dir <- file.path(base_outdir, "diagnostics_mice")
#     dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
#     
#     base_name <- paste0("model_", cfg$model_id, "_mice_diagnostics")
#     
#     file1 <- file.path(diag_dir, paste0(base_name, "_per_imputation.csv"))
#     file2 <- file.path(diag_dir, paste0(base_name, "_summary.csv"))
#     file3 <- file.path(diag_dir, paste0(base_name, "_stability.csv"))
#     
#     readr::write_csv(diag_result$per_imputation, file1)
#     readr::write_csv(diag_result$summary, file2)
#     readr::write_csv(diag_result$stability, file3)
#     
#     c(file1, file2, file3)
# }