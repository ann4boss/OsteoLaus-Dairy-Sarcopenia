# =============================================================================
# GAMM PIPELINE
# Mirrors the LMM pipeline structure (fit_lmm_complete / fit_lmm_mice / etc.)
# Backend: mgcv::gamm() wrapped in tryCatch for robustness
# =============================================================================

# =============================================================================
# 1. COVARIATE SETS
# =============================================================================

gamm_covariate_sets <- list(
    
    minimal = c(
        "age_at_baseline",
        "BMI"
    ),
    
    full_alcohol_conso = c(
        "alcohol_category_conso",
        "age_at_baseline", "BMI",
        "mrtsts2", "education_level",
        "smoking_status",
        "pa_levels_tertile_f1",
        "diabetes_status",
        "hrt_status",
        "htn_status",
        "hypolip_drug_status", "corticoids_status", "vitD_status", "calcium_status",
        "benzo_status", "bisphosphonate_status",
        "sumtot1"
    ),
    
    full_alcohol_sumalco = c(
        "alcohol_category_sumalco",
        "age_at_baseline", "BMI",
        "mrtsts2", "education_level",
        "smoking_status",
        "pa_levels_tertile_f1",
        "diabetes_status",
        "hrt_status",
        "htn_status",
        "hypolip_drug_status", "corticoids_status", "vitD_status", "calcium_status",
        "benzo_status", "bisphosphonate_status",
        "sumtot1"
    )
)

# =============================================================================
# 2. EXPOSURE DEFINITIONS & MODEL GRID
# =============================================================================

# exposure_type is one of: "linear", "smooth", "categorical"
# For "smooth", the exposure enters as s(exposure, bs = "tp", k = k_smooth)
# For "linear" and "categorical", it enters as a standard parametric term.
# ref_level is used only for categorical exposures.

gamm_exposure_definitions <- tibble::tribble(
    ~exposure,                  ~exposure_type,  ~ref_level,
    
    "dairy_total_gday",         "linear",         NA,
    "dairy_total_gday",         "smooth",         NA,
    
    "dairy_quartile_baseline",  "categorical",    "Q1",
    
    "dairy_guidelines_port",    "categorical",    "< 2 servings/day"
)


create_gamm_grid <- function(
        outcomes,
        gamm_exposure_definitions,
        datasets      = c("cc", "mice"),
        interactions  = c(TRUE, FALSE),   # exposure x time_var interaction
        cov_sets      = names(gamm_covariate_sets),
        k_smooth      = 4L                # basis dimension for s() terms
) {
    
    tidyr::crossing(
        outcome = outcomes,
        gamm_exposure_definitions,
        dataset     = datasets,
        interaction = interactions,
        cov_set     = cov_sets
    ) |>
        dplyr::mutate(
            k_smooth  = k_smooth,
            model_id  = dplyr::row_number()
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
    
    numeric_covs <- covariates[sapply(df[covariates], is.numeric)]
    
    df[numeric_covs] <- lapply(df[numeric_covs], function(x) {
        sx <- stats::sd(x, na.rm = TRUE)
        if (is.na(sx) || sx == 0) return(x)
        as.numeric(scale(x))
    })
    
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
                "Categorical exposure ", exposure,
                " has fewer than 2 levels.",
                call. = FALSE
            )
        }
        
        if (!is.null(ref_level) && !is.na(ref_level)) {
            if (!ref_level %in% levs) {
                stop(
                    "Reference level '", ref_level,
                    "' not found in ", exposure,
                    call. = FALSE
                )
            }
            df[[exposure]] <- stats::relevel(
                factor(df[[exposure]], ordered = FALSE),
                ref = ref_level
            )
        }
    }
    
    df
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


get_covariates <- function(config, gamm_covariate_sets) {
    cov_set    <- get_config_value(config, "cov_set")
    covariates <- gamm_covariate_sets[[cov_set]]
    
    if (is.null(covariates)) {
        stop(
            "Unknown covariate set: ", cov_set,
            ". Available sets: ",
            paste(names(gamm_covariate_sets), collapse = ", "),
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


# ---------------------------------------------------------------------------
# build_gamm_formula()
#
# Produces a formula for mgcv::gamm().
# - "smooth"      -> s(exposure, bs = "tp", k = k_smooth)
#                    interaction adds s(exposure, time_var, bs = "tp")
# - "linear" /
#   "categorical" -> standard parametric term; time_var enters as s(time_var)
#                    interaction adds exposure:time_var
# Random intercept per subject is handled via the random = argument in
# mgcv::gamm(), NOT in the formula; see fit_gamm_complete().
# ---------------------------------------------------------------------------

build_gamm_formula <- function(
        outcome,
        exposure,
        exposure_type,
        covariates,
        time_var,
        interaction,
        k_smooth = 4L
) {
    
    k_smooth <- as.integer(k_smooth)
    
    # ---------- exposure term(s) ----------
    if (exposure_type == "smooth") {
        
        main_exposure_term <- paste0(
            "s(", exposure, ", bs = 'tp', k = ", k_smooth, ")"
        )
        
        time_term <- paste0(
            "s(", time_var, ", bs = 'tp', k = ", k_smooth, ")"
        )
        
        interaction_term <- if (interaction) {
            paste0(
                "ti(", exposure, ", ", time_var,
                ", bs = 'tp', k = c(", k_smooth, ", ", k_smooth, "))"
            )
        } else {
            NULL
        }
        
    } else {
        # "linear" or "categorical"
        # categorical exposures are already factor-releveled in prepare_exposure;
        # we just pass the bare column name and mgcv treats it as parametric.
        
        main_exposure_term <- exposure
        
        time_term <- paste0(
            "s(", time_var, ", bs = 'tp', k = ", k_smooth, ")"
        )
        
        interaction_term <- if (interaction) {
            paste0(exposure, ":", time_var)
        } else {
            NULL
        }
    }
    
    # ---------- parametric covariates ----------
    # Numeric covariates are already standardised; categorical ones enter
    # as-is (mgcv handles factors parametrically inside gamm).
    cov_terms <- covariates
    
    rhs_parts <- c(
        main_exposure_term,
        time_term,
        cov_terms,
        interaction_term        # NULL is silently dropped by c()
    )
    
    as.formula(
        paste(outcome, "~", paste(rhs_parts, collapse = " + "))
    )
}

# =============================================================================
# 4. COMPLETE-CASE GAMM
# =============================================================================

fit_gamm_complete <- function(
        data,
        config,
        gamm_covariate_sets,
        id_var   = "pt",
        time_var = "time_since_baseline"
) {
    
    config        <- normalise_config(config)
    covariates    <- get_covariates(config, gamm_covariate_sets)
    outcome       <- get_config_value(config, "outcome")
    exposure      <- get_config_value(config, "exposure")
    exposure_type <- get_config_value(config, "exposure_type")
    interaction   <- get_config_value(config, "interaction")
    ref_level     <- config[["ref_level"]]   # may be NULL / NA
    k_smooth      <- config[["k_smooth"]] %||% 4L
    
    data <- complete_model_data(
        data,
        unique(c(outcome, exposure, covariates, id_var, time_var))
    )
    
    data <- standardize_covariates(data, covariates)
    
    data <- prepare_exposure(
        data,
        exposure      = exposure,
        exposure_type = exposure_type,
        ref_level     = ref_level
    )
    
    formula <- build_gamm_formula(
        outcome       = outcome,
        exposure      = exposure,
        exposure_type = exposure_type,
        covariates    = covariates,
        time_var      = time_var,
        interaction   = interaction,
        k_smooth      = k_smooth
    )
    
    # mgcv::gamm() takes the random effect via a separate list, not in the
    # formula.  A random intercept per subject is specified as:
    #   random = list(pt = ~1)
    # For a random intercept + random slope over time:
    #   random = list(pt = ~1 + time_since_baseline)
    random_formula <- stats::as.formula(
        paste("~1 +", time_var)
    )
    
    random_list <- stats::setNames(
        list(random_formula),
        id_var
    )
    
    model <- mgcv::gamm(
        formula,
        random = random_list,
        data   = data,
        method = "ML"          # equivalent to REML = FALSE in lme4
    )
    
    # mgcv::gamm() returns a list with $gam and $lme components.
    # We tidy the $gam part for smooth terms and the $lme part for
    # the random-effects / variance summary.
    
    tidy_parametric <- broom::tidy(
        model$gam,
        parametric = TRUE,
        conf.int   = TRUE
    )
    
    tidy_smooth <- broom::tidy(
        model$gam,
        parametric = FALSE
    )
    
    list(
        model            = model,
        tidy_parametric  = tidy_parametric,
        tidy_smooth      = tidy_smooth,
        glance           = broom::glance(model$gam),
        formula          = formula,
        config           = config
    )
}

# =============================================================================
# 5. MICE GAMM
# =============================================================================

fit_gamm_mice <- function(
        mids_object,
        config,
        gamm_covariate_sets,
        id_var   = "pt",
        time_var = "time_since_baseline"
) {
    
    config        <- normalise_config(config)
    covariates    <- get_covariates(config, gamm_covariate_sets)
    outcome       <- get_config_value(config, "outcome")
    exposure      <- get_config_value(config, "exposure")
    exposure_type <- get_config_value(config, "exposure_type")
    interaction   <- get_config_value(config, "interaction")
    ref_level     <- config[["ref_level"]]
    k_smooth      <- config[["k_smooth"]] %||% 4L
    
    # ---- unpack imputed datasets ----------------------------------------
    if (inherits(mids_object, "mids")) {
        imputed_data <- lapply(seq_len(mids_object$m), function(i) {
            mice::complete(mids_object, i)
        })
    } else if (is.data.frame(mids_object) && ".imp" %in% names(mids_object)) {
        imp_ids      <- sort(setdiff(unique(mids_object$.imp), 0L))
        imputed_data <- lapply(imp_ids, function(i) {
            dplyr::filter(mids_object, .data$.imp == i)
        })
    } else {
        stop(
            "MICE data must be a mids object or a long data frame with .imp.",
            call. = FALSE
        )
    }
    
    m      <- length(imputed_data)
    models <- vector("list", m)
    
    random_formula <- stats::as.formula(paste("~1 +", time_var))
    random_list    <- stats::setNames(list(random_formula), id_var)
    
    formula <- NULL   # built once, reused across imputations
    
    for (i in seq_len(m)) {
        
        df <- complete_model_data(
            imputed_data[[i]],
            unique(c(outcome, exposure, covariates, id_var, time_var))
        )
        
        df <- standardize_covariates(df, covariates)
        
        df <- prepare_exposure(
            df,
            exposure      = exposure,
            exposure_type = exposure_type,
            ref_level     = ref_level
        )
        
        formula <- build_gamm_formula(
            outcome       = outcome,
            exposure      = exposure,
            exposure_type = exposure_type,
            covariates    = covariates,
            time_var      = time_var,
            interaction   = interaction,
            k_smooth      = k_smooth
        )
        
        models[[i]] <- mgcv::gamm(
            formula,
            random = random_list,
            data   = df,
            method = "ML"
        )
    }
    
    # ---- pool parametric terms via Rubin's rules -------------------------
    # Extract parametric estimates + SE from each $gam object, then pool
    # manually (mice::pool requires objects with a coef/vcov method).
    # We wrap each $gam in a lightweight list that mice can handle via
    # mice::as.mira() → mice::pool().
    
    gam_models <- lapply(models, `[[`, "gam")
    
    pooled <- tryCatch(
        {
            mice::pool(mice::as.mira(gam_models))
        },
        error = function(e) {
            warning(
                "mice::pool() failed for GAMM: ", conditionMessage(e),
                "\nFalling back to manual Rubin pooling of parametric terms.",
                call. = FALSE
            )
            pool_gamm_manual(gam_models)
        }
    )
    
    pooled_tidy <- if (inherits(pooled, "mipo")) {
        summary(pooled, conf.int = TRUE) |> tibble::as_tibble()
    } else {
        pooled   # already a tibble from pool_gamm_manual()
    }
    
    # ---- smooth-term summaries (average EDF across imputations) ----------
    smooth_summary <- pool_gamm_smooths(gam_models)
    
    list(
        models         = models,
        pooled         = pooled,
        tidy_parametric = pooled_tidy,
        tidy_smooth    = smooth_summary,
        formula        = formula,
        config         = config
    )
}


# ---------------------------------------------------------------------------
# pool_gamm_manual()
# Fallback: manual Rubin's rules for parametric terms.
# Returns a tibble matching the layout of summary(mice::pool(...)).
# ---------------------------------------------------------------------------

pool_gamm_manual <- function(gam_models) {
    
    m <- length(gam_models)
    
    coef_list <- lapply(gam_models, function(g) {
        s  <- summary(g)
        pt <- s$p.table
        data.frame(
            term     = rownames(pt),
            estimate = pt[, "Estimate"],
            std.error = pt[, "Std. Error"],
            stringsAsFactors = FALSE
        )
    })
    
    terms <- coef_list[[1]]$term
    
    pooled_rows <- lapply(terms, function(trm) {
        ests <- sapply(coef_list, function(d) d$estimate[d$term == trm])
        ses  <- sapply(coef_list, function(d) d$std.error[d$term == trm])
        
        Q_bar  <- mean(ests)
        U_bar  <- mean(ses^2)
        B      <- var(ests)
        T_var  <- U_bar + (1 + 1/m) * B
        se_T   <- sqrt(T_var)
        
        df_lambda <- (1 + 1/m) * B / T_var
        df_obs    <- (m - 1) / df_lambda^2   # Barnard-Rubin (simplified)
        
        tibble::tibble(
            term      = trm,
            estimate  = Q_bar,
            std.error = se_T,
            statistic = Q_bar / se_T,
            df        = df_obs,
            p.value   = 2 * stats::pt(abs(Q_bar / se_T), df = df_obs, lower.tail = FALSE),
            conf.low  = Q_bar - stats::qt(0.975, df_obs) * se_T,
            conf.high = Q_bar + stats::qt(0.975, df_obs) * se_T
        )
    })
    
    dplyr::bind_rows(pooled_rows)
}


# ---------------------------------------------------------------------------
# pool_gamm_smooths()
# Average EDF and p-values for smooth terms across imputations.
# p-values are combined via Fisher's method.
# ---------------------------------------------------------------------------

pool_gamm_smooths <- function(gam_models) {
    
    m <- length(gam_models)
    
    smooth_list <- lapply(gam_models, function(g) {
        s  <- summary(g)
        st <- s$s.table
        data.frame(
            term  = rownames(st),
            edf   = st[, "edf"],
            p_val = st[, "p-value"],
            stringsAsFactors = FALSE
        )
    })
    
    terms <- smooth_list[[1]]$term
    
    pooled_smooths <- lapply(terms, function(trm) {
        edfs   <- sapply(smooth_list, function(d) d$edf[d$term == trm])
        pvals  <- sapply(smooth_list, function(d) d$p_val[d$term == trm])
        
        # Fisher combined p-value
        chi2   <- -2 * sum(log(pmax(pvals, .Machine$double.eps)))
        p_fish <- stats::pchisq(chi2, df = 2 * m, lower.tail = FALSE)
        
        tibble::tibble(
            term       = trm,
            edf_mean   = mean(edfs),
            edf_sd     = stats::sd(edfs),
            p_fisher   = p_fish
        )
    })
    
    dplyr::bind_rows(pooled_smooths)
}

# =============================================================================
# 6. MAIN WRAPPER
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b


run_gamm_model <- function(
        config,
        cc_data,
        mice_data,
        gamm_covariate_sets,
        id_var   = "pt",
        time_var = "time_since_baseline"
) {
    
    config <- normalise_config(config)
    
    tryCatch(
        {
            if (get_config_value(config, "dataset") == "cc") {
                fit_gamm_complete(
                    data           = cc_data,
                    config         = config,
                    gamm_covariate_sets = gamm_covariate_sets,
                    id_var         = id_var,
                    time_var       = time_var
                )
            } else {
                fit_gamm_mice(
                    mids_object    = mice_data,
                    config         = config,
                    gamm_covariate_sets = gamm_covariate_sets,
                    id_var         = id_var,
                    time_var       = time_var
                )
            }
        },
        error = function(e) {
            list(
                model           = NULL,
                tidy_parametric = tibble::tibble(
                    status  = "error",
                    message = conditionMessage(e)
                ),
                tidy_smooth  = tibble::tibble(),
                glance       = tibble::tibble(),
                formula      = NA,
                config       = config
            )
        }
    )
}


run_gamm_models <- function(
        model_grid,
        cc_data,
        mice_data,
        gamm_covariate_sets,
        id_var   = "pt",
        time_var = "time_since_baseline"
) {
    purrr::pmap(
        model_grid,
        function(...) {
            run_gamm_model(
                config         = list(...),
                cc_data        = cc_data,
                mice_data      = mice_data,
                gamm_covariate_sets = gamm_covariate_sets,
                id_var         = id_var,
                time_var       = time_var
            )
        }
    )
}

# =============================================================================
# 7. EXPORT
# =============================================================================

gamm_make_output_dir <- function(outcome,
                            prefix    = "GAMM",
                            subfolder = NULL) {
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    base      <- file.path(
        "06_outputs",
        paste0(prefix, "_", outcome, "_", timestamp)
    )
    
    if (!is.null(subfolder)) base <- file.path(base, subfolder)
    
    dir.create(base, recursive = TRUE, showWarnings = FALSE)
    base
}


export_gamm_results <- function(result) {
    
    if (is.list(result) && !is.null(result$tidy_parametric)) {
        return(export_one_gamm_result(result))
    }
    
    unlist(
        lapply(result, export_one_gamm_result),
        use.names = FALSE
    )
}


export_one_gamm_result <- function(result) {
    
    cfg       <- normalise_config(result$config)
    cfg_value <- function(name) as.character(get_config_value(cfg, name))
    
    outdir <- gamm_gamm_make_output_dir(cfg_value("outcome"))
    
    stem <- paste(
        cfg_value("model_id"),
        cfg_value("dataset"),
        cfg_value("exposure"),
        cfg_value("exposure_type"),
        cfg_value("cov_set"),
        ifelse(isTRUE(get_config_value(cfg, "interaction")), "interaction", "nointeraction"),
        sep = "_"
    )
    
    # --- parametric terms ---
    para_file <- file.path(outdir, paste0(stem, "_parametric.csv"))
    readr::write_csv(result$tidy_parametric, para_file)
    
    # --- smooth terms ---
    smooth_file <- file.path(outdir, paste0(stem, "_smooth.csv"))
    if (!is.null(result$tidy_smooth) && nrow(result$tidy_smooth) > 0) {
        readr::write_csv(result$tidy_smooth, smooth_file)
    }
    
    c(para_file, smooth_file)
}

# =============================================================================
# 8. DIAGNOSTICS
# =============================================================================

run_gamm_diagnostics <- function(result,
                                 base_outdir = NULL) {
    
    cfg <- result$config
    
    if (!is.null(cfg$dataset) && cfg$dataset != "cc") {
        return(list(model_id = cfg$model_id, status = "skipped_non_cc"))
    }
    
    if (is.null(result$model)) {
        return(list(model_id = cfg$model_id, status = "skipped_no_model"))
    }
    
    if (is.null(base_outdir)) {
        base_outdir <- gamm_gamm_make_output_dir(cfg$outcome, prefix = "GAMM")
    }
    
    diag_dir <- file.path(base_outdir, "diagnostics")
    dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
    
    gam_obj <- result$model$gam
    
    resid  <- stats::resid(gam_obj)
    fitted <- stats::fitted(gam_obj)
    df_diag <- data.frame(fitted = fitted, residuals = resid)
    
    # tuned-residuals vs fitted
    p_ta <- ggplot2::ggplot(df_diag, ggplot2::aes(fitted, residuals)) +
        ggplot2::geom_point(alpha = 0.3, size = 0.8) +
        ggplot2::geom_hline(yintercept = 0, colour = "steelblue") +
        ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "tomato", linewidth = 0.8) +
        ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
        ggplot2::theme_minimal()
    
    # QQ
    p_qq <- ggplot2::ggplot(df_diag, ggplot2::aes(sample = residuals)) +
        ggplot2::stat_qq(alpha = 0.3, size = 0.8) +
        ggplot2::stat_qq_line(colour = "steelblue") +
        ggplot2::labs(title = "Normal Q-Q") +
        ggplot2::theme_minimal()
    
    # gam.check() equivalent: histogram of residuals
    p_hist <- ggplot2::ggplot(df_diag, ggplot2::aes(x = residuals)) +
        ggplot2::geom_histogram(bins = 40, fill = "grey60", colour = "white") +
        ggplot2::labs(title = "Residual distribution", x = "Residuals") +
        ggplot2::theme_minimal()
    
    base_name <- paste0("model_", cfg$model_id, "_", cfg$dataset)
    
    ggplot2::ggsave(file.path(diag_dir, paste0(base_name, "_TA.png")),   p_ta,   width = 6, height = 4)
    ggplot2::ggsave(file.path(diag_dir, paste0(base_name, "_QQ.png")),   p_qq,   width = 6, height = 4)
    ggplot2::ggsave(file.path(diag_dir, paste0(base_name, "_HIST.png")), p_hist, width = 6, height = 4)
    
    # basis dimension check (k-index) — prints to console; capture as text
    k_check_txt <- tryCatch(
        {
            tc <- utils::capture.output(mgcv::k.check(gam_obj))
            paste(tc, collapse = "\n")
        },
        error = function(e) paste("k.check failed:", conditionMessage(e))
    )
    
    writeLines(
        k_check_txt,
        file.path(diag_dir, paste0(base_name, "_k_check.txt"))
    )
    
    list(
        model_id = cfg$model_id,
        status   = "done",
        outdir   = diag_dir
    )
}


run_mice_gamm_diagnostics <- function(mice_result) {
    
    models <- mice_result$models
    cfg    <- mice_result$config
    
    if (is.null(models) || length(models) == 0) {
        return(tibble::tibble(status = "no_models"))
    }
    
    diag_list <- lapply(seq_along(models), function(i) {
        gam_obj <- models[[i]]$gam
        resid   <- stats::resid(gam_obj)
        fitted  <- stats::fitted(gam_obj)
        
        data.frame(
            imp        = i,
            resid_mean = mean(resid, na.rm = TRUE),
            resid_sd   = stats::sd(resid, na.rm = TRUE),
            fitted_min = min(fitted, na.rm = TRUE),
            fitted_max = max(fitted, na.rm = TRUE),
            deviance   = gam_obj$deviance,
            n          = length(resid)
        )
    })
    
    diag_df <- dplyr::bind_rows(diag_list)
    
    summary_df <- diag_df |>
        dplyr::summarise(
            dplyr::across(where(is.numeric), list(mean = mean, sd = stats::sd))
        )
    
    list(
        per_imputation = diag_df,
        summary        = summary_df,
        config         = cfg
    )
}


check_mice_gamm_stability <- function(diag_df) {
    
    tibble::tibble(
        resid_mean_sd     = stats::sd(diag_df$resid_mean, na.rm = TRUE),
        resid_sd_sd       = stats::sd(diag_df$resid_sd,   na.rm = TRUE),
        fitted_range_sd   = stats::sd(diag_df$fitted_max - diag_df$fitted_min),
        deviance_sd       = stats::sd(diag_df$deviance,   na.rm = TRUE),
        n_imputations     = dplyr::n_distinct(diag_df$imp)
    ) |>
        dplyr::mutate(
            warning = resid_mean_sd > 0.05 | resid_sd_sd > 0.1
        )
}


run_mice_gamm_diagnostics_full <- function(mice_result) {
    
    diag      <- run_mice_gamm_diagnostics(mice_result)
    stability <- check_mice_gamm_stability(diag$per_imputation)
    
    list(
        per_imputation = diag$per_imputation,
        summary        = diag$summary,
        stability      = stability,
        config         = mice_result$config
    )
}


export_mice_gamm_diagnostics <- function(diag_result,
                                         base_outdir = NULL) {
    
    cfg <- diag_result$config
    
    if (is.null(base_outdir)) {
        base_outdir <- gamm_make_output_dir(cfg$outcome, prefix = "GAMM")
    }
    
    diag_dir <- file.path(base_outdir, "diagnostics_mice")
    dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
    
    base_name <- paste0("model_", cfg$model_id, "_mice_diagnostics")
    
    readr::write_csv(
        diag_result$per_imputation,
        file.path(diag_dir, paste0(base_name, "_per_imputation.csv"))
    )
    readr::write_csv(
        diag_result$summary,
        file.path(diag_dir, paste0(base_name, "_summary.csv"))
    )
    readr::write_csv(
        diag_result$stability,
        file.path(diag_dir, paste0(base_name, "_stability.csv"))
    )
    
    invisible(file.path(diag_dir, base_name))
}

# =============================================================================
# 9. SMOOTH VISUALISATION
# =============================================================================
#
# Three public functions:
#
#   extract_smooth_data()      — pulls the smooth estimate + 95% CI from a
#                                fitted $gam object into a plain tibble.
#                                Works for CC results and for every imputed
#                                model inside a MICE result.
#
#   plot_smooth()              — ggplot of the smooth + CI for a single model
#                                (CC or one imputed dataset).
#
#   plot_smooth_mice()         — overlays all imputed smooths (light ribbons)
#                                plus the pooled mean curve so you can see
#                                between-imputation variability at a glance.
#
#   save_smooth_plots()        — convenience wrapper: picks the right plot
#                                function from the result type and writes PNG.
#
# All functions accept the list returned by run_gamm_model() / fit_gamm_*().
# =============================================================================

# ---------------------------------------------------------------------------
# extract_smooth_data()
#
# Uses mgcv::plot.gam() with se = TRUE and n = 200 points internally,
# then captures the plot data instead of drawing anything.
#
# Returns a tibble with columns:
#   x, fit, se, lower, upper, smooth_label
# ---------------------------------------------------------------------------

extract_smooth_data <- function(gam_obj,
                                exposure,
                                n_points = 200,
                                ci_level = 0.95) {
    
    mult <- stats::qnorm(1 - (1 - ci_level) / 2)   # 1.96 for 95%
    
    # Silence the actual plot; we only want the computed values.
    plot_data <- grDevices::dev.new()
    on.exit(grDevices::dev.off(), add = TRUE)
    
    pd <- mgcv::plot.gam(
        gam_obj,
        select   = 0,    # don't draw anything
        n        = n_points,
        se       = TRUE,
        seWithMean = TRUE,   # include uncertainty in the intercept
        pages    = 1
    )
    
    # Find the panel whose x variable matches `exposure`
    # pd is a list of panels; each has $xlab, $x, $fit, $se
    match_idx <- which(
        sapply(pd, function(p) {
            !is.null(p$xlab) && grepl(exposure, p$xlab, fixed = TRUE)
        })
    )
    
    if (length(match_idx) == 0) {
        # Fallback: take the first smooth panel
        match_idx <- 1L
        warning(
            "Could not match exposure '", exposure,
            "' to a smooth panel label; using panel 1.",
            call. = FALSE
        )
    }
    
    panel <- pd[[match_idx[1]]]
    
    tibble::tibble(
        x     = panel$x,
        fit   = panel$fit,
        se    = panel$se,
        lower = panel$fit - mult * panel$se,
        upper = panel$fit + mult * panel$se,
        smooth_label = panel$xlab
    )
}


# ---------------------------------------------------------------------------
# plot_smooth()
#
# Single smooth plot for a complete-case GAMM result.
# Pass the list returned by fit_gamm_complete() / run_gamm_model().
# ---------------------------------------------------------------------------

plot_smooth <- function(result,
                        exposure    = NULL,
                        outcome_lab = NULL,
                        exposure_lab = NULL,
                        ci_level    = 0.95,
                        rug         = TRUE,
                        data        = NULL) {
    
    cfg <- normalise_config(result$config)
    
    if (is.null(exposure))    exposure    <- cfg$exposure
    if (is.null(outcome_lab)) outcome_lab <- cfg$outcome
    if (is.null(exposure_lab)) exposure_lab <- exposure
    
    gam_obj <- result$model$gam
    
    sm_data <- extract_smooth_data(gam_obj, exposure, ci_level = ci_level)
    
    p <- ggplot2::ggplot(sm_data, ggplot2::aes(x, fit)) +
        ggplot2::geom_ribbon(
            ggplot2::aes(ymin = lower, ymax = upper),
            fill  = "steelblue", alpha = 0.25
        ) +
        ggplot2::geom_line(colour = "steelblue", linewidth = 0.9) +
        ggplot2::geom_hline(
            yintercept = 0, linetype = "dashed",
            colour = "grey50", linewidth = 0.5
        ) +
        ggplot2::labs(
            x     = exposure_lab,
            y     = paste0("s(", exposure_lab, ")  [partial effect on ", outcome_lab, "]"),
            title = paste0("Smooth of ", exposure_lab),
            subtitle = paste0(
                "Model ", cfg$model_id, " | ",
                cfg$dataset, " | ",
                cfg$cov_set,
                " | EDF = ", round(result$tidy_smooth$edf_mean[
                    grepl(exposure, result$tidy_smooth$term, fixed = TRUE)
                ][1], 2)
            ),
            caption = paste0(round(ci_level * 100), "% pointwise CI (seWithMean = TRUE)")
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(colour = "grey40", size = 9),
            plot.caption  = ggplot2::element_text(colour = "grey50", size = 8)
        )
    
    # Optional rug of observed exposure values
    if (rug && !is.null(data) && exposure %in% names(data)) {
        rug_df <- data.frame(x = data[[exposure]])
        p <- p + ggplot2::geom_rug(
            data    = rug_df,
            ggplot2::aes(x = x),
            sides   = "b",
            alpha   = 0.2,
            inherit.aes = FALSE
        )
    }
    
    p
}


# ---------------------------------------------------------------------------
# plot_smooth_mice()
#
# Overlays all imputed smooths (semi-transparent ribbons + thin lines)
# and the pooled mean curve on top.
# This is the right visual for checking between-imputation variability.
# ---------------------------------------------------------------------------

plot_smooth_mice <- function(result,
                             exposure     = NULL,
                             outcome_lab  = NULL,
                             exposure_lab = NULL,
                             ci_level     = 0.95) {
    
    cfg <- normalise_config(result$config)
    
    if (is.null(exposure))     exposure     <- cfg$exposure
    if (is.null(outcome_lab))  outcome_lab  <- cfg$outcome
    if (is.null(exposure_lab)) exposure_lab <- exposure
    
    # Extract smooth data from every imputed $gam
    imp_smooths <- lapply(seq_along(result$models), function(i) {
        sm <- extract_smooth_data(
            result$models[[i]]$gam,
            exposure,
            ci_level = ci_level
        )
        sm$imp <- i
        sm
    })
    
    all_smooths <- dplyr::bind_rows(imp_smooths)
    
    # Pooled mean curve: average fit and bounds across imputations at each x
    # We interpolate onto a common x grid first.
    x_grid <- seq(
        min(all_smooths$x, na.rm = TRUE),
        max(all_smooths$x, na.rm = TRUE),
        length.out = 200
    )
    
    pooled_curve <- lapply(imp_smooths, function(sm) {
        tibble::tibble(
            x   = x_grid,
            fit = stats::approx(sm$x, sm$fit,   xout = x_grid)$y,
            lwr = stats::approx(sm$x, sm$lower, xout = x_grid)$y,
            upr = stats::approx(sm$x, sm$upper, xout = x_grid)$y
        )
    }) |>
        dplyr::bind_rows(.id = "imp") |>
        dplyr::group_by(x) |>
        dplyr::summarise(
            fit_mean  = mean(fit, na.rm = TRUE),
            lwr_mean  = mean(lwr, na.rm = TRUE),
            upr_mean  = mean(upr, na.rm = TRUE),
            .groups   = "drop"
        )
    
    m <- length(result$models)
    
    ggplot2::ggplot() +
        # --- per-imputation ribbons (very light) ---
        ggplot2::geom_ribbon(
            data = all_smooths,
            ggplot2::aes(x = x, ymin = lower, ymax = upper,
                         group = imp),
            fill  = "steelblue", alpha = 0.06
        ) +
        # --- per-imputation curves (thin) ---
        ggplot2::geom_line(
            data = all_smooths,
            ggplot2::aes(x = x, y = fit, group = imp),
            colour    = "steelblue", alpha = 0.35, linewidth = 0.4
        ) +
        # --- pooled mean ribbon ---
        ggplot2::geom_ribbon(
            data = pooled_curve,
            ggplot2::aes(x = x, ymin = lwr_mean, ymax = upr_mean),
            fill  = "navy", alpha = 0.20
        ) +
        # --- pooled mean curve ---
        ggplot2::geom_line(
            data = pooled_curve,
            ggplot2::aes(x = x, y = fit_mean),
            colour = "navy", linewidth = 1
        ) +
        ggplot2::geom_hline(
            yintercept = 0, linetype = "dashed",
            colour = "grey50", linewidth = 0.5
        ) +
        ggplot2::labs(
            x        = exposure_lab,
            y        = paste0("s(", exposure_lab, ")  [partial effect on ", outcome_lab, "]"),
            title    = paste0("Smooth of ", exposure_lab, "  (MICE, m = ", m, ")"),
            subtitle = paste0(
                "Model ", cfg$model_id, " | ",
                cfg$cov_set,
                "  |  light blue = per-imputation  |  navy = pooled mean"
            ),
            caption  = paste0(round(ci_level * 100), "% pointwise CI per imputation")
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(colour = "grey40", size = 9),
            plot.caption  = ggplot2::element_text(colour = "grey50", size = 8)
        )
}


# ---------------------------------------------------------------------------
# save_smooth_plots()
#
# Convenience wrapper: dispatches to plot_smooth() or plot_smooth_mice()
# depending on the result type, then saves the PNG.
# Can be mapped over a list of results.
# ---------------------------------------------------------------------------

save_smooth_plots <- function(result,
                              base_outdir  = NULL,
                              exposure     = NULL,
                              outcome_lab  = NULL,
                              exposure_lab = NULL,
                              ci_level     = 0.95,
                              width        = 7,
                              height       = 5,
                              data         = NULL) {
    
    cfg <- normalise_config(result$config)
    
    # Only meaningful for smooth exposure type
    exp_type <- cfg$exposure_type %||% ""
    if (exp_type != "smooth") {
        return(invisible(NULL))
    }
    
    if (is.null(base_outdir)) {
        base_outdir <- gamm_make_output_dir(cfg$outcome, prefix = "GAMM")
    }
    
    plot_dir <- file.path(base_outdir, "smooth_plots")
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    
    stem <- paste0(
        "model_", cfg$model_id, "_",
        cfg$dataset, "_smooth"
    )
    
    if (cfg$dataset == "cc") {
        p <- plot_smooth(
            result,
            exposure     = exposure,
            outcome_lab  = outcome_lab,
            exposure_lab = exposure_lab,
            ci_level     = ci_level,
            rug          = !is.null(data),
            data         = data
        )
    } else {
        p <- plot_smooth_mice(
            result,
            exposure     = exposure,
            outcome_lab  = outcome_lab,
            exposure_lab = exposure_lab,
            ci_level     = ci_level
        )
    }
    
    out_path <- file.path(plot_dir, paste0(stem, ".png"))
    ggplot2::ggsave(out_path, p, width = width, height = height, dpi = 150)
    
    invisible(out_path)
}