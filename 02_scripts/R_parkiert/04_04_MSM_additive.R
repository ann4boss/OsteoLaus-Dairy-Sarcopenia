# =============================================================================
# MARGINAL STRUCTURAL MODEL — ADDITIVE EFFECTS PIPELINE
# -----------------------------------------------------------------------------
# Targets the Average Total Causal Effect (ATE) of a continuous or categorical
# dairy exposure on a cognitive outcome, using inverse-probability weighting
# and weighted GEE (MSM) to estimate population-average effects.
#
# Pipeline
#   1.  Estimate exposure weights   (confounding adjustment)
#   2.  Estimate censoring weights  (informative dropout)
#   3.  Multiply weights            (combined MSM weights)
#   4.  Truncate extreme weights    (stability)
#   5.  Check covariate balance     (pseudo-population diagnostics)
#   6.  Fit weighted GEE / MSM      (causal effect estimation)
#   7.  Export results
#   8.  Diagnostics
#
# Required packages
#   WeightIt, cobalt, survey, geepack, broom, tidyverse, cli, ggplot2, purrr
# =============================================================================


# =============================================================================
# 1.  MODEL GRID
# =============================================================================

# ── Exposure definitions ─────────────────────────────────────────────────────
# Each row is a distinct exposure specification that will be crossed with the
# other grid dimensions.
msm_exposure_definitions <- tibble::tribble(
    ~exposure,              ~exposure_type,  ~ref_level,
    
    "dairy_total_gday",     "linear",        NA,
    "dairy_ferm_gday",      "linear",        NA,
    "dairy_nonferm_gday",   "linear",        NA,
    "dairy_fullfat_gday",   "linear",        NA,
    "dairy_nonfat_gday",    "linear",        NA,
    "dairy_sugary_gday",    "linear",        NA,
    
    "dairy_quartile",       "categorical",   "Q1"
)


# ── Food-group confounders (from derive_food_groups) ─────────────────────────
# These covariates are measured at baseline; the "competing" food group for a
# given dairy exposure type is included so that the weight model conditions on
# the substitution structure (addition-effect / isocaloric contrasts).
.FOOD_GROUPS <- c(
    "animal_protein_gday",
    "plant_protein_gday",
    "veg_gday",
    "fru_gday",
    "grains_gday",
    "fats_gday",
    "processed_gday",
    "alcohol_gday"
)

# ── Sociodemographic / clinical confounders ───────────────────────────────────
.SOCIO_CLINICAL <- c(
    "age_cat", "sex", "edu",
    "sm_b",                 # smoking
    "pa_b",                 # physical activity
    "bmi_cat",
    "HTA_b",                # hypertension
    "depre_b",              # depression
    "cvevent_b",            # cardiovascular event
    "diab_b",               # diabetes
    "famincome_b",          # family income
    "occ_b"                 # occupation
)

# ── Censoring predictors (loss-to-follow-up model) ───────────────────────────
.LTFU_PREDICTORS <- c(
    "age_cat", "sex", "occ_b", "bmi_cat",
    "sm_b", "cvevent_b", "HTA_b", "depre_b",
    "pa_b", "edu", "famincome_b"
)

# ── Covariate sets (for reproducibility / sensitivity analyses) ───────────────
msm_covariate_sets <- list(
    minimal   = .SOCIO_CLINICAL,
    full      = c(.SOCIO_CLINICAL, .FOOD_GROUPS),
    food_only = .FOOD_GROUPS
)

# ── Build the model grid ─────────────────────────────────────────────────────
create_msm_grid <- function(
        outcomes,
        exposure_definitions = msm_exposure_definitions,
        datasets             = c("cc", "mice"),
        cov_sets             = names(msm_covariate_sets),
        truncation_quantiles = 0.995
) {
    tidyr::crossing(
        outcome              = outcomes,
        exposure_definitions,
        dataset              = datasets,
        cov_set              = cov_sets,
        trunc_q              = truncation_quantiles
    ) |>
        dplyr::mutate(model_id = dplyr::row_number())
}


# =============================================================================
# 2.  HELPERS
# =============================================================================

#' Normalise a one-row data frame or list to a plain list
normalise_msm_config <- function(config) {
    if (is.data.frame(config)) config <- as.list(config[1L, , drop = FALSE])
    config
}

#' Safe scalar extraction from config
get_msm_val <- function(config, name) {
    v <- config[[name]]
    if (length(v) != 1L)
        stop("Config field must be length-1: ", name, call. = FALSE)
    v
}

#' Retrieve covariate names for a named covariate set
get_msm_covariates <- function(config, covariate_sets) {
    cs <- get_msm_val(config, "cov_set")
    cv <- covariate_sets[[cs]]
    if (is.null(cv))
        stop("Unknown covariate set: ", cs,
             ". Available: ", paste(names(covariate_sets), collapse = ", "),
             call. = FALSE)
    cv
}

#' Remove the "competing" dairy subtype from the food-group confounders so that
#' the weight model does not condition on a variable that is a direct function
#' of the exposure (e.g. when exposure = dairy_ferm_gday, condition on
#' dairy_nonferm_gday but not on dairy_ferm_gday).
drop_exposure_from_covariates <- function(covariates, exposure) {
    setdiff(covariates, exposure)
}

#' Build a right-hand-side formula string
build_rhs <- function(covariates) paste(covariates, collapse = " + ")

#' Truncate a numeric weight vector at a given upper quantile
truncate_weights <- function(w, quantile_upper = 0.995) {
    thr <- stats::quantile(w, quantile_upper, na.rm = TRUE)
    w[w > thr] <- thr
    w
}

#' Standardise covariates (z-score) before balance checking
standardize_msm_covariates <- function(df, covariates) {
    num_covs <- covariates[sapply(df[covariates], is.numeric)]
    df[num_covs] <- lapply(df[num_covs], function(x) {
        sx <- stats::sd(x, na.rm = TRUE)
        if (is.na(sx) || sx == 0) return(x)
        as.numeric(scale(x))
    })
    df
}

#' Complete-case filter: keep rows with no NA in any model variable
complete_msm_data <- function(data, vars) {
    missing <- setdiff(vars, names(data))
    if (length(missing) > 0)
        stop("Missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
    data |> dplyr::filter(dplyr::if_all(dplyr::all_of(vars), ~ !is.na(.x)))
}

#' Set the reference level for a categorical exposure
prepare_msm_exposure <- function(df, exposure, exposure_type, ref_level = NULL) {
    if (!exposure %in% names(df))
        stop("Missing exposure column: ", exposure, call. = FALSE)
    
    if (exposure_type == "categorical") {
        df[[exposure]] <- droplevels(factor(df[[exposure]]))
        if (!is.null(ref_level) && !is.na(ref_level)) {
            if (!ref_level %in% levels(df[[exposure]]))
                stop("Reference level '", ref_level,
                     "' not found in exposure '", exposure, "'.", call. = FALSE)
            df[[exposure]] <- stats::relevel(
                factor(df[[exposure]], ordered = FALSE), ref = ref_level
            )
        }
    }
    df
}


# =============================================================================
# 3.  STEP 1 — ESTIMATE EXPOSURE WEIGHTS
# =============================================================================
# Uses WeightIt::weightit() with propensity-score / generalised propensity-
# score estimation and kernel density for the denominator (continuous exposures).
# For categorical exposures, multinomial logistic regression is used.

estimate_exposure_weights <- function(data, exposure, exposure_type, covariates) {
    
    covariates <- drop_exposure_from_covariates(covariates, exposure)
    
    formula_str <- paste(exposure, "~", build_rhs(covariates))
    wt_formula  <- stats::as.formula(formula_str)
    
    # Choose WeightIt method
    if (exposure_type == "linear") {
        # Continuous exposure: generalised propensity score via kernel density
        wt_method  <- "ps"
        wt_obj <- WeightIt::weightit(
            formula    = wt_formula,
            data       = data,
            method     = wt_method,
            use.kernel = TRUE,   # kernel density for GPS denominator
            estimand   = "ATE"
        )
    } else {
        # Categorical exposure: multinomial logistic
        wt_obj <- WeightIt::weightit(
            formula  = wt_formula,
            data     = data,
            method   = "ps",
            estimand = "ATE"
        )
    }
    
    wt_obj
}


# =============================================================================
# 4.  STEP 2 — ESTIMATE CENSORING (IPCW) WEIGHTS
# =============================================================================
# ltfu_var must be a binary indicator: 1 = lost to follow-up (censored),
# 0 = observed at outcome assessment.

estimate_censoring_weights <- function(data,
                                       ltfu_var       = "ltfu",
                                       ltfu_predictors = .LTFU_PREDICTORS) {
    
    if (!ltfu_var %in% names(data))
        stop("Loss-to-follow-up indicator '", ltfu_var,
             "' not found in data.", call. = FALSE)
    
    formula_str <- paste(ltfu_var, "~", build_rhs(ltfu_predictors))
    wt_formula  <- stats::as.formula(formula_str)
    
    wt_obj <- WeightIt::weightit(
        formula    = wt_formula,
        data       = data,
        method     = "ps",
        use.kernel = TRUE,
        estimand   = "ATE"
    )
    
    wt_obj
}


# =============================================================================
# 5.  STEP 3 & 4 — COMBINE AND TRUNCATE WEIGHTS
# =============================================================================

combine_and_truncate_weights <- function(data,
                                         exp_weights,
                                         cens_weights,
                                         exposure_col,
                                         trunc_q = 0.995) {
    
    data$w_exposure  <- exp_weights$weights
    data$w_censoring <- cens_weights$weights
    data$w_msm_raw   <- data$w_exposure * data$w_censoring
    
    # Truncate combined weights
    data$w_msm <- truncate_weights(data$w_msm_raw, quantile_upper = trunc_q)
    
    # Summarise weight distribution (for logging)
    wt_summary <- summary(data$w_msm)
    cli::cli_inform(c(
        "v" = "Weights combined and truncated at {trunc_q} quantile.",
        " " = "Min  : {round(wt_summary['Min.'], 3)}",
        " " = "Median: {round(wt_summary['Median'], 3)}",
        " " = "Mean : {round(wt_summary['Mean'], 3)}",
        " " = "Max  : {round(wt_summary['Max.'], 3)}"
    ))
    
    data
}


# =============================================================================
# 6.  STEP 5 — COVARIATE BALANCE
# =============================================================================
# Returns a cobalt balance table and saves a Love plot.

check_msm_balance <- function(data,
                              exposure,
                              covariates,
                              weight_col  = "w_msm",
                              threshold   = 0.1,
                              out_dir     = NULL,
                              model_id    = NULL) {
    
    # cobalt::bal.tab accepts a formula and weights argument
    bt <- cobalt::bal.tab(
        stats::as.formula(paste(exposure, "~", build_rhs(covariates))),
        data      = data,
        weights   = data[[weight_col]],
        thresholds = c(cor = threshold),   # continuous; uses r/ρ
        un        = TRUE                   # also show unweighted balance
    )
    
    # Love plot
    p_love <- cobalt::love.plot(
        bt,
        threshold   = threshold,
        abs         = TRUE,
        var.order   = "unadjusted",
        title       = paste("Covariate Balance —", exposure),
        colors      = c("Unweighted" = "#E15759", "Weighted" = "#4E79A7")
    )
    
    if (!is.null(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        filename <- paste0("balance_love_", model_id, "_", exposure, ".png")
        ggplot2::ggsave(file.path(out_dir, filename), p_love,
                        width = 8, height = 6, dpi = 150)
        cli::cli_inform("i" = "Love plot saved: {file.path(out_dir, filename)}")
    }
    
    list(balance_table = bt, love_plot = p_love)
}


# =============================================================================
# 7.  STEP 6 — FIT WEIGHTED GEE / MSM
# =============================================================================
# Estimates the population-average (marginal) additive causal effect of the
# exposure on the outcome using GEE with MSM weights.

build_msm_formula <- function(outcome, exposure, exposure_type) {
    exp_term <- if (exposure_type == "categorical") {
        paste0("factor(", exposure, ")")
    } else {
        exposure   # linear: raw continuous term
    }
    stats::as.formula(paste(outcome, "~", exp_term))
}


fit_msm_gee <- function(data,
                        outcome,
                        exposure,
                        exposure_type,
                        id_var     = "pt",
                        weight_col = "w_msm",
                        corstr     = "independence") {
    
    formula <- build_msm_formula(outcome, exposure, exposure_type)
    
    model <- geepack::geeglm(
        formula = formula,
        data    = data,
        weights = data[[weight_col]],
        id      = data[[id_var]],
        corstr  = corstr
    )
    
    # Tidy output: beta, SE, 95% CI, Wald p
    tidy <- broom::tidy(model, conf.int = TRUE)
    
    list(
        model   = model,
        tidy    = tidy,
        formula = formula
    )
}


# =============================================================================
# 8.  COMPLETE-CASE MSM WRAPPER
# =============================================================================

fit_msm_complete <- function(data,
                             config,
                             covariate_sets   = msm_covariate_sets,
                             ltfu_var         = "ltfu",
                             ltfu_predictors  = .LTFU_PREDICTORS,
                             id_var           = "pt",
                             corstr           = "independence") {
    
    config        <- normalise_msm_config(config)
    outcome       <- get_msm_val(config, "outcome")
    exposure      <- get_msm_val(config, "exposure")
    exposure_type <- get_msm_val(config, "exposure_type")
    ref_level     <- config$ref_level
    trunc_q       <- get_msm_val(config, "trunc_q")
    covariates    <- get_msm_covariates(config, covariate_sets)
    model_id      <- config$model_id
    
    # ── Complete cases ────────────────────────────────────────────────────────
    all_vars <- unique(c(outcome, exposure, covariates,
                         ltfu_var, ltfu_predictors, id_var))
    data <- complete_msm_data(data, all_vars)
    data <- prepare_msm_exposure(data, exposure, exposure_type, ref_level)
    
    cli::cli_h2("MSM [{model_id}] | {exposure} ({exposure_type}) → {outcome}")
    cli::cli_inform("i" = "n = {nrow(data)} complete-case observations")
    
    # ── Step 1: Exposure weights ──────────────────────────────────────────────
    cli::cli_inform(">" = "Step 1: Estimating exposure weights ...")
    exp_wt <- estimate_exposure_weights(data, exposure, exposure_type, covariates)
    
    # ── Step 2: Censoring weights ─────────────────────────────────────────────
    cli::cli_inform(">" = "Step 2: Estimating censoring (IPCW) weights ...")
    cens_wt <- estimate_censoring_weights(data, ltfu_var, ltfu_predictors)
    
    # ── Steps 3 & 4: Combine & truncate ──────────────────────────────────────
    cli::cli_inform(">" = "Steps 3-4: Combining and truncating weights ...")
    data <- combine_and_truncate_weights(
        data         = data,
        exp_weights  = exp_wt,
        cens_weights = cens_wt,
        exposure_col = exposure,
        trunc_q      = trunc_q
    )
    
    # ── Step 5: Balance ───────────────────────────────────────────────────────
    cli::cli_inform(">" = "Step 5: Checking covariate balance ...")
    balance <- check_msm_balance(
        data       = data,
        exposure   = exposure,
        covariates = drop_exposure_from_covariates(covariates, exposure),
        weight_col = "w_msm",
        model_id   = model_id
        # out_dir set later by run_msm_model()
    )
    
    # ── Step 6: Weighted GEE ──────────────────────────────────────────────────
    cli::cli_inform(">" = "Step 6: Fitting weighted GEE (MSM) ...")
    gee_result <- fit_msm_gee(
        data          = data,
        outcome       = outcome,
        exposure      = exposure,
        exposure_type = exposure_type,
        id_var        = id_var,
        weight_col    = "w_msm",
        corstr        = corstr
    )
    
    list(
        model         = gee_result$model,
        tidy          = gee_result$tidy,
        formula       = gee_result$formula,
        balance       = balance,
        weight_summary = summary(data$w_msm),
        data          = data,     # data with weights attached
        config        = config
    )
}


# =============================================================================
# 9.  MICE (MULTIPLE IMPUTATION) MSM WRAPPER
# =============================================================================
# Fits one MSM per imputed dataset, then pools estimates via Rubin's rules
# using mice::pool() on the GEE models wrapped with mice::as.mira().

fit_msm_mice <- function(mids_object,
                         config,
                         covariate_sets  = msm_covariate_sets,
                         ltfu_var        = "ltfu",
                         ltfu_predictors = .LTFU_PREDICTORS,
                         id_var          = "pt",
                         corstr          = "independence") {
    
    config        <- normalise_msm_config(config)
    outcome       <- get_msm_val(config, "outcome")
    exposure      <- get_msm_val(config, "exposure")
    exposure_type <- get_msm_val(config, "exposure_type")
    ref_level     <- config$ref_level
    trunc_q       <- get_msm_val(config, "trunc_q")
    covariates    <- get_msm_covariates(config, covariate_sets)
    model_id      <- config$model_id
    
    # ── Unpack imputed datasets ───────────────────────────────────────────────
    if (inherits(mids_object, "mids")) {
        imp_list <- lapply(seq_len(mids_object$m),
                           function(i) mice::complete(mids_object, i))
    } else if (is.data.frame(mids_object) && ".imp" %in% names(mids_object)) {
        imp_ids  <- sort(setdiff(unique(mids_object$.imp), 0L))
        imp_list <- lapply(imp_ids,
                           function(i) dplyr::filter(mids_object, .imp == i))
    } else {
        stop("mids_object must be a mids object or long data frame with .imp.",
             call. = FALSE)
    }
    
    m      <- length(imp_list)
    models <- vector("list", m)
    
    cli::cli_h2("MSM-MICE [{model_id}] | {exposure} → {outcome} | m = {m}")
    
    for (i in seq_len(m)) {
        cli::cli_inform(">" = "Imputation {i}/{m}")
        df <- imp_list[[i]]
        
        all_vars <- unique(c(outcome, exposure, covariates,
                             ltfu_var, ltfu_predictors, id_var))
        df <- complete_msm_data(df, all_vars)
        df <- prepare_msm_exposure(df, exposure, exposure_type, ref_level)
        
        exp_wt  <- estimate_exposure_weights(df, exposure, exposure_type, covariates)
        cens_wt <- estimate_censoring_weights(df, ltfu_var, ltfu_predictors)
        
        df <- combine_and_truncate_weights(
            data         = df,
            exp_weights  = exp_wt,
            cens_weights = cens_wt,
            exposure_col = exposure,
            trunc_q      = trunc_q
        )
        
        gee_res    <- fit_msm_gee(df, outcome, exposure, exposure_type,
                                  id_var, "w_msm", corstr)
        models[[i]] <- gee_res$model
    }
    
    # ── Rubin's rules pooling ─────────────────────────────────────────────────
    pooled      <- mice::pool(mice::as.mira(models))
    pooled_tidy <- summary(pooled, conf.int = TRUE) |> tibble::as_tibble()
    
    list(
        models      = models,
        pooled      = pooled,
        tidy        = pooled_tidy,
        formula     = build_msm_formula(outcome, exposure, exposure_type),
        config      = config
    )
}


# =============================================================================
# 10. MAIN WRAPPER
# =============================================================================

run_msm_model <- function(config,
                          cc_data,
                          mice_data,
                          covariate_sets  = msm_covariate_sets,
                          ltfu_var        = "ltfu",
                          ltfu_predictors = .LTFU_PREDICTORS,
                          id_var          = "pt",
                          corstr          = "independence") {
    
    config    <- normalise_msm_config(config)
    timestamp <- format(Sys.time(), "%d%m%Y_%H%M")
    
    # ── Output directory ──────────────────────────────────────────────────────
    config_tag <- paste(
        get_msm_val(config, "dataset"),
        get_msm_val(config, "outcome"),
        get_msm_val(config, "exposure"),
        get_msm_val(config, "exposure_type"),
        get_msm_val(config, "cov_set"),
        paste0("trunc", get_msm_val(config, "trunc_q")),
        sep = "_"
    )
    out_dir <- file.path("03_outputs", "MSM", timestamp, config_tag)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # ── Fit ───────────────────────────────────────────────────────────────────
    tryCatch(
        {
            result <- if (get_msm_val(config, "dataset") == "cc") {
                fit_msm_complete(
                    data            = cc_data,
                    config          = config,
                    covariate_sets  = covariate_sets,
                    ltfu_var        = ltfu_var,
                    ltfu_predictors = ltfu_predictors,
                    id_var          = id_var,
                    corstr          = corstr
                )
            } else {
                fit_msm_mice(
                    mids_object     = mice_data,
                    config          = config,
                    covariate_sets  = covariate_sets,
                    ltfu_var        = ltfu_var,
                    ltfu_predictors = ltfu_predictors,
                    id_var          = id_var,
                    corstr          = corstr
                )
            }
            
            # Retro-fit balance Love plot into out_dir (complete-case only)
            if (!is.null(result$balance)) {
                bal_dir <- file.path(out_dir, "balance")
                dir.create(bal_dir, recursive = TRUE, showWarnings = FALSE)
                ggplot2::ggsave(
                    file.path(bal_dir, paste0("love_", config_tag, ".png")),
                    result$balance$love_plot,
                    width = 8, height = 6, dpi = 150
                )
            }
            
            result$out_dir    <- out_dir
            result$config_tag <- config_tag
            result
        },
        error = function(e) {
            cli::cli_warn("Model {config$model_id} failed: {conditionMessage(e)}")
            list(
                model      = NULL,
                tidy       = tibble::tibble(status = "error",
                                            message = conditionMessage(e)),
                formula    = NA,
                config     = config,
                out_dir    = out_dir,
                config_tag = config_tag
            )
        }
    )
}


run_msm_models <- function(model_grid,
                           cc_data,
                           mice_data,
                           covariate_sets  = msm_covariate_sets,
                           ltfu_var        = "ltfu",
                           ltfu_predictors = .LTFU_PREDICTORS,
                           id_var          = "pt",
                           corstr          = "independence") {
    purrr::pmap(
        model_grid,
        function(...) {
            run_msm_model(
                config          = list(...),
                cc_data         = cc_data,
                mice_data       = mice_data,
                covariate_sets  = covariate_sets,
                ltfu_var        = ltfu_var,
                ltfu_predictors = ltfu_predictors,
                id_var          = id_var,
                corstr          = corstr
            )
        }
    )
}


# =============================================================================
# 11. EXPORT
# =============================================================================

export_msm_results <- function(result) {
    if (is.list(result) && !is.null(result$tidy) && !is.list(result$tidy[[1]])) {
        return(export_one_msm_result(result))
    }
    unlist(lapply(result, export_one_msm_result), use.names = FALSE)
}

export_one_msm_result <- function(result) {
    
    cfg     <- normalise_msm_config(result$config)
    out_dir <- result$out_dir %||% {
        file.path("03_outputs", "MSM",
                  format(Sys.time(), "%d%m%Y_%H%M"),
                  "fallback")
    }
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    filename <- paste(
        get_msm_val(cfg, "model_id"),
        get_msm_val(cfg, "dataset"),
        get_msm_val(cfg, "outcome"),
        get_msm_val(cfg, "exposure"),
        get_msm_val(cfg, "exposure_type"),
        get_msm_val(cfg, "cov_set"),
        sep = "_"
    )
    
    # Main results table
    tidy_file <- file.path(out_dir, paste0(filename, "_tidy.csv"))
    readr::write_csv(result$tidy, tidy_file)
    
    # Balance table (complete-case only)
    if (!is.null(result$balance$balance_table)) {
        bt_df <- cobalt::tidy(result$balance$balance_table)
        bt_file <- file.path(out_dir, paste0(filename, "_balance.csv"))
        readr::write_csv(bt_df, bt_file)
    }
    
    tidy_file
}

# rlang-style null coalescing (avoid rlang dependency)
`%||%` <- function(x, y) if (!is.null(x)) x else y


# =============================================================================
# 12. DIAGNOSTICS
# =============================================================================
# Weight distribution plot + balance summary, saved to out_dir/diagnostics/.

run_msm_diagnostics <- function(result) {
    
    cfg <- result$config
    
    if (is.null(result$data)) {
        return(list(model_id = cfg$model_id, status = "skipped_no_data"))
    }
    
    diag_dir <- file.path(result$out_dir, "diagnostics")
    dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
    
    data     <- result$data
    base     <- paste0("model_", cfg$model_id, "_", cfg$dataset)
    
    # ── Weight distribution plot ──────────────────────────────────────────────
    p_wt <- ggplot2::ggplot(data, ggplot2::aes(x = w_msm)) +
        ggplot2::geom_histogram(bins = 60, fill = "#4E79A7", colour = "white") +
        ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
        ggplot2::labs(
            title    = paste("MSM weight distribution —", cfg$exposure),
            subtitle = paste("Truncated at", cfg$trunc_q, "quantile"),
            x        = "Weight",
            y        = "Count"
        ) +
        ggplot2::theme_minimal()
    
    ggplot2::ggsave(
        file.path(diag_dir, paste0(base, "_weights.png")),
        p_wt, width = 7, height = 4, dpi = 150
    )
    
    # ── Raw vs truncated weight scatter ──────────────────────────────────────
    p_trunc <- ggplot2::ggplot(data, ggplot2::aes(x = w_msm_raw, y = w_msm)) +
        ggplot2::geom_point(alpha = 0.3, size = 0.8) +
        ggplot2::geom_abline(slope = 1, intercept = 0,
                             linetype = "dashed", colour = "firebrick") +
        ggplot2::labs(
            title = "Raw vs truncated MSM weights",
            x     = "Raw weight",
            y     = "Truncated weight"
        ) +
        ggplot2::theme_minimal()
    
    ggplot2::ggsave(
        file.path(diag_dir, paste0(base, "_trunc_scatter.png")),
        p_trunc, width = 6, height = 5, dpi = 150
    )
    
    list(
        model_id = cfg$model_id,
        status   = "done",
        out_dir  = diag_dir
    )
}


# =============================================================================
# 13. EXAMPLE USAGE
# =============================================================================
if (FALSE) {
    
    # ── Load packages ─────────────────────────────────────────────────────────
    library(tidyverse)
    library(WeightIt)
    library(cobalt)
    library(geepack)
    library(mice)
    library(broom)
    library(cli)
    
    # ── Define outcomes ───────────────────────────────────────────────────────
    outcomes <- c("CDR_score", "SCD_yn", "MMSE")
    
    # ── Build grid ────────────────────────────────────────────────────────────
    grid <- create_msm_grid(
        outcomes             = outcomes,
        exposure_definitions = msm_exposure_definitions,
        datasets             = c("cc", "mice"),
        cov_sets             = c("full"),
        truncation_quantiles = 0.995
    )
    
    # ── Run all models ────────────────────────────────────────────────────────
    results <- run_msm_models(
        model_grid      = grid,
        cc_data         = my_cc_data,      # complete-case data frame
        mice_data       = my_mids_object,  # mice mids object or long data frame
        ltfu_var        = "ltfu",
        ltfu_predictors = .LTFU_PREDICTORS,
        id_var          = "pt"
    )
    
    # ── Export ────────────────────────────────────────────────────────────────
    paths <- export_msm_results(results)
    
    # ── Diagnostics (complete-case only) ──────────────────────────────────────
    diag_results <- purrr::map(results, run_msm_diagnostics)
}