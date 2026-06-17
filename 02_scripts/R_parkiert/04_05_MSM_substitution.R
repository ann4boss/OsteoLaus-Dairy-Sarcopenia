# =============================================================================
# MARGINAL STRUCTURAL MODEL — SUBSTITUTION EFFECTS PIPELINE
# -----------------------------------------------------------------------------
# Estimates the Average Causal Substitution Effect (ACSE): the effect of
# replacing a fixed amount of one food (the "substitute") with the same
# amount of a focal food, holding total dietary intake constant.
#
# Key structural differences from the additive-effects pipeline
# ─────────────────────────────────────────────────────────────
# • Exposures are defined as *pairs*  (focal food, substitute food).
# • The exposure weight model conditions on the *substitute* food so that
#   the generalised propensity score captures the isocaloric contrast.
# • The MSM outcome formula includes *both* foods:
#       outcome ~ focal_food + substitute_food
#   The β on focal_food then equals the effect of swapping one unit of
#   substitute_food for one unit of focal_food (Willett / Katan method).
# • Covariate balance is verified for both members of the pair.
#
# Pipeline
#   1.  Define substitution pairs
#   2.  Estimate exposure weights  (confounding + substitution structure)
#   3.  Estimate censoring weights (informative dropout)
#   4.  Multiply weights
#   5.  Truncate extreme weights
#   6.  Check covariate balance
#   7.  Fit weighted GEE / MSM    (isocaloric substitution effect)
#   8.  Export results
#   9.  Diagnostics
#
# Required packages
#   WeightIt, cobalt, geepack, mice, broom, tidyverse, cli, ggplot2, purrr
# =============================================================================


# =============================================================================
# 1.  SUBSTITUTION PAIR DEFINITIONS
# =============================================================================
# Each row defines one isocaloric contrast:
#   focal_food   – the food whose intake is *increased*
#   substitute   – the food whose intake is *decreased* by the same amount
#   exposure_type – "linear" (continuous g/day) or "categorical" (quartiles)
#   ref_level    – reference category for categorical exposures (NA if linear)
#   units        – interpretive label for one unit of swap

msm_substitution_pairs <- tibble::tribble(
    ~focal_food,           ~substitute,           ~exposure_type,  ~ref_level, ~units,
    
    # ── Dairy subtype vs its direct complement ───────────────────────────────
    "dairy_ferm_gday",     "dairy_nonferm_gday",  "linear",        NA,         "100 g/day",
    "dairy_nonferm_gday",  "dairy_ferm_gday",     "linear",        NA,         "100 g/day",
    
    "dairy_fullfat_gday",  "dairy_nonfat_gday",   "linear",        NA,         "100 g/day",
    "dairy_nonfat_gday",   "dairy_fullfat_gday",  "linear",        NA,         "100 g/day",
    
    "dairy_sugary_gday",   "dairy_nonsugary_gday","linear",        NA,         "100 g/day",
    "dairy_nonsugary_gday","dairy_sugary_gday",   "linear",        NA,         "100 g/day",
    
    # ── Total dairy vs major non-dairy food groups ───────────────────────────
    "dairy_total_gday",    "animal_protein_gday", "linear",        NA,         "100 g/day",
    "dairy_total_gday",    "plant_protein_gday",  "linear",        NA,         "100 g/day",
    "dairy_total_gday",    "grains_gday",         "linear",        NA,         "100 g/day",
    "dairy_total_gday",    "fats_gday",           "linear",        NA,         "100 g/day",
    
    # ── Categorical version of fermented substitution ─────────────────────────
    "dairy_ferm_quartile", "dairy_nonferm_gday",  "categorical",   "Q1",       "quartile"
)


# =============================================================================
# 2.  COVARIATE SETS
# =============================================================================

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

.SOCIO_CLINICAL <- c(
    "age_cat", "sex", "edu",
    "sm_b",       # smoking
    "pa_b",       # physical activity
    "bmi_cat",
    "HTA_b",      # hypertension
    "depre_b",    # depression
    "cvevent_b",  # cardiovascular event
    "diab_b",     # diabetes
    "famincome_b",
    "occ_b"
)

.LTFU_PREDICTORS <- c(
    "age_cat", "sex", "occ_b", "bmi_cat",
    "sm_b", "cvevent_b", "HTA_b", "depre_b",
    "pa_b", "edu", "famincome_b"
)

msm_sub_covariate_sets <- list(
    minimal   = .SOCIO_CLINICAL,
    full      = c(.SOCIO_CLINICAL, .FOOD_GROUPS),
    food_only = .FOOD_GROUPS
)


# =============================================================================
# 3.  MODEL GRID
# =============================================================================

create_substitution_grid <- function(
        outcomes,
        substitution_pairs   = msm_substitution_pairs,
        datasets             = c("cc", "mice"),
        cov_sets             = names(msm_sub_covariate_sets),
        truncation_quantiles = 0.995
) {
    tidyr::crossing(
        outcome              = outcomes,
        substitution_pairs,
        dataset              = datasets,
        cov_set              = cov_sets,
        trunc_q              = truncation_quantiles
    ) |>
        dplyr::mutate(model_id = dplyr::row_number())
}


# =============================================================================
# 4.  HELPERS
# =============================================================================

normalise_sub_config <- function(config) {
    if (is.data.frame(config)) config <- as.list(config[1L, , drop = FALSE])
    config
}

get_sub_val <- function(config, name) {
    v <- config[[name]]
    if (length(v) != 1L)
        stop("Config field must be length-1: ", name, call. = FALSE)
    v
}

get_sub_covariates <- function(config, covariate_sets) {
    cs <- get_sub_val(config, "cov_set")
    cv <- covariate_sets[[cs]]
    if (is.null(cv))
        stop("Unknown covariate set: ", cs,
             ". Available: ", paste(names(covariate_sets), collapse = ", "),
             call. = FALSE)
    cv
}

#' For the substitution weight model we must *not* regress the focal food on
#' itself, and we also exclude the substitute because it is part of the
#' exposure definition — it enters the RHS of the *outcome* model, not the
#' weight model.  All other food-group confounders are retained.
build_exposure_weight_covariates <- function(covariates, focal_food, substitute) {
    setdiff(covariates, c(focal_food, substitute))
}

#' The substitute IS included in the exposure weight model so the GPS
#' captures the isocaloric structure (conditioning on substitute intake).
build_weight_rhs <- function(background_covs, substitute) {
    unique(c(substitute, background_covs))
}

build_rhs <- function(covariates) paste(covariates, collapse = " + ")

truncate_weights <- function(w, quantile_upper = 0.995) {
    thr <- stats::quantile(w, quantile_upper, na.rm = TRUE)
    w[w > thr] <- thr
    w
}

complete_sub_data <- function(data, vars) {
    missing <- setdiff(vars, names(data))
    if (length(missing) > 0)
        stop("Missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
    data |> dplyr::filter(dplyr::if_all(dplyr::all_of(vars), ~ !is.na(.x)))
}

prepare_sub_exposure <- function(df, focal_food, exposure_type, ref_level = NULL) {
    if (!focal_food %in% names(df))
        stop("Missing focal food column: ", focal_food, call. = FALSE)
    if (exposure_type == "categorical") {
        df[[focal_food]] <- droplevels(factor(df[[focal_food]]))
        if (!is.null(ref_level) && !is.na(ref_level)) {
            if (!ref_level %in% levels(df[[focal_food]]))
                stop("Reference level '", ref_level,
                     "' not in '", focal_food, "'.", call. = FALSE)
            df[[focal_food]] <- stats::relevel(
                factor(df[[focal_food]], ordered = FALSE), ref = ref_level
            )
        }
    }
    df
}

`%||%` <- function(x, y) if (!is.null(x)) x else y


# =============================================================================
# 5.  STEP 1 — EXPOSURE WEIGHTS (SUBSTITUTION-AWARE)
# =============================================================================
# The weight model regresses the *focal food* on the *substitute* plus
# background confounders.  Conditioning on the substitute means the GPS
# describes variation in focal-food intake *given* the substitute, which
# identifies the isocaloric contrast.

estimate_substitution_exposure_weights <- function(data,
                                                   focal_food,
                                                   substitute,
                                                   exposure_type,
                                                   background_covariates) {
    
    weight_rhs <- build_weight_rhs(background_covariates, substitute)
    formula    <- stats::as.formula(
        paste(focal_food, "~", build_rhs(weight_rhs))
    )
    
    if (exposure_type == "linear") {
        # Continuous focal food: GPS via kernel density
        wt_obj <- WeightIt::weightit(
            formula    = formula,
            data       = data,
            method     = "ps",
            use.kernel = TRUE,
            estimand   = "ATE"
        )
    } else {
        # Categorical: multinomial logistic PS
        wt_obj <- WeightIt::weightit(
            formula  = formula,
            data     = data,
            method   = "ps",
            estimand = "ATE"
        )
    }
    
    wt_obj
}


# =============================================================================
# 6.  STEP 2 — CENSORING WEIGHTS (IPCW)
# =============================================================================

estimate_sub_censoring_weights <- function(data,
                                           ltfu_var        = "ltfu",
                                           ltfu_predictors = .LTFU_PREDICTORS) {
    
    if (!ltfu_var %in% names(data))
        stop("LTFU indicator '", ltfu_var, "' not found.", call. = FALSE)
    
    formula <- stats::as.formula(
        paste(ltfu_var, "~", build_rhs(ltfu_predictors))
    )
    
    WeightIt::weightit(
        formula    = formula,
        data       = data,
        method     = "ps",
        use.kernel = TRUE,
        estimand   = "ATE"
    )
}


# =============================================================================
# 7.  STEPS 3 & 4 — COMBINE AND TRUNCATE WEIGHTS
# =============================================================================

combine_and_truncate_sub_weights <- function(data,
                                             exp_weights,
                                             cens_weights,
                                             trunc_q = 0.995) {
    
    data$w_exposure  <- exp_weights$weights
    data$w_censoring <- cens_weights$weights
    data$w_msm_raw   <- data$w_exposure * data$w_censoring
    data$w_msm       <- truncate_weights(data$w_msm_raw, trunc_q)
    
    wt_sum <- summary(data$w_msm)
    cli::cli_inform(c(
        "v" = "Weights combined and truncated at {trunc_q} quantile.",
        " " = "Min   : {round(wt_sum['Min.'],    3)}",
        " " = "Median: {round(wt_sum['Median'],  3)}",
        " " = "Mean  : {round(wt_sum['Mean'],    3)}",
        " " = "Max   : {round(wt_sum['Max.'],    3)}"
    ))
    
    data
}


# =============================================================================
# 8.  STEP 5 — COVARIATE BALANCE
# =============================================================================
# For a substitution model we check balance on the focal food *and* report
# balance of the substitute separately so the reviewer can verify that the
# pseudo-population is balanced across both members of the pair.

check_substitution_balance <- function(data,
                                       focal_food,
                                       substitute,
                                       background_covariates,
                                       weight_col  = "w_msm",
                                       threshold   = 0.1,
                                       out_dir     = NULL,
                                       model_id    = NULL) {
    
    # Covariates to check: substitute + background (the weight model RHS)
    balance_covs <- build_weight_rhs(background_covariates, substitute)
    balance_formula <- stats::as.formula(
        paste(focal_food, "~", build_rhs(balance_covs))
    )
    
    bt <- cobalt::bal.tab(
        balance_formula,
        data       = data,
        weights    = data[[weight_col]],
        thresholds = c(cor = threshold),
        un         = TRUE
    )
    
    p_love <- cobalt::love.plot(
        bt,
        threshold = threshold,
        abs       = TRUE,
        var.order = "unadjusted",
        title     = paste0("Balance: ", focal_food, " vs ", substitute),
        colors    = c("Unweighted" = "#E15759", "Weighted" = "#4E79A7")
    )
    
    if (!is.null(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        fname <- paste0("love_", model_id, "_",
                        focal_food, "_sub_", substitute, ".png")
        ggplot2::ggsave(file.path(out_dir, fname), p_love,
                        width = 8, height = 6, dpi = 150)
        cli::cli_inform("i" = "Love plot: {file.path(out_dir, fname)}")
    }
    
    list(balance_table = bt, love_plot = p_love)
}


# =============================================================================
# 9.  STEP 6 — FIT WEIGHTED GEE / SUBSTITUTION MSM
# =============================================================================
# The outcome model includes BOTH the focal food and the substitute.
# The β on focal_food is the effect of replacing one unit of substitute with
# one unit of focal_food, holding the sum (focal + substitute) constant —
# i.e. the isocaloric substitution effect (Katan / Willett partition method).
#
# outcome ~ focal_food + substitute + (no other covariates in MSM)
#
# Marginalisation over confounders is achieved via the IPW; including them
# in the outcome model would induce collider bias in a pure MSM.

build_substitution_formula <- function(outcome,
                                       focal_food,
                                       substitute,
                                       exposure_type) {
    
    focal_term <- if (exposure_type == "categorical") {
        paste0("factor(", focal_food, ")")
    } else {
        focal_food
    }
    
    # Substitute always enters as continuous (g/day)
    stats::as.formula(
        paste(outcome, "~", focal_term, "+", substitute)
    )
}


fit_substitution_gee <- function(data,
                                 outcome,
                                 focal_food,
                                 substitute,
                                 exposure_type,
                                 id_var     = "pt",
                                 weight_col = "w_msm",
                                 corstr     = "independence") {
    
    formula <- build_substitution_formula(
        outcome, focal_food, substitute, exposure_type
    )
    
    model <- geepack::geeglm(
        formula = formula,
        data    = data,
        weights = data[[weight_col]],
        id      = data[[id_var]],
        corstr  = corstr
    )
    
    tidy <- broom::tidy(model, conf.int = TRUE)
    
    # Flag which term is the focal substitution effect
    focal_pattern <- if (exposure_type == "categorical") {
        paste0("factor\\(", focal_food, "\\)")
    } else {
        focal_food
    }
    tidy <- tidy |>
        dplyr::mutate(
            is_substitution_effect = grepl(focal_pattern, term)
        )
    
    list(model = model, tidy = tidy, formula = formula)
}


# =============================================================================
# 10. COMPLETE-CASE WRAPPER
# =============================================================================

fit_substitution_complete <- function(data,
                                      config,
                                      covariate_sets  = msm_sub_covariate_sets,
                                      ltfu_var        = "ltfu",
                                      ltfu_predictors = .LTFU_PREDICTORS,
                                      id_var          = "pt",
                                      corstr          = "independence") {
    
    config        <- normalise_sub_config(config)
    outcome       <- get_sub_val(config, "outcome")
    focal_food    <- get_sub_val(config, "focal_food")
    substitute    <- get_sub_val(config, "substitute")
    exposure_type <- get_sub_val(config, "exposure_type")
    ref_level     <- config$ref_level
    trunc_q       <- get_sub_val(config, "trunc_q")
    model_id      <- config$model_id
    covariates    <- get_sub_covariates(config, covariate_sets)
    
    # Confounders that go into the weight model (exclude both foods)
    background_covs <- build_exposure_weight_covariates(
        covariates, focal_food, substitute
    )
    
    # Required variables
    all_vars <- unique(c(
        outcome, focal_food, substitute,
        background_covs, ltfu_var, ltfu_predictors, id_var
    ))
    
    data <- complete_sub_data(data, all_vars)
    data <- prepare_sub_exposure(data, focal_food, exposure_type, ref_level)
    
    cli::cli_h2(paste0(
        "SUB-MSM [{model_id}] | ",
        focal_food, " \u2194 ", substitute,
        " (\u2018", exposure_type, "\u2019) \u2192 ", outcome
    ))
    cli::cli_inform("i" = "n = {nrow(data)} complete-case observations")
    
    # ── Step 1: Exposure weights ──────────────────────────────────────────────
    cli::cli_inform(">" = "Step 1: Exposure weights (substitution-aware) ...")
    exp_wt <- estimate_substitution_exposure_weights(
        data                  = data,
        focal_food            = focal_food,
        substitute            = substitute,
        exposure_type         = exposure_type,
        background_covariates = background_covs
    )
    
    # ── Step 2: Censoring weights ─────────────────────────────────────────────
    cli::cli_inform(">" = "Step 2: Censoring (IPCW) weights ...")
    cens_wt <- estimate_sub_censoring_weights(data, ltfu_var, ltfu_predictors)
    
    # ── Steps 3 & 4: Combine and truncate ────────────────────────────────────
    cli::cli_inform(">" = "Steps 3-4: Combining and truncating weights ...")
    data <- combine_and_truncate_sub_weights(data, exp_wt, cens_wt, trunc_q)
    
    # ── Step 5: Balance ───────────────────────────────────────────────────────
    cli::cli_inform(">" = "Step 5: Covariate balance ...")
    balance <- check_substitution_balance(
        data                  = data,
        focal_food            = focal_food,
        substitute            = substitute,
        background_covariates = background_covs,
        weight_col            = "w_msm",
        model_id              = model_id
        # out_dir appended later by run_substitution_model()
    )
    
    # ── Step 6: Weighted GEE ──────────────────────────────────────────────────
    cli::cli_inform(">" = "Step 6: Fitting substitution GEE (MSM) ...")
    gee_result <- fit_substitution_gee(
        data          = data,
        outcome       = outcome,
        focal_food    = focal_food,
        substitute    = substitute,
        exposure_type = exposure_type,
        id_var        = id_var,
        weight_col    = "w_msm",
        corstr        = corstr
    )
    
    list(
        model          = gee_result$model,
        tidy           = gee_result$tidy,
        formula        = gee_result$formula,
        balance        = balance,
        weight_summary = summary(data$w_msm),
        data           = data,
        config         = config
    )
}


# =============================================================================
# 11. MICE WRAPPER
# =============================================================================

fit_substitution_mice <- function(mids_object,
                                  config,
                                  covariate_sets  = msm_sub_covariate_sets,
                                  ltfu_var        = "ltfu",
                                  ltfu_predictors = .LTFU_PREDICTORS,
                                  id_var          = "pt",
                                  corstr          = "independence") {
    
    config        <- normalise_sub_config(config)
    outcome       <- get_sub_val(config, "outcome")
    focal_food    <- get_sub_val(config, "focal_food")
    substitute    <- get_sub_val(config, "substitute")
    exposure_type <- get_sub_val(config, "exposure_type")
    ref_level     <- config$ref_level
    trunc_q       <- get_sub_val(config, "trunc_q")
    model_id      <- config$model_id
    covariates    <- get_sub_covariates(config, covariate_sets)
    background_covs <- build_exposure_weight_covariates(
        covariates, focal_food, substitute
    )
    
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
    
    cli::cli_h2(paste0(
        "SUB-MSM-MICE [{model_id}] | ",
        focal_food, " \u2194 ", substitute,
        " \u2192 ", outcome, " | m = ", m
    ))
    
    for (i in seq_len(m)) {
        cli::cli_inform(">" = "Imputation {i}/{m}")
        df <- imp_list[[i]]
        
        all_vars <- unique(c(
            outcome, focal_food, substitute,
            background_covs, ltfu_var, ltfu_predictors, id_var
        ))
        df <- complete_sub_data(df, all_vars)
        df <- prepare_sub_exposure(df, focal_food, exposure_type, ref_level)
        
        exp_wt  <- estimate_substitution_exposure_weights(
            df, focal_food, substitute, exposure_type, background_covs
        )
        cens_wt <- estimate_sub_censoring_weights(df, ltfu_var, ltfu_predictors)
        df      <- combine_and_truncate_sub_weights(df, exp_wt, cens_wt, trunc_q)
        
        gee_res    <- fit_substitution_gee(
            df, outcome, focal_food, substitute, exposure_type,
            id_var, "w_msm", corstr
        )
        models[[i]] <- gee_res$model
    }
    
    pooled      <- mice::pool(mice::as.mira(models))
    pooled_tidy <- summary(pooled, conf.int = TRUE) |> tibble::as_tibble()
    
    # Re-attach the substitution-effect flag (pooling strips it)
    focal_pattern <- if (exposure_type == "categorical") {
        paste0("factor(", focal_food, ")")
    } else {
        focal_food
    }
    pooled_tidy <- pooled_tidy |>
        dplyr::mutate(
            is_substitution_effect = grepl(focal_pattern, term)
        )
    
    list(
        models      = models,
        pooled      = pooled,
        tidy        = pooled_tidy,
        formula     = build_substitution_formula(
            outcome, focal_food, substitute, exposure_type
        ),
        config      = config
    )
}


# =============================================================================
# 12. MAIN WRAPPER
# =============================================================================

run_substitution_model <- function(config,
                                   cc_data,
                                   mice_data,
                                   covariate_sets  = msm_sub_covariate_sets,
                                   ltfu_var        = "ltfu",
                                   ltfu_predictors = .LTFU_PREDICTORS,
                                   id_var          = "pt",
                                   corstr          = "independence") {
    
    config    <- normalise_sub_config(config)
    timestamp <- format(Sys.time(), "%d%m%Y_%H%M")
    
    config_tag <- paste(
        get_sub_val(config, "dataset"),
        get_sub_val(config, "outcome"),
        get_sub_val(config, "focal_food"),
        "vs",
        get_sub_val(config, "substitute"),
        get_sub_val(config, "exposure_type"),
        get_sub_val(config, "cov_set"),
        paste0("trunc", get_sub_val(config, "trunc_q")),
        sep = "_"
    )
    
    out_dir <- file.path("03_outputs", "MSM_substitution", timestamp, config_tag)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    tryCatch(
        {
            result <- if (get_sub_val(config, "dataset") == "cc") {
                fit_substitution_complete(
                    data            = cc_data,
                    config          = config,
                    covariate_sets  = covariate_sets,
                    ltfu_var        = ltfu_var,
                    ltfu_predictors = ltfu_predictors,
                    id_var          = id_var,
                    corstr          = corstr
                )
            } else {
                fit_substitution_mice(
                    mids_object     = mice_data,
                    config          = config,
                    covariate_sets  = covariate_sets,
                    ltfu_var        = ltfu_var,
                    ltfu_predictors = ltfu_predictors,
                    id_var          = id_var,
                    corstr          = corstr
                )
            }
            
            # Save Love plot now that out_dir is known
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
                tidy       = tibble::tibble(status  = "error",
                                            message = conditionMessage(e)),
                formula    = NA,
                config     = config,
                out_dir    = out_dir,
                config_tag = config_tag
            )
        }
    )
}


run_substitution_models <- function(model_grid,
                                    cc_data,
                                    mice_data,
                                    covariate_sets  = msm_sub_covariate_sets,
                                    ltfu_var        = "ltfu",
                                    ltfu_predictors = .LTFU_PREDICTORS,
                                    id_var          = "pt",
                                    corstr          = "independence") {
    purrr::pmap(
        model_grid,
        function(...) {
            run_substitution_model(
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
# 13. EXPORT
# =============================================================================

export_substitution_results <- function(result) {
    if (is.list(result) && !is.null(result$tidy) &&
        !is.list(result$tidy[[1]])) {
        return(export_one_substitution_result(result))
    }
    unlist(lapply(result, export_one_substitution_result), use.names = FALSE)
}

export_one_substitution_result <- function(result) {
    
    cfg     <- normalise_sub_config(result$config)
    out_dir <- result$out_dir %||%
        file.path("03_outputs", "MSM_substitution",
                  format(Sys.time(), "%d%m%Y_%H%M"), "fallback")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    filename <- paste(
        get_sub_val(cfg, "model_id"),
        get_sub_val(cfg, "dataset"),
        get_sub_val(cfg, "outcome"),
        get_sub_val(cfg, "focal_food"),
        "vs",
        get_sub_val(cfg, "substitute"),
        get_sub_val(cfg, "exposure_type"),
        get_sub_val(cfg, "cov_set"),
        sep = "_"
    )
    
    # Main tidy results
    tidy_file <- file.path(out_dir, paste0(filename, "_tidy.csv"))
    readr::write_csv(result$tidy, tidy_file)
    
    # Substitution-effect rows only (convenience extract)
    if ("is_substitution_effect" %in% names(result$tidy)) {
        sub_effect_file <- file.path(out_dir, paste0(filename, "_subeffect.csv"))
        result$tidy |>
            dplyr::filter(is_substitution_effect) |>
            readr::write_csv(sub_effect_file)
    }
    
    # Balance table
    if (!is.null(result$balance$balance_table)) {
        bt_df    <- cobalt::tidy(result$balance$balance_table)
        bt_file  <- file.path(out_dir, paste0(filename, "_balance.csv"))
        readr::write_csv(bt_df, bt_file)
    }
    
    tidy_file
}


# =============================================================================
# 14. DIAGNOSTICS
# =============================================================================

run_substitution_diagnostics <- function(result) {
    
    cfg <- result$config
    
    if (is.null(result$data))
        return(list(model_id = cfg$model_id, status = "skipped_no_data"))
    
    diag_dir <- file.path(result$out_dir, "diagnostics")
    dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
    
    data     <- result$data
    base     <- paste0("model_", cfg$model_id, "_", cfg$dataset)
    focal    <- cfg$focal_food
    sub      <- cfg$substitute
    
    # ── 1. Weight distribution ────────────────────────────────────────────────
    p_wt <- ggplot2::ggplot(data, ggplot2::aes(x = w_msm)) +
        ggplot2::geom_histogram(bins = 60, fill = "#4E79A7", colour = "white") +
        ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                            colour = "grey40") +
        ggplot2::labs(
            title    = paste0("MSM weight distribution\n",
                              focal, " \u2194 ", sub),
            subtitle = paste("Truncated at", cfg$trunc_q, "quantile"),
            x = "Weight", y = "Count"
        ) +
        ggplot2::theme_minimal()
    
    ggplot2::ggsave(file.path(diag_dir, paste0(base, "_weights.png")),
                    p_wt, width = 7, height = 4, dpi = 150)
    
    # ── 2. Raw vs truncated scatter ───────────────────────────────────────────
    p_trunc <- ggplot2::ggplot(data,
                               ggplot2::aes(x = w_msm_raw, y = w_msm)) +
        ggplot2::geom_point(alpha = 0.3, size = 0.8) +
        ggplot2::geom_abline(slope = 1, intercept = 0,
                             linetype = "dashed", colour = "firebrick") +
        ggplot2::labs(
            title = "Raw vs truncated weights",
            x = "Raw weight", y = "Truncated weight"
        ) +
        ggplot2::theme_minimal()
    
    ggplot2::ggsave(file.path(diag_dir, paste0(base, "_trunc_scatter.png")),
                    p_trunc, width = 6, height = 5, dpi = 150)
    
    # ── 3. Focal food vs substitute scatter (observed data) ───────────────────
    # Useful sanity check: if the two are perfectly collinear the substitution
    # contrast is not identified.
    p_pair <- ggplot2::ggplot(data,
                              ggplot2::aes(x = .data[[sub]],
                                           y = .data[[focal]])) +
        ggplot2::geom_point(alpha = 0.2, size = 0.7) +
        ggplot2::geom_smooth(method = "loess", se = TRUE,
                             colour = "#E15759") +
        ggplot2::labs(
            title = "Focal food vs substitute (observed)",
            x     = sub,
            y     = focal
        ) +
        ggplot2::theme_minimal()
    
    ggplot2::ggsave(file.path(diag_dir, paste0(base, "_pair_scatter.png")),
                    p_pair, width = 6, height = 5, dpi = 150)
    
    list(model_id = cfg$model_id, status = "done", out_dir = diag_dir)
}


# =============================================================================
# 15. EXAMPLE USAGE
# =============================================================================
if (FALSE) {
    
    library(tidyverse)
    library(WeightIt)
    library(cobalt)
    library(geepack)
    library(mice)
    library(broom)
    library(cli)
    library(ggplot2)
    
    # ── Outcomes ──────────────────────────────────────────────────────────────
    outcomes <- c("CDR_score", "SCD_yn", "MMSE")
    
    # ── Build grid ────────────────────────────────────────────────────────────
    grid <- create_substitution_grid(
        outcomes             = outcomes,
        substitution_pairs   = msm_substitution_pairs,
        datasets             = c("cc", "mice"),
        cov_sets             = "full",
        truncation_quantiles = 0.995
    )
    
    # ── Run all models ────────────────────────────────────────────────────────
    results <- run_substitution_models(
        model_grid      = grid,
        cc_data         = my_cc_data,
        mice_data       = my_mids_object,
        ltfu_var        = "ltfu",
        ltfu_predictors = .LTFU_PREDICTORS,
        id_var          = "pt"
    )
    
    # ── Export ────────────────────────────────────────────────────────────────
    paths <- export_substitution_results(results)
    
    # ── Diagnostics ───────────────────────────────────────────────────────────
    diag_results <- purrr::map(results, run_substitution_diagnostics)
    
    # ── Quick view of substitution effects across models ──────────────────────
    substitution_effects_summary <- purrr::map_dfr(results, function(r) {
        r$tidy |>
            dplyr::filter(is_substitution_effect) |>
            dplyr::mutate(
                focal_food = r$config$focal_food,
                substitute = r$config$substitute,
                outcome    = r$config$outcome,
                dataset    = r$config$dataset,
                cov_set    = r$config$cov_set,
                model_id   = r$config$model_id
            )
    })
    
    print(substitution_effects_summary)
}