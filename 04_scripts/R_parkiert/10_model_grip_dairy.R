# =============================================================================
# R/10_model_grip_dairy.R
# =============================================================================
# Mixed-effects longitudinal model: grip strength ~ dairy intake.
#
# Model formula structure
# -----------------------
#   handgrip_max_all ~
#     <exposure> [* time_since_bsl_yr if add_interaction]
#     + time_since_bsl_yr
#     + <covariates for selected tier>
#     + (1 + time_since_bsl_yr | pt)
#
# Outputs (one set per target in _targets.R)
# ------------------------------------------
#   grip_model_data     model-ready tibble (built once; shared across tiers)
#   grip_model_fit_M3   lmerMod, primary model (M3, no interaction)
#   grip_model_fit_M0   lmerMod, crude model
#   grip_model_fit_M1   lmerMod, minimal adjustment
#   grip_model_fit_M2   lmerMod, lifestyle adjustment
#   grip_model_fit_M3_interaction  lmerMod, secondary trajectory analysis
#   grip_model_fit_S1   lmerMod, lag-1 exposure sensitivity (M3)
#   grip_model_fit_S2   lmerMod, concurrent exposure sensitivity (M3)
#
# =============================================================================


# =============================================================================
# Dataset construction
# =============================================================================

#' Assemble the model-ready dataset for grip strength ~ dairy.
#'
#' Builds the dataset once. All model tiers and sensitivity analyses share
#' this same analytical sample so that coefficient changes across tiers
#' reflect adjustment only, not sample composition differences.
#'
#' Exclusions applied here:
#'   - Rows with missing handgrip_max_all or dairy_cumavg (primary exposure)
#'   - Participants with fewer than 2 non-missing outcome rows
#'
#' Note: dairy_total_lag1 and dairy_total_gday are retained as audit columns
#' for exposure-metric sensitivity analyses.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A tibble ready for fit_grip_model().
build_grip_model_data <- function(analysis_long) {
    
    # All covariates that might be needed across tiers
    all_covs <- unique(unlist(.COVARIATE_TIERS$grip))
    
    model_data <- analysis_long |>
        dplyr::filter(eligible_hgs) |>
        dplyr::select(
            pt, osteo_wave, osteo_wave_num, time_since_bsl_yr,
            handgrip_max_all,
            # All three exposure metrics (primary + sensitivity columns)
            dairy_cumavg,
            dairy_total_lag1,
            dairy_total_gday,
            dairy_cumavg_lag1,   # audit only
            # All covariates needed by any tier
            dplyr::any_of(all_covs),
            eligible_hgs
        )
    
    n_start <- dplyr::n_distinct(model_data$pt)
    
    # ── Row-level exclusions: missing primary outcome or primary exposure ────────
    # Use dairy_cumavg as the completeness criterion (primary exposure).
    # Rows missing only lag/current dairy are retained — those rows simply
    # cannot contribute to lag/current-exposure sensitivity models.
    model_data <- model_data |>
        dplyr::filter(
            !is.na(handgrip_max_all),
            !is.na(dairy_cumavg)
        )
    
    n_after_row <- dplyr::n_distinct(model_data$pt)
    
    # ── Participant-level: need >= 2 valid outcome rows for slope estimation ─────
    pts_with_enough <- model_data |>
        dplyr::group_by(pt) |>
        dplyr::summarise(n_valid = dplyr::n(), .groups = "drop") |>
        dplyr::filter(n_valid >= 2L) |>
        dplyr::pull(pt)
    
    model_data <- dplyr::filter(model_data, pt %in% pts_with_enough)
    
    n_final   <- dplyr::n_distinct(model_data$pt)
    n_rows    <- nrow(model_data)
    n_waves   <- dplyr::n_distinct(model_data$osteo_wave)
    mean_obs  <- round(n_rows / n_final, 1)
    
    # ── Factor reference levels (shared helper from model_specs.R) ───────────────
    model_data <- set_reference_levels(model_data)
    
    # ── Summary ──────────────────────────────────────────────────────────────────
    cli::cli_inform(c(
        "i" = "build_grip_model_data() complete:",
        "*" = "Started with {n_start} eligible_hgs participants",
        "*" = "{n_start - n_after_row} lost: missing handgrip_max_all or dairy_cumavg",
        "*" = "{n_after_row - n_final} lost: < 2 valid obs",
        "v" = "{n_final} participants | {n_rows} rows | {n_waves} waves | \\
           mean {mean_obs} obs/pt",
        "i" = "All model tiers and exposure-metric sensitivities share this sample."
    ))
    
    # ── Covariate completeness (for M3 — the tier with most covariates) ──────────
    report_completeness(model_data,
                        covariates = .COVARIATE_TIERS$grip$M3,
                        n_before   = n_final)
    
    model_data
}


# =============================================================================
# Model fitting
# =============================================================================

#' Fit a linear mixed-effects model for grip strength ~ dairy intake.
#'
#' Parameterised to support four covariate tiers (M0–M3), three exposure
#' metrics, and optional dairy × time interaction. All combinations share
#' the same dataset from build_grip_model_data().
#'
#' @param grip_model_data  Output of build_grip_model_data().
#' @param tier             One of "M0", "M1", "M2", "M3" (default "M3").
#'   M0: crude (dairy + time only).
#'   M1: + Age, BMI, energy_kcal.
#'   M2: + protein_pct, pa_levels, sbsmk, alcohol_category.
#'   M3: full pre-specified model (primary).
#' @param exposure         Exposure column name. One of:
#'   "dairy_cumavg"     [DEFAULT — primary analysis]
#'   "dairy_total_lag1" [sensitivity S1 — one-wave lag]
#'   "dairy_total_gday" [sensitivity S2 — concurrent wave]
#' @param add_interaction  Logical. If TRUE, adds exposure × time_since_bsl_yr
#'   interaction (secondary hypothesis). Default FALSE (primary hypothesis:
#'   average association across follow-up).
#' @param random_effects   Random-effects term string. Default
#'   "(1 + time_since_bsl_yr | pt)". Pass "(1 | pt)" if the full random
#'   slope structure fails to converge.
#' @return A list with elements:
#'   $fit_reml   lmerMod (REML — for reporting coefficients)
#'   $fit_ml     lmerMod (ML — for LRT / AIC model comparison)
#'   $formula    The formula used
#'   $tier       The tier label
#'   $exposure   The exposure column used
#'   $add_interaction  Logical
fit_grip_model <- function(grip_model_data,
                           tier            = "M3",
                           exposure        = "dairy_cumavg",
                           add_interaction = FALSE,
                           random_effects  = "(1 + time_since_bsl_yr | pt)") {
    
    stopifnot(tier %in% c("M0", "M1", "M2", "M3"))
    stopifnot(exposure %in% .EXPOSURE_OPTIONS)
    stopifnot(exposure %in% names(grip_model_data))
    
    # ── Build formula ─────────────────────────────────────────────────────────────
    dairy_term <- if (add_interaction) {
        paste0(exposure, " * time_since_bsl_yr")
    } else {
        # Main effect of dairy + separate time term (time_since_bsl_yr is also in
        # the covariate list for M1-M3, so use union to avoid duplication)
        paste0(exposure, " + time_since_bsl_yr")
    }
    
    covariates <- .COVARIATE_TIERS$grip[[tier]]
    # Remove time_since_bsl_yr from covariates list: it is already in dairy_term
    covariates <- setdiff(covariates, "time_since_bsl_yr")
    # Remove the exposure variable itself if it crept into the covariate list
    covariates <- setdiff(covariates, exposure)
    # Keep only columns present in the dataset
    covariates <- intersect(covariates, names(grip_model_data))
    
    rhs <- paste(
        c(dairy_term, covariates, random_effects),
        collapse = " + "
    )
    
    f <- stats::as.formula(paste("handgrip_max_all ~", rhs))
    
    # ── Prepare fitting data ────────────────────────────────────────────────────
    # prepare_fit_data() drops empty factor levels AND removes covariates that
    # are constant in this subsample, then we rebuild rhs and f with the
    # cleaned covariate list so lmer never sees a degenerate predictor.
    prep       <- prepare_fit_data(grip_model_data, covariates,
                                   caller           = glue::glue("fit_grip_model [{tier}]"),
                                   exposure         = exposure,
                                   min_obs_lme      = 2L,
                                   m3_covariates    = .COVARIATE_TIERS$grip$M3,
                                   scale_covariates = TRUE)
    fit_data   <- prep$data
    covariates <- prep$covariates   # degenerate columns removed
    
    # Rebuild formula with cleaned covariates
    rhs <- paste(c(dairy_term, covariates, random_effects), collapse = " + ")
    f   <- stats::as.formula(paste("handgrip_max_all ~", rhs))
    
    cli::cli_inform(c(
        "i" = "fit_grip_model() — tier={tier}, exposure={exposure}, \\
           interaction={add_interaction}",
        "*" = deparse(f)
    ))
    
    # ── Fit ML first (needed for LRT / AIC comparisons across tiers) ─────────────
    fit_ml <- lmerTest::lmer(
        formula = f,
        data    = fit_data,
        REML    = FALSE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    # ── Refit REML for reported estimates ────────────────────────────────────────
    fit_reml <- lmerTest::lmer(
        formula = f,
        data    = fit_data,
        REML    = TRUE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    # ── Convergence / singularity check ─────────────────────────────────────────
    if (lme4::isSingular(fit_reml)) {
        cli::cli_warn(c(
            "!" = "Singular fit detected (tier={tier}, exposure={exposure}).",
            "i" = "Consider simplifying to random_effects = '(1 | pt)'."
        ))
    }
    
    list(
        fit_reml        = fit_reml,
        fit_ml          = fit_ml,
        formula         = f,
        tier            = tier,
        exposure        = exposure,
        add_interaction = add_interaction
    )
}


# =============================================================================
# Coefficient stability table (across tiers)
# =============================================================================

#' Assemble a coefficient stability table for the dairy term across M0–M3.
#'
#' Extracts the dairy coefficient (beta, 95% CI, p) from each tier model and
#' stacks them into a single tibble. Useful as a supplementary robustness table.
#'
#' @param fits Named list of fit objects from fit_grip_model():
#'   e.g. list(M0 = ..., M1 = ..., M2 = ..., M3 = ...)
#' @param exposure Column name of the dairy exposure (must match across fits).
#' @return A tibble with one row per tier.
make_grip_stability_table <- function(fits, exposure = "dairy_cumavg") {
    
    purrr::imap_dfr(fits, function(fit_obj, tier_label) {
        # Extract fixed-effect summary from REML fit
        coef_tbl <- as.data.frame(summary(fit_obj$fit_reml)$coefficients)
        coef_tbl <- tibble::rownames_to_column(coef_tbl, "term")
        
        # Find the dairy main-effect row (not the interaction)
        dairy_row <- coef_tbl |>
            dplyr::filter(term == exposure)
        
        if (nrow(dairy_row) == 0L) {
            cli::cli_warn("Dairy term '{exposure}' not found in tier {tier_label}.")
            return(NULL)
        }
        
        se    <- dairy_row[["Std. Error"]]
        beta  <- dairy_row[["Estimate"]]
        df_sw <- dairy_row[["df"]]         # Satterthwaite df
        tval  <- dairy_row[["t value"]]
        pval  <- dairy_row[["Pr(>|t|)"]]
        
        tibble::tibble(
            tier        = tier_label,
            beta        = round(beta, 4),
            se          = round(se, 4),
            ci_lo       = round(beta - 1.96 * se, 4),
            ci_hi       = round(beta + 1.96 * se, 4),
            p_value     = pval,
            df_satterthwaite = round(df_sw, 1)
        )
    })
}


# =============================================================================
# Model table (gtsummary — primary M3 model)
# =============================================================================

#' Publication-ready table for the primary grip model (M3, REML fit).
#'
#' @param grip_fit_M3  The fit object from fit_grip_model(tier = "M3").
#' @return A gtsummary tbl_regression object.
make_grip_model_table <- function(grip_fit_M3) {
    
    all_labels <- list(
        dairy_cumavg                     ~ "Cumulative dairy intake (g/day)",
        time_since_bsl_yr                ~ "Time since Baseline (yr)",
        `dairy_cumavg:time_since_bsl_yr` ~ "Dairy \u00d7 Time interaction",
        Age                              ~ "Age (yr)",
        BMI                              ~ "BMI (kg/m\u00b2)",
        education_level                  ~ "Education level",
        sbsmk                            ~ "Smoking status",
        alcohol_category                 ~ "Alcohol intake",
        pa_levels                        ~ "Physical activity",
        diabetes_status                  ~ "Diabetes",
        HTN_status                       ~ "Hypertension",
        hrt_status                       ~ "HRT",
        corticoids_status                ~ "Systemic corticosteroids",
        calcium_status                   ~ "Calcium supplement",
        vitD_status                      ~ "Vitamin D supplement",
        bisphosphonate_status            ~ "Bisphosphonate use",
        energy_kcal                      ~ "Total energy (kcal/day)",
        protein_pct                      ~ "Protein (% of energy)"
    )
    
    grip_fit_M3$fit_reml |>
        gtsummary::tbl_regression(
            label      = safe_labels(grip_fit_M3$fit_reml, all_labels),
            pvalue_fun = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_caption(
            "**Table 2.** Linear mixed-effects model: grip strength ~ \\
       cumulative dairy intake (fully adjusted, M3)"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "\u03b2 (95% CI) from lmerTest::lmer() with Satterthwaite degrees of \\
         freedom. Random effects: random intercept + slope per participant. \\
         Primary model: no dairy \u00d7 time interaction (average association \\
         across follow-up). See supplementary Table S2 for trajectory-modification \\
         analysis (dairy \u00d7 time). Reference: education Low; smoking Never; \\
         alcohol Non-drinker; binary covariates No."
        )
}


# =============================================================================
# Diagnostic plots  (unchanged from previous version)
# =============================================================================

#' Validation plots for the grip LME model.
#'
#' Accepts the fit object list returned by fit_grip_model() so it works with
#' any tier. Uses the REML fit for residuals.
#'
#' @param grip_fit       Fit object from fit_grip_model().
#' @param grip_model_data Output of build_grip_model_data().
#' @return A patchwork plot object (2 × 2 grid).
make_grip_model_plots <- function(grip_fit, grip_model_data) {
    
    fit <- grip_fit$fit_reml
    
    row_idx  <- as.integer(names(stats::residuals(fit)))
    
    resid_df <- tibble::tibble(
        fitted     = stats::fitted(fit),
        residual   = stats::residuals(fit),
        pt         = grip_model_data$pt[row_idx],
        osteo_wave = grip_model_data$osteo_wave[row_idx]
    ) |>
        dplyr::mutate(residual_std = scale(residual)[, 1])
    
    p_ta <- ggplot2::ggplot(
        resid_df, ggplot2::aes(x = fitted, y = residual)
    ) +
        ggplot2::geom_point(alpha = 0.25, size = 0.9, colour = "grey30") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "tomato", linewidth = 0.6) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                             se = TRUE, colour = "#2E86AB",
                             fill = "#2E86AB", alpha = 0.15,
                             linewidth = 0.7) +
        ggplot2::labs(
            title    = "A: Tukey-Anscombe plot",
            subtitle = glue::glue("tier={grip_fit$tier} | exposure={grip_fit$exposure}"),
            x        = "Fitted (kg)", y = "Residuals (kg)"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    
    p_qq_resid <- ggplot2::ggplot(
        resid_df, ggplot2::aes(sample = residual_std)
    ) +
        ggplot2::stat_qq(alpha = 0.25, size = 0.9, colour = "grey30") +
        ggplot2::stat_qq_line(colour = "tomato", linetype = "dashed",
                              linewidth = 0.6) +
        ggplot2::labs(
            title    = "B: Q-Q residuals",
            subtitle = "Normality check",
            x        = "Theoretical quantiles", y = "Standardised residuals"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    
    ranef_pt  <- lme4::ranef(fit)$pt
    has_slope <- "time_since_bsl_yr" %in% names(ranef_pt)
    
    ranef_df <- tibble::tibble(
        intercept     = ranef_pt[["(Intercept)"]],
        slope         = if (has_slope) ranef_pt[["time_since_bsl_yr"]] else NA_real_
    ) |>
        dplyr::mutate(intercept_std = scale(intercept)[, 1])
    
    p_qq_ranef <- ggplot2::ggplot(
        ranef_df, ggplot2::aes(sample = intercept_std)
    ) +
        ggplot2::stat_qq(alpha = 0.3, size = 0.9, colour = "grey30") +
        ggplot2::stat_qq_line(colour = "tomato", linetype = "dashed",
                              linewidth = 0.6) +
        ggplot2::labs(
            title    = "C: Q-Q random intercepts (BLUPs)",
            subtitle = "Random-effects normality",
            x        = "Theoretical quantiles", y = "Standardised intercepts"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    
    set.seed(42L)
    n_show     <- min(30L, dplyr::n_distinct(resid_df$pt))
    sample_pts <- sample(unique(resid_df$pt), n_show)
    fitted_lookup <- dplyr::select(resid_df, pt, osteo_wave, fitted)
    
    obs_fit_df <- grip_model_data |>
        dplyr::filter(pt %in% sample_pts) |>
        dplyr::left_join(fitted_lookup, by = c("pt", "osteo_wave"))
    
    p_obsfit <- ggplot2::ggplot(
        obs_fit_df, ggplot2::aes(x = time_since_bsl_yr, group = pt)
    ) +
        ggplot2::geom_line(ggplot2::aes(y = handgrip_max_all),
                           colour = "grey65", alpha = 0.7, linewidth = 0.35) +
        ggplot2::geom_line(ggplot2::aes(y = fitted),
                           colour = "#2E86AB", alpha = 0.85, linewidth = 0.5) +
        ggplot2::geom_point(ggplot2::aes(y = handgrip_max_all),
                            colour = "grey50", size = 0.7, alpha = 0.5) +
        ggplot2::labs(
            title    = "D: Observed (grey) vs fitted (blue) trajectories",
            subtitle = glue::glue("{n_show} randomly selected participants"),
            x        = "Time since Baseline (yr)", y = "Grip strength (kg)"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    
    patchwork::wrap_plots(p_ta, p_qq_resid, p_qq_ranef, p_obsfit, ncol = 2) +
        patchwork::plot_annotation(
            title    = glue::glue(
                "LME diagnostics: grip strength ~ dairy ({grip_fit$tier}, \\
         exposure={grip_fit$exposure})"
            ),
            subtitle = "A-B: residual checks. C: random-effects. D: trajectory fit.",
            theme    = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 13, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
}