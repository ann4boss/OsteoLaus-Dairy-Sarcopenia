# =============================================================================
# R/10_model_alm_dairy.R  (revised)
# =============================================================================
# Mixed-effects longitudinal model: appendicular lean mass index ~ dairy intake.
#
# Research question
# -----------------
# Is higher cumulative dairy intake (dairy_cumavg, g/day) associated with
# better appendicular lean mass index (ALM_HT2, kg/m²) over time in OsteoLaus
# women?
#
# Key changes from previous version
# ----------------------------------
# 1. fit_alm_model() accepts `tier`, `exposure`, and `add_interaction`,
#    enabling the M0–M3 covariate progression, three exposure-metric
#    sensitivities, and a secondary trajectory-modification model — all
#    without duplicating code or rebuilding the dataset.
#
# 2. `add_interaction = FALSE` by default. The primary hypothesis is the
#    average association of dairy with ALMI across follow-up. Set
#    add_interaction = TRUE for the supplementary trajectory analysis.
#
# 3. Covariate tiers are drawn from .COVARIATE_TIERS$alm (model_specs.R).
#    BMI is excluded from all tiers; Height is used instead.
#    Rationale: ALMI = ALM / height². BMI = weight / height². Lean mass is
#    the dominant component of body weight, so BMI and ALMI share both a
#    common denominator and a correlated numerator. Including BMI inflates
#    VIF for both terms and can produce sign reversal. Height is the
#    orthogonal anthropometric adjustment.
#
# 4. A make_alm_stability_table() function is added (mirrors the grip version)
#    to display the dairy beta across M0–M3 in one supplementary tibble.
#
# Outputs (one set per target in _targets.R)
# ------------------------------------------
#   alm_model_data              model-ready tibble (built once; shared across tiers)
#   alm_model_fit_M3            lmerMod list, primary model (M3, no interaction)
#   alm_model_fit_M0/M1/M2      lmerMod lists, covariate progression
#   alm_stability_table         dairy beta across M0–M3
#   alm_model_fit_S1            lmerMod list, lag-1 exposure (M3)
#   alm_model_fit_S2            lmerMod list, concurrent exposure (M3)
#   alm_model_fit_interaction   lmerMod list, trajectory modification (M3)
#   alm_model_table             gtsummary table (primary M3 model)
#   alm_model_plots             patchwork diagnostics (primary M3 model)
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================


# =============================================================================
# Dataset construction
# =============================================================================

#' Assemble the model-ready dataset for ALMI ~ dairy.
#'
#' Builds the dataset once. All model tiers and sensitivity analyses share
#' this same analytical sample so that coefficient changes across tiers
#' reflect adjustment only, not sample composition differences.
#'
#' Exclusions applied here:
#'   - Rows with missing ALM_HT2 or dairy_cumavg (primary exposure)
#'   - Participants with fewer than 2 non-missing outcome rows
#'
#' dairy_total_lag1 and dairy_total_gday are retained as audit columns for
#' exposure-metric sensitivity analyses.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A tibble ready for fit_alm_model().
build_alm_model_data <- function(analysis_long) {
    
    # All covariates needed across any tier (union of M0–M3)
    all_covs <- unique(unlist(.COVARIATE_TIERS$alm))
    
    model_data <- analysis_long |>
        dplyr::filter(eligible_alm) |>
        dplyr::select(
            pt, osteo_wave, osteo_wave_num, time_since_bsl_yr,
            # Outcome
            ALM_HT2,
            # All three exposure metrics (primary + sensitivity columns)
            dairy_cumavg,
            dairy_total_lag1,
            dairy_total_gday,
            dairy_cumavg_lag1,   # audit only
            # All covariates needed by any tier
            dplyr::any_of(all_covs),
            eligible_alm
        )
    
    n_start <- dplyr::n_distinct(model_data$pt)
    
    # ── Row-level exclusions ──────────────────────────────────────────────────
    # Use dairy_cumavg as the completeness criterion (primary exposure).
    # Rows missing only lag/current dairy are retained — those rows simply
    # cannot contribute to lag/current-exposure sensitivity models.
    model_data <- model_data |>
        dplyr::filter(
            !is.na(ALM_HT2),
            !is.na(dairy_cumavg)
        )
    
    n_after_row <- dplyr::n_distinct(model_data$pt)
    
    # ── Participant-level: need >= 2 valid outcome rows for slope estimation ──
    pts_with_enough <- model_data |>
        dplyr::group_by(pt) |>
        dplyr::summarise(n_valid = dplyr::n(), .groups = "drop") |>
        dplyr::filter(n_valid >= 2L) |>
        dplyr::pull(pt)
    
    model_data <- dplyr::filter(model_data, pt %in% pts_with_enough)
    
    n_final  <- dplyr::n_distinct(model_data$pt)
    n_rows   <- nrow(model_data)
    n_waves  <- dplyr::n_distinct(model_data$osteo_wave)
    mean_obs <- round(n_rows / n_final, 1)
    
    # ── Factor reference levels (shared helper from model_specs.R) ────────────
    model_data <- set_reference_levels(model_data)
    
    # ── Summary ───────────────────────────────────────────────────────────────
    cli::cli_inform(c(
        "i" = "build_alm_model_data() complete:",
        "*" = "Started with {n_start} eligible_alm participants",
        "*" = "{n_start - n_after_row} lost: missing ALM_HT2 or dairy_cumavg",
        "*" = "{n_after_row - n_final} lost: < 2 valid obs",
        "v" = "{n_final} participants | {n_rows} rows | {n_waves} waves | \\
               mean {mean_obs} obs/pt",
        "i" = "All model tiers and exposure-metric sensitivities share this sample."
    ))
    
    # ── Covariate completeness (reported for M3 — most demanding tier) ────────
    report_completeness(model_data,
                        covariates = .COVARIATE_TIERS$alm$M3,
                        n_before   = n_final)
    
    model_data
}


# =============================================================================
# Model fitting
# =============================================================================

#' Fit a linear mixed-effects model for ALMI ~ dairy intake.
#'
#' Parameterised to support four covariate tiers (M0–M3), three exposure
#' metrics, and optional dairy × time interaction. All combinations share
#' the same dataset from build_alm_model_data().
#'
#' BMI is excluded from all tiers — Height is used instead. See file header
#' for the collinearity rationale.
#'
#' @param alm_model_data   Output of build_alm_model_data().
#' @param tier             One of "M0", "M1", "M2", "M3" (default "M3").
#'   M0: crude (dairy + time only).
#'   M1: + Age, Height, energy_kcal.
#'   M2: + protein_pct, pa_levels, sbsmk, alcohol_category.
#'   M3: full pre-specified model (primary).
#' @param exposure         Exposure column name. One of:
#'   "dairy_cumavg"     [DEFAULT — primary analysis]
#'   "dairy_total_lag1" [sensitivity S1 — one-wave lag]
#'   "dairy_total_gday" [sensitivity S2 — concurrent wave]
#' @param add_interaction  Logical. If TRUE, adds exposure × time_since_bsl_yr
#'   (secondary hypothesis: does dairy modify the rate of ALMI decline?).
#'   Default FALSE (primary hypothesis: average association across follow-up).
#' @param random_effects   Random-effects term string. Default
#'   "(1 + time_since_bsl_yr | pt)". Pass "(1 | pt)" if singular.
#' @return A named list:
#'   $fit_reml   lmerMod (REML — for reporting)
#'   $fit_ml     lmerMod (ML   — for LRT / AIC)
#'   $formula    formula used
#'   $tier       tier label
#'   $exposure   exposure column
#'   $add_interaction  logical
fit_alm_model <- function(alm_model_data,
                          tier            = "M3",
                          exposure        = "dairy_cumavg",
                          add_interaction = FALSE,
                          random_effects  = "(1 + time_since_bsl_yr | pt)") {
    
    stopifnot(tier %in% c("M0", "M1", "M2", "M3"))
    stopifnot(exposure %in% .EXPOSURE_OPTIONS)
    stopifnot(exposure %in% names(alm_model_data))
    
    # ── Build formula ─────────────────────────────────────────────────────────
    dairy_term <- if (add_interaction) {
        paste0(exposure, " * time_since_bsl_yr")
    } else {
        paste0(exposure, " + time_since_bsl_yr")
    }
    
    covariates <- .COVARIATE_TIERS$alm[[tier]]
    covariates <- setdiff(covariates, "time_since_bsl_yr")  # already in dairy_term
    covariates <- setdiff(covariates, exposure)             # avoid duplication
    covariates <- intersect(covariates, names(alm_model_data))
    
    rhs <- paste(
        c(dairy_term, covariates, random_effects),
        collapse = " + "
    )
    
    f <- stats::as.formula(paste("ALM_HT2 ~", rhs))
    
    # ── Prepare fitting data ──────────────────────────────────────────────────
    # prepare_fit_data() drops empty factor levels AND removes covariates that
    # are constant in this subsample, then we rebuild rhs and f with the
    # cleaned covariate list so lmer never sees a degenerate predictor.
    prep       <- prepare_fit_data(alm_model_data, covariates,
                                   caller           = glue::glue("fit_alm_model [{tier}]"),
                                   exposure         = exposure,
                                   min_obs_lme      = 2L,
                                   m3_covariates    = .COVARIATE_TIERS$alm$M3,
                                   scale_covariates = TRUE)
    fit_data   <- prep$data
    covariates <- prep$covariates   # degenerate columns removed
    
    # Rebuild formula with cleaned covariates
    rhs <- paste(c(dairy_term, covariates, random_effects), collapse = " + ")
    f   <- stats::as.formula(paste("ALM_HT2 ~", rhs))
    
    cli::cli_inform(c(
        "i" = "fit_alm_model() — tier={tier}, exposure={exposure}, \\
               interaction={add_interaction}",
        "*" = deparse(f)
    ))
    
    # ── Fit ML (for LRT / AIC) ────────────────────────────────────────────────
    fit_ml <- lmerTest::lmer(
        formula = f,
        data    = fit_data,
        REML    = FALSE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    # ── Refit REML (for reported estimates) ───────────────────────────────────
    fit_reml <- lmerTest::lmer(
        formula = f,
        data    = fit_data,
        REML    = TRUE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    # ── Convergence / singularity check ──────────────────────────────────────
    if (lme4::isSingular(fit_reml)) {
        cli::cli_warn(c(
            "!" = "Singular fit (tier={tier}, exposure={exposure}).",
            "i" = "Consider simplifying to random_effects = '(1 | pt)'."
        ))
    }
    
    # ── Summary ───────────────────────────────────────────────────────────────
    n_pt  <- dplyr::n_distinct(fit_data$pt)
    n_obs <- nrow(fit_data)
    vc    <- lme4::VarCorr(fit_reml)
    re_sd <- sqrt(diag(as.matrix(vc$pt)))
    
    cli::cli_inform(c(
        "v" = "fit_alm_model() complete [{tier}, {exposure}]:",
        "*" = "N participants: {n_pt} | N observations: {n_obs}",
        "*" = "Fixed effects: {length(lme4::fixef(fit_reml))}",
        "*" = "Random effects SD — intercept: {round(re_sd[1], 3)}\\
               {if (length(re_sd) > 1) paste0(', slope: ', round(re_sd[2], 3)) else ''}",
        "*" = "Residual SD: {round(sigma(fit_reml), 3)}",
        "*" = "ICC (intercept): \\
               {round(re_sd[1]^2 / (re_sd[1]^2 + sigma(fit_reml)^2), 3)}"
    ))
    
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

#' Dairy coefficient stability across M0–M3 for the ALM model.
#'
#' Extracts the dairy main-effect beta (95% CI, p) from each tier and
#' stacks into a single tibble for a supplementary robustness table.
#'
#' @param fits     Named list of fit objects, e.g. list(M0=..., M1=..., M3=...).
#' @param exposure Column name of the dairy exposure (must match across fits).
#' @return A tibble with one row per tier.
make_alm_stability_table <- function(fits, exposure = "dairy_cumavg") {
    
    purrr::imap_dfr(fits, function(fit_obj, tier_label) {
        
        coef_tbl <- as.data.frame(summary(fit_obj$fit_reml)$coefficients)
        coef_tbl <- tibble::rownames_to_column(coef_tbl, "term")
        
        dairy_row <- dplyr::filter(coef_tbl, term == exposure)
        
        if (nrow(dairy_row) == 0L) {
            cli::cli_warn("Dairy term '{exposure}' not found in tier {tier_label}.")
            return(NULL)
        }
        
        se   <- dairy_row[["Std. Error"]]
        beta <- dairy_row[["Estimate"]]
        pval <- dairy_row[["Pr(>|t|)"]]
        df_s <- dairy_row[["df"]]
        
        tibble::tibble(
            tier             = tier_label,
            beta             = round(beta, 4),
            se               = round(se,   4),
            ci_lo            = round(beta - 1.96 * se, 4),
            ci_hi            = round(beta + 1.96 * se, 4),
            p_value          = pval,
            df_satterthwaite = round(df_s, 1)
        )
    })
}


# =============================================================================
# Model table (gtsummary — primary M3 model)
# =============================================================================

#' Publication-ready table for the primary ALM model (M3, REML fit).
#'
#' @param alm_fit_M3  Fit object from fit_alm_model(tier = "M3").
#' @return A gtsummary tbl_regression object.
make_alm_model_table <- function(alm_fit_M3) {
    
    # Full candidate label list. safe_labels() drops any entry whose term is
    # absent from the fitted model (e.g. the interaction when
    # add_interaction = FALSE, or a covariate removed by prepare_fit_data()).
    all_labels <- list(
        dairy_cumavg                     ~ "Cumulative dairy intake (g/day)",
        time_since_bsl_yr                ~ "Time since Baseline (yr)",
        `dairy_cumavg:time_since_bsl_yr` ~ "Dairy \u00d7 Time interaction",
        Age                              ~ "Age (yr)",
        Height                           ~ "Height (cm)",
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
    
    alm_fit_M3$fit_reml |>
        gtsummary::tbl_regression(
            label      = safe_labels(alm_fit_M3$fit_reml, all_labels),
            pvalue_fun = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_caption(
            "**Table 3.** Linear mixed-effects model: \\
             appendicular lean mass index ~ cumulative dairy intake \\
             (fully adjusted, M3)"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "\u03b2 (95% CI) from lmerTest::lmer() with Satterthwaite \\
                 degrees of freedom. Random effects: random intercept + slope \\
                 per participant. BMI excluded (collinear with ALMI); Height \\
                 included instead. Primary model: no dairy \u00d7 time interaction \\
                 (average association across follow-up). See supplementary \\
                 Table S3 for trajectory-modification analysis (dairy \u00d7 time) \\
                 and Table S5 for M0-M2 covariate progression. Reference: \\
                 education Low; smoking Never; alcohol Non-drinker; \\
                 binary covariates No."
        )
}


# =============================================================================
# Diagnostic plots
# =============================================================================

#' Validation plots for the ALM LME model.
#'
#' Accepts the fit object list returned by fit_alm_model() so it works with
#' any tier. Uses the REML fit for residuals.
#'
#' Four panels (2 × 2 grid):
#'   A  Tukey-Anscombe (residuals vs fitted) — linearity, homoscedasticity
#'   B  Q-Q plot of residuals — normality check
#'   C  Q-Q plot of random intercepts (BLUPs) — random-effects normality
#'   D  Observed vs fitted trajectories (30 random participants)
#'
#' @param alm_fit        Fit object from fit_alm_model().
#' @param alm_model_data Output of build_alm_model_data().
#' @return A patchwork plot object (2 × 2 grid).
make_alm_model_plots <- function(alm_fit, alm_model_data) {
    
    fit <- alm_fit$fit_reml
    
    row_idx <- as.integer(names(stats::residuals(fit)))
    
    resid_df <- tibble::tibble(
        fitted     = stats::fitted(fit),
        residual   = stats::residuals(fit),
        pt         = alm_model_data$pt[row_idx],
        osteo_wave = alm_model_data$osteo_wave[row_idx]
    ) |>
        dplyr::mutate(residual_std = scale(residual)[, 1])
    
    # ── A: Tukey-Anscombe ─────────────────────────────────────────────────────
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
            subtitle = glue::glue(
                "tier={alm_fit$tier} | exposure={alm_fit$exposure}"
            ),
            x = "Fitted values (kg/m\u00b2)",
            y = "Residuals (kg/m\u00b2)"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40")
        )
    
    # ── B: Q-Q plot of residuals ──────────────────────────────────────────────
    p_qq_resid <- ggplot2::ggplot(
        resid_df, ggplot2::aes(sample = residual_std)
    ) +
        ggplot2::stat_qq(alpha = 0.25, size = 0.9, colour = "grey30") +
        ggplot2::stat_qq_line(colour = "tomato", linetype = "dashed",
                              linewidth = 0.6) +
        ggplot2::labs(
            title    = "B: Q-Q plot of residuals",
            subtitle = "Checks normality of residual distribution",
            x        = "Theoretical normal quantiles",
            y        = "Standardised residuals"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40")
        )
    
    # ── C: Q-Q plot of random intercepts ─────────────────────────────────────
    ranef_pt  <- lme4::ranef(fit)$pt
    has_slope <- "time_since_bsl_yr" %in% names(ranef_pt)
    
    ranef_df <- tibble::tibble(
        intercept = ranef_pt[["(Intercept)"]],
        slope     = if (has_slope) ranef_pt[["time_since_bsl_yr"]] else NA_real_
    ) |>
        dplyr::mutate(intercept_std = scale(intercept)[, 1])
    
    p_qq_ranef <- ggplot2::ggplot(
        ranef_df, ggplot2::aes(sample = intercept_std)
    ) +
        ggplot2::stat_qq(alpha = 0.3, size = 0.9, colour = "grey30") +
        ggplot2::stat_qq_line(colour = "tomato", linetype = "dashed",
                              linewidth = 0.6) +
        ggplot2::labs(
            title    = "C: Q-Q plot of random intercepts (BLUPs)",
            subtitle = "Checks normality of random-effects distribution",
            x        = "Theoretical normal quantiles",
            y        = "Standardised random intercepts"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40")
        )
    
    # ── D: Observed vs fitted trajectories ───────────────────────────────────
    set.seed(42L)
    n_show     <- min(30L, dplyr::n_distinct(resid_df$pt))
    sample_pts <- sample(unique(resid_df$pt), n_show)
    
    fitted_lookup <- dplyr::select(resid_df, pt, osteo_wave, fitted)
    
    obs_fit_df <- alm_model_data |>
        dplyr::filter(pt %in% sample_pts) |>
        dplyr::left_join(fitted_lookup, by = c("pt", "osteo_wave"))
    
    p_obsfit <- ggplot2::ggplot(
        obs_fit_df,
        ggplot2::aes(x = time_since_bsl_yr, group = pt)
    ) +
        ggplot2::geom_line(ggplot2::aes(y = ALM_HT2),
                           colour = "grey65", alpha = 0.7, linewidth = 0.35) +
        ggplot2::geom_line(ggplot2::aes(y = fitted),
                           colour = "#2E86AB", alpha = 0.85, linewidth = 0.5) +
        ggplot2::geom_point(ggplot2::aes(y = ALM_HT2),
                            colour = "grey50", size = 0.7, alpha = 0.5) +
        ggplot2::labs(
            title    = "D: Observed (grey) vs fitted (blue) trajectories",
            subtitle = glue::glue("{n_show} randomly selected participants"),
            x        = "Time since Baseline (yr)",
            y        = "ALMI (kg/m\u00b2)"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40")
        )
    
    patchwork::wrap_plots(p_ta, p_qq_resid, p_qq_ranef, p_obsfit, ncol = 2) +
        patchwork::plot_annotation(
            title    = glue::glue(
                "LME diagnostics: ALMI ~ dairy \\
                 ({alm_fit$tier}, exposure={alm_fit$exposure})"
            ),
            subtitle = paste0(
                "A-B: residual checks. ",
                "C: random-effects normality. ",
                "D: trajectory fit."
            ),
            theme = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 13, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
}