# =============================================================================
# R/10_model_gait_dairy.R  (revised)
# =============================================================================
# Mixed-effects longitudinal model: gait speed ~ cumulative dairy intake.
#
# Research question
# -----------------
# Is higher cumulative dairy intake (dairy_cumavg, g/day) associated with
# faster gait speed (m/s) at V4 and/or V5 in OsteoLaus women?
#
# Key changes from previous version
# ----------------------------------
# 1. fit_gait_model() accepts `tier`, `exposure`, and `add_interaction`,
#    enabling the M0–M3 covariate progression and exposure-metric
#    sensitivities — same pattern as the grip and ALM models.
#
# 2. `add_interaction = FALSE` by default. The primary hypothesis is the
#    average association of dairy with gait speed across V4–V5. Set
#    add_interaction = TRUE for the supplementary trajectory analysis.
#
# 3. Covariate tiers are drawn from .COVARIATE_TIERS$grip (model_specs.R).
#    The gait model shares the grip covariate list (BMI included, Height not
#    needed) because gait speed is a functional performance measure, not a
#    body composition index.
#
# 4. A make_gait_stability_table() function is added for the supplementary
#    covariate-progression table.
#
# Design constraint: random intercept ONLY  (unchanged from previous version)
# ---------------------------------------------------------------------------
# Gait speed was measured at V4 and V5 only — at most 2 observations per
# participant. With 2 points, a straight line is perfectly determined,
# leaving zero within-person residual variance for a random slope to be
# estimated from. The random-effects structure is therefore (1 | pt).
#
# This means:
#   - The fixed effect of time_since_bsl_yr captures the AVERAGE change
#     from V4 to V5 across all participants.
#   - The dairy × time interaction tests whether higher dairy intake
#     modifies that average change.
#   - Participants with only 1 valid observation are retained: they
#     contribute to fixed-effect estimation even without a slope.
#
# Outputs
# -------
#   gait_model_data              model-ready tibble (built once)
#   gait_model_fit_M3            lmerMod list, primary model
#   gait_model_fit_M0/M1/M2      lmerMod lists, covariate progression
#   gait_stability_table         dairy beta across M0–M3
#   gait_model_fit_S1            lmerMod list, lag-1 exposure (M3)
#   gait_model_fit_S2            lmerMod list, concurrent exposure (M3)
#   gait_model_fit_interaction   lmerMod list, trajectory modification (M3)
#   gait_model_table             gtsummary table (primary M3 model)
#   gait_model_plots             patchwork diagnostics (primary M3 model)
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================


# =============================================================================
# Dataset construction
# =============================================================================

#' Assemble the model-ready dataset for gait speed ~ dairy.
#'
#' Restricts to V4 and V5 (the only waves with gait speed data) among
#' eligible_gait participants. Builds once; all tiers share the same sample.
#'
#' Unlike grip and ALM, participants with only 1 valid observation are
#' retained. The random-intercept model uses them for fixed-effect estimation.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A tibble ready for fit_gait_model().
build_gait_model_data <- function(analysis_long) {
    
    # Gait model shares the grip covariate list (BMI, no Height)
    all_covs <- unique(unlist(.COVARIATE_TIERS$grip))
    
    model_data <- analysis_long |>
        dplyr::filter(
            eligible_gait,
            osteo_wave %in% c("V4", "V5")
        ) |>
        dplyr::select(
            pt, osteo_wave, osteo_wave_num, time_since_bsl_yr,
            # Outcome
            gait_speed,
            # All three exposure metrics
            dairy_cumavg,
            dairy_total_lag1,
            dairy_total_gday,
            dairy_cumavg_lag1,   # audit only
            # All covariates needed by any tier
            dplyr::any_of(all_covs),
            eligible_gait
        ) |>
        dplyr::mutate(
            # Wave indicator: V4 = reference, V5 = follow-up.
            # Used in Panel D of diagnostic plots.
            gait_wave = factor(osteo_wave, levels = c("V4", "V5"))
        )
    
    n_start <- dplyr::n_distinct(model_data$pt)
    
    # ── Row-level exclusions ──────────────────────────────────────────────────
    model_data <- model_data |>
        dplyr::filter(
            !is.na(gait_speed),
            !is.na(dairy_cumavg)
        )
    
    n_final <- dplyr::n_distinct(model_data$pt)
    n_rows  <- nrow(model_data)
    
    # ── Observation count per participant ─────────────────────────────────────
    obs_counts <- model_data |>
        dplyr::count(pt, name = "n_obs")
    
    n_one_obs <- sum(obs_counts$n_obs == 1L)
    n_two_obs <- sum(obs_counts$n_obs == 2L)
    
    # ── Factor reference levels (shared helper from model_specs.R) ────────────
    model_data <- model_data |>
        dplyr::left_join(obs_counts, by = "pt") |>
        set_reference_levels()
    
    # ── Summary ───────────────────────────────────────────────────────────────
    cli::cli_inform(c(
        "i" = "build_gait_model_data() complete:",
        "*" = "Started with {n_start} eligible_gait participants",
        "*" = "{n_start - n_final} lost: missing gait_speed or dairy_cumavg",
        "v" = "{n_final} participants | {n_rows} rows",
        "*" = "  2 observations (V4 + V5): {n_two_obs}",
        "*" = "  1 observation  (V4 or V5 only): {n_one_obs}",
        "i" = "Random intercept only — random slope not estimable (max 2 obs/pt).",
        "i" = "All model tiers and exposure-metric sensitivities share this sample."
    ))
    
    # ── Covariate completeness (M3) ───────────────────────────────────────────
    report_completeness(model_data,
                        covariates = .COVARIATE_TIERS$grip$M3,
                        n_before   = n_final)
    
    model_data
}


# =============================================================================
# Model fitting
# =============================================================================

#' Fit a linear mixed-effects model for gait speed ~ dairy intake.
#'
#' Random intercept ONLY — (1 | pt). A random slope is not estimable because
#' each participant contributes at most 2 observations (V4, V5).
#'
#' @param gait_model_data  Output of build_gait_model_data().
#' @param tier             One of "M0", "M1", "M2", "M3" (default "M3").
#'   Draws from .COVARIATE_TIERS$grip (same covariate list as grip model).
#' @param exposure         Exposure column name. One of:
#'   "dairy_cumavg"     [DEFAULT — primary analysis]
#'   "dairy_total_lag1" [sensitivity S1 — one-wave lag]
#'   "dairy_total_gday" [sensitivity S2 — concurrent wave]
#' @param add_interaction  Logical. If TRUE, adds exposure × time_since_bsl_yr.
#'   Tests whether higher dairy modifies the V4→V5 change in gait speed.
#'   Default FALSE (primary: average association across V4 and V5).
#' @return A named list:
#'   $fit_reml   lmerMod (REML)
#'   $fit_ml     lmerMod (ML)
#'   $formula    formula used
#'   $tier, $exposure, $add_interaction
fit_gait_model <- function(gait_model_data,
                           tier            = "M3",
                           exposure        = "dairy_cumavg",
                           add_interaction = FALSE) {
    
    stopifnot(tier %in% c("M0", "M1", "M2", "M3"))
    stopifnot(exposure %in% .EXPOSURE_OPTIONS)
    stopifnot(exposure %in% names(gait_model_data))
    
    # ── Build formula ─────────────────────────────────────────────────────────
    dairy_term <- if (add_interaction) {
        paste0(exposure, " * time_since_bsl_yr")
    } else {
        paste0(exposure, " + time_since_bsl_yr")
    }
    
    # Gait model uses the grip covariate tiers (both use BMI, not Height)
    covariates <- .COVARIATE_TIERS$grip[[tier]]
    covariates <- setdiff(covariates, "time_since_bsl_yr")  # already in dairy_term
    covariates <- setdiff(covariates, exposure)
    covariates <- intersect(covariates, names(gait_model_data))
    
    # Random intercept only — random slope is not estimable with max 2 obs/pt
    rhs <- paste(
        c(dairy_term, covariates, "(1 | pt)"),
        collapse = " + "
    )
    
    f <- stats::as.formula(paste("gait_speed ~", rhs))
    
    # ── Prepare fitting data ──────────────────────────────────────────────────
    # prepare_fit_data() (model_specs.R) does two things:
    #   1. droplevels() — removes factor levels with zero observations
    #   2. removes covariates that are constant in this subsample
    #      (single-level factor OR numeric constant), then rebuilds the
    #      formula so lmer never sees a degenerate predictor.
    prep      <- prepare_fit_data(gait_model_data, covariates,
                                  caller           = glue::glue("fit_gait_model [{tier}]"),
                                  exposure         = exposure,
                                  min_obs_lme      = 2L,
                                  m3_covariates    = .COVARIATE_TIERS$grip$M3,
                                  scale_covariates = TRUE)
    fit_data  <- prep$data
    covariates <- prep$covariates   # degenerate columns already removed
    
    # Rebuild formula with the cleaned covariate list
    rhs <- paste(c(dairy_term, covariates, "(1 | pt)"), collapse = " + ")
    f   <- stats::as.formula(paste("gait_speed ~", rhs))
    
    cli::cli_inform(c(
        "i" = "fit_gait_model() — tier={tier}, exposure={exposure}, \\
               interaction={add_interaction}",
        "*" = deparse(f)
    ))
    
    # ── Fit ML ────────────────────────────────────────────────────────────────
    fit_ml <- lmerTest::lmer(
        formula = f,
        data    = fit_data,
        REML    = FALSE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    # ── Refit REML ────────────────────────────────────────────────────────────
    fit_reml <- lmerTest::lmer(
        formula = f,
        data    = fit_data,
        REML    = TRUE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    # ── Singularity check ────────────────────────────────────────────────────
    # With random intercept only this is less likely, but between-person
    # variance in gait speed may be small in some subsamples.
    if (lme4::isSingular(fit_reml)) {
        cli::cli_warn(c(
            "!" = "Singular fit (tier={tier}, exposure={exposure}).",
            "i" = "Between-person variance in gait speed is very small. \\
                   Consider OLS as a sensitivity."
        ))
    }
    
    # ── Summary ───────────────────────────────────────────────────────────────
    n_pt  <- dplyr::n_distinct(fit_data$pt)
    n_obs <- nrow(fit_data)
    vc    <- lme4::VarCorr(fit_reml)
    re_sd <- as.data.frame(vc)$sdcor[1]
    icc   <- re_sd^2 / (re_sd^2 + sigma(fit_reml)^2)
    
    cli::cli_inform(c(
        "v" = "fit_gait_model() complete [{tier}, {exposure}]:",
        "*" = "N participants: {n_pt} | N observations: {n_obs}",
        "*" = "Fixed effects: {length(lme4::fixef(fit_reml))}",
        "*" = "Random intercept SD: {round(re_sd, 3)}",
        "*" = "Residual SD: {round(sigma(fit_reml), 3)}",
        "*" = "ICC: {round(icc, 3)}"
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

#' Dairy coefficient stability across M0–M3 for the gait speed model.
#'
#' @param fits     Named list of fit objects, e.g. list(M0=..., M1=..., M3=...).
#' @param exposure Column name of the dairy exposure (must match across fits).
#' @return A tibble with one row per tier.
make_gait_stability_table <- function(fits, exposure = "dairy_cumavg") {
    
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

#' Publication-ready table for the primary gait speed model (M3, REML fit).
#'
#' @param gait_fit_M3  Fit object from fit_gait_model(tier = "M3").
#' @return A gtsummary tbl_regression object.
make_gait_model_table <- function(gait_fit_M3) {
    
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
    
    gait_fit_M3$fit_reml |>
        gtsummary::tbl_regression(
            label      = safe_labels(gait_fit_M3$fit_reml, all_labels),
            pvalue_fun = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_caption(
            "**Table 4.** Linear mixed-effects model: \\
             gait speed ~ cumulative dairy intake (fully adjusted, M3)"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "\u03b2 (95% CI) from lmerTest::lmer() with Satterthwaite \\
                 degrees of freedom. Random intercept only (1 | pt): \\
                 random slope not estimable (max 2 observations per \\
                 participant: V4 and V5). Primary model: no dairy \u00d7 time \\
                 interaction (average association across V4 and V5). See \\
                 supplementary Table S6 for M0-M2 covariate progression. \\
                 Reference: education Low; smoking Never; \\
                 alcohol Non-drinker; binary covariates No."
        )
}


# =============================================================================
# Diagnostic plots
# =============================================================================

#' Validation plots for the gait speed LME model.
#'
#' Accepts the fit object list from fit_gait_model(). Uses the REML fit.
#' Panel D shows V4 and V5 as discrete points rather than a continuous
#' trajectory, which better reflects the two-wave design.
#'
#' Four panels (2 × 2 grid):
#'   A  Tukey-Anscombe — linearity, homoscedasticity
#'   B  Q-Q plot of residuals — normality
#'   C  Q-Q plot of random intercepts (BLUPs) — random-effects normality
#'   D  Observed vs fitted at V4 and V5 (random sample of participants)
#'
#' @param gait_fit        Fit object from fit_gait_model().
#' @param gait_model_data Output of build_gait_model_data().
#' @return A patchwork plot object (2 × 2 grid).
make_gait_model_plots <- function(gait_fit, gait_model_data) {
    
    fit <- gait_fit$fit_reml
    
    row_idx <- as.integer(names(stats::residuals(fit)))
    
    resid_df <- tibble::tibble(
        fitted     = stats::fitted(fit),
        residual   = stats::residuals(fit),
        pt         = gait_model_data$pt[row_idx],
        osteo_wave = gait_model_data$osteo_wave[row_idx]
    ) |>
        dplyr::mutate(residual_std = scale(residual)[, 1])
    
    # ── A: Tukey-Anscombe ─────────────────────────────────────────────────────
    p_ta <- ggplot2::ggplot(
        resid_df, ggplot2::aes(x = fitted, y = residual)
    ) +
        ggplot2::geom_point(alpha = 0.3, size = 0.9, colour = "grey30") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "tomato", linewidth = 0.6) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                             se = TRUE, colour = "#2E86AB",
                             fill = "#2E86AB", alpha = 0.15,
                             linewidth = 0.7) +
        ggplot2::labs(
            title    = "A: Tukey-Anscombe plot",
            subtitle = glue::glue(
                "tier={gait_fit$tier} | exposure={gait_fit$exposure}"
            ),
            x = "Fitted values (m/s)",
            y = "Residuals (m/s)"
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
    # Random intercept only — no slope BLUP for this model
    ranef_pt <- lme4::ranef(fit)$pt
    
    ranef_df <- tibble::tibble(
        intercept = ranef_pt[["(Intercept)"]]
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
    
    # ── D: Observed vs fitted at V4 and V5 ───────────────────────────────────
    # Show paired V4–V5 lines for a random sample. Each participant is one
    # grey line (observed) overlaid with a blue line (fitted). X-axis is
    # discrete (V4 = 0, V5 = 1) to reflect the two-wave design.
    set.seed(42L)
    n_show     <- min(40L, dplyr::n_distinct(resid_df$pt))
    sample_pts <- sample(unique(resid_df$pt), n_show)
    
    fitted_lookup <- dplyr::select(resid_df, pt, osteo_wave, fitted)
    
    obs_fit_df <- gait_model_data |>
        dplyr::filter(pt %in% sample_pts) |>
        dplyr::left_join(fitted_lookup, by = c("pt", "osteo_wave")) |>
        dplyr::mutate(wave_x = dplyr::if_else(osteo_wave == "V4", 0, 1))
    
    p_obsfit <- ggplot2::ggplot(
        obs_fit_df,
        ggplot2::aes(x = wave_x, group = pt)
    ) +
        ggplot2::geom_line(ggplot2::aes(y = gait_speed),
                           colour = "grey65", alpha = 0.6, linewidth = 0.35) +
        ggplot2::geom_line(ggplot2::aes(y = fitted),
                           colour = "#2E86AB", alpha = 0.8, linewidth = 0.5) +
        ggplot2::geom_point(ggplot2::aes(y = gait_speed),
                            colour = "grey50", size = 0.8, alpha = 0.5) +
        ggplot2::scale_x_continuous(
            breaks = c(0, 1),
            labels = c("V4", "V5")
        ) +
        ggplot2::labs(
            title    = "D: Observed (grey) vs fitted (blue) — V4 and V5",
            subtitle = glue::glue("{n_show} randomly selected participants"),
            x        = "Wave",
            y        = "Gait speed (m/s)"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40")
        )
    
    patchwork::wrap_plots(p_ta, p_qq_resid, p_qq_ranef, p_obsfit, ncol = 2) +
        patchwork::plot_annotation(
            title    = glue::glue(
                "LME diagnostics: gait speed ~ dairy \\
                 ({gait_fit$tier}, exposure={gait_fit$exposure})"
            ),
            subtitle = paste0(
                "Random intercept only (1 | pt) — max 2 obs/pt (V4, V5). ",
                "A-B: residual checks. C: random-effects. D: V4-V5 fit."
            ),
            theme = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 13, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
}