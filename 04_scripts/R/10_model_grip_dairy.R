# =============================================================================
# R/10_model_grip_dairy.R
# =============================================================================
# Mixed-effects longitudinal model: grip strength ~ cumulative dairy intake.
#
# Research question
# -----------------
# Is higher cumulative dairy intake (dairy_cumavg, g/day) associated with
# better grip strength (handgrip_max_all, kg) over time in OsteoLaus women,
# after adjustment for known confounders?
#
# Dataset
# -------
# build_grip_model_data() assembles a model-ready long tibble from analysis_long.
# One row per participant x OsteoLaus wave. Participants must be eligible_hgs.
#
# Model
# -----
# fit_grip_model() fits a linear mixed-effects model via lmerTest::lmer(),
#
#   handgrip_max_all ~ dairy_cumavg * time_since_bsl_yr
#                    + time_since_bsl_yr
#                    + Age                          [time-varying]
#                    + BMI                          [time-varying]
#                    + education_level              [fixed, from participants]
#                    + sbsmk                        [time-varying]
#                    + alcohol_category             [time-varying]
#                    + pa_levels                    [time-varying]
#                    + diabetes_status              [time-varying]
#                    + HTN_status                   [time-varying]
#                    + hrt_status                   [time-varying]
#                    + corticoids_status            [time-varying]
#                    + calcium_status               [time-varying]
#                    + vitD_status                  [time-varying]
#                    + bisphosphonate_status        [time-varying]
#                    + energy_kcal                  [dietary confounder]
#                    + protein_pct                  [dietary confounder]
#                    + (1 + time_since_bsl_yr | pt) [random intercept + slope]
#
# Covariates selected on the basis of:
#   - Age and BMI: primary anthropometric confounders of grip strength
#   - Education: socioeconomic proxy for nutrition quality
#   - Smoking, alcohol, PA: lifestyle confounders of both diet and muscle
#   - Diabetes, HTN: metabolic conditions that impair muscle function
#   - HRT, corticoids, Ca/VitD/bisphosphonate: medications affecting bone/muscle
#   - Energy and protein: dietary confounders (total intake and protein fraction)
#
# The interaction dairy_cumavg × time_since_bsl_yr tests whether the
# association between dairy intake and grip strength changes over follow-up
# (i.e. whether higher dairy intake modifies the rate of grip decline).
#
# Outputs
# -------
#   grip_model_data     model-ready tibble (from build_grip_model_data)
#   grip_model_fit      lmerMod object (from fit_grip_model)
#   grip_model_table    gtsummary tbl_regression (from make_grip_model_table)
#   grip_model_plots    patchwork of diagnostic plots (from make_grip_model_plots)
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================

# Covariates used in both dataset construction and model formula — defined
# once to keep build and fit in sync.
.GRIP_COVARIATES <- c(
    # Time-varying (from visits or exposures)
    "time_since_bsl_yr",
    "Age",
    "BMI",
    # Fixed (from participants, broadcast to all rows via analysis_long join)
    "education_level",
    # Time-varying behavioural
    "sbsmk",
    "alcohol_category",
    "pa_levels",
    # Time-varying clinical
    "diabetes_status",
    "HTN_status",
    "hrt_status",
    "corticoids_status",
    "calcium_status",
    "vitD_status",
    "bisphosphonate_status",
    # Dietary confounders
    "energy_kcal",
    "protein_pct"
)

# =============================================================================
# Dataset construction
# =============================================================================

#' Assemble the model-ready dataset for the grip strength ~ dairy analysis.
#'
#' Filters analysis_long to participants eligible for the grip strength
#' outcome (eligible_hgs), selects the outcome, primary exposure, all
#' covariates, and audit columns, then applies final model exclusions:
#'   - Rows with missing handgrip_max_all (outcome)
#'   - Rows with missing dairy_cumavg (primary exposure)
#'   - Participants with fewer than 2 non-missing outcome rows after the above
#'     (cannot estimate an individual trajectory)
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A tibble: one row per pt x OsteoLaus wave, ready for lmer().
build_grip_model_data <- function(analysis_long) {
    
    # ── Select eligible participants and relevant columns ─────────────────────
    # eligible_hgs requires handgrip_max_all non-missing at OsteoLaus Baseline.
    # We then also keep rows from non-Baseline waves (grip measured later).
    model_data <- analysis_long |>
        dplyr::filter(eligible_hgs) |>
        dplyr::select(
            # Keys and time
            pt, osteo_wave, osteo_wave_num, time_since_bsl_yr,
            # Outcome
            handgrip_max_all,
            # Primary exposure
            dairy_cumavg,
            # Audit: instantaneous dairy and lag (for sensitivity checks)
            dairy_total_gday,
            dairy_total_lag1,
            dairy_cumavg_lag1,
            # All covariates
            dplyr::any_of(.GRIP_COVARIATES),
            # Eligible flag (kept for downstream checks)
            eligible_hgs
        )
    
    n_start <- dplyr::n_distinct(model_data$pt)
    
    # ── Row-level exclusions: missing outcome or exposure ─────────────────────
    model_data <- model_data |>
        dplyr::filter(
            !is.na(handgrip_max_all),
            !is.na(dairy_cumavg)
        )
    
    n_after_row <- dplyr::n_distinct(model_data$pt)
    
    # ── Participant-level exclusion: need >= 2 valid rows per pt ──────────────
    pts_with_enough <- model_data |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            n_valid = dplyr::n(),
            .groups = "drop"
        ) |>
        dplyr::filter(n_valid >= 2L) |>
        dplyr::pull(pt)
    
    model_data <- dplyr::filter(model_data, pt %in% pts_with_enough)
    
    n_final <- dplyr::n_distinct(model_data$pt)
    
    # ── Factor reference levels ────────────────────────────────────────────────
    # Set reference levels explicitly so model coefficients are interpretable.
    model_data <- model_data |>
        dplyr::mutate(
            # education_level: Low is reference (highest risk group)
            education_level   = forcats::fct_relevel(
                education_level, "Low"
            ),
            # sbsmk: Never is reference
            sbsmk             = forcats::fct_relevel(sbsmk, "Never"),
            # alcohol_category: Non-drinker is reference
            alcohol_category  = forcats::fct_relevel(
                alcohol_category, "Non-drinker"
            ),
            # pa_levels: lowest activity is reference
            pa_levels         = forcats::fct_relevel(
                pa_levels, levels(pa_levels)[1]
            ),
            # Binary covariates: No is reference (already coded No/Yes)
            across(
                dplyr::any_of(c(
                    "diabetes_status", "HTN_status", "hrt_status",
                    "corticoids_status", "calcium_status",
                    "vitD_status", "bisphosphonate_status"
                )),
                ~ forcats::fct_relevel(.x, "No")
            )
        )
    
    # ── Summary ───────────────────────────────────────────────────────────────
    n_rows    <- nrow(model_data)
    n_waves   <- dplyr::n_distinct(model_data$osteo_wave)
    mean_obs  <- round(n_rows / n_final, 1)
    
    cli::cli_inform(c(
        "i" = "build_grip_model_data() complete:",
        "*" = "Started with {n_start} eligible_hgs participants",
        "*" = "{n_start - n_after_row} lost to missing outcome/exposure",
        "*" = "{n_after_row - n_final} lost: < 2 valid obs after row exclusions",
        "v" = "{n_final} participants | {n_rows} rows | {n_waves} waves | \\
               mean {mean_obs} obs/pt"
    ))
    
    # ── Check covariate completeness ──────────────────────────────────────────
    miss_pct <- model_data |>
        dplyr::summarise(
            dplyr::across(
                dplyr::any_of(.GRIP_COVARIATES),
                ~ round(mean(is.na(.x)) * 100, 1)
            )
        ) |>
        tidyr::pivot_longer(
            dplyr::everything(),
            names_to  = "covariate",
            values_to = "pct_missing"
        ) |>
        dplyr::filter(pct_missing > 0)
    
    if (nrow(miss_pct) > 0) {
        cli::cli_warn(c(
            "!" = "Covariates with missing values (listwise deletion will apply):",
            "*" = paste(
                glue::glue("{miss_pct$covariate}: {miss_pct$pct_missing}%"),
                collapse = "\n"
            )
        ))
    } else {
        cli::cli_inform(c("v" = "No missing values in covariates."))
    }
    
    model_data
}


# =============================================================================
# Model fitting
# =============================================================================

#' Fit the linear mixed-effects model for grip strength ~ dairy intake.
#'
#' Uses lmerTest::lmer() — a drop-in replacement for lme4::lmer() that adds
#' Satterthwaite degrees of freedom so p-values are available in tbl_regression().
#' Returns the REML-fit model.
#'
#' Random effects: random intercept + random slope over time per participant
#' (1 + time_since_bsl_yr | pt). This allows each participant to have their
#' own baseline grip strength AND their own rate of grip change over time.
#'
#' The dairy × time interaction tests whether higher cumulative dairy intake
#' is associated with a slower rate of grip decline.
#'
#' @param grip_model_data Output of build_grip_model_data().
#' @return A lmerMod object (REML fit).
fit_grip_model <- function(grip_model_data) {
    
    # ── Formula ────────────────────────────────────────────────────────────────
    # Main effect of dairy_cumavg: cross-sectional association with grip
    # dairy_cumavg × time interaction: does dairy modify grip trajectory?
    # time_since_bsl_yr main effect: average grip decline over follow-up
    fixed_covs <- paste(
        intersect(.GRIP_COVARIATES, names(grip_model_data)),
        collapse = " + "
    )
    
    f <- stats::as.formula(paste0(
        "handgrip_max_all ~ ",
        "dairy_cumavg * time_since_bsl_yr + ",
        fixed_covs,
        " + (1 + time_since_bsl_yr | pt)"
    ))
    
    cli::cli_inform(c("i" = "Fitting LME model formula:"))
    cli::cli_inform(c("*" = deparse(f)))
    
    # ── Fit with REML = FALSE first for LRT / AIC comparisons ─────────────────
    # lmerTest::lmer() is a drop-in replacement for lme4::lmer() that adds
    # Satterthwaite degrees of freedom, enabling p-values in tbl_regression().
    fit_ml <- lmerTest::lmer(
        formula = f,
        data    = grip_model_data,
        REML    = FALSE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    # ── Refit with REML = TRUE for final parameter estimates ──────────────────
    fit_reml <- lmerTest::lmer(
        formula = f,
        data    = grip_model_data,
        REML    = TRUE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    # ── Check for singular fit ─────────────────────────────────────────────────
    if (lme4::isSingular(fit_reml)) {
        cli::cli_warn(
            "fit_grip_model(): singular fit detected. \\
       The random slope for time may be poorly identified. \\
       Consider simplifying to random intercept only: (1 | pt)."
        )
    }
    
    # ── Summary ────────────────────────────────────────────────────────────────
    n_pt  <- dplyr::n_distinct(grip_model_data$pt)
    n_obs <- nrow(grip_model_data)
    
    vc    <- lme4::VarCorr(fit_reml)
    re_sd <- sqrt(diag(as.matrix(vc$pt)))
    
    cli::cli_inform(c(
        "v" = "fit_grip_model() complete:",
        "*" = "N participants: {n_pt} | N observations: {n_obs}",
        "*" = "Fixed effects: {length(lme4::fixef(fit_reml))}",
        "*" = "Random effects SD — intercept: {round(re_sd[1], 2)}, \\
               slope: {round(re_sd[2], 2)}",
        "*" = "Residual SD: {round(sigma(fit_reml), 2)}",
        "*" = "ICC (intercept): {round(re_sd[1]^2 / (re_sd[1]^2 + sigma(fit_reml)^2), 3)}"
    ))
    
    fit_reml
}


# =============================================================================
# Model table
# =============================================================================

#' Produce a publication-ready table from the grip strength LME model.
#'
#' Uses gtsummary::tbl_regression() which calls broom.mixed::tidy() for
#' lmerMod objects. Confidence intervals are Wald-based (default for lmer).
#'
#' @param grip_model_fit Output of fit_grip_model().
#' @return A gtsummary tbl_regression object.
make_grip_model_table <- function(grip_model_fit) {
    
    grip_model_fit |>
        gtsummary::tbl_regression(
            label = list(
                dairy_cumavg                        ~ "Cumulative dairy intake (g/day)",
                time_since_bsl_yr                   ~ "Time since Baseline (yr)",
                `dairy_cumavg:time_since_bsl_yr`    ~ "Dairy \u00d7 Time interaction",
                Age                                 ~ "Age (yr)",
                BMI                                 ~ "BMI (kg/m\u00b2)",
                education_level                     ~ "Education level",
                sbsmk                               ~ "Smoking status",
                alcohol_category                    ~ "Alcohol intake",
                pa_levels                           ~ "Physical activity",
                diabetes_status                     ~ "Diabetes",
                HTN_status                          ~ "Hypertension",
                hrt_status                          ~ "HRT",
                corticoids_status                   ~ "Systemic corticosteroids",
                calcium_status                      ~ "Calcium supplement",
                vitD_status                         ~ "Vitamin D supplement",
                bisphosphonate_status               ~ "Bisphosphonate use",
                energy_kcal                         ~ "Total energy (kcal/day)",
                protein_pct                         ~ "Protein (% of energy)"
            ),
            pvalue_fun = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_caption(
            "**Table 2.** Linear mixed-effects model: grip strength ~ \\
             cumulative dairy intake"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "\u03b2 (95% CI) from lmerTest::lmer() with Satterthwaite degrees of freedom. \\
                 Random effects: random intercept + slope per participant. \\
                 Reference: education Low; smoking Never; \\
                 alcohol Non-drinker; binary covariates No."
        )
}


# =============================================================================
# Diagnostic plots
# =============================================================================

#' Validation plots for the LME grip strength model.
#'
#' Four panels presented in a 2 × 2 grid:
#'
#'   A  Tukey-Anscombe plot (residuals vs fitted values)
#'      Checks: linearity, homoscedasticity (constant spread).
#'      Ideal: points scattered symmetrically around y = 0 with no pattern.
#'      The LOESS smoother should run close to the horizontal.
#'
#'   B  Q-Q plot of residuals
#'      Checks: normality of the residual distribution.
#'      Ideal: points lie along the 45-degree reference line throughout.
#'      Heavy tails or S-curves indicate non-normality.
#'
#'   C  Q-Q plot of random intercepts (BLUPs)
#'      Checks: normality of the random-effects distribution — a model
#'      assumption separate from residual normality.
#'
#'   D  Observed vs model-fitted trajectories (30 random participants)
#'      Visual check of how well individual trajectories are captured.
#'
#' @param grip_model_fit  Output of fit_grip_model() (lmerTest object).
#' @param grip_model_data Output of build_grip_model_data().
#' @return A patchwork plot object (2 x 2 grid).
make_grip_model_plots <- function(grip_model_fit, grip_model_data) {
    
    # ── Assemble residual data frame (row-safe) ────────────────────────────────
    # stats::fitted() and stats::residuals() return named numeric vectors whose
    # names are row indices of the data used to fit the model. Match by name
    # rather than position to guard against any row reordering by lmer.
    row_idx  <- as.integer(names(stats::residuals(grip_model_fit)))
    
    resid_df <- tibble::tibble(
        fitted     = stats::fitted(grip_model_fit),
        residual   = stats::residuals(grip_model_fit),
        pt         = grip_model_data$pt[row_idx],
        osteo_wave = grip_model_data$osteo_wave[row_idx]
    )
    
    # Standardised residuals for Q-Q plot (zero mean, unit SD)
    resid_df <- dplyr::mutate(
        resid_df,
        residual_std = scale(residual)[, 1]
    )
    
    # ── A: Tukey-Anscombe plot ────────────────────────────────────────────────
    # Residuals vs fitted values. Checks linearity and homoscedasticity.
    p_ta <- ggplot2::ggplot(
        resid_df,
        ggplot2::aes(x = fitted, y = residual)
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
            subtitle = "Residuals vs fitted values — checks linearity & homoscedasticity",
            x        = "Fitted values (kg)",
            y        = "Residuals (kg)"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
    # ── B: Q-Q plot of residuals ──────────────────────────────────────────────
    # Normal probability plot. Checks residual normality.
    p_qq_resid <- ggplot2::ggplot(
        resid_df,
        ggplot2::aes(sample = residual_std)
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
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
    # ── C: Q-Q plot of random intercepts ─────────────────────────────────────
    # Checks the normality assumption of the random-effects distribution.
    ranef_pt  <- lme4::ranef(grip_model_fit)$pt
    
    # Guard: random slope column may not exist if model was simplified
    has_slope <- "time_since_bsl_yr" %in% names(ranef_pt)
    
    ranef_df <- tibble::tibble(
        intercept = ranef_pt[["(Intercept)"]],
        slope     = if (has_slope) ranef_pt[["time_since_bsl_yr"]] else NA_real_
    ) |>
        dplyr::mutate(
            intercept_std = scale(intercept)[, 1]
        )
    
    p_qq_ranef <- ggplot2::ggplot(
        ranef_df,
        ggplot2::aes(sample = intercept_std)
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
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
    # ── D: Observed vs fitted trajectories (random sample) ───────────────────
    set.seed(42L)
    n_show    <- min(30L, dplyr::n_distinct(resid_df$pt))
    sample_pts <- sample(unique(resid_df$pt), n_show)
    
    # Safe merge: join fitted values back onto grip_model_data by pt + wave
    fitted_lookup <- resid_df |>
        dplyr::select(pt, osteo_wave, fitted)
    
    obs_fit_df <- grip_model_data |>
        dplyr::filter(pt %in% sample_pts) |>
        dplyr::left_join(fitted_lookup, by = c("pt", "osteo_wave"))
    
    p_obsfit <- ggplot2::ggplot(
        obs_fit_df,
        ggplot2::aes(x = time_since_bsl_yr, group = pt)
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
            x        = "Time since Baseline (yr)",
            y        = "Grip strength (kg)"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
    patchwork::wrap_plots(p_ta, p_qq_resid, p_qq_ranef, p_obsfit, ncol = 2) +
        patchwork::plot_annotation(
            title    = "LME model validation: grip strength ~ cumulative dairy intake",
            subtitle = "Panels A-B: residual checks. Panel C: random-effects check. Panel D: trajectory fit.",
            theme    = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 13, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
}