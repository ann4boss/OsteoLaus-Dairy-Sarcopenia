# =============================================================================
# R/10_model_alm_dairy.R
# =============================================================================
# Mixed-effects longitudinal model: appendicular lean mass index ~ dairy intake.
#
# Research question
# -----------------
# Is higher cumulative dairy intake (dairy_cumavg, g/day) associated with
# better appendicular lean mass index (ALM_HT2, kg/m²) over time in OsteoLaus
# women, after adjustment for known confounders?
#
# Outcome
# -------
# ALM_HT2 — appendicular lean mass / height² (kg/m²), measured by DXA at each
# OsteoLaus visit. This is the EWGSOP2 recommended index of muscle quantity.
#
# Note on covariate selection vs grip model
# -----------------------------------------
# BMI is excluded here because ALM_HT2 = ALM / height² — including BMI in the
# model would introduce collinearity (BMI = weight / height², and lean mass is
# a major component of weight). Height is retained as the anthropometric
# adjustment; weight is excluded for the same reason.
# All other covariates are shared with the grip model.
#
# Model
# -----
# fit_alm_model() fits via lmerTest::lmer():
#
#   ALM_HT2 ~ dairy_cumavg * time_since_bsl_yr
#            + time_since_bsl_yr
#            + Age                          [time-varying]
#            + Height                       [time-varying; replaces BMI]
#            + education_level              [fixed]
#            + sbsmk                        [time-varying]
#            + alcohol_category             [time-varying]
#            + pa_levels                    [time-varying]
#            + diabetes_status              [time-varying]
#            + HTN_status                   [time-varying]
#            + hrt_status                   [time-varying]
#            + corticoids_status            [time-varying]
#            + calcium_status               [time-varying]
#            + vitD_status                  [time-varying]
#            + bisphosphonate_status        [time-varying]
#            + energy_kcal                  [dietary confounder]
#            + protein_pct                  [dietary confounder]
#            + (1 + time_since_bsl_yr | pt) [random intercept + slope]
#
# Outputs
# -------
#   alm_model_data    model-ready tibble (from build_alm_model_data)
#   alm_model_fit     lmerMod object (from fit_alm_model)
#   alm_model_table   gtsummary tbl_regression (from make_alm_model_table)
#   alm_model_plots   patchwork of diagnostic plots (from make_alm_model_plots)
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================

# Covariates for the ALM model.
# BMI excluded (collinear with ALM_HT2); Height used instead.
.ALM_COVARIATES <- c(
    # Time and anthropometrics
    "time_since_bsl_yr",
    "Age",
    "Height",          # replaces BMI — see rationale above
    # Fixed participant characteristic
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

#' Assemble the model-ready dataset for the muscle mass ~ dairy analysis.
#'
#' Filters analysis_long to participants eligible for the ALM_HT2 outcome
#' (eligible_alm), selects the outcome, primary exposure, all covariates,
#' and audit columns, then applies final model exclusions:
#'   - Rows with missing ALM_HT2 (outcome)
#'   - Rows with missing dairy_cumavg (primary exposure)
#'   - Participants with fewer than 2 non-missing outcome rows after the above
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A tibble: one row per pt x OsteoLaus wave, ready for lmer().
build_alm_model_data <- function(analysis_long) {
    
    model_data <- analysis_long |>
        dplyr::filter(eligible_alm) |>
        dplyr::select(
            # Keys and time
            pt, osteo_wave, osteo_wave_num, time_since_bsl_yr,
            # Outcome
            ALM_HT2,
            # Primary exposure
            dairy_cumavg,
            # Audit columns
            dairy_total_gday,
            dairy_total_lag1,
            dairy_cumavg_lag1,
            # All covariates
            dplyr::any_of(.ALM_COVARIATES),
            # Eligible flag
            eligible_alm
        )
    
    n_start <- dplyr::n_distinct(model_data$pt)
    
    # ── Row-level exclusions ──────────────────────────────────────────────────
    model_data <- model_data |>
        dplyr::filter(
            !is.na(ALM_HT2),
            !is.na(dairy_cumavg)
        )
    
    n_after_row <- dplyr::n_distinct(model_data$pt)
    
    # ── Participant-level: need >= 2 valid outcome rows ───────────────────────
    pts_with_enough <- model_data |>
        dplyr::group_by(pt) |>
        dplyr::summarise(n_valid = dplyr::n(), .groups = "drop") |>
        dplyr::filter(n_valid >= 2L) |>
        dplyr::pull(pt)
    
    model_data <- dplyr::filter(model_data, pt %in% pts_with_enough)
    
    n_final <- dplyr::n_distinct(model_data$pt)
    
    # ── Factor reference levels ───────────────────────────────────────────────
    model_data <- model_data |>
        dplyr::mutate(
            education_level  = forcats::fct_relevel(education_level, "Low"),
            sbsmk            = forcats::fct_relevel(sbsmk, "Never"),
            alcohol_category = forcats::fct_relevel(alcohol_category,
                                                    "Non-drinker"),
            pa_levels        = forcats::fct_relevel(pa_levels,
                                                    levels(pa_levels)[1]),
            dplyr::across(
                dplyr::any_of(c(
                    "diabetes_status", "HTN_status", "hrt_status",
                    "corticoids_status", "calcium_status",
                    "vitD_status", "bisphosphonate_status"
                )),
                ~ forcats::fct_relevel(.x, "No")
            )
        )
    
    # ── Summary ───────────────────────────────────────────────────────────────
    n_rows   <- nrow(model_data)
    n_waves  <- dplyr::n_distinct(model_data$osteo_wave)
    mean_obs <- round(n_rows / n_final, 1)
    
    cli::cli_inform(c(
        "i" = "build_alm_model_data() complete:",
        "*" = "Started with {n_start} eligible_alm participants",
        "*" = "{n_start - n_after_row} lost to missing outcome/exposure",
        "*" = "{n_after_row - n_final} lost: < 2 valid obs after row exclusions",
        "v" = "{n_final} participants | {n_rows} rows | {n_waves} waves | \
               mean {mean_obs} obs/pt"
    ))
    
    # ── Covariate completeness check ──────────────────────────────────────────
    miss_pct <- model_data |>
        dplyr::summarise(
            dplyr::across(
                dplyr::any_of(.ALM_COVARIATES),
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

#' Fit the LME model for appendicular lean mass index ~ dairy intake.
#'
#' Uses lmerTest::lmer() for Satterthwaite p-values.
#' Random effects: random intercept + random slope over time (1 + t | pt).
#' The dairy × time interaction tests whether higher cumulative dairy intake
#' is associated with a slower rate of lean mass decline.
#'
#' @param alm_model_data Output of build_alm_model_data().
#' @return A lmerMod object (REML fit).
fit_alm_model <- function(alm_model_data) {
    
    fixed_covs <- paste(
        intersect(.ALM_COVARIATES, names(alm_model_data)),
        collapse = " + "
    )
    
    f <- stats::as.formula(paste0(
        "ALM_HT2 ~ ",
        "dairy_cumavg * time_since_bsl_yr + ",
        fixed_covs,
        " + (1 + time_since_bsl_yr | pt)"
    ))
    
    cli::cli_inform(c("i" = "Fitting ALM LME model formula:"))
    cli::cli_inform(c("*" = deparse(f)))
    
    fit_ml <- lmerTest::lmer(
        formula = f,
        data    = alm_model_data,
        REML    = FALSE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    fit_reml <- lmerTest::lmer(
        formula = f,
        data    = alm_model_data,
        REML    = TRUE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    if (lme4::isSingular(fit_reml)) {
        cli::cli_warn(
            "fit_alm_model(): singular fit detected. \
       The random slope for time may be poorly identified. \
       Consider simplifying to (1 | pt)."
        )
    }
    
    n_pt  <- dplyr::n_distinct(alm_model_data$pt)
    n_obs <- nrow(alm_model_data)
    vc    <- lme4::VarCorr(fit_reml)
    re_sd <- sqrt(diag(as.matrix(vc$pt)))
    
    cli::cli_inform(c(
        "v" = "fit_alm_model() complete:",
        "*" = "N participants: {n_pt} | N observations: {n_obs}",
        "*" = "Fixed effects: {length(lme4::fixef(fit_reml))}",
        "*" = "Random effects SD — intercept: {round(re_sd[1], 3)}, \
               slope: {round(re_sd[2], 3)}",
        "*" = "Residual SD: {round(sigma(fit_reml), 3)}",
        "*" = "ICC (intercept): {round(re_sd[1]^2 / (re_sd[1]^2 + sigma(fit_reml)^2), 3)}"
    ))
    
    fit_reml
}


# =============================================================================
# Model table
# =============================================================================

#' Publication-ready table from the ALM LME model.
#'
#' @param alm_model_fit Output of fit_alm_model().
#' @return A gtsummary tbl_regression object.
make_alm_model_table <- function(alm_model_fit) {
    
    alm_model_fit |>
        gtsummary::tbl_regression(
            label = list(
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
            ),
            pvalue_fun = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_caption(
            "**Table 3.** Linear mixed-effects model: \
             appendicular lean mass index ~ cumulative dairy intake"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "\u03b2 (95% CI) from lmerTest::lmer() with Satterthwaite \
                 degrees of freedom. Outcome: ALM/height\u00b2 (kg/m\u00b2). \
                 BMI excluded (collinear with outcome); Height included instead. \
                 Reference: education Low; smoking Never; \
                 alcohol Non-drinker; binary covariates No."
        )
}


# =============================================================================
# Diagnostic plots
# =============================================================================

#' Validation plots for the ALM LME model.
#'
#' Identical four-panel layout to make_grip_model_plots():
#'   A  Tukey-Anscombe plot
#'   B  Q-Q plot of residuals
#'   C  Q-Q plot of random intercepts (BLUPs)
#'   D  Observed vs fitted trajectories (30 random participants)
#'
#' @param alm_model_fit  Output of fit_alm_model() (lmerTest object).
#' @param alm_model_data Output of build_alm_model_data().
#' @return A patchwork plot object (2 x 2 grid).
make_alm_model_plots <- function(alm_model_fit, alm_model_data) {
    
    row_idx  <- as.integer(names(stats::residuals(alm_model_fit)))
    
    resid_df <- tibble::tibble(
        fitted     = stats::fitted(alm_model_fit),
        residual   = stats::residuals(alm_model_fit),
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
            subtitle = "Residuals vs fitted values — checks linearity & homoscedasticity",
            x        = "Fitted values (kg/m\u00b2)",
            y        = "Residuals (kg/m\u00b2)"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
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
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
    # ── C: Q-Q plot of random intercepts ─────────────────────────────────────
    ranef_pt  <- lme4::ranef(alm_model_fit)$pt
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
            title    = "C: Q-Q plot of random intercepts (BLUPs)",
            subtitle = "Checks normality of random-effects distribution",
            x        = "Theoretical normal quantiles",
            y        = "Standardised random intercepts"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
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
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
    patchwork::wrap_plots(p_ta, p_qq_resid, p_qq_ranef, p_obsfit, ncol = 2) +
        patchwork::plot_annotation(
            title    = "LME model validation: ALMI ~ cumulative dairy intake",
            subtitle = "Panels A-B: residual checks. Panel C: random-effects check. Panel D: trajectory fit.",
            theme    = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 13, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
}