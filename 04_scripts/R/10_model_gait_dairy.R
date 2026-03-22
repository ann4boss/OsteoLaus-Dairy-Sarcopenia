# =============================================================================
# R/10_model_gait_dairy.R
# =============================================================================
# Mixed-effects longitudinal model: gait speed ~ cumulative dairy intake.
#
# Research question
# -----------------
# Is higher cumulative dairy intake (dairy_cumavg, g/day) associated with
# faster gait speed (m/s) at V4 and/or V5 in OsteoLaus women, after
# adjustment for known confounders?
#
# Design constraint: maximum 2 time points per participant
# --------------------------------------------------------
# Gait speed (6-metre walk test) was only measured at V4 and V5. Each
# participant therefore contributes at most 2 observations. This has a direct
# consequence for the random-effects structure:
#
#   Random intercept + random slope (1 + time | pt):
#     UNIDENTIFIABLE with only 2 observations per person. Two data points
#     perfectly determine a straight line, leaving zero within-person residual
#     degrees of freedom. lmer would produce a singular fit or fail to converge.
#
#   Random intercept only (1 | pt):
#     IDENTIFIABLE. Allows each participant to have their own baseline gait
#     speed level. The fixed effect of time_since_bsl_yr then captures the
#     average change from V4 to V5 across all participants, and the
#     dairy × time interaction tests whether higher dairy intake modifies
#     that average change.
#
# The model is therefore:
#
#   gait_speed ~ dairy_cumavg * time_since_bsl_yr
#              + time_since_bsl_yr
#              + Age
#              + BMI
#              + education_level
#              + sbsmk
#              + alcohol_category
#              + pa_levels
#              + diabetes_status
#              + HTN_status
#              + hrt_status
#              + corticoids_status
#              + calcium_status
#              + vitD_status
#              + bisphosphonate_status
#              + energy_kcal
#              + protein_pct
#              + (1 | pt)              ← random intercept ONLY
#
# Alternatively, since V4/V5 are the only waves, a simpler wave indicator
# (wave = "V5" vs "V4" as reference) may be used in place of time_since_bsl_yr
# when the absolute time elapsed is less informative than wave identity.
# Both are computed; time_since_bsl_yr is used in the primary model because
# it captures actual variation in the inter-visit interval between participants.
#
# Eligibility
# -----------
# eligible_gait: non-missing gait_speed at at least one of V4 or V5.
# The model further excludes rows with missing gait_speed or dairy_cumavg.
# Participants with only 1 valid row are retained in the random-intercept
# model (unlike the grip/ALM models which require >= 2 rows for slope
# estimation). With 1 observation the participant contributes to fixed
# effects estimation only.
#
# Outputs
# -------
#   gait_model_data   model-ready tibble (from build_gait_model_data)
#   gait_model_fit    lmerMod object (from fit_gait_model)
#   gait_model_table  gtsummary tbl_regression (from make_gait_model_table)
#   gait_model_plots  patchwork diagnostics (from make_gait_model_plots)
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================

.GAIT_COVARIATES <- c(
    "time_since_bsl_yr",
    "Age",
    "BMI",
    "education_level",
    "sbsmk",
    "alcohol_category",
    "pa_levels",
    "diabetes_status",
    "HTN_status",
    "hrt_status",
    "corticoids_status",
    "calcium_status",
    "vitD_status",
    "bisphosphonate_status",
    "energy_kcal",
    "protein_pct"
)

# =============================================================================
# Dataset construction
# =============================================================================

#' Assemble the model-ready dataset for the gait speed ~ dairy analysis.
#'
#' Filters analysis_long to V4 and V5 rows only (the waves where gait speed
#' is measured) among eligible_gait participants, then applies row-level
#' exclusions for missing outcome or exposure.
#'
#' Unlike the grip and ALM models, participants with only 1 valid observation
#' are retained: the random-intercept-only model can use them for fixed-effect
#' estimation (they contribute to the pooled estimate of the dairy effect).
#'
#' A wave indicator (gait_wave: "V4" / "V5", reference = "V4") is added
#' alongside time_since_bsl_yr for descriptive and sensitivity use.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A tibble: one row per pt x {V4, V5}, ready for lmer().
build_gait_model_data <- function(analysis_long) {
    
    # Restrict to V4 and V5 — the only waves with gait speed data
    model_data <- analysis_long |>
        dplyr::filter(
            eligible_gait,
            osteo_wave %in% c("V4", "V5")
        ) |>
        dplyr::select(
            pt, osteo_wave, osteo_wave_num, time_since_bsl_yr,
            gait_speed,
            dairy_cumavg,
            dairy_total_gday,
            dairy_total_lag1,
            dairy_cumavg_lag1,
            dplyr::any_of(.GAIT_COVARIATES),
            eligible_gait
        ) |>
        dplyr::mutate(
            # Wave indicator: V4 = reference, V5 = follow-up
            gait_wave = factor(osteo_wave, levels = c("V4", "V5"))
        )
    
    n_start <- dplyr::n_distinct(model_data$pt)
    
    # ── Row-level exclusions: missing outcome or primary exposure ─────────────
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
    
    # ── Factor reference levels ───────────────────────────────────────────────
    model_data <- model_data |>
        dplyr::left_join(obs_counts, by = "pt") |>
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
    
    cli::cli_inform(c(
        "i" = "build_gait_model_data() complete:",
        "*" = "Started with {n_start} eligible_gait participants",
        "*" = "{n_start - n_final} lost to missing gait_speed or dairy_cumavg",
        "v" = "{n_final} participants | {n_rows} rows",
        "*" = "  2 observations (V4 + V5): {n_two_obs} participants",
        "*" = "  1 observation  (V4 or V5 only): {n_one_obs} participants",
        "i" = "Random intercept only (1 | pt) — random slope not estimable \\
               with at most 2 obs per participant."
    ))
    
    # ── Covariate completeness ────────────────────────────────────────────────
    miss_pct <- model_data |>
        dplyr::summarise(
            dplyr::across(
                dplyr::any_of(.GAIT_COVARIATES),
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

#' Fit the LME model for gait speed ~ dairy intake.
#'
#' Uses a random intercept only (1 | pt) because gait speed is measured at
#' at most 2 time points (V4 and V5). A random slope is not estimable:
#' with only 2 observations per person a line is perfectly determined,
#' leaving no within-person residual variance for a slope distribution to
#' be estimated from.
#'
#' Uses lmerTest::lmer() for Satterthwaite p-values.
#'
#' @param gait_model_data Output of build_gait_model_data().
#' @return A lmerMod object (REML fit).
fit_gait_model <- function(gait_model_data) {
    
    fixed_covs <- paste(
        intersect(.GAIT_COVARIATES, names(gait_model_data)),
        collapse = " + "
    )
    
    # Random intercept only — no random slope (see header rationale)
    f <- stats::as.formula(paste0(
        "gait_speed ~ ",
        "dairy_cumavg * time_since_bsl_yr + ",
        fixed_covs,
        " + (1 | pt)"
    ))
    
    cli::cli_inform(c("i" = "Fitting gait speed LME model formula:"))
    cli::cli_inform(c("*" = deparse(f)))
    
    fit_ml <- lmerTest::lmer(
        formula = f,
        data    = gait_model_data,
        REML    = FALSE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    fit_reml <- lmerTest::lmer(
        formula = f,
        data    = gait_model_data,
        REML    = TRUE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    # Singular fit is less likely here since we already removed the random slope,
    # but check anyway
    if (lme4::isSingular(fit_reml)) {
        cli::cli_warn(
            "fit_gait_model(): singular fit — between-person variance in \\
       gait speed is very small. Consider an OLS model without random effects."
        )
    }
    
    n_pt  <- dplyr::n_distinct(gait_model_data$pt)
    n_obs <- nrow(gait_model_data)
    vc    <- lme4::VarCorr(fit_reml)
    re_sd <- as.data.frame(vc)$sdcor[1]   # intercept SD only
    
    icc <- re_sd^2 / (re_sd^2 + sigma(fit_reml)^2)
    
    cli::cli_inform(c(
        "v" = "fit_gait_model() complete:",
        "*" = "N participants: {n_pt} | N observations: {n_obs}",
        "*" = "Fixed effects: {length(lme4::fixef(fit_reml))}",
        "*" = "Random intercept SD: {round(re_sd, 3)}",
        "*" = "Residual SD: {round(sigma(fit_reml), 3)}",
        "*" = "ICC: {round(icc, 3)} \\
               (proportion of variance due to between-person differences)"
    ))
    
    fit_reml
}


# =============================================================================
# Model table
# =============================================================================

#' Publication-ready table from the gait speed LME model.
#'
#' @param gait_model_fit Output of fit_gait_model().
#' @return A gtsummary tbl_regression object.
make_gait_model_table <- function(gait_model_fit) {
    
    gait_model_fit |>
        gtsummary::tbl_regression(
            label = list(
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
            ),
            pvalue_fun = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_caption(
            "**Table 4.** Linear mixed-effects model: \
             gait speed ~ cumulative dairy intake"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "\u03b2 (95% CI) from lmerTest::lmer() with Satterthwaite \
                 degrees of freedom. Random intercept only (1 | pt) — \
                 random slope not estimable (max 2 obs per participant: V4, V5). \
                 Reference: education Low; smoking Never; \
                 alcohol Non-drinker; binary covariates No."
        )
}


# =============================================================================
# Diagnostic plots
# =============================================================================

#' Validation plots for the gait speed LME model.
#'
#' Same four-panel layout as grip and ALM models, adapted for the
#' random-intercept-only structure (panel C shows BLUPs from a single
#' random intercept per participant rather than intercept + slope).
#' Panel D shows observed vs fitted at V4 and V5 rather than a continuous
#' trajectory, which better reflects the two-wave design.
#'
#' @param gait_model_fit  Output of fit_gait_model() (lmerTest object).
#' @param gait_model_data Output of build_gait_model_data().
#' @return A patchwork plot object (2 x 2 grid).
make_gait_model_plots <- function(gait_model_fit, gait_model_data) {
    
    row_idx  <- as.integer(names(stats::residuals(gait_model_fit)))
    
    resid_df <- tibble::tibble(
        fitted     = stats::fitted(gait_model_fit),
        residual   = stats::residuals(gait_model_fit),
        pt         = gait_model_data$pt[row_idx],
        osteo_wave = gait_model_data$osteo_wave[row_idx]
    ) |>
        dplyr::mutate(residual_std = scale(residual)[, 1])
    
    # ── A: Tukey-Anscombe ─────────────────────────────────────────────────────
    p_ta <- ggplot2::ggplot(
        resid_df, ggplot2::aes(x = fitted, y = residual)
    ) +
        ggplot2::geom_point(alpha = 0.3, size = 0.9,
                            colour = "grey30") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "tomato", linewidth = 0.6) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                             se = TRUE, colour = "#2E86AB",
                             fill = "#2E86AB", alpha = 0.15,
                             linewidth = 0.7) +
        ggplot2::labs(
            title    = "A: Tukey-Anscombe plot",
            subtitle = "Residuals vs fitted — checks linearity & homoscedasticity",
            x        = "Fitted values (m/s)",
            y        = "Residuals (m/s)"
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
    # Random intercept only — no slope BLUP
    ranef_pt <- lme4::ranef(gait_model_fit)$pt
    
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
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
    # ── D: Observed vs fitted at V4 and V5 ───────────────────────────────────
    # With only 2 waves, show paired V4-V5 lines rather than a continuous curve.
    # Each participant is a line connecting their observed and fitted values
    # at V4 and V5.
    set.seed(42L)
    n_show     <- min(40L, dplyr::n_distinct(resid_df$pt))
    sample_pts <- sample(unique(resid_df$pt), n_show)
    
    fitted_lookup <- dplyr::select(resid_df, pt, osteo_wave, fitted)
    
    obs_fit_df <- gait_model_data |>
        dplyr::filter(pt %in% sample_pts) |>
        dplyr::left_join(fitted_lookup, by = c("pt", "osteo_wave")) |>
        dplyr::mutate(
            wave_x = dplyr::if_else(osteo_wave == "V4", 0, 1)
        )
    
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
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
    patchwork::wrap_plots(p_ta, p_qq_resid, p_qq_ranef, p_obsfit, ncol = 2) +
        patchwork::plot_annotation(
            title    = "LME model validation: gait speed ~ cumulative dairy intake",
            subtitle = paste0(
                "Random intercept only (1 | pt) — max 2 obs per participant (V4, V5). ",
                "Panels A-B: residual checks. Panel C: random-effects check. Panel D: V4-V5 fit."
            ),
            theme    = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 13, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
}