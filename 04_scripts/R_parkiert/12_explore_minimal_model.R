# =============================================================================
# R/12_explore_minimal_models.R
# =============================================================================
# Exploratory script: marginal distributions + minimal mixed-effects models.
#
# Purpose
# -------
# Before committing to the full M3 specification, this script provides:
#   1. Marginal histograms for all key variables at baseline and across waves
#   2. Minimal (M1) mixed-effects models for each outcome
#      - Grip strength ~ dairy_cumavg + Age + BMI/Height + energy_kcal + (1+time|pt)
#      - ALMI          ~ dairy_cumavg + Age + Height    + energy_kcal + (1+time|pt)
#      - Gait speed    ~ dairy_cumavg + Age + BMI       + energy_kcal + (1|pt)
#   3. Dairy × time interaction models (secondary hypothesis: does higher dairy
#      attenuate the rate of decline?)
#      - Adds dairy_cumavg * time_since_bsl_yr to each minimal model
#      - Interaction plot: predicted trajectories by dairy quartile
#      - LRT comparing main-effect vs interaction model (grip and ALM only)
#
# This script is standalone — it reads analysis_long from the targets cache
# and writes all outputs to 06_outputs/explore/.
#
# Usage
#   targets::tar_load(analysis_long)   # load from cache in interactive session
#   source("04_scripts/R/12_explore_minimal_models.R")
#   run_exploration(analysis_long)
#
# Or run end-to-end without targets:
#   analysis_long <- readRDS("path/to/analysis_long.rds")
#   source("04_scripts/R/12_explore_minimal_models.R")
#   run_exploration(analysis_long)
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================

# ── Package guard ─────────────────────────────────────────────────────────────
.check_pkgs <- function() {
    needed <- c("dplyr", "tidyr", "ggplot2", "patchwork", "lme4",
                "lmerTest", "broom.mixed", "forcats", "scales", "cli",
                "tibble", "glue")
    missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0)
        stop("Install missing packages: ", paste(missing, collapse = ", "))
    invisible(NULL)
}

# =============================================================================
# 1. VARIABLE CATALOGUE
# Variables to plot, grouped for layout purposes.
# =============================================================================

.HIST_CONTINUOUS <- c(
    # Outcomes
    "handgrip_max_all", "ALM_HT2", "gait_speed",
    # Primary exposure
    "dairy_cumavg", "dairy_total_gday",
    # Sub-categories
    "dairy_fermented_gday", "dairy_non_fermented_gday",
    "dairy_lowfat_gday", "dairy_highfat_gday",
    # Dietary confounders
    "energy_kcal", "protein_pct",
    # Anthropometrics
    "Age", "BMI", "Height", "Weight"
)

.HIST_CATEGORICAL <- c(
    "education_level", "sbsmk", "alcohol_category", "pa_levels",
    "diabetes_status", "HTN_status", "hrt_status",
    "corticoids_status", "calcium_status", "vitD_status",
    "bisphosphonate_status", "BMI_category"
)

.VAR_LABELS <- c(
    handgrip_max_all         = "Grip strength (kg)",
    ALM_HT2                  = "ALMI (kg/m\u00b2)",
    gait_speed               = "Gait speed (m/s)",
    dairy_cumavg             = "Cumulative dairy (g/day)",
    dairy_total_gday         = "Instantaneous dairy (g/day)",
    dairy_fermented_gday     = "Fermented dairy (g/day)",
    dairy_non_fermented_gday = "Non-fermented dairy (g/day)",
    dairy_lowfat_gday        = "Low-fat dairy (g/day)",
    dairy_highfat_gday       = "High-fat dairy (g/day)",
    energy_kcal              = "Energy intake (kcal/day)",
    protein_pct              = "Protein (% energy)",
    Age                      = "Age (yr)",
    BMI                      = "BMI (kg/m\u00b2)",
    Height                   = "Height (cm)",
    Weight                   = "Weight (kg)",
    education_level          = "Education level",
    sbsmk                    = "Smoking status",
    alcohol_category         = "Alcohol intake",
    pa_levels                = "Physical activity",
    diabetes_status          = "Diabetes status",
    HTN_status               = "Hypertension",
    hrt_status               = "HRT status",
    corticoids_status        = "Corticosteroids",
    calcium_status           = "Calcium supplement",
    vitD_status              = "Vitamin D supplement",
    bisphosphonate_status    = "Bisphosphonate",
    BMI_category             = "BMI category"
)


# =============================================================================
# 2. MARGINAL HISTOGRAM FUNCTIONS
# =============================================================================

#' Plot marginal histograms for all continuous variables at Baseline.
#'
#' One histogram per variable, coloured by OsteoLaus wave. Panels are arranged
#' on a fixed grid. Variables absent from analysis_long are skipped silently.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @param waves Character vector of waves to include. Default all waves.
#' @param bins  Number of histogram bins. Default 35.
#' @return A patchwork plot object.
plot_continuous_histograms <- function(analysis_long,
                                       waves = NULL,
                                       bins  = 35L) {
    
    df <- analysis_long
    if (!is.null(waves))
        df <- dplyr::filter(df, osteo_wave %in% waves)
    
    present <- intersect(.HIST_CONTINUOUS, names(df))
    if (length(present) == 0L) {
        cli::cli_warn("plot_continuous_histograms(): no matching columns found.")
        return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    
    df_long <- df |>
        dplyr::select(pt, osteo_wave, osteo_wave_num, dplyr::all_of(present)) |>
        tidyr::pivot_longer(
            cols      = dplyr::all_of(present),
            names_to  = "variable",
            values_to = "value"
        ) |>
        dplyr::filter(!is.na(value)) |>
        dplyr::mutate(
            label = dplyr::recode(variable, !!!.VAR_LABELS),
            label = factor(label, levels = .VAR_LABELS[intersect(names(.VAR_LABELS), present)]),
            wave  = forcats::fct_reorder(osteo_wave, osteo_wave_num)
        )
    
    # Per-variable median lines for annotation
    medians <- df_long |>
        dplyr::group_by(label, wave) |>
        dplyr::summarise(med = median(value, na.rm = TRUE), .groups = "drop")
    
    ggplot2::ggplot(df_long, ggplot2::aes(x = value, fill = wave)) +
        ggplot2::geom_histogram(
            bins     = bins,
            colour   = "white",
            linewidth = 0.15,
            alpha    = 0.75,
            position = "identity"
        ) +
        ggplot2::geom_vline(
            data        = medians,
            ggplot2::aes(xintercept = med, colour = wave),
            linetype    = "dashed",
            linewidth   = 0.5,
            show.legend = FALSE
        ) +
        ggplot2::facet_wrap(~ label, scales = "free", ncol = 4L) +
        ggplot2::scale_fill_brewer(palette = "Set2",  name = "Wave") +
        ggplot2::scale_colour_brewer(palette = "Set2") +
        ggplot2::labs(
            title    = "Marginal distributions — continuous variables",
            subtitle = "Dashed lines = per-wave medians. Waves overlaid.",
            x        = NULL,
            y        = "Count",
            caption  = glue::glue(
                "n rows: {scales::comma(nrow(df_long))} | ",
                "Waves: {paste(levels(df_long$wave), collapse = ', ')}"
            )
        ) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(
            legend.position  = "bottom",
            strip.text       = ggplot2::element_text(size = 8, face = "bold"),
            axis.text.x      = ggplot2::element_text(size = 7),
            axis.text.y      = ggplot2::element_text(size = 7),
            panel.grid.minor = ggplot2::element_blank()
        )
}


#' Plot marginal bar charts for all categorical variables at Baseline.
#'
#' One bar chart per variable, showing % in each category per wave.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @param waves Character vector of waves to include. Default all waves.
#' @return A patchwork plot object.
plot_categorical_bars <- function(analysis_long,
                                  waves = NULL) {
    
    df <- analysis_long
    if (!is.null(waves))
        df <- dplyr::filter(df, osteo_wave %in% waves)
    
    present <- intersect(.HIST_CATEGORICAL, names(df))
    if (length(present) == 0L) {
        cli::cli_warn("plot_categorical_bars(): no matching columns found.")
        return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    
    df_long <- df |>
        dplyr::select(pt, osteo_wave, osteo_wave_num, dplyr::all_of(present)) |>
        dplyr::mutate(
            dplyr::across(dplyr::all_of(present), as.character)
        ) |>
        tidyr::pivot_longer(
            cols      = dplyr::all_of(present),
            names_to  = "variable",
            values_to = "value"
        ) |>
        dplyr::filter(!is.na(value)) |>
        dplyr::mutate(
            label = dplyr::recode(variable, !!!.VAR_LABELS),
            label = factor(label, levels = .VAR_LABELS[intersect(names(.VAR_LABELS), present)]),
            wave  = forcats::fct_reorder(osteo_wave, osteo_wave_num)
        )
    
    pct_df <- df_long |>
        dplyr::count(label, wave, value) |>
        dplyr::group_by(label, wave) |>
        dplyr::mutate(pct = n / sum(n) * 100) |>
        dplyr::ungroup()
    
    ggplot2::ggplot(
        pct_df,
        ggplot2::aes(x = value, y = pct, fill = wave)
    ) +
        ggplot2::geom_col(
            position  = ggplot2::position_dodge(width = 0.8),
            width     = 0.7,
            colour    = "white",
            linewidth  = 0.15
        ) +
        ggplot2::facet_wrap(~ label, scales = "free_x", ncol = 3L) +
        ggplot2::scale_fill_brewer(palette = "Set2", name = "Wave") +
        ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        ggplot2::labs(
            title    = "Marginal distributions — categorical variables",
            subtitle = "% of non-missing observations per wave",
            x        = NULL,
            y        = "% participants"
        ) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(
            legend.position  = "bottom",
            strip.text       = ggplot2::element_text(size = 8, face = "bold"),
            axis.text.x      = ggplot2::element_text(size = 7, angle = 30, hjust = 1),
            axis.text.y      = ggplot2::element_text(size = 7),
            panel.grid.minor = ggplot2::element_blank()
        )
}


#' Exposure-outcome scatter matrix with LOESS smoother.
#'
#' Plots dairy_cumavg on the x-axis against each continuous outcome on y,
#' facetted by wave, with a LOESS smoother per wave.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A patchwork plot object.
plot_exposure_outcome_scatter <- function(analysis_long) {
    
    outcomes <- c("handgrip_max_all", "ALM_HT2", "gait_speed")
    present  <- intersect(outcomes, names(analysis_long))
    
    if (!"dairy_cumavg" %in% names(analysis_long) || length(present) == 0L) {
        cli::cli_warn("plot_exposure_outcome_scatter(): required columns absent.")
        return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    
    plots <- purrr::map(present, function(outcome) {
        
        df <- analysis_long |>
            dplyr::filter(!is.na(dairy_cumavg), !is.na(.data[[outcome]])) |>
            dplyr::mutate(
                wave = forcats::fct_reorder(osteo_wave, osteo_wave_num)
            )
        
        y_label <- .VAR_LABELS[outcome]
        if (is.na(y_label)) y_label <- outcome
        
        ggplot2::ggplot(
            df, ggplot2::aes(x = dairy_cumavg, y = .data[[outcome]],
                             colour = wave)
        ) +
            ggplot2::geom_point(alpha = 0.12, size = 0.6) +
            ggplot2::geom_smooth(
                method    = "loess",
                formula   = y ~ x,
                se        = TRUE,
                linewidth  = 0.8,
                alpha     = 0.15
            ) +
            ggplot2::facet_wrap(~ wave, nrow = 1L) +
            ggplot2::scale_colour_brewer(palette = "Set1", guide = "none") +
            ggplot2::labs(
                title = y_label,
                x     = "Cumulative dairy intake (g/day)",
                y     = y_label
            ) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(
                plot.title = ggplot2::element_text(size = 10, face = "bold")
            )
    })
    
    patchwork::wrap_plots(plots, ncol = 1L) +
        patchwork::plot_annotation(
            title    = "Exposure-outcome associations by wave (LOESS)",
            subtitle = "x = cumulative dairy intake; lines = per-wave LOESS smoother",
            theme    = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 12, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
}


# =============================================================================
# 3. MINIMAL MODEL FITTING
# =============================================================================
# Minimal = M1 covariates only: Age + BMI (or Height for ALM) + energy_kcal
# No protein_pct, pa_levels, clinical covariates.
# Useful to verify direction and order-of-magnitude before full adjustment.
# =============================================================================

#' Fit a minimal LME model for grip strength ~ dairy_cumavg.
#'
#' Formula:
#'   handgrip_max_all ~ dairy_cumavg + time_since_bsl_yr + Age + BMI +
#'                      energy_kcal + (1 + time_since_bsl_yr | pt)
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @param random_slope  Logical. Include random slope for time? Default TRUE.
#'   Set FALSE if singular fit.
#' @return Named list: $fit (lmerMod, REML), $data, $formula, $n_pt, $n_obs.
fit_minimal_grip <- function(analysis_long,
                             random_slope = TRUE) {
    
    required <- c("handgrip_max_all", "dairy_cumavg", "time_since_bsl_yr",
                  "Age", "BMI", "energy_kcal")
    
    df <- analysis_long |>
        dplyr::filter(
            eligible_hgs,
            !is.na(handgrip_max_all),
            !is.na(dairy_cumavg),
            dplyr::if_all(dplyr::any_of(required), ~ !is.na(.x))
        ) |>
        # Standardise continuous covariates for convergence
        dplyr::mutate(
            Age_z        = as.numeric(scale(Age)),
            BMI_z        = as.numeric(scale(BMI)),
            energy_z     = as.numeric(scale(energy_kcal)),
            dairy_cumavg = dairy_cumavg  # exposure kept on original scale
        )
    
    # Minimum 2 obs per participant
    df <- df |>
        dplyr::group_by(pt) |>
        dplyr::filter(dplyr::n() >= 2L) |>
        dplyr::ungroup()
    
    re_term <- if (random_slope) "(1 + time_since_bsl_yr | pt)" else "(1 | pt)"
    
    f <- stats::as.formula(glue::glue(
        "handgrip_max_all ~ dairy_cumavg + time_since_bsl_yr + ",
        "Age_z + BMI_z + energy_z + {re_term}"
    ))
    
    cli::cli_inform(c("i" = "fit_minimal_grip(): {deparse(f)}",
                      "*" = "N = {dplyr::n_distinct(df$pt)} pts | {nrow(df)} obs"))
    
    fit <- lmerTest::lmer(
        formula = f,
        data    = df,
        REML    = TRUE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    if (lme4::isSingular(fit) && random_slope) {
        cli::cli_warn(c(
            "!" = "fit_minimal_grip(): singular fit with random slope.",
            "i" = "Re-run with random_slope = FALSE."
        ))
    }
    
    list(
        fit     = fit,
        data    = df,
        formula = f,
        n_pt    = dplyr::n_distinct(df$pt),
        n_obs   = nrow(df)
    )
}


#' Fit a minimal LME model for ALMI ~ dairy_cumavg.
#'
#' Height replaces BMI (collinearity with ALMI — see 00_utils_models.R).
#'
#' Formula:
#'   ALM_HT2 ~ dairy_cumavg + time_since_bsl_yr + Age + Height +
#'             energy_kcal + (1 + time_since_bsl_yr | pt)
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @param random_slope  Logical. Include random slope for time? Default TRUE.
#' @return Named list: $fit, $data, $formula, $n_pt, $n_obs.
fit_minimal_alm <- function(analysis_long,
                            random_slope = TRUE) {
    
    required <- c("ALM_HT2", "dairy_cumavg", "time_since_bsl_yr",
                  "Age", "Height", "energy_kcal")
    
    df <- analysis_long |>
        dplyr::filter(
            eligible_alm,
            !is.na(ALM_HT2),
            !is.na(dairy_cumavg),
            dplyr::if_all(dplyr::any_of(required), ~ !is.na(.x))
        ) |>
        dplyr::mutate(
            Age_z    = as.numeric(scale(Age)),
            Height_z = as.numeric(scale(Height)),
            energy_z = as.numeric(scale(energy_kcal))
        )
    
    df <- df |>
        dplyr::group_by(pt) |>
        dplyr::filter(dplyr::n() >= 2L) |>
        dplyr::ungroup()
    
    re_term <- if (random_slope) "(1 + time_since_bsl_yr | pt)" else "(1 | pt)"
    
    f <- stats::as.formula(glue::glue(
        "ALM_HT2 ~ dairy_cumavg + time_since_bsl_yr + ",
        "Age_z + Height_z + energy_z + {re_term}"
    ))
    
    cli::cli_inform(c("i" = "fit_minimal_alm(): {deparse(f)}",
                      "*" = "N = {dplyr::n_distinct(df$pt)} pts | {nrow(df)} obs"))
    
    fit <- lmerTest::lmer(
        formula = f,
        data    = df,
        REML    = TRUE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    if (lme4::isSingular(fit) && random_slope) {
        cli::cli_warn(c(
            "!" = "fit_minimal_alm(): singular fit with random slope.",
            "i" = "Re-run with random_slope = FALSE."
        ))
    }
    
    list(
        fit     = fit,
        data    = df,
        formula = f,
        n_pt    = dplyr::n_distinct(df$pt),
        n_obs   = nrow(df)
    )
}


#' Fit a minimal LME model for gait speed ~ dairy_cumavg.
#'
#' Gait speed is measured at V4 and V5 only (max 2 obs/pt).
#' Random intercept only — random slope is not estimable.
#'
#' Formula:
#'   gait_speed ~ dairy_cumavg + time_since_bsl_yr + Age + BMI +
#'                energy_kcal + (1 | pt)
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return Named list: $fit, $data, $formula, $n_pt, $n_obs.
fit_minimal_gait <- function(analysis_long) {
    
    required <- c("gait_speed", "dairy_cumavg", "time_since_bsl_yr",
                  "Age", "BMI", "energy_kcal")
    
    df <- analysis_long |>
        dplyr::filter(
            eligible_gait,
            osteo_wave %in% c("V4", "V5"),
            !is.na(gait_speed),
            !is.na(dairy_cumavg),
            dplyr::if_all(dplyr::any_of(required), ~ !is.na(.x))
        ) |>
        dplyr::mutate(
            Age_z    = as.numeric(scale(Age)),
            BMI_z    = as.numeric(scale(BMI)),
            energy_z = as.numeric(scale(energy_kcal))
        )
    
    f <- stats::as.formula(
        "gait_speed ~ dairy_cumavg + time_since_bsl_yr + Age_z + BMI_z + energy_z + (1 | pt)"
    )
    
    cli::cli_inform(c("i" = "fit_minimal_gait(): {deparse(f)}",
                      "*" = "N = {dplyr::n_distinct(df$pt)} pts | {nrow(df)} obs"))
    
    fit <- lmerTest::lmer(
        formula = f,
        data    = df,
        REML    = TRUE,
        control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    list(
        fit     = fit,
        data    = df,
        formula = f,
        n_pt    = dplyr::n_distinct(df$pt),
        n_obs   = nrow(df)
    )
}


# =============================================================================
# 4. COEFFICIENT SUMMARY TABLE
# =============================================================================

#' Print a tidy summary of a minimal model fit.
#'
#' Extracts fixed effects with 95% CI and flags the dairy term.
#'
#' @param fit_obj  Named list from fit_minimal_*().
#' @param outcome  Character label for printing.
#' @return Tibble with columns: term, estimate, se, ci_lo, ci_hi, p_value.
summarise_minimal_fit <- function(fit_obj, outcome = "outcome") {
    
    coef_df <- as.data.frame(summary(fit_obj$fit)$coefficients)
    coef_df <- tibble::rownames_to_column(coef_df, "term")
    
    result <- tibble::tibble(
        outcome   = outcome,
        term      = coef_df$term,
        estimate  = round(coef_df$Estimate,     4),
        se        = round(coef_df[["Std. Error"]], 4),
        ci_lo     = round(coef_df$Estimate - 1.96 * coef_df[["Std. Error"]], 4),
        ci_hi     = round(coef_df$Estimate + 1.96 * coef_df[["Std. Error"]], 4),
        p_value   = coef_df[["Pr(>|t|)"]],
        dairy_term = grepl("dairy_cumavg", coef_df$term, fixed = TRUE)
    )
    
    cli::cli_inform(c(
        "v" = "Minimal model [{outcome}]: {fit_obj$n_pt} pts / {fit_obj$n_obs} obs",
        "*" = paste(
            capture.output(
                print(dplyr::select(result, term, estimate, ci_lo, ci_hi, p_value),
                      n = Inf)
            ),
            collapse = "\n"
        )
    ))
    
    result
}


#' Combine coefficient summaries from all three minimal models.
#'
#' @param grip_fit  From fit_minimal_grip().
#' @param alm_fit   From fit_minimal_alm().
#' @param gait_fit  From fit_minimal_gait().
#' @return Tibble with one row per outcome × term.
make_minimal_coef_table <- function(grip_fit, alm_fit, gait_fit) {
    dplyr::bind_rows(
        summarise_minimal_fit(grip_fit, "Grip strength (kg)"),
        summarise_minimal_fit(alm_fit,  "ALMI (kg/m\u00b2)"),
        summarise_minimal_fit(gait_fit, "Gait speed (m/s)")
    ) |>
        dplyr::arrange(outcome, term)
}


# =============================================================================
# 5. MINIMAL MODEL DIAGNOSTIC PLOTS
# =============================================================================

#' Quick residual plots for a minimal LME fit.
#'
#' Two-panel: Tukey-Anscombe + Q-Q residuals.
#'
#' @param fit_obj  Named list from fit_minimal_*().
#' @param title    Plot title.
#' @return A patchwork plot.
plot_minimal_diagnostics <- function(fit_obj, title = "Minimal model diagnostics") {
    
    fit <- fit_obj$fit
    df  <- tibble::tibble(
        fitted   = stats::fitted(fit),
        residual = stats::residuals(fit)
    ) |>
        dplyr::mutate(resid_std = as.numeric(scale(residual)))
    
    p_ta <- ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = residual)) +
        ggplot2::geom_point(alpha = 0.2, size = 0.7, colour = "grey30") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "tomato", linewidth = 0.5) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                             colour = "#2E86AB", fill = "#2E86AB",
                             alpha = 0.15, linewidth = 0.6) +
        ggplot2::labs(title = "Tukey-Anscombe", x = "Fitted", y = "Residuals") +
        ggplot2::theme_minimal(base_size = 10)
    
    p_qq <- ggplot2::ggplot(df, ggplot2::aes(sample = resid_std)) +
        ggplot2::stat_qq(alpha = 0.2, size = 0.7, colour = "grey30") +
        ggplot2::stat_qq_line(colour = "tomato", linetype = "dashed",
                              linewidth = 0.5) +
        ggplot2::labs(title = "Q-Q residuals",
                      x = "Theoretical quantiles", y = "Std. residuals") +
        ggplot2::theme_minimal(base_size = 10)
    
    (p_ta | p_qq) +
        patchwork::plot_annotation(
            title = title,
            theme = ggplot2::theme(
                plot.title = ggplot2::element_text(size = 11, face = "bold")
            )
        )
}


# =============================================================================
# 6. DAIRY × TIME INTERACTION MODELS
# =============================================================================
# Secondary hypothesis: does higher cumulative dairy intake attenuate the rate
# of muscle decline over time?
#
# Each minimal model is re-fitted adding the dairy_cumavg * time_since_bsl_yr
# product term. The interaction coefficient answers: "for every 100 g/day more
# dairy, how much does the annual rate of change differ?"
#
# A likelihood-ratio test (ML fits) compares the main-effect model against the
# interaction model. A significant LRT (p < 0.05) means dairy modifies the
# trajectory — not just the average level.
#
# Gait note: only 2 time points (V4, V5) so the interaction is estimable but
# has very low power; interpret with caution.
# =============================================================================

#' Fit the dairy × time interaction model for grip strength.
#'
#' Extends fit_minimal_grip() by adding dairy_cumavg * time_since_bsl_yr.
#' Returns both REML (for reporting) and ML (for LRT) fits.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @param random_slope  Logical. Include random slope for time? Default TRUE.
#' @return Named list: $fit_reml, $fit_ml, $fit_main_ml (main-effect ML for
#'   LRT), $data, $formula, $lrt, $n_pt, $n_obs.
fit_interaction_grip <- function(analysis_long,
                                 random_slope = TRUE) {
    
    required <- c("handgrip_max_all", "dairy_cumavg", "time_since_bsl_yr",
                  "Age", "BMI", "energy_kcal")
    
    df <- analysis_long |>
        dplyr::filter(
            eligible_hgs,
            !is.na(handgrip_max_all),
            !is.na(dairy_cumavg),
            dplyr::if_all(dplyr::any_of(required), ~ !is.na(.x))
        ) |>
        dplyr::mutate(
            Age_z        = as.numeric(scale(Age)),
            BMI_z        = as.numeric(scale(BMI)),
            energy_z     = as.numeric(scale(energy_kcal))
        ) |>
        dplyr::group_by(pt) |>
        dplyr::filter(dplyr::n() >= 2L) |>
        dplyr::ungroup()
    
    re_term <- if (random_slope) "(1 + time_since_bsl_yr | pt)" else "(1 | pt)"
    
    # Interaction formula: * expands to main effects + product term
    f_int  <- stats::as.formula(glue::glue(
        "handgrip_max_all ~ dairy_cumavg * time_since_bsl_yr + ",
        "Age_z + BMI_z + energy_z + {re_term}"
    ))
    f_main <- stats::as.formula(glue::glue(
        "handgrip_max_all ~ dairy_cumavg + time_since_bsl_yr + ",
        "Age_z + BMI_z + energy_z + {re_term}"
    ))
    
    cli::cli_inform(c("i" = "fit_interaction_grip(): {deparse(f_int)}"))
    
    fit_reml    <- lmerTest::lmer(f_int,  data = df, REML = TRUE,
                                  control = lme4::lmerControl(optimizer = "bobyqa"))
    fit_ml      <- lmerTest::lmer(f_int,  data = df, REML = FALSE,
                                  control = lme4::lmerControl(optimizer = "bobyqa"))
    fit_main_ml <- lmerTest::lmer(f_main, data = df, REML = FALSE,
                                  control = lme4::lmerControl(optimizer = "bobyqa"))
    
    lrt <- anova(fit_main_ml, fit_ml)
    
    if (lme4::isSingular(fit_reml) && random_slope)
        cli::cli_warn("fit_interaction_grip(): singular fit — consider random_slope = FALSE.")
    
    list(
        fit_reml    = fit_reml,
        fit_ml      = fit_ml,
        fit_main_ml = fit_main_ml,
        data        = df,
        formula     = f_int,
        lrt         = lrt,
        n_pt        = dplyr::n_distinct(df$pt),
        n_obs       = nrow(df)
    )
}


#' Fit the dairy × time interaction model for ALMI.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @param random_slope  Logical. Default TRUE.
#' @return Named list: $fit_reml, $fit_ml, $fit_main_ml, $data, $formula,
#'   $lrt, $n_pt, $n_obs.
fit_interaction_alm <- function(analysis_long,
                                random_slope = TRUE) {
    
    required <- c("ALM_HT2", "dairy_cumavg", "time_since_bsl_yr",
                  "Age", "Height", "energy_kcal")
    
    df <- analysis_long |>
        dplyr::filter(
            eligible_alm,
            !is.na(ALM_HT2),
            !is.na(dairy_cumavg),
            dplyr::if_all(dplyr::any_of(required), ~ !is.na(.x))
        ) |>
        dplyr::mutate(
            Age_z    = as.numeric(scale(Age)),
            Height_z = as.numeric(scale(Height)),
            energy_z = as.numeric(scale(energy_kcal))
        ) |>
        dplyr::group_by(pt) |>
        dplyr::filter(dplyr::n() >= 2L) |>
        dplyr::ungroup()
    
    re_term <- if (random_slope) "(1 + time_since_bsl_yr | pt)" else "(1 | pt)"
    
    f_int  <- stats::as.formula(glue::glue(
        "ALM_HT2 ~ dairy_cumavg * time_since_bsl_yr + ",
        "Age_z + Height_z + energy_z + {re_term}"
    ))
    f_main <- stats::as.formula(glue::glue(
        "ALM_HT2 ~ dairy_cumavg + time_since_bsl_yr + ",
        "Age_z + Height_z + energy_z + {re_term}"
    ))
    
    cli::cli_inform(c("i" = "fit_interaction_alm(): {deparse(f_int)}"))
    
    fit_reml    <- lmerTest::lmer(f_int,  data = df, REML = TRUE,
                                  control = lme4::lmerControl(optimizer = "bobyqa"))
    fit_ml      <- lmerTest::lmer(f_int,  data = df, REML = FALSE,
                                  control = lme4::lmerControl(optimizer = "bobyqa"))
    fit_main_ml <- lmerTest::lmer(f_main, data = df, REML = FALSE,
                                  control = lme4::lmerControl(optimizer = "bobyqa"))
    
    lrt <- anova(fit_main_ml, fit_ml)
    
    if (lme4::isSingular(fit_reml) && random_slope)
        cli::cli_warn("fit_interaction_alm(): singular fit — consider random_slope = FALSE.")
    
    list(
        fit_reml    = fit_reml,
        fit_ml      = fit_ml,
        fit_main_ml = fit_main_ml,
        data        = df,
        formula     = f_int,
        lrt         = lrt,
        n_pt        = dplyr::n_distinct(df$pt),
        n_obs       = nrow(df)
    )
}


#' Fit the dairy × time interaction model for gait speed.
#'
#' Random intercept only (max 2 obs per participant at V4/V5).
#' The interaction term dairy_cumavg * time_since_bsl_yr tests whether higher
#' dairy is associated with a different V4→V5 change in gait speed.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return Named list: $fit_reml, $fit_ml, $fit_main_ml, $data, $formula,
#'   $lrt, $n_pt, $n_obs.
fit_interaction_gait <- function(analysis_long) {
    
    required <- c("gait_speed", "dairy_cumavg", "time_since_bsl_yr",
                  "Age", "BMI", "energy_kcal")
    
    df <- analysis_long |>
        dplyr::filter(
            eligible_gait,
            osteo_wave %in% c("V4", "V5"),
            !is.na(gait_speed),
            !is.na(dairy_cumavg),
            dplyr::if_all(dplyr::any_of(required), ~ !is.na(.x))
        ) |>
        dplyr::mutate(
            Age_z    = as.numeric(scale(Age)),
            BMI_z    = as.numeric(scale(BMI)),
            energy_z = as.numeric(scale(energy_kcal))
        )
    
    f_int  <- stats::as.formula(
        "gait_speed ~ dairy_cumavg * time_since_bsl_yr + Age_z + BMI_z + energy_z + (1 | pt)"
    )
    f_main <- stats::as.formula(
        "gait_speed ~ dairy_cumavg + time_since_bsl_yr + Age_z + BMI_z + energy_z + (1 | pt)"
    )
    
    cli::cli_inform(c(
        "i" = "fit_interaction_gait(): {deparse(f_int)}",
        "!" = "Only 2 time points (V4, V5) — interaction has low power."
    ))
    
    fit_reml    <- lmerTest::lmer(f_int,  data = df, REML = TRUE,
                                  control = lme4::lmerControl(optimizer = "bobyqa"))
    fit_ml      <- lmerTest::lmer(f_int,  data = df, REML = FALSE,
                                  control = lme4::lmerControl(optimizer = "bobyqa"))
    fit_main_ml <- lmerTest::lmer(f_main, data = df, REML = FALSE,
                                  control = lme4::lmerControl(optimizer = "bobyqa"))
    
    lrt <- anova(fit_main_ml, fit_ml)
    
    list(
        fit_reml    = fit_reml,
        fit_ml      = fit_ml,
        fit_main_ml = fit_main_ml,
        data        = df,
        formula     = f_int,
        lrt         = lrt,
        n_pt        = dplyr::n_distinct(df$pt),
        n_obs       = nrow(df)
    )
}


# =============================================================================
# 6b. INTERACTION SUMMARY TABLE
# =============================================================================

#' Extract the interaction term from one fit and return a one-row tibble.
#'
#' @param fit_obj  From fit_interaction_*().
#' @param outcome  Label string.
#' @return One-row tibble: outcome, estimate, se, ci_lo, ci_hi, p_value,
#'   lrt_p (LRT p-value comparing main-effect vs interaction model).
summarise_interaction <- function(fit_obj, outcome) {
    
    coef_df <- as.data.frame(summary(fit_obj$fit_reml)$coefficients)
    coef_df <- tibble::rownames_to_column(coef_df, "term")
    
    int_row <- dplyr::filter(
        coef_df,
        grepl("dairy_cumavg:time_since_bsl_yr", term, fixed = TRUE)
    )
    
    lrt_p <- tryCatch(
        as.data.frame(fit_obj$lrt)[2, "Pr(>Chisq)"],
        error = function(e) NA_real_
    )
    
    if (nrow(int_row) == 0L) {
        cli::cli_warn("summarise_interaction(): interaction term not found for {outcome}.")
        return(tibble::tibble(
            outcome = outcome, term = "dairy_cumavg:time_since_bsl_yr",
            estimate = NA_real_, se = NA_real_,
            ci_lo = NA_real_, ci_hi = NA_real_,
            p_value = NA_real_, lrt_p = lrt_p
        ))
    }
    
    beta <- int_row$Estimate
    se   <- int_row[["Std. Error"]]
    pval <- int_row[["Pr(>|t|)"]]
    
    cli::cli_inform(c(
        "v" = "Interaction [{outcome}]: beta = {round(beta, 5)}, p = {round(pval, 4)}, LRT p = {round(lrt_p, 4)}"
    ))
    
    tibble::tibble(
        outcome  = outcome,
        term     = "dairy_cumavg:time_since_bsl_yr",
        estimate = round(beta,           5),
        se       = round(se,             5),
        ci_lo    = round(beta - 1.96*se, 5),
        ci_hi    = round(beta + 1.96*se, 5),
        p_value  = pval,
        lrt_p    = lrt_p
    )
}


#' Combine interaction summaries across all three outcomes.
#'
#' @param grip_int  From fit_interaction_grip().
#' @param alm_int   From fit_interaction_alm().
#' @param gait_int  From fit_interaction_gait().
#' @return Tibble with one row per outcome.
make_interaction_table <- function(grip_int, alm_int, gait_int) {
    dplyr::bind_rows(
        summarise_interaction(grip_int, "Grip strength (kg)"),
        summarise_interaction(alm_int,  "ALMI (kg/m\u00b2)"),
        summarise_interaction(gait_int, "Gait speed (m/s)")
    )
}


# =============================================================================
# 6c. INTERACTION TRAJECTORY PLOT
# =============================================================================

#' Predicted outcome trajectories by dairy intake quartile.
#'
#' Uses the interaction model to predict the outcome at a grid of time points
#' for four representative dairy intake levels (Q1–Q4 medians). Other
#' covariates are held at their sample median/mode. Visualises whether higher
#' dairy is associated with a flatter decline slope.
#'
#' @param fit_obj      From fit_interaction_*().
#' @param outcome_col  Column name of the outcome in fit_obj$data.
#' @param outcome_lbl  Y-axis label string.
#' @param time_col     Time column. Default "time_since_bsl_yr".
#' @return A ggplot object.
plot_interaction_trajectories <- function(fit_obj,
                                          outcome_col,
                                          outcome_lbl,
                                          time_col = "time_since_bsl_yr") {
    
    df  <- fit_obj$data
    fit <- fit_obj$fit_reml
    
    # Dairy quartile boundaries from the fitting data
    q_breaks <- quantile(df$dairy_cumavg, probs = c(0, .25, .5, .75, 1),
                         na.rm = TRUE)
    q_meds <- tapply(
        df$dairy_cumavg,
        cut(df$dairy_cumavg, breaks = q_breaks, include.lowest = TRUE),
        median, na.rm = TRUE
    )
    q_labels <- paste0("Q", 1:4, " (", round(q_meds, 0), " g/day)")
    
    # Time grid spanning the observed range
    t_range <- range(df[[time_col]], na.rm = TRUE)
    t_grid  <- seq(t_range[1], t_range[2], length.out = 50L)
    
    # Hold nuisance covariates at median (numeric) — works for z-scored columns
    nuisance_cols <- setdiff(
        names(df),
        c("pt", outcome_col, "dairy_cumavg", time_col,
          ".cohort", ".wave", ".wave_num", "osteo_wave", "osteo_wave_num",
          "exam_date", "n_obs")
    )
    nuisance_cols <- nuisance_cols[
        vapply(nuisance_cols, function(x) is.numeric(df[[x]]), logical(1))
    ]
    
    median_row <- purrr::map_dfc(nuisance_cols, function(col) {
        tibble::tibble(!!col := median(df[[col]], na.rm = TRUE))
    })
    
    # Build prediction grid: 4 quartiles × 50 time points
    pred_grid <- purrr::imap_dfr(q_meds, function(dairy_val, q_label) {
        base <- tibble::tibble(
            dairy_cumavg = dairy_val,
            !!time_col   := t_grid,
            quartile     = q_label
        )
        if (ncol(median_row) > 0L)
            base <- dplyr::bind_cols(base, median_row[rep(1L, nrow(base)), ])
        base
    }) |>
        dplyr::mutate(
            quartile = factor(quartile, levels = q_labels[order(q_meds)])
        )
    
    # Population-average predictions (re.form = NA suppresses random effects)
    pred_grid$predicted <- tryCatch(
        predict(fit, newdata = pred_grid, re.form = NA),
        error = function(e) {
            cli::cli_warn("plot_interaction_trajectories(): predict() failed — {conditionMessage(e)}")
            rep(NA_real_, nrow(pred_grid))
        }
    )
    
    # Observed data: individual trajectories, thin and transparent
    obs_df <- df |>
        dplyr::mutate(
            dairy_q = cut(dairy_cumavg, breaks = q_breaks, include.lowest = TRUE,
                          labels = q_labels[order(q_meds)])
        ) |>
        dplyr::filter(!is.na(dairy_q), !is.na(.data[[outcome_col]]))
    
    ggplot2::ggplot() +
        # Individual observed trajectories (light background)
        ggplot2::geom_line(
            data    = obs_df,
            mapping = ggplot2::aes(
                x     = .data[[time_col]],
                y     = .data[[outcome_col]],
                group = pt,
                colour = dairy_q
            ),
            alpha     = 0.08,
            linewidth  = 0.25
        ) +
        # Population-average predicted trajectories (bold foreground)
        ggplot2::geom_line(
            data    = pred_grid,
            mapping = ggplot2::aes(
                x      = .data[[time_col]],
                y      = predicted,
                colour = quartile
            ),
            linewidth = 1.1
        ) +
        ggplot2::scale_colour_manual(
            values = c("#2D6A4F", "#52B788", "#F4A261", "#E76F51"),
            name   = "Dairy quartile"
        ) +
        ggplot2::labs(
            title    = glue::glue("{outcome_lbl}: predicted trajectories by dairy quartile"),
            subtitle = "Bold lines = population-average prediction; faint lines = individual observed",
            x        = "Time since Baseline (yr)",
            y        = outcome_lbl,
            caption  = "Covariates held at sample median. Interaction model (M1 covariates)."
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "right")
}


# =============================================================================
# 7. TOP-LEVEL WRAPPER
# =============================================================================

#' Run the full exploration: histograms + minimal models + interaction models.
#'
#' Creates 06_outputs/explore/ and writes all plots and coefficient tables.
#'
#' Output files
#' ────────────
#'   01_histograms_continuous.*          Marginal histograms, continuous vars
#'   02_barplots_categorical.*           Bar charts, categorical vars
#'   03_scatter_exposure_outcomes.*      LOESS scatter per outcome × wave
#'   04_minimal_model_coefficients.csv   Fixed effects from M1 models
#'   05_diagnostics_minimal_models.*     Tukey-Anscombe + Q-Q per outcome
#'   06_interaction_coefficients.csv     Interaction term + LRT per outcome
#'   07_interaction_trajectories.*       Predicted trajectories by dairy Q1-Q4
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @param out_dir       Output directory. Default "06_outputs/explore".
#' @param device        Plot device: "png" or "pdf". Default "png".
#' @param width         Plot width in inches. Default 14.
#' @param height_cont   Height for continuous histogram panel. Default 12.
#' @param height_cat    Height for categorical bar chart panel. Default 10.
#' @return Invisibly returns a named list of all fit objects and tables.
run_exploration <- function(analysis_long,
                            out_dir     = "06_outputs/explore",
                            device      = "png",
                            width       = 14,
                            height_cont = 12,
                            height_cat  = 10) {
    
    .check_pkgs()
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    .save <- function(p, name, height) {
        path <- file.path(out_dir, paste0(name, ".", device))
        ggplot2::ggsave(path, plot = p, width = width, height = height,
                        dpi = 150)
        cli::cli_inform(c("v" = "Saved: {.path {path}}"))
        invisible(path)
    }
    
    cli::cli_h1("12_explore_minimal_models: starting exploration")
    
    # ── 1/7  Continuous histograms ───────────────────────────────────────────
    cli::cli_h2("1/7  Continuous histograms")
    p_cont <- plot_continuous_histograms(analysis_long)
    .save(p_cont, "01_histograms_continuous", height_cont)
    
    # ── 2/7  Categorical bar charts ──────────────────────────────────────────
    cli::cli_h2("2/7  Categorical bar charts")
    p_cat <- plot_categorical_bars(analysis_long)
    .save(p_cat, "02_barplots_categorical", height_cat)
    
    # ── 3/7  Exposure-outcome scatter ────────────────────────────────────────
    cli::cli_h2("3/7  Exposure-outcome scatter")
    p_scatter <- plot_exposure_outcome_scatter(analysis_long)
    .save(p_scatter, "03_scatter_exposure_outcomes", height = 10)
    
    # ── 4/7  Minimal main-effect models ─────────────────────────────────────
    cli::cli_h2("4/7  Minimal main-effect models")
    
    grip_fit <- fit_minimal_grip(analysis_long)
    alm_fit  <- fit_minimal_alm(analysis_long)
    gait_fit <- fit_minimal_gait(analysis_long)
    
    coef_table <- make_minimal_coef_table(grip_fit, alm_fit, gait_fit)
    coef_path  <- file.path(out_dir, "04_minimal_model_coefficients.csv")
    utils::write.csv(coef_table, coef_path, row.names = FALSE)
    cli::cli_inform(c("v" = "Coefficient table saved: {.path {coef_path}}"))
    
    # ── 5/7  Residual diagnostics ────────────────────────────────────────────
    cli::cli_h2("5/7  Residual diagnostic plots")
    
    p_diag_all <- patchwork::wrap_plots(
        plot_minimal_diagnostics(grip_fit, "Grip: minimal model diagnostics"),
        plot_minimal_diagnostics(alm_fit,  "ALMI: minimal model diagnostics"),
        plot_minimal_diagnostics(gait_fit, "Gait: minimal model diagnostics"),
        ncol = 1L
    ) +
        patchwork::plot_annotation(
            title = "Minimal model diagnostics — all outcomes",
            theme = ggplot2::theme(
                plot.title = ggplot2::element_text(size = 13, face = "bold")
            )
        )
    .save(p_diag_all, "05_diagnostics_minimal_models", height = 12)
    
    # ── 6/7  Dairy × time interaction models ────────────────────────────────
    cli::cli_h2("6/7  Dairy x time interaction models")
    
    grip_int <- fit_interaction_grip(analysis_long)
    alm_int  <- fit_interaction_alm(analysis_long)
    gait_int <- fit_interaction_gait(analysis_long)
    
    int_table <- make_interaction_table(grip_int, alm_int, gait_int)
    int_path  <- file.path(out_dir, "06_interaction_coefficients.csv")
    utils::write.csv(int_table, int_path, row.names = FALSE)
    cli::cli_inform(c("v" = "Interaction table saved: {.path {int_path}}"))
    
    # Print LRT results to console for quick review
    cli::cli_inform(c(
        "i" = "LRT: main-effect vs interaction model",
        "*" = "Grip  LRT p = {round(as.data.frame(grip_int$lrt)[2, 'Pr(>Chisq)'], 4)}",
        "*" = "ALMI  LRT p = {round(as.data.frame(alm_int$lrt)[2,  'Pr(>Chisq)'], 4)}",
        "*" = "Gait  LRT p = {round(as.data.frame(gait_int$lrt)[2, 'Pr(>Chisq)'], 4)}"
    ))
    
    # ── 7/7  Interaction trajectory plots ───────────────────────────────────
    cli::cli_h2("7/7  Interaction trajectory plots")
    
    p_traj_grip <- plot_interaction_trajectories(
        grip_int, "handgrip_max_all", "Grip strength (kg)"
    )
    p_traj_alm <- plot_interaction_trajectories(
        alm_int, "ALM_HT2", "ALMI (kg/m\u00b2)"
    )
    p_traj_gait <- plot_interaction_trajectories(
        gait_int, "gait_speed", "Gait speed (m/s)"
    )
    
    p_traj_all <- patchwork::wrap_plots(
        p_traj_grip, p_traj_alm, p_traj_gait,
        ncol = 1L
    ) +
        patchwork::plot_annotation(
            title    = "Dairy \u00d7 time interaction: predicted trajectories by quartile",
            subtitle = "Does higher dairy intake attenuate the rate of decline?",
            theme    = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 13, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
    .save(p_traj_all, "07_interaction_trajectories", height = 14)
    
    cli::cli_h1("run_exploration() complete. Outputs in {.path {out_dir}}")
    
    invisible(list(
        grip_fit   = grip_fit,
        alm_fit    = alm_fit,
        gait_fit   = gait_fit,
        grip_int   = grip_int,
        alm_int    = alm_int,
        gait_int   = gait_int,
        coef_table = coef_table,
        int_table  = int_table
    ))
}