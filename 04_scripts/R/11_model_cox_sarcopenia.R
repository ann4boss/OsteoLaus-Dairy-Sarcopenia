# =============================================================================
# R/11_model_cox_sarcopenia.R
# =============================================================================
# Cox proportional hazards model: time to incident EWGSOP2 sarcopenia.
#
# Research question
# -----------------
# Is higher dairy intake associated with a lower hazard of developing
# sarcopenia (EWGSOP2 criteria) over the OsteoLaus follow-up period?
#
# Outcome definition
# ------------------
# Event: first OsteoLaus visit at which ewgsop2_sarcopenia_stage is
#   "Probable", "Confirmed", or "Severe" (i.e. any stage > "No sarcopenia").
# Time: time_since_bsl_yr at that visit.
# Censoring: last visit with a non-missing ewgsop2_sarcopenia_stage where
#   the stage is still "No sarcopenia". Participants lost to follow-up
#   (no further measurements after last "No sarcopenia" observation) are
#   right-censored at their last valid assessment time.
#
# Prevalent case exclusion
# ------------------------
# Participants with sarcopenia at OsteoLaus Baseline are excluded.
# This is the central design decision for incident-only analysis:
#   - eligible_ewgsop2 = TRUE (non-missing stage + all component inputs at Baseline)
#   - ewgsop2_sarcopenia_stage == "No sarcopenia" at Baseline
# Participants without a Baseline sarcopenia assessment (eligible_ewgsop2 = FALSE)
# are also excluded because their incident status cannot be established.
#
# Exposure
# --------
# PRIMARY:   baseline_dairy_quartile (Q1–Q4; Q1 = reference)
#            Fixed at Baseline — avoids the methodological complexity of
#            time-varying exposure in a Cox model and aligns with the
#            study design (dairy intake assessed at CoLaus F1, which
#            corresponds to the OsteoLaus Baseline period).
#
# SECONDARY: dairy_cumavg at Baseline as a continuous exposure.
#            Provides a dose-response estimate complementing the quartile model.
#
# Both are included in build_cox_model_data() and two separate model fits
# are produced (fit_cox_quartile and fit_cox_continuous).
#
# Data structure
# --------------
# Standard survival format: one row per participant.
#   surv_time  — time from Baseline to event or censoring (years)
#   event      — 1 = developed sarcopenia; 0 = censored
#
# Covariates
# ----------
# All taken at Baseline (fixed). Time-varying covariates would require
# counting-process format (start, stop, event) — see note in build function.
#
# Proportional hazards assumption
# --------------------------------
# Tested via Schoenfeld residuals using survival::cox.zph() in the
# diagnostic function make_cox_model_plots().
#
# Outputs
# -------
#   cox_model_data        survival-ready tibble (from build_cox_model_data)
#   cox_quartile_fit      coxph object, quartile exposure
#   cox_continuous_fit    coxph object, continuous dairy_cumavg
#   cox_quartile_table    gtsummary tbl_regression (HR, 95% CI, p)
#   cox_continuous_table  gtsummary tbl_regression
#   cox_model_plots       diagnostic plots (PH test, KM curves)
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================

# Baseline covariates shared by both Cox models
.COX_COVARIATES <- c(
    "baseline_osteo_age",    # age at OsteoLaus Baseline
    "baseline_bmi",          # BMI at OsteoLaus Baseline
    "education_level",       # fixed
    "sbsmk",                 # smoking at Baseline
    "alcohol_category",      # alcohol at Baseline
    "pa_levels",             # physical activity at Baseline
    "diabetes_status",       # diabetes at Baseline
    "HTN_status",            # hypertension at Baseline
    "hrt_status",            # HRT at Baseline
    "corticoids_status",     # corticosteroids at Baseline
    "calcium_status",        # calcium supplement at Baseline
    "vitD_status",           # vitamin D supplement at Baseline
    "bisphosphonate_status", # bisphosphonate at Baseline
    "energy_kcal",           # total energy at Baseline
    "protein_pct"            # protein % at Baseline
)

# =============================================================================
# Dataset construction
# =============================================================================

#' Build the survival dataset for the incident sarcopenia Cox model.
#'
#' One row per participant. Excludes:
#'   (a) Participants without a valid Baseline sarcopenia assessment
#'       (eligible_ewgsop2 = FALSE)
#'   (b) Participants with sarcopenia at Baseline (prevalent cases)
#'
#' Time origin = OsteoLaus Baseline (time_since_bsl_yr = 0).
#' Event = first wave at which ewgsop2_sarcopenia_stage != "No sarcopenia".
#' Censoring = last wave with non-missing stage and "No sarcopenia".
#'
#' Baseline covariates are pulled from the Baseline wave row of analysis_long.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A one-row-per-participant survival tibble.
build_cox_model_data <- function(analysis_long) {
    
    # ── Step 1: Restrict to eligible_ewgsop2 participants ─────────────────────
    eligible_pts <- analysis_long |>
        dplyr::filter(eligible_ewgsop2) |>
        dplyr::distinct(pt) |>
        dplyr::pull(pt)
    
    surv_data <- analysis_long |>
        dplyr::filter(pt %in% eligible_pts)
    
    n_eligible <- length(eligible_pts)
    
    # ── Step 2: Exclude prevalent cases (sarcopenia at Baseline) ──────────────
    prevalent_pts <- surv_data |>
        dplyr::filter(
            osteo_wave == "Baseline",
            !is.na(ewgsop2_sarcopenia_stage),
            ewgsop2_sarcopenia_stage != "No sarcopenia"
        ) |>
        dplyr::distinct(pt) |>
        dplyr::pull(pt)
    
    surv_data <- dplyr::filter(surv_data, !pt %in% prevalent_pts)
    
    n_after_prevalent <- dplyr::n_distinct(surv_data$pt)
    
    # ── Step 3: Derive time-to-event per participant ───────────────────────────
    # For each participant, find:
    #   - The first wave at which sarcopenia was detected (event)
    #   - The last wave with a non-missing "No sarcopenia" assessment (censoring)
    # Only rows with non-missing ewgsop2_sarcopenia_stage contribute.
    
    stage_data <- surv_data |>
        dplyr::filter(!is.na(ewgsop2_sarcopenia_stage)) |>
        dplyr::arrange(pt, time_since_bsl_yr) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            # Event: first time sarcopenia detected (any stage > No sarcopenia)
            event_time = {
                event_rows <- time_since_bsl_yr[
                    ewgsop2_sarcopenia_stage != "No sarcopenia"
                ]
                if (length(event_rows) > 0L) min(event_rows) else NA_real_
            },
            # Censoring: last time with non-missing No sarcopenia
            censor_time = {
                censor_rows <- time_since_bsl_yr[
                    ewgsop2_sarcopenia_stage == "No sarcopenia"
                ]
                if (length(censor_rows) > 0L) max(censor_rows) else NA_real_
            },
            # Event indicator
            event = !is.na(event_time),
            # Survival time: event time if event, else censoring time
            surv_time = dplyr::if_else(event, event_time, censor_time),
            # Stage at event (for descriptive use)
            sarco_stage_at_event = dplyr::if_else(
                event,
                as.character(ewgsop2_sarcopenia_stage[
                    time_since_bsl_yr == event_time
                ][1]),
                NA_character_
            ),
            .groups = "drop"
        )
    
    # Drop participants with no valid surv_time (no sarcopenia data at all
    # after Baseline — should be very rare given the hard exclusion criteria)
    n_missing_time <- sum(is.na(stage_data$surv_time))
    if (n_missing_time > 0) {
        cli::cli_warn(
            "build_cox_model_data(): {n_missing_time} participant(s) have no \\
       valid surv_time after sarcopenia stage assessment — excluded."
        )
        stage_data <- dplyr::filter(stage_data, !is.na(surv_time))
    }
    
    # Exclude zero-time participants (event at Baseline — should be 0 after
    # prevalent case exclusion, but guard anyway)
    n_zero_time <- sum(stage_data$surv_time <= 0, na.rm = TRUE)
    if (n_zero_time > 0) {
        cli::cli_warn(
            "build_cox_model_data(): {n_zero_time} participant(s) with \\
       surv_time <= 0 excluded."
        )
        stage_data <- dplyr::filter(stage_data, surv_time > 0)
    }
    
    # ── Step 4: Pull Baseline covariates ──────────────────────────────────────
    # Take the Baseline row for each participant for all fixed covariates.
    # Covariates that only exist in exposures (alcohol_category, pa_levels, etc.)
    # are already joined into analysis_long via freeze_dataset().
    bsl_covs <- surv_data |>
        dplyr::filter(osteo_wave == "Baseline") |>
        dplyr::select(
            pt,
            # Exposure variables
            baseline_dairy_quartile,
            dairy_cumavg,          # cumavg at Baseline = first FFQ value
            dairy_total_gday,      # instantaneous (audit)
            # Baseline covariates
            dplyr::any_of(.COX_COVARIATES)
        )
    
    # ── Step 5: Assemble final dataset ────────────────────────────────────────
    cox_data <- stage_data |>
        dplyr::left_join(bsl_covs, by = "pt") |>
        dplyr::filter(!is.na(surv_time))
    
    n_final   <- nrow(cox_data)
    n_events  <- sum(cox_data$event)
    pct_event <- round(n_events / n_final * 100, 1)
    
    # ── Step 6: Factor reference levels ───────────────────────────────────────
    cox_data <- cox_data |>
        dplyr::mutate(
            # Dairy quartile: Q1 = lowest intake = reference
            baseline_dairy_quartile = forcats::fct_relevel(
                baseline_dairy_quartile, "Q1"
            ),
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
    cli::cli_inform(c(
        "i" = "build_cox_model_data() complete:",
        "*" = "{n_eligible} eligible_ewgsop2 participants",
        "*" = "{length(prevalent_pts)} excluded: sarcopenia at Baseline",
        "*" = "{n_after_prevalent - n_final} excluded: missing surv_time / \\
               zero-time",
        "v" = "{n_final} participants in survival dataset",
        "*" = "Events (incident sarcopenia): {n_events} / {n_final} \\
               ({pct_event}%)",
        "*" = "Censored: {n_final - n_events} ({round(100 - pct_event, 1)}%)"
    ))
    
    # ── Covariate completeness ────────────────────────────────────────────────
    miss_pct <- cox_data |>
        dplyr::summarise(
            dplyr::across(
                dplyr::any_of(c(.COX_COVARIATES,
                                "baseline_dairy_quartile", "dairy_cumavg")),
                ~ round(mean(is.na(.x)) * 100, 1)
            )
        ) |>
        tidyr::pivot_longer(dplyr::everything(),
                            names_to = "covariate", values_to = "pct_missing") |>
        dplyr::filter(pct_missing > 0)
    
    if (nrow(miss_pct) > 0) {
        cli::cli_warn(c(
            "!" = "Covariates with missing values (complete-case analysis):",
            "*" = paste(
                glue::glue("{miss_pct$covariate}: {miss_pct$pct_missing}%"),
                collapse = "\n"
            )
        ))
    }
    
    cox_data
}


# =============================================================================
# Model fitting — quartile exposure
# =============================================================================

#' Cox model: incident sarcopenia ~ baseline dairy quartile.
#'
#' Hazard ratios for Q2, Q3, Q4 vs Q1 (lowest intake).
#'
#' @param cox_model_data Output of build_cox_model_data().
#' @return A coxph object.
fit_cox_quartile <- function(cox_model_data) {
    
    fixed_covs <- paste(
        intersect(.COX_COVARIATES, names(cox_model_data)),
        collapse = " + "
    )
    
    f <- stats::as.formula(paste0(
        "survival::Surv(surv_time, event) ~ ",
        "baseline_dairy_quartile + ",
        fixed_covs
    ))
    
    cli::cli_inform(c("i" = "Fitting Cox model (quartile exposure):"))
    cli::cli_inform(c("*" = deparse(f)))
    
    fit <- survival::coxph(
        formula = f,
        data    = cox_model_data,
        ties    = "efron",
        x       = TRUE   # retain design matrix for cox.zph()
    )
    
    .log_cox_summary(fit, cox_model_data, "quartile")
    fit
}


# =============================================================================
# Model fitting — continuous exposure
# =============================================================================

#' Cox model: incident sarcopenia ~ continuous dairy_cumavg at Baseline.
#'
#' Per 100 g/day increase in dairy intake — scale the coefficient for
#' interpretability since a 1 g/day change is clinically trivial.
#'
#' @param cox_model_data Output of build_cox_model_data().
#' @return A coxph object.
fit_cox_continuous <- function(cox_model_data) {
    
    # Scale dairy_cumavg to per-100-g/day units for interpretable HR
    cox_model_data <- dplyr::mutate(
        cox_model_data,
        dairy_per100 = dairy_cumavg / 100
    )
    
    fixed_covs <- paste(
        intersect(.COX_COVARIATES, names(cox_model_data)),
        collapse = " + "
    )
    
    f <- stats::as.formula(paste0(
        "survival::Surv(surv_time, event) ~ ",
        "dairy_per100 + ",
        fixed_covs
    ))
    
    cli::cli_inform(c("i" = "Fitting Cox model (continuous exposure, per 100 g/day):"))
    cli::cli_inform(c("*" = deparse(f)))
    
    fit <- survival::coxph(
        formula = f,
        data    = cox_model_data,
        ties    = "efron",
        x       = TRUE
    )
    
    .log_cox_summary(fit, cox_model_data, "continuous")
    fit
}


# =============================================================================
# Model tables
# =============================================================================

#' Publication-ready table: Cox quartile model.
#'
#' @param cox_quartile_fit Output of fit_cox_quartile().
#' @return A gtsummary tbl_regression object with HR (95% CI, p).
make_cox_quartile_table <- function(cox_quartile_fit) {
    
    cox_quartile_fit |>
        gtsummary::tbl_regression(
            exponentiate = TRUE,   # report HR instead of log-HR
            label        = .cox_labels(include_quartile = TRUE),
            pvalue_fun   = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_caption(
            "**Table 5.** Cox proportional hazards model: \\
             incident sarcopenia ~ baseline dairy intake quartile"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "HR (95% CI) from survival::coxph() with Efron tie-breaking. \\
                 Event: first EWGSOP2 sarcopenia stage \u2265 Probable. \\
                 Participants with sarcopenia at Baseline excluded. \\
                 Reference: Q1 (lowest dairy intake); education Low; \\
                 smoking Never; alcohol Non-drinker; binary covariates No."
        )
}

#' Publication-ready table: Cox continuous model.
#'
#' @param cox_continuous_fit Output of fit_cox_continuous().
#' @return A gtsummary tbl_regression object with HR (95% CI, p).
make_cox_continuous_table <- function(cox_continuous_fit) {
    
    cox_continuous_fit |>
        gtsummary::tbl_regression(
            exponentiate = TRUE,
            label        = c(
                .cox_labels(include_quartile = FALSE),
                list(dairy_per100 ~ "Dairy intake (per 100 g/day)")
            ),
            pvalue_fun   = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_caption(
            "**Table 6.** Cox model: incident sarcopenia ~ \\
             continuous dairy intake (per 100 g/day)"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "HR (95% CI). Exposure scaled to per 100 g/day for \\
                 interpretability. Event and exclusions as Table 5."
        )
}


# =============================================================================
# Diagnostic plots
# =============================================================================

#' Diagnostic plots for the Cox sarcopenia models.
#'
#' Four panels:
#'   A  Kaplan-Meier curves by baseline dairy quartile (unadjusted)
#'   B  Schoenfeld residuals vs time — overall PH test (quartile model)
#'   C  Schoenfeld residuals per covariate (quartile model, top 6 by p-value)
#'   D  Martingale residuals vs continuous dairy (continuous model) —
#'      checks functional form of the exposure
#'
#' @param cox_model_data      Output of build_cox_model_data().
#' @param cox_quartile_fit    Output of fit_cox_quartile().
#' @param cox_continuous_fit  Output of fit_cox_continuous().
#' @return A patchwork plot object.
make_cox_model_plots <- function(cox_model_data,
                                 cox_quartile_fit,
                                 cox_continuous_fit) {
    
    # ── A: Kaplan-Meier by dairy quartile ─────────────────────────────────────
    km_fit <- survival::survfit(
        survival::Surv(surv_time, event) ~ baseline_dairy_quartile,
        data = cox_model_data
    )
    
    km_df <- broom::tidy(km_fit) |>
        dplyr::mutate(
            quartile = stringr::str_remove(strata,
                                           "baseline_dairy_quartile=")
        )
    
    p_km <- ggplot2::ggplot(
        km_df,
        ggplot2::aes(x = time, y = 1 - estimate,
                     colour = quartile, fill = quartile)
    ) +
        ggplot2::geom_step(linewidth = 0.7) +
        ggplot2::geom_ribbon(
            ggplot2::aes(ymin = 1 - conf.high, ymax = 1 - conf.low),
            alpha = 0.1, colour = NA
        ) +
        ggplot2::scale_colour_brewer(palette = "RdYlGn", direction = -1,
                                     name = "Dairy quartile") +
        ggplot2::scale_fill_brewer(palette = "RdYlGn", direction = -1,
                                   name = "Dairy quartile") +
        ggplot2::scale_y_continuous(
            labels = scales::label_percent(scale = 100),
            limits = c(0, 1)
        ) +
        ggplot2::labs(
            title    = "A: Kaplan-Meier — cumulative incidence of sarcopenia",
            subtitle = "By baseline dairy intake quartile (unadjusted)",
            x        = "Time since Baseline (years)",
            y        = "Cumulative incidence"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position  = "right",
            plot.subtitle    = ggplot2::element_text(size = 8, colour = "grey40")
        )
    
    # ── B & C: Schoenfeld residuals (PH assumption) ───────────────────────────
    zph_test <- survival::cox.zph(cox_quartile_fit)
    
    zph_df <- as.data.frame(zph_test$y) |>
        tibble::rownames_to_column("time_str") |>
        dplyr::mutate(time = as.numeric(time_str)) |>
        tidyr::pivot_longer(
            cols      = -c(time, time_str),
            names_to  = "covariate",
            values_to = "schoenfeld"
        )
    
    # Overall test p-value
    global_p <- zph_test$table["GLOBAL", "p"]
    
    # Panel B: overall scaled Schoenfeld residuals (first covariate as example)
    first_cov <- unique(zph_df$covariate)[1]
    p_zph_overall <- zph_df |>
        dplyr::filter(covariate == first_cov) |>
        ggplot2::ggplot(ggplot2::aes(x = time, y = schoenfeld)) +
        ggplot2::geom_point(alpha = 0.3, size = 0.8, colour = "grey30") +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                             se = TRUE, colour = "tomato",
                             fill = "tomato", alpha = 0.15,
                             linewidth = 0.7) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "#2E86AB", linewidth = 0.5) +
        ggplot2::labs(
            title    = "B: Schoenfeld residuals — PH assumption check",
            subtitle = glue::glue(
                "Global PH test p = {gtsummary::style_pvalue(global_p)} \\
                 (non-significant = PH holds)"
            ),
            x = "Time (years)", y = glue::glue("Scaled Schoenfeld ({first_cov})")
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
    # Panel C: PH test p-values per covariate (forest-style dot plot)
    zph_pvals <- as.data.frame(zph_test$table) |>
        tibble::rownames_to_column("covariate") |>
        dplyr::filter(covariate != "GLOBAL") |>
        dplyr::arrange(p) |>
        dplyr::slice_head(n = 10L) |>
        dplyr::mutate(
            covariate = forcats::fct_reorder(covariate, p),
            ph_ok     = p >= 0.05
        )
    
    p_zph_covs <- ggplot2::ggplot(
        zph_pvals,
        ggplot2::aes(x = p, y = covariate, colour = ph_ok)
    ) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(xintercept = 0.05, linetype = "dashed",
                            colour = "tomato", linewidth = 0.5) +
        ggplot2::scale_colour_manual(
            values = c("TRUE" = "#2D6A4F", "FALSE" = "#E84855"),
            labels = c("TRUE" = "PH holds (p \u2265 0.05)",
                       "FALSE" = "PH violated (p < 0.05)"),
            name   = NULL
        ) +
        ggplot2::labs(
            title    = "C: PH test p-values per covariate",
            subtitle = "Top 10 by ascending p-value",
            x        = "Schoenfeld test p-value",
            y        = NULL
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position  = "bottom",
            plot.subtitle    = ggplot2::element_text(size = 8, colour = "grey40")
        )
    
    # ── D: Martingale residuals vs dairy (functional form check) ─────────────
    mart_resid <- stats::residuals(cox_continuous_fit, type = "martingale")
    
    mart_df <- tibble::tibble(
        dairy_cumavg = cox_model_data$dairy_cumavg[
            as.integer(names(mart_resid))
        ],
        martingale = mart_resid
    )
    
    p_mart <- ggplot2::ggplot(
        mart_df,
        ggplot2::aes(x = dairy_cumavg, y = martingale)
    ) +
        ggplot2::geom_point(alpha = 0.2, size = 0.8, colour = "grey30") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "tomato", linewidth = 0.5) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                             se = TRUE, colour = "#2E86AB",
                             fill = "#2E86AB", alpha = 0.15,
                             linewidth = 0.7) +
        ggplot2::labs(
            title    = "D: Martingale residuals vs dairy intake",
            subtitle = "Checks linearity of the log-hazard in dairy (continuous model)",
            x        = "Dairy intake at Baseline (g/day)",
            y        = "Martingale residuals"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8,
                                                             colour = "grey40"))
    
    patchwork::wrap_plots(p_km, p_zph_overall, p_zph_covs, p_mart, ncol = 2) +
        patchwork::plot_annotation(
            title    = "Cox model diagnostics: incident sarcopenia ~ dairy intake",
            subtitle = paste0(
                "A: unadjusted KM curves. B-C: proportional hazards assumption. ",
                "D: functional form of continuous exposure."
            ),
            theme    = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 13, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
}


# =============================================================================
# PRIVATE HELPERS
# =============================================================================

# Label list shared by both Cox model tables
.cox_labels <- function(include_quartile = TRUE) {
    base <- list(
        baseline_osteo_age    ~ "Age at Baseline (yr)",
        baseline_bmi          ~ "BMI at Baseline (kg/m\u00b2)",
        education_level       ~ "Education level",
        sbsmk                 ~ "Smoking status",
        alcohol_category      ~ "Alcohol intake",
        pa_levels             ~ "Physical activity",
        diabetes_status       ~ "Diabetes",
        HTN_status            ~ "Hypertension",
        hrt_status            ~ "HRT",
        corticoids_status     ~ "Systemic corticosteroids",
        calcium_status        ~ "Calcium supplement",
        vitD_status           ~ "Vitamin D supplement",
        bisphosphonate_status ~ "Bisphosphonate use",
        energy_kcal           ~ "Total energy (kcal/day)",
        protein_pct           ~ "Protein (% of energy)"
    )
    if (include_quartile) {
        c(list(baseline_dairy_quartile ~ "Dairy intake quartile"), base)
    } else {
        base
    }
}

# Log concordance, events, and follow-up time
.log_cox_summary <- function(fit, data, model_name) {
    conc      <- summary(fit)$concordance
    n_events  <- fit$nevent
    n_total   <- fit$n
    med_fup   <- round(median(data$surv_time, na.rm = TRUE), 2)
    
    cli::cli_inform(c(
        "v" = "fit_cox_{model_name}() complete:",
        "*" = "N: {n_total} | Events: {n_events}",
        "*" = "Median follow-up: {med_fup} yr",
        "*" = "Concordance (C-index): {round(conc[1], 3)} \\
               (SE {round(conc[2], 3)})"
    ))
}