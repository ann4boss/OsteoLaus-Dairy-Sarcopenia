# =============================================================================
# R/11_model_cox_sarcopenia.R  (revised)
# =============================================================================
# Cox proportional hazards model: time to incident EWGSOP2 sarcopenia.
#
# Key changes from previous version
# ----------------------------------
# 1. fit_cox_quartile() and fit_cox_continuous() now accept a `tier` argument
#    drawing from .COVARIATE_TIERS$cox (M0–M3).
#
# 2. Both functions also accept an `exposure` argument; for the continuous
#    model this allows switching between dairy_cumavg, dairy_total_lag1,
#    and dairy_total_gday (all pulled from the Baseline row).
#    Note: at Baseline, dairy_cumavg == dairy_total_gday (first FFQ value);
#    the distinction matters for the LME models but is included here for
#    consistency with the sensitivity framework.
#
# 3. A make_cox_stability_table() function is added to display HR and 95% CI
#    for the dairy term across M0–M3 in a single supplementary table.
#
# Outputs
# -------
#   cox_model_data          survival-ready tibble (built once)
#   cox_quartile_M3         coxph, M3, quartile exposure [PRIMARY]
#   cox_continuous_M3       coxph, M3, continuous exposure [PRIMARY]
#   cox_quartile_M0         coxph, crude, quartile
#   cox_quartile_M1         coxph, M1, quartile
#   cox_quartile_M2         coxph, M2, quartile
#   cox_continuous_S1       coxph, M3, lag-1 exposure [SENSITIVITY]
#   cox_continuous_S2       coxph, M3, concurrent exposure [SENSITIVITY]
#   cox_model_plots         diagnostic plots (KM, PH, Martingale)
# =============================================================================


# =============================================================================
# Dataset construction  (unchanged from previous version)
# =============================================================================

#' Build the survival dataset for the incident sarcopenia Cox model.
#'
#' One row per participant (standard survival format).
#'   surv_time — time from Baseline to event or censoring (years)
#'   event     — 1 = developed sarcopenia; 0 = censored
#'
#' All exposure columns (dairy_cumavg, dairy_total_lag1, dairy_total_gday)
#' are pulled from the Baseline row so that sensitivity analyses using
#' different exposure metrics can share this single dataset.
#'
#' Prevalent case exclusion is logged here so the count appears in the
#' pipeline output (it does NOT appear in the main flow log from
#' freeze_dataset() — add a note to the CONSORT diagram manually).
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A one-row-per-participant survival tibble.
build_cox_model_data <- function(analysis_long) {
    
    # All covariates needed across any tier
    all_covs <- unique(unlist(.COVARIATE_TIERS$cox))
    
    # ── Step 1: Restrict to eligible_ewgsop2 participants ────────────────────────
    eligible_pts <- analysis_long |>
        dplyr::filter(eligible_ewgsop2) |>
        dplyr::distinct(pt) |>
        dplyr::pull(pt)
    
    surv_data <- analysis_long |>
        dplyr::filter(pt %in% eligible_pts)
    
    n_eligible <- length(eligible_pts)
    
    # ── Step 2: Exclude prevalent cases ──────────────────────────────────────────
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
    
    # Log prevalent exclusion so it can be added to the CONSORT diagram
    cli::cli_inform(c(
        "i" = "Cox prevalent-case exclusion (NOT in main flow log — add manually):",
        "*" = "Eligible (ewgsop2): {n_eligible}",
        "*" = "Excluded (sarcopenia at Baseline): {length(prevalent_pts)}",
        "*" = "Remaining after prevalent exclusion: {n_after_prevalent}"
    ))
    
    # ── Step 3: Derive time-to-event ─────────────────────────────────────────────
    stage_data <- surv_data |>
        dplyr::filter(!is.na(ewgsop2_sarcopenia_stage)) |>
        dplyr::arrange(pt, time_since_bsl_yr) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            event_time = {
                ev <- time_since_bsl_yr[ewgsop2_sarcopenia_stage != "No sarcopenia"]
                if (length(ev) > 0L) min(ev) else NA_real_
            },
            censor_time = {
                ce <- time_since_bsl_yr[ewgsop2_sarcopenia_stage == "No sarcopenia"]
                if (length(ce) > 0L) max(ce) else NA_real_
            },
            event       = !is.na(event_time),
            surv_time   = dplyr::if_else(event, event_time, censor_time),
            sarco_stage_at_event = dplyr::if_else(
                event,
                as.character(ewgsop2_sarcopenia_stage[
                    time_since_bsl_yr == event_time
                ][1]),
                NA_character_
            ),
            .groups = "drop"
        )
    
    # Guard: drop participants with no valid surv_time
    n_missing_time <- sum(is.na(stage_data$surv_time))
    if (n_missing_time > 0) {
        cli::cli_warn(
            "{n_missing_time} participant(s) with no valid surv_time excluded."
        )
        stage_data <- dplyr::filter(stage_data, !is.na(surv_time))
    }
    
    # Guard: drop zero-time participants
    n_zero_time <- sum(stage_data$surv_time <= 0, na.rm = TRUE)
    if (n_zero_time > 0) {
        cli::cli_warn("{n_zero_time} participant(s) with surv_time <= 0 excluded.")
        stage_data <- dplyr::filter(stage_data, surv_time > 0)
    }
    
    # ── Step 4: Pull Baseline covariates + all exposure metrics ──────────────────
    bsl_covs <- surv_data |>
        dplyr::filter(osteo_wave == "Baseline") |>
        dplyr::select(
            pt,
            # Exposure columns — primary and sensitivity
            baseline_dairy_quartile,
            dairy_cumavg,        # == dairy_total_gday at Baseline (first FFQ)
            dairy_total_gday,    # same as dairy_cumavg at Baseline; kept for clarity
            dairy_total_lag1,    # lag-1 (CoLaus visit prior to OsteoLaus Baseline)
            # All covariates for any tier
            dplyr::any_of(all_covs)
        )
    
    # ── Step 5: Assemble final dataset ───────────────────────────────────────────
    cox_data <- stage_data |>
        dplyr::left_join(bsl_covs, by = "pt") |>
        dplyr::filter(!is.na(surv_time))
    
    n_final  <- nrow(cox_data)
    n_events <- sum(cox_data$event)
    
    # ── Factor reference levels ───────────────────────────────────────────────────
    cox_data <- cox_data |>
        dplyr::mutate(
            baseline_dairy_quartile = forcats::fct_relevel(
                baseline_dairy_quartile, "Q1"
            )
        ) |>
        set_reference_levels()   # shared helper from model_specs.R
    
    cli::cli_inform(c(
        "v" = "build_cox_model_data() complete:",
        "*" = "{n_final} participants | Events: {n_events} \\
           ({round(n_events/n_final*100,1)}%) | \\
           Censored: {n_final - n_events}",
        "i" = "All Cox model tiers and exposure sensitivities share this sample."
    ))
    
    report_completeness(cox_data,
                        covariates = .COVARIATE_TIERS$cox$M3,
                        n_before   = n_final)
    
    cox_data
}


# =============================================================================
# Model fitting — quartile exposure
# =============================================================================

#' Cox model: incident sarcopenia ~ baseline dairy quartile.
#'
#' @param cox_model_data  Output of build_cox_model_data().
#' @param tier            Covariate tier: "M0", "M1", "M2", or "M3" (default).
#' @return A list: $fit (coxph), $tier, $exposure, $formula.
fit_cox_quartile <- function(cox_model_data,
                             tier = "M3") {
    
    stopifnot(tier %in% c("M0", "M1", "M2", "M3"))
    
    covariates <- intersect(
        .COVARIATE_TIERS$cox[[tier]],
        names(cox_model_data)
    )
    
    rhs <- paste(
        c("baseline_dairy_quartile", covariates),
        collapse = " + "
    )
    
    f <- stats::as.formula(
        paste("survival::Surv(surv_time, event) ~", rhs)
    )
    
    # ── Prepare fitting data ────────────────────────────────────────────────────
    prep       <- prepare_fit_data(cox_model_data, covariates,
                                   caller           = glue::glue("fit_cox_quartile [{tier}]"),
                                   exposure         = "baseline_dairy_quartile",
                                   m3_covariates    = .COVARIATE_TIERS$cox$M3,
                                   scale_covariates = TRUE)
    fit_data   <- prep$data
    covariates <- prep$covariates   # degenerate columns removed
    
    # Rebuild formula with cleaned covariates
    rhs <- paste(c("baseline_dairy_quartile", covariates), collapse = " + ")
    f   <- stats::as.formula(paste("survival::Surv(surv_time, event) ~", rhs))
    
    cli::cli_inform(c(
        "i" = "fit_cox_quartile() — tier={tier}",
        "*" = deparse(f)
    ))
    
    fit <- survival::coxph(
        formula = f,
        data    = fit_data,
        ties    = "efron",
        x       = TRUE
    )
    
    .log_cox_summary(fit, fit_data, glue::glue("quartile_{tier}"))
    
    list(
        fit      = fit,
        data     = fit_data,   # stored so plot functions index the right rows
        tier     = tier,
        exposure = "baseline_dairy_quartile",
        formula  = f
    )
}


# =============================================================================
# Model fitting — continuous exposure
# =============================================================================

#' Cox model: incident sarcopenia ~ continuous dairy intake at Baseline.
#'
#' Dairy is scaled to per-100 g/day for interpretable HRs.
#'
#' @param cox_model_data  Output of build_cox_model_data().
#' @param tier            Covariate tier: "M0", "M1", "M2", or "M3" (default).
#' @param exposure        Exposure column. Default "dairy_cumavg".
#'   At Baseline, dairy_cumavg == dairy_total_gday (first FFQ).
#'   For lag-1 sensitivity: "dairy_total_lag1".
#' @return A list: $fit (coxph), $tier, $exposure, $formula.
fit_cox_continuous <- function(cox_model_data,
                               tier     = "M3",
                               exposure = "dairy_cumavg") {
    
    stopifnot(tier %in% c("M0", "M1", "M2", "M3"))
    stopifnot(exposure %in% names(cox_model_data))
    
    # Scale to per-100 g/day (1 g/day change is not clinically meaningful)
    scaled_col <- paste0(exposure, "_per100")
    cox_model_data <- dplyr::mutate(
        cox_model_data,
        !!scaled_col := .data[[exposure]] / 100
    )
    
    covariates <- intersect(
        .COVARIATE_TIERS$cox[[tier]],
        names(cox_model_data)
    )
    
    rhs <- paste(
        c(scaled_col, covariates),
        collapse = " + "
    )
    
    f <- stats::as.formula(
        paste("survival::Surv(surv_time, event) ~", rhs)
    )
    
    # ── Prepare fitting data ────────────────────────────────────────────────────
    prep       <- prepare_fit_data(cox_model_data, covariates,
                                   caller           = glue::glue("fit_cox_continuous [{tier}]"),
                                   exposure         = exposure,
                                   m3_covariates    = .COVARIATE_TIERS$cox$M3,
                                   scale_covariates = TRUE)
    fit_data   <- prep$data
    covariates <- prep$covariates   # degenerate columns removed
    
    # Rebuild formula with cleaned covariates
    rhs <- paste(c(scaled_col, covariates), collapse = " + ")
    f   <- stats::as.formula(paste("survival::Surv(surv_time, event) ~", rhs))
    
    cli::cli_inform(c(
        "i" = "fit_cox_continuous() — tier={tier}, exposure={exposure}",
        "*" = deparse(f)
    ))
    
    fit <- survival::coxph(
        formula = f,
        data    = fit_data,
        ties    = "efron",
        x       = TRUE
    )
    
    .log_cox_summary(fit, fit_data,
                     glue::glue("continuous_{tier}_{exposure}"))
    
    list(
        fit      = fit,
        data     = fit_data,   # stored so plot functions index the right rows
        tier     = tier,
        exposure = exposure,
        formula  = f
    )
}


# =============================================================================
# Coefficient stability table (across tiers)
# =============================================================================

#' HR stability table for the dairy term across model tiers.
#'
#' @param fits Named list of Cox fit objects (e.g. list(M0=..., M1=..., M3=...)).
#' @param exposure Regex to identify the dairy term in the model summary.
#' @return A tibble with one row per tier: HR, 95% CI, p-value.
make_cox_stability_table <- function(fits, exposure_pattern = "dairy") {
    
    purrr::imap_dfr(fits, function(fit_obj, tier_label) {
        s      <- summary(fit_obj$fit)$coefficients
        s_conf <- summary(fit_obj$fit)$conf.int
        
        rows <- rownames(s)[grepl(exposure_pattern, rownames(s),
                                  ignore.case = TRUE)]
        
        if (length(rows) == 0L) {
            cli::cli_warn("No dairy term found in tier {tier_label}.")
            return(NULL)
        }
        
        purrr::map_dfr(rows, function(rn) {
            tibble::tibble(
                tier    = tier_label,
                term    = rn,
                hr      = round(s_conf[rn, "exp(coef)"], 3),
                ci_lo   = round(s_conf[rn, "lower .95"],  3),
                ci_hi   = round(s_conf[rn, "upper .95"],  3),
                p_value = s[rn, "Pr(>|z|)"]
            )
        })
    })
}


# =============================================================================
# Model tables (gtsummary)
# =============================================================================

#' Publication-ready table for the primary quartile Cox model (M3).
#'
#' @param cox_quartile_M3  Fit object from fit_cox_quartile(tier = "M3").
#' @return A gtsummary tbl_regression.
make_cox_quartile_table <- function(cox_quartile_M3) {
    
    cox_quartile_M3$fit |>
        gtsummary::tbl_regression(
            exponentiate = TRUE,
            label        = .cox_labels(include_quartile = TRUE),
            pvalue_fun   = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_caption(
            "**Table 5.** Cox model: incident sarcopenia ~ \\
       baseline dairy intake quartile (M3, fully adjusted)"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "HR (95% CI). Efron tie-handling. Reference: dairy Q1 (lowest intake). \\
         See supplementary Table S4 for M0-M2 crude and minimally adjusted models."
        )
}


#' Publication-ready table for the primary continuous Cox model (M3).
#'
#' @param cox_continuous_M3  Fit object from fit_cox_continuous(tier = "M3").
#' @return A gtsummary tbl_regression.
make_cox_continuous_table <- function(cox_continuous_M3) {
    
    cox_continuous_M3$fit |>
        gtsummary::tbl_regression(
            exponentiate = TRUE,
            label        = c(
                .cox_labels(include_quartile = FALSE),
                list(dairy_cumavg_per100 ~ "Dairy intake (per 100 g/day)")
            ),
            pvalue_fun = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_caption(
            "**Table 6.** Cox model: incident sarcopenia ~ \\
       continuous dairy intake at Baseline (M3, fully adjusted)"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "HR (95% CI) per 100 g/day increase in dairy intake (Baseline value). \\
         Efron tie-handling. Fully adjusted (M3)."
        )
}


# =============================================================================
# Diagnostic plots  (unchanged from previous version)
# =============================================================================

#' Diagnostic plots for Cox models.
#'
#' Uses the primary M3 fits. Panel layout:
#'   A  KM curves by dairy quartile (unadjusted)
#'   B  Schoenfeld residuals — PH test (global p)
#'   C  Per-covariate PH test p-values
#'   D  Martingale residuals vs continuous dairy (functional form check)
#'
#' @param cox_quartile_M3   Fit object from fit_cox_quartile(tier = "M3").
#' @param cox_continuous_M3 Fit object from fit_cox_continuous(tier = "M3").
#' @param cox_model_data    Output of build_cox_model_data().
#' @return A patchwork plot object.
make_cox_model_plots <- function(cox_quartile_M3,
                                 cox_continuous_M3,
                                 cox_model_data) {
    
    # ── A: Kaplan-Meier ───────────────────────────────────────────────────────────
    # Use cox_model_data (full build output) for the unadjusted KM — it has all
    # participants before any covariate-degeneracy filtering.
    km_fit <- survival::survfit(
        survival::Surv(surv_time, event) ~ baseline_dairy_quartile,
        data = cox_model_data
    )
    
    km_df <- broom::tidy(km_fit) |>
        dplyr::mutate(
            strata = stringr::str_remove(strata, "baseline_dairy_quartile=")
        )
    
    p_km <- ggplot2::ggplot(
        km_df,
        ggplot2::aes(x = time, y = 1 - estimate,
                     colour = strata, fill = strata)
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
        ggplot2::scale_y_continuous(labels = scales::label_percent(),
                                    limits = c(0, 1)) +
        ggplot2::labs(
            title    = "A: Kaplan-Meier — cumulative incidence of sarcopenia",
            subtitle = "By baseline dairy intake quartile (unadjusted)",
            x = "Time since Baseline (years)", y = "Cumulative incidence"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    
    # ── B & C: Schoenfeld residuals ───────────────────────────────────────────────
    # cox.zph() inverts the information matrix. If prepare_fit_data() dropped any
    # covariate that was near-constant, the stored design matrix can be
    # rank-deficient and solve() throws "system is computationally singular".
    # Catch that case and substitute informative placeholder panels.
    zph_result <- tryCatch(
        survival::cox.zph(cox_quartile_M3$fit),
        error = function(e) {
            cli::cli_warn(c(
                "!" = "make_cox_model_plots(): cox.zph() failed — \\
               panels B and C replaced with a placeholder.",
                "i" = "Underlying error: {conditionMessage(e)}"
            ))
            NULL
        }
    )
    
    if (!is.null(zph_result)) {
        
        global_p <- zph_result$table["GLOBAL", "p"]
        
        zph_df <- as.data.frame(zph_result$y) |>
            tibble::rownames_to_column("time_str") |>
            dplyr::mutate(time = as.numeric(time_str)) |>
            tidyr::pivot_longer(
                cols      = -c(time, time_str),
                names_to  = "covariate",
                values_to = "schoenfeld"
            )
        
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
                title    = "B: Schoenfeld residuals",
                subtitle = glue::glue(
                    "Global PH test p = {gtsummary::style_pvalue(global_p)}"
                ),
                x = "Time (years)",
                y = glue::glue("Scaled Schoenfeld ({first_cov})")
            ) +
            ggplot2::theme_minimal(base_size = 11)
        
        zph_pvals <- as.data.frame(zph_result$table) |>
            tibble::rownames_to_column("covariate") |>
            dplyr::filter(covariate != "GLOBAL") |>
            dplyr::arrange(p) |>
            dplyr::slice_head(n = 10L) |>
            dplyr::mutate(
                covariate = forcats::fct_reorder(covariate, p),
                ph_ok     = p >= 0.05
            )
        
        p_zph_covs <- ggplot2::ggplot(
            zph_pvals, ggplot2::aes(x = p, y = covariate, colour = ph_ok)
        ) +
            ggplot2::geom_point(size = 2.5) +
            ggplot2::geom_vline(xintercept = 0.05, linetype = "dashed",
                                colour = "tomato", linewidth = 0.5) +
            ggplot2::scale_colour_manual(
                values = c("TRUE" = "#2D6A4F", "FALSE" = "#E84855"),
                labels = c("TRUE" = "PH holds", "FALSE" = "PH violated"),
                name   = NULL
            ) +
            ggplot2::labs(
                title = "C: PH test p-values per covariate",
                x     = "Schoenfeld test p-value", y = NULL
            ) +
            ggplot2::theme_minimal(base_size = 11)
        
    } else {
        # Placeholder panels when cox.zph() fails
        placeholder_df <- tibble::tibble(x = 0.5, y = 0.5,
                                         label = "cox.zph() unavailable\n(singular information matrix)")
        make_placeholder <- function(title) {
            ggplot2::ggplot(placeholder_df,
                            ggplot2::aes(x = x, y = y, label = label)) +
                ggplot2::geom_text(size = 3.5, colour = "grey40") +
                ggplot2::labs(title = title, x = NULL, y = NULL) +
                ggplot2::theme_minimal(base_size = 11) +
                ggplot2::theme(axis.text = ggplot2::element_blank(),
                               axis.ticks = ggplot2::element_blank())
        }
        p_zph_overall <- make_placeholder("B: Schoenfeld residuals")
        p_zph_covs    <- make_placeholder("C: PH test p-values")
    }
    
    # ── D: Martingale residuals ───────────────────────────────────────────────────
    # Use cox_continuous_M3$data — the actual data the model was fitted on after
    # prepare_fit_data(). Residual names are row indices within that data frame.
    # Using cox_model_data here would misalign rows if any were dropped.
    cont_fit_data <- cox_continuous_M3$data
    
    mart_resid <- stats::residuals(cox_continuous_M3$fit, type = "martingale")
    row_idx    <- as.integer(names(mart_resid))
    
    mart_df <- tibble::tibble(
        dairy_cumavg = cont_fit_data[[cox_continuous_M3$exposure]][row_idx],
        martingale   = mart_resid
    )
    
    p_mart <- ggplot2::ggplot(
        mart_df, ggplot2::aes(x = dairy_cumavg, y = martingale)
    ) +
        ggplot2::geom_point(alpha = 0.2, size = 0.8, colour = "grey30") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "tomato", linewidth = 0.5) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                             se = TRUE, colour = "#2E86AB",
                             fill = "#2E86AB", alpha = 0.15,
                             linewidth = 0.7) +
        ggplot2::labs(
            title    = "D: Martingale residuals vs dairy (functional form, M3)",
            subtitle = glue::glue(
                "Exposure: {cox_continuous_M3$exposure} — \\
         linear log-hazard assumption check"
            ),
            x = glue::glue("Dairy intake at Baseline ({cox_continuous_M3$exposure})"),
            y = "Martingale residuals"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    
    patchwork::wrap_plots(p_km, p_zph_overall, p_zph_covs, p_mart, ncol = 2) +
        patchwork::plot_annotation(
            title    = "Cox model diagnostics: incident sarcopenia ~ dairy intake",
            subtitle = "A: KM curves. B-C: PH assumption. D: functional form (M3).",
            theme    = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 13, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
}


# =============================================================================
# PRIVATE HELPERS
# =============================================================================

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

.log_cox_summary <- function(fit, data, model_name) {
    conc     <- summary(fit)$concordance
    cli::cli_inform(c(
        "v" = "Cox fit [{model_name}]:",
        "*" = "N={fit$n} | Events={fit$nevent} | \\
           Median FU={round(median(data$surv_time, na.rm=TRUE),2)} yr",
        "*" = "C-index={round(conc[1],3)} (SE {round(conc[2],3)})"
    ))
}