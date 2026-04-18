# =============================================================================
# R/14_explore_cox_simple.R
# =============================================================================
# Simple Cox proportional hazards models: time to incident sarcopenia.
#
# Purpose
# -------
# Minimal exploratory Cox models complementing the LME null findings.
# Keeps the same M1 adjustment philosophy (Age, BMI, energy only) and
# mirrors the structure of Scripts 12 and 13 so results are directly
# comparable.
#
# Models fitted
# -------------
#   A. Continuous dairy (dairy_cumavg, per 100 g/day)
#   B. Dairy quartiles  (Q1 = reference)
#   C. Sub-types        (fermented, non-fermented, low-fat, high-fat)
#      — each sub-type in a separate model
#
# Diagnostics
# -----------
#   - Kaplan-Meier curves by dairy quartile
#   - Schoenfeld residuals (proportional hazards test)
#   - Martingale residuals vs continuous dairy (functional form)
#   - Log-log survival curves (visual PH check)
#
# Outputs (written to 06_outputs/cox_simple/)
# -------------------------------------------
#   01_km_curves.png                KM by dairy quartile
#   02_loglog_curves.png            Log-log survival curves
#   03_continuous_coefficients.csv  HR per 100 g/day, all exposures
#   04_quartile_coefficients.csv    HR per quartile
#   05_subtype_coefficients.csv     HR per sub-type
#   06_forest_plot.png              Forest plot: all HRs in one figure
#   07_schoenfeld.png               PH assumption check
#   08_martingale.png               Functional form check
#
# Usage
#   targets::tar_load(analysis_long)
#   source("04_scripts/R/14_explore_cox_simple.R")
#   run_cox_simple(analysis_long)
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================

# ── Package guard ─────────────────────────────────────────────────────────────
.check_pkgs_cox <- function() {
    needed <- c("dplyr", "tidyr", "ggplot2", "patchwork", "survival",
                "broom", "forcats", "scales", "cli", "tibble", "glue", "purrr")
    missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0)
        stop("Install missing packages: ", paste(missing, collapse = ", "))
    invisible(NULL)
}

# =============================================================================
# CONSTANTS
# =============================================================================

.COX_SUBTYPES <- c(
    "dairy_fermented_gday",
    "dairy_non_fermented_gday",
    "dairy_lowfat_gday",
    "dairy_highfat_gday"
)

.COX_SUBTYPE_LABELS <- c(
    dairy_fermented_gday     = "Fermented dairy",
    dairy_non_fermented_gday = "Non-fermented dairy",
    dairy_lowfat_gday        = "Low-fat dairy",
    dairy_highfat_gday       = "High-fat dairy"
)

# M1 covariates (baseline values only for Cox)
.COX_COVARIATES <- c("baseline_osteo_age", "baseline_bmi", "energy_kcal")


# =============================================================================
# 1. BUILD SURVIVAL DATASET
# =============================================================================

#' Construct a one-row-per-participant survival dataset.
#'
#' Excludes prevalent sarcopenia cases at Baseline. Derives surv_time as
#' time from Baseline to first sarcopenic wave (event = 1) or last observed
#' non-sarcopenic wave (event = 0, censored).
#'
#' Baseline covariates and all exposure columns are pulled from the Baseline
#' row so the dataset can be shared across all Cox models.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return One-row-per-participant tibble ready for coxph().
build_cox_simple_data <- function(analysis_long) {
    
    # ── Restrict to ewgsop2-eligible participants ─────────────────────────────
    eligible_pts <- analysis_long |>
        dplyr::filter(eligible_ewgsop2) |>
        dplyr::distinct(pt) |>
        dplyr::pull(pt)
    
    df <- dplyr::filter(analysis_long, pt %in% eligible_pts)
    
    n_eligible <- length(eligible_pts)
    
    # ── Exclude prevalent cases (sarcopenia at Baseline) ──────────────────────
    prevalent <- df |>
        dplyr::filter(
            osteo_wave == "Baseline",
            !is.na(ewgsop2_sarcopenia_stage),
            ewgsop2_sarcopenia_stage != "No sarcopenia"
        ) |>
        dplyr::pull(pt)
    
    df <- dplyr::filter(df, !pt %in% prevalent)
    n_after <- dplyr::n_distinct(df$pt)
    
    cli::cli_inform(c(
        "i" = "build_cox_simple_data():",
        "*" = "Eligible (ewgsop2):           {n_eligible}",
        "*" = "Excluded (prevalent at Bsl):  {length(prevalent)}",
        "*" = "Remaining:                    {n_after}"
    ))
    
    # ── Derive time-to-event ──────────────────────────────────────────────────
    surv_df <- df |>
        dplyr::filter(!is.na(ewgsop2_sarcopenia_stage)) |>
        dplyr::arrange(pt, time_since_bsl_yr) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            event_time  = {
                ev <- time_since_bsl_yr[ewgsop2_sarcopenia_stage != "No sarcopenia"]
                if (length(ev) > 0L) min(ev) else NA_real_
            },
            censor_time = {
                ce <- time_since_bsl_yr[ewgsop2_sarcopenia_stage == "No sarcopenia"]
                if (length(ce) > 0L) max(ce) else NA_real_
            },
            event       = !is.na(event_time),
            surv_time   = dplyr::if_else(event, event_time, censor_time),
            .groups     = "drop"
        ) |>
        dplyr::filter(!is.na(surv_time), surv_time > 0)
    
    # ── Pull Baseline covariates and exposures ────────────────────────────────
    bsl <- df |>
        dplyr::filter(osteo_wave == "Baseline") |>
        dplyr::select(
            pt,
            # Primary exposure
            dairy_cumavg,
            dairy_total_gday,
            # Sub-types
            dplyr::any_of(.COX_SUBTYPES),
            # Baseline covariates from participants table
            dplyr::any_of(.COX_COVARIATES),
            # Quartile assignment
            dplyr::any_of("baseline_dairy_quartile")
        )
    
    cox_data <- surv_df |>
        dplyr::left_join(bsl, by = "pt") |>
        dplyr::filter(!is.na(dairy_cumavg))
    
    # Compute dairy quartile from Baseline values if not already present
    if (!"baseline_dairy_quartile" %in% names(cox_data) ||
        all(is.na(cox_data$baseline_dairy_quartile))) {
        
        q_breaks <- quantile(cox_data$dairy_cumavg,
                             probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
        cox_data <- dplyr::mutate(
            cox_data,
            baseline_dairy_quartile = cut(
                dairy_cumavg,
                breaks         = q_breaks,
                labels         = c("Q1", "Q2", "Q3", "Q4"),
                include.lowest = TRUE
            )
        )
    }
    
    # Set Q1 as reference
    cox_data$baseline_dairy_quartile <- forcats::fct_relevel(
        cox_data$baseline_dairy_quartile, "Q1"
    )
    
    # Z-score covariates
    for (col in intersect(.COX_COVARIATES, names(cox_data))) {
        z_col <- paste0(col, "_z")
        cox_data[[z_col]] <- as.numeric(scale(cox_data[[col]]))
    }
    
    n_final  <- nrow(cox_data)
    n_events <- sum(cox_data$event)
    
    cli::cli_inform(c(
        "v" = "Cox dataset ready:",
        "*" = "N = {n_final} | Events = {n_events} ({round(n_events/n_final*100,1)}%) | Censored = {n_final - n_events}",
        "*" = "Median follow-up = {round(median(cox_data$surv_time, na.rm=TRUE), 2)} yr"
    ))
    
    cox_data
}


# =============================================================================
# 2. KAPLAN-MEIER CURVES
# =============================================================================

#' KM curves by dairy intake quartile (unadjusted).
#'
#' @param cox_data Output of build_cox_simple_data().
#' @return A ggplot object.
plot_km_curves <- function(cox_data) {
    
    km_fit <- survival::survfit(
        survival::Surv(surv_time, event) ~ baseline_dairy_quartile,
        data = cox_data
    )
    
    km_df <- broom::tidy(km_fit) |>
        dplyr::mutate(
            quartile = stringr::str_remove(strata,
                                           "baseline_dairy_quartile="),
            cumulative_incidence = 1 - estimate,
            ci_lo = 1 - conf.high,
            ci_hi = 1 - conf.low
        )
    
    # Number at risk table
    n_risk <- broom::tidy(km_fit) |>
        dplyr::mutate(quartile = stringr::str_remove(
            strata, "baseline_dairy_quartile=")) |>
        dplyr::group_by(quartile) |>
        dplyr::summarise(
            n_total  = dplyr::first(n.risk),
            n_events = sum(n.event),
            .groups  = "drop"
        )
    
    subtitle_str <- paste(
        purrr::pmap_chr(n_risk, function(quartile, n_total, n_events, ...) {
            glue::glue("{quartile}: n={n_total}, events={n_events}")
        }),
        collapse = " | "
    )
    
    ggplot2::ggplot(
        km_df,
        ggplot2::aes(x = time, colour = quartile, fill = quartile)
    ) +
        ggplot2::geom_ribbon(
            ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
            alpha = 0.1, colour = NA
        ) +
        ggplot2::geom_step(
            ggplot2::aes(y = cumulative_incidence),
            linewidth = 0.8
        ) +
        ggplot2::scale_colour_manual(
            values = c(Q1 = "#2D6A4F", Q2 = "#52B788",
                       Q3 = "#F4A261", Q4 = "#E76F51"),
            name   = "Dairy quartile"
        ) +
        ggplot2::scale_fill_manual(
            values = c(Q1 = "#2D6A4F", Q2 = "#52B788",
                       Q3 = "#F4A261", Q4 = "#E76F51"),
            name   = "Dairy quartile"
        ) +
        ggplot2::scale_y_continuous(
            labels = scales::label_percent(),
            limits = c(0, NA)
        ) +
        ggplot2::labs(
            title    = "Kaplan-Meier: cumulative incidence of EWGSOP2 sarcopenia",
            subtitle = subtitle_str,
            x        = "Time since Baseline (years)",
            y        = "Cumulative incidence",
            caption  = "Unadjusted. Shaded bands = 95% CI."
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "right")
}


# =============================================================================
# 3. LOG-LOG SURVIVAL CURVES (visual PH check)
# =============================================================================

#' Log-log survival curves by dairy quartile.
#'
#' Parallel log-log curves support the proportional hazards assumption.
#' Crossing or converging curves suggest PH violation.
#'
#' @param cox_data Output of build_cox_simple_data().
#' @return A ggplot object.
plot_loglog_curves <- function(cox_data) {
    
    km_fit <- survival::survfit(
        survival::Surv(surv_time, event) ~ baseline_dairy_quartile,
        data = cox_data
    )
    
    ll_df <- broom::tidy(km_fit) |>
        dplyr::mutate(
            quartile = stringr::str_remove(strata, "baseline_dairy_quartile="),
            loglog   = log(-log(estimate))
        ) |>
        dplyr::filter(is.finite(loglog), estimate > 0, estimate < 1)
    
    ggplot2::ggplot(
        ll_df,
        ggplot2::aes(x = log(time), y = loglog, colour = quartile)
    ) +
        ggplot2::geom_step(linewidth = 0.8) +
        ggplot2::scale_colour_manual(
            values = c(Q1 = "#2D6A4F", Q2 = "#52B788",
                       Q3 = "#F4A261", Q4 = "#E76F51"),
            name   = "Dairy quartile"
        ) +
        ggplot2::labs(
            title    = "Log-log survival curves (PH assumption check)",
            subtitle = "Parallel lines support proportional hazards",
            x        = "log(time)",
            y        = "log(-log(survival))"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "right")
}


# =============================================================================
# 4. MODEL FITTING HELPERS
# =============================================================================

#' Fit one Cox model and return a tidy coefficient tibble.
#'
#' @param cox_data     Survival dataset.
#' @param exposure     Column name of the exposure.
#' @param exp_label    Label for the result tibble.
#' @param scale_100    Logical. Scale exposure to per-100 g/day? Default TRUE.
#' @return Named list: $coef_row (tibble), $fit (coxph).
.fit_one_cox <- function(cox_data, exposure, exp_label,
                         scale_100 = TRUE) {
    
    # Scale continuous exposures to per-100 g/day
    exp_col <- exposure
    if (scale_100 && is.numeric(cox_data[[exposure]])) {
        exp_col <- paste0(exposure, "_per100")
        cox_data[[exp_col]] <- cox_data[[exposure]] / 100
    }
    
    covar_z_cols <- paste0(.COX_COVARIATES, "_z")
    present_covs <- intersect(covar_z_cols, names(cox_data))
    
    rhs <- paste(c(exp_col, present_covs), collapse = " + ")
    f   <- stats::as.formula(
        paste("survival::Surv(surv_time, event) ~", rhs)
    )
    
    fit <- tryCatch(
        survival::coxph(f, data = cox_data, ties = "efron", x = TRUE),
        error = function(e) {
            cli::cli_warn("Cox model failed [{exp_label}]: {conditionMessage(e)}")
            NULL
        }
    )
    
    if (is.null(fit)) {
        return(list(
            coef_row = tibble::tibble(
                exposure = exp_label,
                hr = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                p_value = NA_real_, n = NA_integer_, n_events = NA_integer_
            ),
            fit = NULL
        ))
    }
    
    s      <- summary(fit)
    conf   <- s$conf.int
    coef_s <- s$coefficients
    
    # Extract the exposure row (first term)
    exp_row_name <- exp_col
    if (!exp_row_name %in% rownames(conf)) {
        # For factor exposures (quartile), extract all quartile rows
        exp_rows <- grep(exposure, rownames(conf), value = TRUE)
    } else {
        exp_rows <- exp_row_name
    }
    
    rows <- purrr::map_dfr(exp_rows, function(rn) {
        tibble::tibble(
            exposure = if (length(exp_rows) == 1L) exp_label
            else paste0(exp_label, ": ", rn),
            hr       = round(conf[rn, "exp(coef)"],  3),
            ci_lo    = round(conf[rn, "lower .95"],   3),
            ci_hi    = round(conf[rn, "upper .95"],   3),
            p_value  = coef_s[rn, "Pr(>|z|)"],
            n        = fit$n,
            n_events = fit$nevent
        )
    })
    
    conc <- s$concordance
    cli::cli_inform(c(
        "v" = "Cox [{exp_label}]: n={fit$n}, events={fit$nevent}, C={round(conc[1],3)}"
    ))
    
    list(coef_row = rows, fit = fit)
}


# =============================================================================
# 5. CONTINUOUS DAIRY MODEL
# =============================================================================

#' Cox model with continuous dairy exposure (per 100 g/day).
#'
#' @param cox_data Output of build_cox_simple_data().
#' @return Named list: $coef_row, $fit.
fit_cox_continuous_simple <- function(cox_data) {
    cli::cli_h2("Continuous dairy Cox model")
    .fit_one_cox(cox_data, "dairy_cumavg", "Total dairy (per 100 g/day)")
}


# =============================================================================
# 6. QUARTILE MODEL
# =============================================================================

#' Cox model with dairy quartile exposure (Q1 = reference).
#'
#' @param cox_data Output of build_cox_simple_data().
#' @return Named list: $coef_row, $fit.
fit_cox_quartile_simple <- function(cox_data) {
    cli::cli_h2("Quartile dairy Cox model")
    .fit_one_cox(cox_data, "baseline_dairy_quartile",
                 "Dairy quartile", scale_100 = FALSE)
}


# =============================================================================
# 7. SUB-TYPE MODELS
# =============================================================================

#' Cox models for each dairy sub-type (separate models, per 100 g/day).
#'
#' @param cox_data Output of build_cox_simple_data().
#' @return Tibble with one row per sub-type.
fit_cox_subtypes_simple <- function(cox_data) {
    
    present <- intersect(.COX_SUBTYPES, names(cox_data))
    if (length(present) == 0L) {
        cli::cli_warn("fit_cox_subtypes_simple(): no sub-type columns found.")
        return(tibble::tibble())
    }
    
    cli::cli_h2("Sub-type Cox models")
    
    purrr::map_dfr(present, function(subtype) {
        res <- .fit_one_cox(
            cox_data  = cox_data,
            exposure  = subtype,
            exp_label = paste0(.COX_SUBTYPE_LABELS[subtype], " (per 100 g/day)")
        )
        res$coef_row
    })
}


# =============================================================================
# 8. FOREST PLOT
# =============================================================================

#' Forest plot combining continuous, quartile, and sub-type HRs.
#'
#' @param cont_row    coef_row from fit_cox_continuous_simple().
#' @param quart_row   coef_row from fit_cox_quartile_simple().
#' @param subtype_row coef_row from fit_cox_subtypes_simple().
#' @return A ggplot object.
plot_cox_forest <- function(cont_row, quart_row, subtype_row) {
    
    df <- dplyr::bind_rows(
        dplyr::mutate(cont_row,    model = "Continuous"),
        dplyr::mutate(quart_row,   model = "Quartile"),
        dplyr::mutate(subtype_row, model = "Sub-type")
    ) |>
        dplyr::filter(!is.na(hr)) |>
        dplyr::mutate(
            sig      = p_value < 0.05,
            exposure = factor(exposure, levels = rev(unique(exposure)))
        )
    
    ggplot2::ggplot(
        df,
        ggplot2::aes(x = hr, y = exposure, xmin = ci_lo, xmax = ci_hi,
                     colour = sig)
    ) +
        ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                            colour = "grey50", linewidth = 0.5) +
        ggplot2::geom_errorbarh(height = 0.3, linewidth = 0.6) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::facet_wrap(~ model, scales = "free_y", ncol = 1L) +
        ggplot2::scale_colour_manual(
            values = c("FALSE" = "grey50", "TRUE" = "#E76F51"),
            labels = c("FALSE" = "p \u2265 0.05", "TRUE" = "p < 0.05"),
            name   = NULL
        ) +
        ggplot2::scale_x_log10() +
        ggplot2::labs(
            title    = "Cox models: HR for incident EWGSOP2 sarcopenia",
            subtitle = "Adjusted for age, BMI, energy intake (M1). Reference line at HR = 1.",
            x        = "Hazard ratio (log scale)",
            y        = NULL,
            caption  = "Continuous and sub-type exposures scaled to per 100 g/day."
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position  = "bottom",
            strip.text       = ggplot2::element_text(size = 9, face = "bold"),
            panel.grid.minor = ggplot2::element_blank()
        )
}


# =============================================================================
# 9. DIAGNOSTIC PLOTS
# =============================================================================

#' Schoenfeld residuals and Martingale residuals in one patchwork.
#'
#' @param cont_fit  coxph fit from fit_cox_continuous_simple()$fit.
#' @param cox_data  Survival dataset.
#' @return A patchwork plot.
plot_cox_diagnostics <- function(cont_fit, cox_data) {
    
    if (is.null(cont_fit)) {
        cli::cli_warn("plot_cox_diagnostics(): no fit object supplied.")
        return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    
    # ── Schoenfeld residuals ──────────────────────────────────────────────────
    zph <- tryCatch(
        survival::cox.zph(cont_fit),
        error = function(e) {
            cli::cli_warn("cox.zph() failed: {conditionMessage(e)}")
            NULL
        }
    )
    
    p_zph <- if (!is.null(zph)) {
        global_p <- zph$table["GLOBAL", "p"]
        
        zph_df <- as.data.frame(zph$y) |>
            tibble::rownames_to_column("time_str") |>
            dplyr::mutate(time = as.numeric(time_str)) |>
            tidyr::pivot_longer(-c(time, time_str),
                                names_to = "covariate",
                                values_to = "schoenfeld") |>
            dplyr::filter(covariate == covariate[1])
        
        ggplot2::ggplot(zph_df, ggplot2::aes(x = time, y = schoenfeld)) +
            ggplot2::geom_point(alpha = 0.3, size = 0.8, colour = "grey30") +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                colour = "tomato", linewidth = 0.5) +
            ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                 se = TRUE, colour = "#2E86AB",
                                 fill = "#2E86AB", alpha = 0.15,
                                 linewidth = 0.6) +
            ggplot2::labs(
                title    = "A: Schoenfeld residuals (dairy term)",
                subtitle = glue::glue("Global PH test p = {round(global_p, 4)}"),
                x        = "Time (years)",
                y        = "Scaled Schoenfeld residual"
            ) +
            ggplot2::theme_minimal(base_size = 11)
        
    } else {
        ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "Schoenfeld test unavailable") +
            ggplot2::theme_void() +
            ggplot2::labs(title = "A: Schoenfeld residuals")
    }
    
    # ── Martingale residuals vs dairy ─────────────────────────────────────────
    mart <- tryCatch(
        stats::residuals(cont_fit, type = "martingale"),
        error = function(e) NULL
    )
    
    p_mart <- if (!is.null(mart)) {
        row_idx <- as.integer(names(mart))
        mart_df <- tibble::tibble(
            dairy    = cox_data$dairy_cumavg[row_idx],
            martingale = mart
        )
        
        ggplot2::ggplot(mart_df, ggplot2::aes(x = dairy, y = martingale)) +
            ggplot2::geom_point(alpha = 0.2, size = 0.7, colour = "grey30") +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                colour = "tomato", linewidth = 0.5) +
            ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                 se = TRUE, colour = "#2E86AB",
                                 fill = "#2E86AB", alpha = 0.15,
                                 linewidth = 0.6) +
            ggplot2::labs(
                title    = "B: Martingale residuals vs dairy intake",
                subtitle = "Flat LOESS line supports linear log-hazard assumption",
                x        = "Cumulative dairy intake (g/day)",
                y        = "Martingale residual"
            ) +
            ggplot2::theme_minimal(base_size = 11)
        
    } else {
        ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "Martingale residuals unavailable") +
            ggplot2::theme_void() +
            ggplot2::labs(title = "B: Martingale residuals")
    }
    
    # ── Deviance residuals ────────────────────────────────────────────────────
    dev <- tryCatch(
        stats::residuals(cont_fit, type = "deviance"),
        error = function(e) NULL
    )
    
    p_dev <- if (!is.null(dev)) {
        dev_df <- tibble::tibble(
            fitted    = stats::predict(cont_fit, type = "lp"),
            deviance  = dev
        )
        
        ggplot2::ggplot(dev_df, ggplot2::aes(x = fitted, y = deviance)) +
            ggplot2::geom_point(alpha = 0.2, size = 0.7, colour = "grey30") +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                colour = "tomato", linewidth = 0.5) +
            ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                 se = TRUE, colour = "#2E86AB",
                                 fill = "#2E86AB", alpha = 0.15,
                                 linewidth = 0.6) +
            ggplot2::labs(
                title    = "C: Deviance residuals vs linear predictor",
                subtitle = "Outlier detection — large |deviance| = influential obs",
                x        = "Linear predictor",
                y        = "Deviance residual"
            ) +
            ggplot2::theme_minimal(base_size = 11)
        
    } else {
        ggplot2::ggplot() + ggplot2::theme_void() +
            ggplot2::labs(title = "C: Deviance residuals")
    }
    
    # ── PH p-value dotplot ────────────────────────────────────────────────────
    p_ph_dots <- if (!is.null(zph)) {
        ph_df <- as.data.frame(zph$table) |>
            tibble::rownames_to_column("covariate") |>
            dplyr::filter(covariate != "GLOBAL") |>
            dplyr::mutate(
                covariate = forcats::fct_reorder(covariate, p),
                ph_ok     = p >= 0.05
            )
        
        ggplot2::ggplot(ph_df, ggplot2::aes(x = p, y = covariate,
                                            colour = ph_ok)) +
            ggplot2::geom_point(size = 2.5) +
            ggplot2::geom_vline(xintercept = 0.05, linetype = "dashed",
                                colour = "tomato", linewidth = 0.5) +
            ggplot2::scale_colour_manual(
                values = c("TRUE" = "#2D6A4F", "FALSE" = "#E76F51"),
                labels = c("TRUE" = "PH holds", "FALSE" = "PH violated"),
                name   = NULL
            ) +
            ggplot2::labs(
                title = "D: PH test p-values per covariate",
                x     = "Schoenfeld test p-value",
                y     = NULL
            ) +
            ggplot2::theme_minimal(base_size = 11)
        
    } else {
        ggplot2::ggplot() + ggplot2::theme_void() +
            ggplot2::labs(title = "D: PH test p-values")
    }
    
    patchwork::wrap_plots(p_zph, p_mart, p_dev, p_ph_dots, ncol = 2L) +
        patchwork::plot_annotation(
            title    = "Cox model diagnostics (continuous dairy, M1)",
            theme    = ggplot2::theme(
                plot.title = ggplot2::element_text(size = 13, face = "bold")
            )
        )
}


# =============================================================================
# TOP-LEVEL WRAPPER
# =============================================================================

#' Run the full simple Cox analysis.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @param out_dir       Output directory. Default "06_outputs/cox_simple".
#' @param device        "png" or "pdf". Default "png".
#' @param width         Plot width inches. Default 12.
#' @return Invisibly returns a named list of all fit objects and tables.
run_cox_simple <- function(analysis_long,
                           out_dir = "06_outputs/cox_simple",
                           device  = "png",
                           width   = 12) {
    
    .check_pkgs_cox()
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    .save <- function(p, name, height) {
        path <- file.path(out_dir, paste0(name, ".", device))
        ggplot2::ggsave(path, plot = p, width = width, height = height,
                        dpi = 150)
        cli::cli_inform(c("v" = "Saved: {.path {path}}"))
        invisible(path)
    }
    .csv <- function(tbl, name) {
        path <- file.path(out_dir, paste0(name, ".csv"))
        utils::write.csv(tbl, path, row.names = FALSE)
        cli::cli_inform(c("v" = "Saved: {.path {path}}"))
        invisible(path)
    }
    
    cli::cli_h1("14_explore_cox_simple: starting Cox analysis")
    
    # ── Build dataset ─────────────────────────────────────────────────────────
    cli::cli_h2("Building survival dataset")
    cox_data <- build_cox_simple_data(analysis_long)
    
    # ── 1/8  KM curves ────────────────────────────────────────────────────────
    cli::cli_h2("1/8  Kaplan-Meier curves")
    p_km <- plot_km_curves(cox_data)
    .save(p_km, "01_km_curves", height = 6)
    
    # ── 2/8  Log-log curves ───────────────────────────────────────────────────
    cli::cli_h2("2/8  Log-log survival curves")
    p_ll <- plot_loglog_curves(cox_data)
    .save(p_ll, "02_loglog_curves", height = 5)
    
    # ── 3/8  Continuous model ─────────────────────────────────────────────────
    cli::cli_h2("3/8  Continuous dairy Cox model")
    cont_res  <- fit_cox_continuous_simple(cox_data)
    .csv(cont_res$coef_row, "03_continuous_coefficients")
    
    # ── 4/8  Quartile model ───────────────────────────────────────────────────
    cli::cli_h2("4/8  Quartile dairy Cox model")
    quart_res <- fit_cox_quartile_simple(cox_data)
    .csv(quart_res$coef_row, "04_quartile_coefficients")
    
    # ── 5/8  Sub-type models ──────────────────────────────────────────────────
    cli::cli_h2("5/8  Sub-type Cox models")
    subtype_res <- fit_cox_subtypes_simple(cox_data)
    .csv(subtype_res, "05_subtype_coefficients")
    
    # ── 6/8  Forest plot ──────────────────────────────────────────────────────
    cli::cli_h2("6/8  Forest plot")
    p_forest <- plot_cox_forest(
        cont_row    = cont_res$coef_row,
        quart_row   = quart_res$coef_row,
        subtype_row = subtype_res
    )
    .save(p_forest, "06_forest_plot", height = 10)
    
    # ── 7/8  Schoenfeld + Martingale diagnostics ──────────────────────────────
    cli::cli_h2("7/8  Diagnostic plots")
    p_diag <- plot_cox_diagnostics(cont_res$fit, cox_data)
    .save(p_diag, "07_diagnostics", height = 10)
    
    # ── 8/8  Summary to console ───────────────────────────────────────────────
    cli::cli_h2("8/8  Summary")
    all_coef <- dplyr::bind_rows(
        dplyr::mutate(cont_res$coef_row,  model = "continuous"),
        dplyr::mutate(quart_res$coef_row, model = "quartile"),
        dplyr::mutate(subtype_res,        model = "subtype")
    )
    
    sig_rows <- dplyr::filter(all_coef, !is.na(p_value), p_value < 0.05)
    if (nrow(sig_rows) == 0L) {
        cli::cli_inform(c("i" = "No significant associations found (p < 0.05)."))
    } else {
        cli::cli_inform(c(
            "!" = "{nrow(sig_rows)} significant association(s) (p < 0.05):",
            "*" = paste(
                glue::glue_data(sig_rows,
                                "{exposure}: HR={hr} [{ci_lo}-{ci_hi}], p={round(p_value,4)}"),
                collapse = "\n"
            )
        ))
    }
    
    cli::cli_h1("run_cox_simple() complete. Outputs in {.path {out_dir}}")
    
    invisible(list(
        cox_data    = cox_data,
        cont_res    = cont_res,
        quart_res   = quart_res,
        subtype_res = subtype_res,
        all_coef    = all_coef
    ))
}