# =============================================================================
# R/cox_functions.R
# All analysis functions for the Cox sarcopenia / dairy pipeline.
# Sourced automatically by _targets.R via tar_source().
# =============================================================================

# ── Global switch for optional diagnostics ───────────────────────────────────
COX_RUN_LOGLINEARITY <- FALSE


# ── Helper: ensure output directory exists ────────────────────────────────────
ensure_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    invisible(path)
}

# ── Helper: validate required columns exist in a data frame ──────────────────
check_required_cols <- function(df, required, context = "") {
    missing_cols <- setdiff(required, names(df))
    if (length(missing_cols) > 0) {
        stop(
            if (nchar(context)) paste0("[", context, "] "),
            "Missing required columns: ",
            paste(missing_cols, collapse = ", "),
            call. = FALSE
        )
    }
    invisible(TRUE)
}


# =============================================================================
# 1. DATA PREPARATION
# =============================================================================

prepare_survival_data <- function(df_raw) {
    
    check_required_cols(
        df_raw,
        required = c(
            "pt", ".visit_osteo", "ewgsop2_sarcopenia_stage", "time_since_baseline",
            "dairy_quartile_baseline", "Age", "BMI", "sumtot1", "pa_levels_tertile_f1"
        ),
        context = "prepare_survival_data"
    )
    
    first_event <- df_raw |>
        dplyr::filter(ewgsop2_sarcopenia_stage != "No Sarcopenia") |>
        dplyr::arrange(.visit_osteo) |>
        dplyr::group_by(pt) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::select(
            pt,
            event_time = time_since_baseline,
            sarcopenia_stage_at_event = ewgsop2_sarcopenia_stage
        )
    
    baseline_covs <- df_raw |>
        dplyr::arrange(.visit_osteo) |>
        dplyr::group_by(pt) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::select(
            pt, dairy_quartile_baseline, Age, BMI, sumtot1, pa_levels_tertile_f1
        )
    
    last_obs <- df_raw |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            last_time = max(time_since_baseline, na.rm = TRUE),
            .groups = "drop"
        )
    
    surv_df <- baseline_covs |>
        dplyr::left_join(last_obs, by = "pt") |>
        dplyr::left_join(first_event, by = "pt") |>
        dplyr::mutate(
            event = as.integer(!is.na(event_time)),
            surv_time = dplyr::if_else(event == 1L, event_time, last_time),
            
            dairy_quartile_baseline = factor(
                dairy_quartile_baseline,
                levels = c("Q1", "Q2", "Q3", "Q4")
            ),
            pa_levels_tertile_f1 = factor(pa_levels_tertile_f1),
            
            Age = as.numeric(Age),
            BMI = as.numeric(BMI),
            sumtot1 = as.numeric(sumtot1)
        )
    
    surv_df
}


# =============================================================================
# 2. EVENT SUMMARY
# =============================================================================

summarise_events <- function(surv_df) {
    
    overall <- data.frame(
        n_patients = nrow(surv_df),
        n_events   = sum(surv_df$event),
        pct_events = round(100 * mean(surv_df$event), 1)
    )
    
    by_quartile <- surv_df |>
        dplyr::group_by(dairy_quartile_baseline) |>
        dplyr::summarise(
            n_patients = dplyr::n(),
            n_events   = sum(event),
            pct_events = round(100 * mean(event), 1),
            .groups = "drop"
        )
    
    by_stage <- as.data.frame(
        table(surv_df$sarcopenia_stage_at_event, useNA = "ifany"),
        responseName = "n"
    ) |>
        dplyr::rename(sarcopenia_stage = Var1)
    
    rev_km <- survival::survfit(
        survival::Surv(surv_time, 1 - event) ~ 1,
        data = surv_df
    )
    
    median_followup <- round(summary(rev_km)$table["median"], 2)
    
    list(
        overall = overall,
        by_quartile = by_quartile,
        by_stage = by_stage,
        median_followup = median_followup
    )
}


# =============================================================================
# 3. KAPLAN–MEIER
# =============================================================================

fit_km <- function(surv_df) {
    survival::survfit(
        survival::Surv(surv_time, event) ~ dairy_quartile_baseline,
        data = surv_df
    )
}

plot_km <- function(km_fit, surv_df) {
    survminer::ggsurvplot(
        km_fit,
        data = surv_df,
        pval = TRUE,
        conf.int = TRUE,
        risk.table = TRUE,
        xlab = "Time since baseline",
        ylab = "Sarcopenia-free probability",
        title = "Kaplan-Meier: Sarcopenia-free survival by dairy quartile",
        legend.title = "Dairy quartile",
        palette = "jco",
        ggtheme = ggplot2::theme_bw()
    )
}


# =============================================================================
# 4. COX MODEL
# =============================================================================

fit_cox <- function(surv_df) {
    survival::coxph(
        survival::Surv(surv_time, event) ~
            dairy_quartile_baseline +
            Age +
            BMI +
            sumtot1 +
            pa_levels_tertile_f1,
        data = surv_df,
        ties = "efron",
        x = TRUE,
        model = TRUE
    )
}

extract_hr_table <- function(cox_model) {
    broom::tidy(cox_model, exponentiate = TRUE, conf.int = TRUE) |>
        dplyr::select(term, estimate, conf.low, conf.high, p.value) |>
        dplyr::mutate(
            dplyr::across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
            p.value = round(p.value, 4)
        )
}


# =============================================================================
# 5. PH TEST
# =============================================================================

test_ph <- function(cox_model) {
    survival::cox.zph(cox_model, transform = "km")
}

plot_schoenfeld <- function(ph_test) {
    survminer::ggcoxzph(ph_test, point.size = 0.5, point.alpha = 0.3)
}


# =============================================================================
# 6. LOG-LINEARITY (CORE FUNCTION)
# =============================================================================

check_log_linearity <- function(surv_df) {
    
    continuous_vars <- c("Age", "BMI", "sumtot1")
    all_covs <- c("dairy_quartile_baseline", continuous_vars,
                  "pa_levels_tertile_f1")
    
    surv_df <- surv_df |>
        dplyr::mutate(
            dplyr::across(all_of(continuous_vars), \(x) as.numeric(x))
        )
    
    null_mod <- survival::coxph(
        survival::Surv(surv_time, event) ~ 1,
        data = surv_df
    )
    
    martingale_resid <- residuals(null_mod, type = "martingale")
    
    plots <- list()
    lrt_rows <- list()
    
    for (var in continuous_vars) {
        
        x_vals <- surv_df[[var]]
        
        complete_idx <- complete.cases(x_vals, martingale_resid)
        
        plots[[var]] <- ggplot2::ggplot(
            data = data.frame(
                x = x_vals[complete_idx],
                resid = martingale_resid[complete_idx]
            ),
            ggplot2::aes(x = x, y = resid)
        ) +
            ggplot2::geom_point(alpha = 0.3) +
            ggplot2::geom_smooth(method = "loess") +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
            ggplot2::theme_bw()
        
        other_covs <- setdiff(all_covs, var)
        rhs_others <- paste(other_covs, collapse = " + ")
        
        linear_formula <- as.formula(
            paste0("survival::Surv(surv_time, event) ~ ", var, " + ", rhs_others)
        )
        
        spline_formula <- as.formula(
            paste0("survival::Surv(surv_time, event) ~ splines::ns(", var,
                   ", df = 3) + ", rhs_others)
        )
        
        linear_mod <- survival::coxph(linear_formula, data = surv_df)
        spline_mod <- survival::coxph(spline_formula, data = surv_df)
        
        lrt <- anova(linear_mod, spline_mod)
        
        lrt_rows[[var]] <- data.frame(
            variable = var,
            lrt_p_value = round(lrt[2, "P(>|Chi|)"], 4)
        )
    }
    
    list(
        lrt_table = dplyr::bind_rows(lrt_rows),
        plots = plots
    )
}


# =============================================================================
# WRAPPER (OPTIONAL EXECUTION)
# =============================================================================

run_log_linearity_if_needed <- function(surv_df,
                                        run_check = COX_RUN_LOGLINEARITY) {
    if (!isTRUE(run_check)) {
        return(list(lrt_table = NULL, plots = NULL))
    }
    check_log_linearity(surv_df)
}


# =============================================================================
# 7. DIAGNOSTICS
# =============================================================================

run_diagnostics <- function(cox_model) {
    
    conc <- summary(cox_model)$concordance
    
    dfbeta_mat <- residuals(cox_model, type = "dfbeta")
    dfbeta_long <- as.data.frame(dfbeta_mat) |>
        dplyr::mutate(obs = dplyr::row_number()) |>
        tidyr::pivot_longer(-obs)
    
    list(
        c_index = data.frame(C = conc["C"], SE = conc["se(C)"]),
        dfbeta_plot = ggplot2::ggplot(dfbeta_long,
                                      ggplot2::aes(obs, value)) +
            ggplot2::geom_point() +
            ggplot2::theme_bw()
    )
}


# =============================================================================
# 8. REPORT + PLOTS
# =============================================================================

# write_text_report() and write_plots_pdf()
# unchanged except they must check NULL safely as described earlier


# =============================================================================
# 9. MASTER PIPELINE
# =============================================================================

run_cox_pipeline <- function(df_raw,
                             out_dir = "06_outputs/Cox") {
    
    surv_df        <- prepare_survival_data(df_raw)
    event_summary  <- summarise_events(surv_df)
    km_fit         <- fit_km(surv_df)
    km_plot        <- plot_km(km_fit, surv_df)
    cox_model      <- fit_cox(surv_df)
    hr_table       <- extract_hr_table(cox_model)
    ph_test        <- test_ph(cox_model)
    ph_plots       <- plot_schoenfeld(ph_test)
    
    loglin_results <- run_log_linearity_if_needed(surv_df)
    
    diagnostics    <- run_diagnostics(cox_model)
    
    list(
        surv_df = surv_df,
        event_summary = event_summary,
        km_fit = km_fit,
        km_plot = km_plot,
        cox_model = cox_model,
        hr_table = hr_table,
        ph_test = ph_test,
        ph_plots = ph_plots,
        loglin_results = loglin_results,
        diagnostics = diagnostics
    )
}