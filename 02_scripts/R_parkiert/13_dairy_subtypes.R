# =============================================================================
# R/13_explore_dairy_subtypes.R
# =============================================================================
# Dairy sub-type analyses: fermented, non-fermented, low-fat, high-fat.
#
# Purpose
# -------
# Total dairy showed no association with muscle outcomes (Scripts 12).
# This script tests whether specific sub-types show signals that are masked
# when sub-types are aggregated into total dairy.
#
# Structure
# ---------
#   1. Correlation matrix between sub-type exposures (collinearity check)
#   2. Distribution plots for each sub-type by wave
#   3. Main-effect models: each sub-type as a separate exposure
#      (same M1 covariate set as Script 12, one model per sub-type × outcome)
#   4. Substitution analysis: replacing non-fermented with fermented dairy
#      (isocaloric swap — do the two sub-types differ in their association?)
#   5. Simultaneous model: fermented + non-fermented in the same model
#      (tests incremental effect of each sub-type over the other)
#   6. Dairy × time interaction for each sub-type
#   7. Forest plot summarising all sub-type betas across outcomes
#
# Outputs (written to 06_outputs/subtypes/)
# -----------------------------------------
#   01_subtype_correlations.png         Correlation matrix
#   02_subtype_distributions.png        Histograms by wave
#   03_main_effect_coefficients.csv     Sub-type main-effect betas
#   04_main_effect_forest.png           Forest plot of main-effect betas
#   05_substitution_coefficients.csv    Fermented-for-non-fermented swap betas
#   06_simultaneous_coefficients.csv    Fermented + non-fermented jointly
#   07_interaction_coefficients.csv     Sub-type × time interaction betas
#   08_interaction_trajectories.png     Predicted trajectories by sub-type quartile
#
# Usage
#   targets::tar_load(analysis_long)
#   source("04_scripts/R/13_explore_dairy_subtypes.R")
#   run_subtype_analysis(analysis_long)
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================

# ── Package guard ─────────────────────────────────────────────────────────────
.check_pkgs_subtypes <- function() {
    needed <- c("dplyr", "tidyr", "ggplot2", "patchwork", "lme4",
                "lmerTest", "forcats", "scales", "cli", "tibble",
                "glue", "purrr", "corrplot")
    missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0)
        stop("Install missing packages: ", paste(missing, collapse = ", "),
             "\nRun: install.packages(c('",
             paste(missing, collapse = "', '"), "'))")
    invisible(NULL)
}

# =============================================================================
# CONSTANTS
# =============================================================================

.SUBTYPES <- c(
    "dairy_fermented_gday",
    "dairy_non_fermented_gday",
    "dairy_lowfat_gday",
    "dairy_highfat_gday"
)

.SUBTYPE_LABELS <- c(
    dairy_fermented_gday     = "Fermented dairy (g/day)",
    dairy_non_fermented_gday = "Non-fermented dairy (g/day)",
    dairy_lowfat_gday        = "Low-fat dairy (g/day)",
    dairy_highfat_gday       = "High-fat dairy (g/day)"
)

.OUTCOMES <- list(
    grip = list(
        col       = "handgrip_max_all",
        label     = "Grip strength (kg)",
        eligible  = "eligible_hgs",
        covar_z   = c("Age_z", "BMI_z",    "energy_z"),
        raw_covar = c("Age",   "BMI",       "energy_kcal"),
        re        = "(1 + time_since_bsl_yr | pt)",
        min_obs   = 2L
    ),
    alm = list(
        col       = "ALM_HT2",
        label     = "ALMI (kg/m\u00b2)",
        eligible  = "eligible_alm",
        covar_z   = c("Age_z", "Height_z", "energy_z"),
        raw_covar = c("Age",   "Height",   "energy_kcal"),
        re        = "(1 + time_since_bsl_yr | pt)",
        min_obs   = 2L
    ),
    gait = list(
        col       = "gait_speed",
        label     = "Gait speed (m/s)",
        eligible  = "eligible_gait",
        covar_z   = c("Age_z", "BMI_z",    "energy_z"),
        raw_covar = c("Age",   "BMI",       "energy_kcal"),
        re        = "(1 | pt)",
        min_obs   = 1L,
        waves     = c("V4", "V5")
    )
)


# =============================================================================
# 1. CORRELATION MATRIX
# =============================================================================

#' Correlation matrix of dairy sub-type exposures.
#'
#' Uses Baseline values (one obs per participant) to avoid inflation from
#' repeated measures. Pearson correlations; NA pairs excluded pairwise.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A ggplot object (rendered via corrplot → captured as grob).
plot_subtype_correlations <- function(analysis_long) {
    
    present <- intersect(
        c("dairy_total_gday", .SUBTYPES),
        names(analysis_long)
    )
    
    df <- analysis_long |>
        dplyr::filter(osteo_wave == "Baseline") |>
        dplyr::select(dplyr::all_of(present)) |>
        dplyr::rename_with(
            ~ dplyr::recode(.x,
                            dairy_total_gday         = "Total",
                            dairy_fermented_gday     = "Fermented",
                            dairy_non_fermented_gday = "Non-fermented",
                            dairy_lowfat_gday        = "Low-fat",
                            dairy_highfat_gday       = "High-fat")
        )
    
    cor_mat <- cor(df, use = "pairwise.complete.obs")
    n_obs   <- sum(!is.na(df[[1]]))
    
    # Build tidy long format for ggplot
    cor_long <- as.data.frame(cor_mat) |>
        tibble::rownames_to_column("var1") |>
        tidyr::pivot_longer(-var1, names_to = "var2", values_to = "r") |>
        dplyr::mutate(
            var1 = factor(var1, levels = colnames(cor_mat)),
            var2 = factor(var2, levels = rev(colnames(cor_mat))),
            label = round(r, 2)
        )
    
    ggplot2::ggplot(cor_long, ggplot2::aes(x = var1, y = var2, fill = r)) +
        ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
        ggplot2::geom_text(ggplot2::aes(label = label), size = 3.5,
                           colour = "white", fontface = "bold") +
        ggplot2::scale_fill_gradient2(
            low      = "#E76F51",
            mid      = "white",
            high     = "#2D6A4F",
            midpoint = 0,
            limits   = c(-1, 1),
            name     = "Pearson r"
        ) +
        ggplot2::coord_fixed() +
        ggplot2::labs(
            title    = "Correlation between dairy sub-types (Baseline)",
            subtitle = glue::glue("n = {n_obs} participants with Baseline data"),
            x        = NULL,
            y        = NULL,
            caption  = "High collinearity (|r| > 0.7) may inflate SEs in simultaneous models."
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
            legend.position = "right"
        )
}


# =============================================================================
# 2. DISTRIBUTION PLOTS
# =============================================================================

#' Histograms of each dairy sub-type by wave.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A ggplot object.
plot_subtype_distributions <- function(analysis_long) {
    
    present <- intersect(.SUBTYPES, names(analysis_long))
    if (length(present) == 0L) {
        cli::cli_warn("plot_subtype_distributions(): no sub-type columns found.")
        return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    
    df_long <- analysis_long |>
        dplyr::select(pt, osteo_wave, osteo_wave_num,
                      dplyr::all_of(present)) |>
        tidyr::pivot_longer(
            cols      = dplyr::all_of(present),
            names_to  = "subtype",
            values_to = "gday"
        ) |>
        dplyr::filter(!is.na(gday)) |>
        dplyr::mutate(
            subtype = dplyr::recode(subtype, !!!.SUBTYPE_LABELS),
            subtype = factor(subtype, levels = .SUBTYPE_LABELS),
            wave    = forcats::fct_reorder(osteo_wave, osteo_wave_num)
        )
    
    medians <- df_long |>
        dplyr::group_by(subtype, wave) |>
        dplyr::summarise(med = median(gday, na.rm = TRUE), .groups = "drop")
    
    ggplot2::ggplot(df_long, ggplot2::aes(x = gday, fill = wave)) +
        ggplot2::geom_histogram(
            bins      = 35L,
            colour    = "white",
            linewidth  = 0.15,
            alpha     = 0.75,
            position  = "identity"
        ) +
        ggplot2::geom_vline(
            data        = medians,
            ggplot2::aes(xintercept = med, colour = wave),
            linetype    = "dashed",
            linewidth    = 0.5,
            show.legend = FALSE
        ) +
        ggplot2::facet_wrap(~ subtype, scales = "free", ncol = 2L) +
        ggplot2::scale_fill_brewer(palette = "Set2", name = "Wave") +
        ggplot2::scale_colour_brewer(palette = "Set2") +
        ggplot2::labs(
            title    = "Dairy sub-type distributions by wave",
            subtitle = "Dashed lines = per-wave medians",
            x        = "g / day",
            y        = "Count"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position  = "bottom",
            strip.text       = ggplot2::element_text(size = 9, face = "bold"),
            panel.grid.minor = ggplot2::element_blank()
        )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Prepare a fitting dataset for one outcome spec.
#'
#' Filters to eligible rows, applies wave restriction if needed, z-scores
#' covariates, removes rows missing on required columns, and enforces
#' minimum observations per participant.
#'
#' @param analysis_long  analysis_long tibble.
#' @param out_spec       One element of .OUTCOMES.
#' @param exposure_cols  Character vector of exposure column(s) to require
#'   non-missing.
#' @return Filtered tibble with *_z columns added.
.prep_outcome_data <- function(analysis_long, out_spec, exposure_cols) {
    
    required <- c(out_spec$col, "time_since_bsl_yr",
                  out_spec$raw_covar, exposure_cols)
    
    df <- analysis_long |>
        dplyr::filter(.data[[out_spec$eligible]])
    
    if (!is.null(out_spec$waves))
        df <- dplyr::filter(df, osteo_wave %in% out_spec$waves)
    
    df <- df |>
        dplyr::filter(
            dplyr::if_all(dplyr::any_of(required), ~ !is.na(.x))
        )
    
    # Z-score each covariate independently (per its raw name)
    for (i in seq_along(out_spec$raw_covar)) {
        raw <- out_spec$raw_covar[i]
        z   <- out_spec$covar_z[i]
        if (raw %in% names(df))
            df[[z]] <- as.numeric(scale(df[[raw]]))
    }
    
    if (out_spec$min_obs > 1L) {
        df <- df |>
            dplyr::group_by(pt) |>
            dplyr::filter(dplyr::n() >= out_spec$min_obs) |>
            dplyr::ungroup()
    }
    
    df
}


#' Fit one LME model and return a tidy one-row coefficient tibble.
#'
#' @param df         Prepared data frame from .prep_outcome_data().
#' @param f          Formula.
#' @param exposure   Name of the exposure term to extract from coefficients.
#' @param out_label  Outcome label for the result tibble.
#' @param exp_label  Exposure label for the result tibble.
#' @param add_lrt    Logical. Also fit main-effect model and run LRT?
#'   Used for interaction models. Default FALSE.
#' @param f_main     Main-effect formula (required when add_lrt = TRUE).
#' @return Named list: $coef_row (tibble), $fit (lmerMod REML),
#'   optionally $lrt_p.
.fit_one_lme <- function(df, f, exposure, out_label, exp_label,
                         add_lrt = FALSE, f_main = NULL) {
    
    fit_reml <- tryCatch(
        lmerTest::lmer(f, data = df, REML = TRUE,
                       control = lme4::lmerControl(optimizer = "bobyqa")),
        error = function(e) {
            cli::cli_warn("Model failed [{out_label} ~ {exp_label}]: {conditionMessage(e)}")
            NULL
        }
    )
    
    if (is.null(fit_reml)) {
        return(list(
            coef_row = tibble::tibble(
                outcome = out_label, exposure = exp_label,
                estimate = NA_real_, se = NA_real_,
                ci_lo = NA_real_, ci_hi = NA_real_,
                p_value = NA_real_, n_pt = NA_integer_, n_obs = NA_integer_
            ),
            fit = NULL
        ))
    }
    
    if (lme4::isSingular(fit_reml))
        cli::cli_warn("Singular fit [{out_label} ~ {exp_label}].")
    
    coef_df  <- as.data.frame(summary(fit_reml)$coefficients)
    coef_df  <- tibble::rownames_to_column(coef_df, "term")
    exp_row  <- dplyr::filter(coef_df, term == exposure)
    
    beta <- if (nrow(exp_row) > 0) exp_row$Estimate          else NA_real_
    se   <- if (nrow(exp_row) > 0) exp_row[["Std. Error"]]   else NA_real_
    pval <- if (nrow(exp_row) > 0) exp_row[["Pr(>|t|)"]]     else NA_real_
    
    lrt_p <- NA_real_
    if (add_lrt && !is.null(f_main)) {
        fit_ml      <- lmerTest::lmer(f,      data = df, REML = FALSE,
                                      control = lme4::lmerControl(optimizer = "bobyqa"))
        fit_main_ml <- lmerTest::lmer(f_main, data = df, REML = FALSE,
                                      control = lme4::lmerControl(optimizer = "bobyqa"))
        lrt_p <- tryCatch(
            as.data.frame(anova(fit_main_ml, fit_ml))[2, "Pr(>Chisq)"],
            error = function(e) NA_real_
        )
    }
    
    row <- tibble::tibble(
        outcome  = out_label,
        exposure = exp_label,
        estimate = round(beta,           5),
        se       = round(se,             5),
        ci_lo    = round(beta - 1.96*se, 5),
        ci_hi    = round(beta + 1.96*se, 5),
        p_value  = pval,
        lrt_p    = lrt_p,
        n_pt     = dplyr::n_distinct(df$pt),
        n_obs    = nrow(df)
    )
    
    cli::cli_inform(c(
        "v" = "[{out_label} ~ {exp_label}]: beta={round(beta,5)}, p={round(pval,4)}, n={nrow(df)}"
    ))
    
    list(coef_row = row, fit = fit_reml)
}


# =============================================================================
# 3. MAIN-EFFECT MODELS — one sub-type at a time
# =============================================================================

#' Fit separate main-effect LME models for each sub-type × outcome combination.
#'
#' Each model uses the same M1 covariate set as Script 12. Sub-types are
#' entered one at a time (not simultaneously) to avoid collinearity inflating
#' standard errors.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return Tibble with one row per sub-type × outcome: beta, 95% CI, p-value.
fit_subtype_main_effects <- function(analysis_long) {
    
    present_subtypes <- intersect(.SUBTYPES, names(analysis_long))
    if (length(present_subtypes) == 0L)
        cli::cli_abort("fit_subtype_main_effects(): no sub-type columns found.")
    
    cli::cli_h2("Sub-type main-effect models")
    
    results <- purrr::map_dfr(
        present_subtypes,
        function(subtype) {
            purrr::map_dfr(
                names(.OUTCOMES),
                function(out_name) {
                    spec <- .OUTCOMES[[out_name]]
                    df   <- .prep_outcome_data(analysis_long, spec, subtype)
                    
                    if (nrow(df) == 0L) {
                        cli::cli_warn("No data for [{spec$label} ~ {subtype}] — skipped.")
                        return(NULL)
                    }
                    
                    f <- stats::as.formula(glue::glue(
                        "{spec$col} ~ {subtype} + time_since_bsl_yr + ",
                        "{paste(spec$covar_z, collapse = ' + ')} + {spec$re}"
                    ))
                    
                    res <- .fit_one_lme(
                        df        = df,
                        f         = f,
                        exposure  = subtype,
                        out_label = spec$label,
                        exp_label = .SUBTYPE_LABELS[subtype]
                    )
                    res$coef_row
                }
            )
        }
    )
    
    results
}


# =============================================================================
# 4. SUBSTITUTION ANALYSIS
# =============================================================================
# "What is the association with the outcome when 100 g/day of non-fermented
#  dairy is replaced by 100 g/day of fermented dairy, keeping total dairy
#  constant?"
#
# Method: include both fermented and non-fermented in the same model PLUS
# total dairy. The coefficient on fermented then represents the effect of
# swapping 1 g/day non-fermented → fermented (total held constant).
# Reference: Ibsen et al. Am J Clin Nutr 2021.
# =============================================================================

#' Substitution analysis: fermented for non-fermented dairy.
#'
#' Model: outcome ~ dairy_fermented_gday + dairy_non_fermented_gday +
#'                  dairy_total_gday + time + covariates + (re)
#'
#' The coefficient on dairy_fermented_gday is interpreted as:
#' "replacing 1 g/day of non-fermented dairy with 1 g/day of fermented dairy
#'  (total dairy unchanged) is associated with this change in outcome."
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return Tibble with fermented and non-fermented betas per outcome.
fit_substitution_analysis <- function(analysis_long) {
    
    needed <- c("dairy_fermented_gday", "dairy_non_fermented_gday",
                "dairy_total_gday")
    present <- intersect(needed, names(analysis_long))
    
    if (length(present) < 3L) {
        cli::cli_warn(
            "fit_substitution_analysis(): requires fermented, non-fermented, ",
            "and total dairy columns. Skipping."
        )
        return(tibble::tibble())
    }
    
    cli::cli_h2("Substitution analysis: fermented for non-fermented")
    
    purrr::map_dfr(names(.OUTCOMES), function(out_name) {
        spec <- .OUTCOMES[[out_name]]
        df   <- .prep_outcome_data(
            analysis_long, spec,
            c("dairy_fermented_gday", "dairy_non_fermented_gday", "dairy_total_gday")
        )
        
        if (nrow(df) == 0L) return(NULL)
        
        f <- stats::as.formula(glue::glue(
            "{spec$col} ~ dairy_fermented_gday + dairy_non_fermented_gday + ",
            "dairy_total_gday + time_since_bsl_yr + ",
            "{paste(spec$covar_z, collapse = ' + ')} + {spec$re}"
        ))
        
        fit_reml <- tryCatch(
            lmerTest::lmer(f, data = df, REML = TRUE,
                           control = lme4::lmerControl(optimizer = "bobyqa")),
            error = function(e) {
                cli::cli_warn("Substitution model failed [{spec$label}]: {conditionMessage(e)}")
                NULL
            }
        )
        
        if (is.null(fit_reml)) return(NULL)
        
        coef_df <- as.data.frame(summary(fit_reml)$coefficients) |>
            tibble::rownames_to_column("term") |>
            dplyr::filter(term %in% c("dairy_fermented_gday",
                                      "dairy_non_fermented_gday")) |>
            dplyr::mutate(
                outcome  = spec$label,
                exposure = dplyr::recode(term, !!!.SUBTYPE_LABELS),
                estimate = round(Estimate,           5),
                se       = round(`Std. Error`,       5),
                ci_lo    = round(Estimate - 1.96 * `Std. Error`, 5),
                ci_hi    = round(Estimate + 1.96 * `Std. Error`, 5),
                p_value  = `Pr(>|t|)`,
                n_pt     = dplyr::n_distinct(df$pt),
                n_obs    = nrow(df),
                model    = "substitution (total dairy held constant)"
            ) |>
            dplyr::select(outcome, exposure, estimate, se, ci_lo, ci_hi,
                          p_value, n_pt, n_obs, model)
        
        cli::cli_inform(c(
            "v" = "Substitution [{spec$label}]: {nrow(coef_df)} terms extracted"
        ))
        
        coef_df
    })
}


# =============================================================================
# 5. SIMULTANEOUS MODEL
# =============================================================================
# Include fermented and non-fermented in the same model WITHOUT total dairy.
# Coefficients represent the independent (incremental) association of each
# sub-type over and above the other, but are sensitive to collinearity
# (check correlation matrix first).
# =============================================================================

#' Simultaneous model: fermented + non-fermented together.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return Tibble with fermented and non-fermented betas per outcome.
fit_simultaneous_model <- function(analysis_long) {
    
    needed <- c("dairy_fermented_gday", "dairy_non_fermented_gday")
    if (!all(needed %in% names(analysis_long))) {
        cli::cli_warn("fit_simultaneous_model(): fermented/non-fermented columns missing.")
        return(tibble::tibble())
    }
    
    cli::cli_h2("Simultaneous model: fermented + non-fermented")
    
    purrr::map_dfr(names(.OUTCOMES), function(out_name) {
        spec <- .OUTCOMES[[out_name]]
        df   <- .prep_outcome_data(analysis_long, spec, needed)
        
        if (nrow(df) == 0L) return(NULL)
        
        f <- stats::as.formula(glue::glue(
            "{spec$col} ~ dairy_fermented_gday + dairy_non_fermented_gday + ",
            "time_since_bsl_yr + ",
            "{paste(spec$covar_z, collapse = ' + ')} + {spec$re}"
        ))
        
        fit_reml <- tryCatch(
            lmerTest::lmer(f, data = df, REML = TRUE,
                           control = lme4::lmerControl(optimizer = "bobyqa")),
            error = function(e) {
                cli::cli_warn("Simultaneous model failed [{spec$label}]: {conditionMessage(e)}")
                NULL
            }
        )
        
        if (is.null(fit_reml)) return(NULL)
        
        if (lme4::isSingular(fit_reml))
            cli::cli_warn("Singular fit in simultaneous model [{spec$label}].")
        
        coef_df <- as.data.frame(summary(fit_reml)$coefficients) |>
            tibble::rownames_to_column("term") |>
            dplyr::filter(term %in% needed) |>
            dplyr::mutate(
                outcome  = spec$label,
                exposure = dplyr::recode(term, !!!.SUBTYPE_LABELS),
                estimate = round(Estimate,           5),
                se       = round(`Std. Error`,       5),
                ci_lo    = round(Estimate - 1.96 * `Std. Error`, 5),
                ci_hi    = round(Estimate + 1.96 * `Std. Error`, 5),
                p_value  = `Pr(>|t|)`,
                n_pt     = dplyr::n_distinct(df$pt),
                n_obs    = nrow(df),
                model    = "simultaneous (no total dairy)"
            ) |>
            dplyr::select(outcome, exposure, estimate, se, ci_lo, ci_hi,
                          p_value, n_pt, n_obs, model)
        
        cli::cli_inform(c(
            "v" = "Simultaneous [{spec$label}]: {nrow(coef_df)} terms extracted"
        ))
        
        coef_df
    })
}


# =============================================================================
# 6. DAIRY SUB-TYPE × TIME INTERACTION
# =============================================================================

#' Fit dairy sub-type × time interaction for each sub-type × outcome.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return Tibble: one row per sub-type × outcome, interaction beta + LRT p.
fit_subtype_interactions <- function(analysis_long) {
    
    present_subtypes <- intersect(.SUBTYPES, names(analysis_long))
    
    cli::cli_h2("Sub-type x time interaction models")
    
    purrr::map_dfr(present_subtypes, function(subtype) {
        purrr::map_dfr(names(.OUTCOMES), function(out_name) {
            spec <- .OUTCOMES[[out_name]]
            df   <- .prep_outcome_data(analysis_long, spec, subtype)
            
            if (nrow(df) == 0L) return(NULL)
            
            int_term  <- paste0(subtype, ":time_since_bsl_yr")
            covar_str <- paste(spec$covar_z, collapse = " + ")
            
            f_int  <- stats::as.formula(glue::glue(
                "{spec$col} ~ {subtype} * time_since_bsl_yr + ",
                "{covar_str} + {spec$re}"
            ))
            f_main <- stats::as.formula(glue::glue(
                "{spec$col} ~ {subtype} + time_since_bsl_yr + ",
                "{covar_str} + {spec$re}"
            ))
            
            res <- .fit_one_lme(
                df        = df,
                f         = f_int,
                exposure  = int_term,
                out_label = spec$label,
                exp_label = paste0(.SUBTYPE_LABELS[subtype], " \u00d7 time"),
                add_lrt   = TRUE,
                f_main    = f_main
            )
            res$coef_row
        })
    })
}


# =============================================================================
# 7. FOREST PLOT
# =============================================================================

#' Forest plot of sub-type main-effect betas across outcomes.
#'
#' One panel per outcome (rows = sub-types). Points are betas per 100 g/day
#' increase in the sub-type; error bars are 95% CIs. Vertical line at zero.
#'
#' @param coef_table Output of fit_subtype_main_effects().
#' @return A ggplot object.
plot_subtype_forest <- function(coef_table) {
    
    if (nrow(coef_table) == 0L || all(is.na(coef_table$estimate))) {
        cli::cli_warn("plot_subtype_forest(): no data to plot.")
        return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    
    # Scale betas to per-100 g/day for readability
    df <- coef_table |>
        dplyr::filter(!is.na(estimate)) |>
        dplyr::mutate(
            est_100  = estimate * 100,
            ci_lo100 = ci_lo    * 100,
            ci_hi100 = ci_hi    * 100,
            sig      = p_value < 0.05,
            exposure = factor(exposure, levels = rev(unique(exposure)))
        )
    
    ggplot2::ggplot(
        df,
        ggplot2::aes(x = est_100, y = exposure, colour = sig,
                     xmin = ci_lo100, xmax = ci_hi100)
    ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            colour = "grey50", linewidth = 0.5) +
        ggplot2::geom_errorbarh(height = 0.25, linewidth = 0.6) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::facet_wrap(~ outcome, scales = "free_x", ncol = 3L) +
        ggplot2::scale_colour_manual(
            values = c("FALSE" = "grey50", "TRUE" = "#E76F51"),
            labels = c("FALSE" = "p \u2265 0.05", "TRUE" = "p < 0.05"),
            name   = NULL
        ) +
        ggplot2::labs(
            title    = "Dairy sub-type associations with muscle outcomes",
            subtitle = "Beta (95% CI) per 100 g/day increase in sub-type exposure",
            x        = "Change in outcome per 100 g/day",
            y        = NULL,
            caption  = "Separate models — one sub-type per model. M1 covariates (Age, BMI/Height, energy)."
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position  = "bottom",
            strip.text       = ggplot2::element_text(size = 9, face = "bold"),
            panel.grid.minor = ggplot2::element_blank()
        )
}


# =============================================================================
# 8. INTERACTION TRAJECTORY PLOT (sub-types)
# =============================================================================

#' Predicted trajectories by sub-type quartile for one outcome × sub-type.
#'
#' Reuses the same logic as Script 12's plot_interaction_trajectories() but
#' is parameterised for any sub-type column.
#'
#' @param analysis_long  analysis_long tibble.
#' @param subtype        Sub-type column name.
#' @param out_spec       One element of .OUTCOMES.
#' @return A ggplot object, or NULL if the model fails.
.plot_one_subtype_trajectory <- function(analysis_long, subtype, out_spec) {
    
    df <- .prep_outcome_data(analysis_long, out_spec, subtype)
    if (nrow(df) < 10L) return(NULL)
    
    covar_str <- paste(out_spec$covar_z, collapse = " + ")
    f_int <- stats::as.formula(glue::glue(
        "{out_spec$col} ~ {subtype} * time_since_bsl_yr + ",
        "{covar_str} + {out_spec$re}"
    ))
    
    fit <- tryCatch(
        lmerTest::lmer(f_int, data = df, REML = TRUE,
                       control = lme4::lmerControl(optimizer = "bobyqa")),
        error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    
    q_breaks <- quantile(df[[subtype]], probs = c(0,.25,.5,.75,1), na.rm = TRUE)
    q_meds   <- tapply(
        df[[subtype]],
        cut(df[[subtype]], breaks = q_breaks, include.lowest = TRUE),
        median, na.rm = TRUE
    )
    q_labels <- paste0("Q", 1:4, " (", round(q_meds, 0), " g/day)")
    
    t_range <- range(df$time_since_bsl_yr, na.rm = TRUE)
    t_grid  <- seq(t_range[1], t_range[2], length.out = 50L)
    
    # Median values for nuisance covariates
    nuisance <- intersect(out_spec$covar_z, names(df))
    med_row  <- purrr::map_dfc(nuisance, function(col) {
        tibble::tibble(!!col := median(df[[col]], na.rm = TRUE))
    })
    
    pred_grid <- purrr::imap_dfr(q_meds, function(val, lbl) {
        base <- tibble::tibble(
            !!subtype             := val,
            time_since_bsl_yr     = t_grid,
            quartile              = lbl
        )
        if (ncol(med_row) > 0L)
            base <- dplyr::bind_cols(base, med_row[rep(1L, nrow(base)), ])
        base
    }) |>
        dplyr::mutate(quartile = factor(quartile,
                                        levels = q_labels[order(q_meds)]))
    
    pred_grid$predicted <- tryCatch(
        predict(fit, newdata = pred_grid, re.form = NA),
        error = function(e) rep(NA_real_, nrow(pred_grid))
    )
    
    exp_label <- .SUBTYPE_LABELS[subtype]
    
    ggplot2::ggplot() +
        ggplot2::geom_line(
            data    = df |>
                dplyr::mutate(
                    dairy_q = cut(.data[[subtype]], breaks = q_breaks,
                                  include.lowest = TRUE,
                                  labels = q_labels[order(q_meds)])
                ) |> dplyr::filter(!is.na(dairy_q)),
            mapping = ggplot2::aes(
                x      = time_since_bsl_yr,
                y      = .data[[out_spec$col]],
                group  = pt,
                colour = dairy_q
            ),
            alpha = 0.07, linewidth = 0.25
        ) +
        ggplot2::geom_line(
            data    = pred_grid,
            mapping = ggplot2::aes(
                x      = time_since_bsl_yr,
                y      = predicted,
                colour = quartile
            ),
            linewidth = 1.1
        ) +
        ggplot2::scale_colour_manual(
            values = c("#2D6A4F", "#52B788", "#F4A261", "#E76F51"),
            name   = exp_label
        ) +
        ggplot2::labs(
            title = glue::glue("{out_spec$label} ~ {exp_label}"),
            x     = "Time since Baseline (yr)",
            y     = out_spec$label
        ) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(
            plot.title      = ggplot2::element_text(size = 9, face = "bold"),
            legend.position = "right",
            legend.text     = ggplot2::element_text(size = 7)
        )
}


#' All sub-type × outcome trajectory panels in one patchwork figure.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @return A patchwork plot.
plot_subtype_trajectories <- function(analysis_long) {
    
    present_subtypes <- intersect(.SUBTYPES, names(analysis_long))
    
    plots <- purrr::map(present_subtypes, function(subtype) {
        purrr::map(names(.OUTCOMES), function(out_name) {
            .plot_one_subtype_trajectory(
                analysis_long, subtype, .OUTCOMES[[out_name]]
            )
        })
    }) |>
        purrr::flatten() |>
        purrr::compact()   # drop NULLs from failed models
    
    if (length(plots) == 0L) {
        cli::cli_warn("plot_subtype_trajectories(): no plots generated.")
        return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    
    n_cols <- min(3L, length(.OUTCOMES))
    
    patchwork::wrap_plots(plots, ncol = n_cols) +
        patchwork::plot_annotation(
            title    = "Sub-type \u00d7 time interaction: predicted trajectories",
            subtitle = "Bold = population-average prediction by sub-type quartile",
            theme    = ggplot2::theme(
                plot.title    = ggplot2::element_text(size = 12, face = "bold"),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
            )
        )
}


# =============================================================================
# TOP-LEVEL WRAPPER
# =============================================================================

#' Run the full dairy sub-type analysis.
#'
#' @param analysis_long Output of freeze_dataset()$data.
#' @param out_dir       Output directory. Default "06_outputs/subtypes".
#' @param device        "png" or "pdf". Default "png".
#' @param width         Plot width inches. Default 14.
#' @return Invisibly returns a named list of all result tables and fit objects.
run_subtype_analysis <- function(analysis_long,
                                 out_dir = "06_outputs/subtypes",
                                 device  = "png",
                                 width   = 14) {
    
    .check_pkgs_subtypes()
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    .save <- function(p, name, height) {
        path <- file.path(out_dir, paste0(name, ".", device))
        ggplot2::ggsave(path, plot = p, width = width, height = height, dpi = 150)
        cli::cli_inform(c("v" = "Saved: {.path {path}}"))
        invisible(path)
    }
    .csv <- function(tbl, name) {
        path <- file.path(out_dir, paste0(name, ".csv"))
        utils::write.csv(tbl, path, row.names = FALSE)
        cli::cli_inform(c("v" = "Saved: {.path {path}}"))
        invisible(path)
    }
    
    cli::cli_h1("13_explore_dairy_subtypes: starting sub-type analysis")
    
    # ── 1/8  Correlation matrix ───────────────────────────────────────────────
    cli::cli_h2("1/8  Sub-type correlation matrix")
    p_cor <- plot_subtype_correlations(analysis_long)
    .save(p_cor, "01_subtype_correlations", height = 6)
    
    # ── 2/8  Distributions ────────────────────────────────────────────────────
    cli::cli_h2("2/8  Sub-type distributions")
    p_dist <- plot_subtype_distributions(analysis_long)
    .save(p_dist, "02_subtype_distributions", height = 8)
    
    # ── 3/8  Main-effect models ───────────────────────────────────────────────
    cli::cli_h2("3/8  Main-effect models")
    main_coef <- fit_subtype_main_effects(analysis_long)
    .csv(main_coef, "03_main_effect_coefficients")
    
    # ── 4/8  Forest plot ──────────────────────────────────────────────────────
    cli::cli_h2("4/8  Forest plot")
    p_forest <- plot_subtype_forest(main_coef)
    .save(p_forest, "04_main_effect_forest", height = 6)
    
    # ── 5/8  Substitution analysis ────────────────────────────────────────────
    cli::cli_h2("5/8  Substitution analysis")
    sub_coef <- fit_substitution_analysis(analysis_long)
    .csv(sub_coef, "05_substitution_coefficients")
    
    # ── 6/8  Simultaneous model ───────────────────────────────────────────────
    cli::cli_h2("6/8  Simultaneous model")
    sim_coef <- fit_simultaneous_model(analysis_long)
    .csv(sim_coef, "06_simultaneous_coefficients")
    
    # ── 7/8  Sub-type × time interaction ─────────────────────────────────────
    cli::cli_h2("7/8  Sub-type x time interactions")
    int_coef <- fit_subtype_interactions(analysis_long)
    .csv(int_coef, "07_interaction_coefficients")
    
    # ── 8/8  Interaction trajectory plots ────────────────────────────────────
    cli::cli_h2("8/8  Interaction trajectory plots")
    p_traj <- plot_subtype_trajectories(analysis_long)
    .save(p_traj, "08_interaction_trajectories", height = 14)
    
    cli::cli_h1("run_subtype_analysis() complete. Outputs in {.path {out_dir}}")
    
    invisible(list(
        main_coef = main_coef,
        sub_coef  = sub_coef,
        sim_coef  = sim_coef,
        int_coef  = int_coef
    ))
}