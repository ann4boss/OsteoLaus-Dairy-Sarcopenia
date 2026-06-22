# =============================================================================
# LMM Report for GAIT SPEED — pooled MICE results printed to a dated PDF
#
# One outcome (gait_speed), multiple exposures × multiple covariate sets.
# Uses mice::with() + mice::pool() internally.
# =============================================================================

# ---------------------------------------------------------------------------
# 1.  PER-IMPUTATION DATA PREPARATION
# ---------------------------------------------------------------------------

.prep_one_imputation_gait <- function(  # Changed: _gait suffix
    df,
    outcome       = "gait_speed",
    resp_col      = outcome,
    outcome_fn    = identity,
    id_var        = "pt",
    time_var      = "time_since_baseline",
    scale_centres = NULL
) {
    if (is.null(scale_centres)) {
        scale_centres <- list(
            age_lag    = median(df$age_at_baseline_lag,         na.rm = TRUE),
            dairy_lag  = median(df$dairy_total_gday_cumavg_lag, na.rm = TRUE),
            fermented_lag = median(df$dairy_fermented_gday_cumavg_lag, na.rm = TRUE),  
            nonfermented_lag = median(df$dairy_non_fermented_gday_cumavg_lag, na.rm = TRUE),  
            highfat_lag = median(df$dairy_highfat_gday_cumavg_lag, na.rm = TRUE),  
            lowfat_lag = median(df$dairy_lowfat_gday_cumavg_lag, na.rm = TRUE), 
            time   = median(df[[time_var]],             na.rm = TRUE)
        )
    }
    
    df[[resp_col]] <- outcome_fn(df[[outcome]])
    
    df <- df |>
        dplyr::mutate(
            age_at_baseline_scaled_lag      = as.numeric(scale(df$age_at_baseline_lag,
                                                               center = scale_centres$age_lag,
                                                               scale  = 10)),
            
            dairy_100g_lag       = as.numeric(scale(df$dairy_total_gday_cumavg_lag ,
                                                    center = scale_centres$dairy_lag ,
                                                    scale  = 100)),
            fermented_100g_lag        = as.numeric(scale(df$dairy_fermented_gday_cumavg_lag ,
                                                         center = scale_centres$fermented_lag ,
                                                         scale  = 100)),
            nonfermented_100g_lag        = as.numeric(scale(df$dairy_non_fermented_gday_cumavg_lag ,
                                                            center = scale_centres$nonfermented_lag ,
                                                            scale  = 100)),
            highfat_100g_lag       = as.numeric(scale(df$dairy_highfat_gday_cumavg_lag ,
                                                      center = scale_centres$highfat_lag ,
                                                      scale  = 100)),
            lowfat_100g_lag        = as.numeric(scale(df$dairy_lowfat_gday_cumavg_lag ,
                                                      center = scale_centres$lowfat_lag ,
                                                      scale  = 100)),
            
            dairy_quartile_baseline = factor(dairy_quartile_baseline,
                                             levels  = c("Q1","Q2","Q3","Q4"),
                                             ordered = FALSE) |>
                stats::relevel(ref = "Q1"),
            
            dairy_guidelines_port   = factor(dairy_guidelines_port,
                                             levels  = c("< 2 servings/day",
                                                         ">= 2 servings/day"),
                                             ordered = FALSE) |>
                stats::relevel(ref = "< 2 servings/day"),
            
            dairy_quartile_baseline_lag = factor(dairy_quartile_baseline_lag,  # Fixed: was "airy"
                                                 levels  = c("Q1","Q2","Q3","Q4"),
                                                 ordered = FALSE) |>
                stats::relevel(ref = "Q1"),
            
            dairy_guidelines_port_lag   = factor(dairy_guidelines_port_lag,
                                                 levels  = c("< 2 servings/day",
                                                             "≥ 2 servings/day"),
                                                 ordered = FALSE) |>
                stats::relevel(ref = "< 2 servings/day"),
            
            BMI_category         = factor(BMI_category,
                                          levels  = c("Underweight","Normal",
                                                      "Overweight","Obese"),
                                          ordered = FALSE) |>
                stats::relevel(ref = "Normal"),
            
            education_level      = factor(education_level,
                                          levels  = c("Low (ISCED 0-2)",
                                                      "Medium (ISCED 3-4)",
                                                      "High (ISCED 5-8)"),
                                          ordered = FALSE) |>
                stats::relevel(ref = "Low (ISCED 0-2)"),
            
            smoking_status       = factor(smoking_status,
                                          levels  = c("Never","Former","Current"),
                                          ordered = FALSE) |>
                stats::relevel(ref = "Never"),
            
            pa_levels_tertile_f1 = factor(pa_levels_tertile_f1,
                                          levels  = c("Low","Medium","High"),
                                          ordered = FALSE) |>
                stats::relevel(ref = "Low"),
            
            diabetes_status      = factor(diabetes_status,
                                          levels  = c("No diabetes","Diabetes"),
                                          ordered = FALSE) |>
                stats::relevel(ref = "No diabetes"),
            
            BMI_category_lag        = factor(BMI_category_lag,
                                             levels  = c("Underweight","Normal",
                                                         "Overweight","Obese"),
                                             ordered = FALSE) |>
                stats::relevel(ref = "Normal"),
            
            education_level_lag      = factor(education_level_lag,
                                              levels  = c("Low (ISCED 0-2)",
                                                          "Medium (ISCED 3-4)",
                                                          "High (ISCED 5-8)"),
                                              ordered = FALSE) |>
                stats::relevel(ref = "Low (ISCED 0-2)"),
            
            smoking_status_lag       = factor(smoking_status_lag,
                                              levels  = c("Never","Former","Current"),
                                              ordered = FALSE) |>
                stats::relevel(ref = "Never"),
            
            pa_levels_tertile_f1_lag = factor(pa_levels_tertile_f1_lag,
                                              levels  = c("Low","Medium","High"),
                                              ordered = FALSE) |>
                stats::relevel(ref = "Low"),
            
            diabetes_status_lag      = factor(diabetes_status_lag,
                                              levels  = c("No diabetes","Diabetes"),
                                              ordered = FALSE) |>
                stats::relevel(ref = "No diabetes"),
            
            dplyr::across(dplyr::all_of(id_var),
                          ~ factor(.x, ordered = FALSE))
        )
    
    list(df = df, scale_centres = scale_centres, resp_col = resp_col)
}


# ---------------------------------------------------------------------------
# 2.  FORMULA BUILDER
# ---------------------------------------------------------------------------

.build_formula_gait <- function(  # Changed: _gait suffix
    resp_col, exposure, exposure_type,
    covariates, time_var, id_var,
    random_slope, interaction
) {
    exp_term <- switch(
        exposure_type,
        linear      = exposure,
        categorical = exposure,
        rcs         = paste0("rms::rcs(", exposure, ", 3)"),
        ns          = paste0("splines::ns(", exposure, ", df = 3)"),
        stop("Unknown exposure_type: ", exposure_type)
    )
    
    rhs <- c(exp_term, time_var, covariates)
    
    if (isTRUE(interaction))
        rhs <- c(rhs, paste0(exp_term, ":", time_var))
    
    re <- if (isTRUE(random_slope)) {
        paste0("(1 + ", time_var, " | ", id_var, ")")
    } else {
        paste0("(1 | ", id_var, ")")
    }
    
    stats::as.formula(paste(resp_col, "~", paste(c(rhs, re), collapse = " + ")))
}


# ---------------------------------------------------------------------------
# 3.  POOLED MICE FIT
# ---------------------------------------------------------------------------

fit_pooled_lmm_gait <- function(  # Already has _gait
    mids_object,
    formula,
    outcome      = "gait_speed",  # Changed default
    resp_col     = outcome,
    outcome_fn   = identity,
    ref_col      = NULL,
    ref_lev      = NULL,
    id_var       = "pt",
    time_var     = "time_since_baseline",
    random_slope = TRUE
) {
    m      <- mids_object$m
    ctrl   <- lme4::lmerControl(optimizer = "bobyqa",
                                optCtrl  = list(maxfun = 20000))
    
    # Compute scaling centres once from imputation 1
    df1     <- mice::complete(mids_object, 1)
    prep1   <- .prep_one_imputation_gait(df1,  # Changed: _gait
                                         outcome      = outcome,
                                         resp_col     = resp_col,
                                         outcome_fn   = outcome_fn,
                                         id_var       = id_var,
                                         time_var     = time_var)
    centres <- prep1$scale_centres
    
    models <- vector("list", m)
    for (i in seq_len(m)) {
        df_i   <- mice::complete(mids_object, i)
        prep_i <- .prep_one_imputation_gait(df_i,  # Changed: _gait
                                            outcome       = outcome,
                                            resp_col      = resp_col,
                                            outcome_fn    = outcome_fn,
                                            id_var        = id_var,
                                            time_var      = time_var,
                                            scale_centres = centres)
        df_fit <- prep_i$df
        
        # Apply reference level for categorical exposures
        if (!is.null(ref_col) && !is.null(ref_lev)) {
            df_fit[[ref_col]] <- stats::relevel(
                factor(df_fit[[ref_col]], ordered = FALSE),
                ref = ref_lev
            )
        }
        
        models[[i]] <- lmerTest::lmer(formula, data = df_fit,
                                      REML = FALSE, control = ctrl)
    }
    
    pooled      <- mice::pool(mice::as.mira(models))
    pooled_tidy <- summary(pooled, conf.int = TRUE) |>
        tibble::as_tibble() |>
        dplyr::rename(
            term      = term,
            estimate  = estimate,
            std_error = std.error,
            statistic = statistic,
            df        = df,
            p_value   = p.value,
            conf_low  = `2.5 %`,
            conf_high = `97.5 %`
        ) |>
        dplyr::select(term, estimate, std_error, statistic, df,
                      p_value, conf_low, conf_high)
    
    list(
        pooled_tidy = pooled_tidy,
        first_model = models[[1]],
        formula     = formula,
        n_imp       = m
    )
}


# ---------------------------------------------------------------------------
# 4.  PDF HELPERS
# ---------------------------------------------------------------------------
.pal_gait <- c(  # Changed: _gait suffix
    "#E76254FF","#EF8A47FF","#F7AA58FF","#FFD06FFF",
    "#AADCE0FF","#72BCD5FF","#528FADFF","#376795FF","#1E466EFF"
)

.term_labels_gait <- c(
    dairy_100g_lag                            = "Dairy intake cum. avg. [100g/day]",
    fermented_100g_lag                         = "Fermented Dairy intake cum. avg. [100g/day]",
    nonfermented_100g_lag                     = "Non-Fermented Dairy intake cum. avg. [100g/day]",
    highfat_100g_lag                           = "High Fat Dairy intake cum. avg. [100g/day]",
    lowfat_100g_lag                            = "Low Fat Dairy intake cum. avg. [100g/day]",
    dairy_quartile_baseline_lagQ2             = "Dairy intake Q2",
    dairy_quartile_baseline_lagQ3             = "Dairy intake Q3",
    dairy_quartile_baseline_lagQ4             = "Dairy intake Q4",
    `dairy_guidelines_port_lag>= 2 servings/day` = "Dairy intake ≥ 2 servings/day",
    time_since_baseline                   = "Time since baseline [year]",
    age_at_baseline_scaled_lag               = "Age at T1 [year]", 
    BMI_category_lagUnderweight               = "BMI - Underweight",
    BMI_category_lagOverweight                = "BMI - Overweight",
    BMI_category_lagObese                     = "BMI - Obese",
    `education_level_lagMedium (ISCED 3-4)` = "Education - Medium (ISCED 3-4)",
    `education_level_lagHigh (ISCED 5-8)`   = "Education - High (ISCED 5-8)",
    smoking_status_lagFormer                  = "Smoking - Former",
    smoking_status_lagCurrent                 = "Smoking - Current",
    pa_levels_tertile_f1_lagMedium            = "Physical activity - Medium",
    pa_levels_tertile_f1_lagHigh              = "Physical activity - High",
    diabetes_status_lagDiabetes               = "Diabetes - Yes"
)

.theme_report_gait <- function() {  # Changed: _gait suffix
    ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            panel.grid  = ggplot2::element_blank(),
            axis.line   = ggplot2::element_line(colour = "black", linewidth = 0.4),
            axis.ticks  = ggplot2::element_line(colour = "black"),
            axis.text   = ggplot2::element_text(colour = "black", family = "Helvetica"),
            axis.title  = ggplot2::element_text(colour = "black", family = "Helvetica"),
            plot.title  = ggplot2::element_text(face = "bold", size = 13,
                                                family = "Helvetica"),
            text        = ggplot2::element_text(family = "Helvetica"),
            plot.margin = ggplot2::margin(4, 4, 4, 4)
        )
}

.section_page_gait <- function(title, subtitle = NULL) {  # Changed: _gait suffix
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = .pal_gait[9], col = NA))  # Changed: .pal_gait
    grid::grid.text(title, x = 0.5, y = if (is.null(subtitle)) 0.5 else 0.55,
                    gp = grid::gpar(col = "white", fontsize = 16,
                                    fontface = "bold"))
    if (!is.null(subtitle))
        grid::grid.text(subtitle, x = 0.5, y = 0.43,
                        gp = grid::gpar(col = .pal_gait[5], fontsize = 11))  # Changed: .pal_gait
}

.text_page_gait <- function(lines, title = NULL) {  # Changed: _gait suffix
    grid::grid.newpage()
    y <- 0.97
    if (!is.null(title)) {
        grid::grid.text(title, x = 0.04, y = y, just = "left",
                        gp = grid::gpar(fontface = "bold", fontsize = 12))
        y <- y - 0.05
    }
    for (ln in lines) {
        grid::grid.text(ln, x = 0.04, y = y, just = "left",
                        gp = grid::gpar(fontsize = 7.5, fontfamily = "mono"))
        y <- y - 0.022
        if (y < 0.04) { grid::grid.newpage(); y <- 0.97 }
    }
}

.results_page_gait <- function(tidy_df, title) { 
    
    # Round for display
    disp <- tidy_df |>
        dplyr::mutate(
            dplyr::across(c(estimate, std_error, statistic, conf_low, conf_high),
                          ~ round(.x, 3)),
            p_value = dplyr::case_when(
                p_value < 0.001 ~ "<0.001",
                TRUE            ~ as.character(round(p_value, 3))
            ),
            df = round(df, 1)
        )
    
    # ── Coefficient plot (exclude intercept) ----------------------------------
    plot_df <- tidy_df |>
        dplyr::filter(term != "(Intercept)") |>
        dplyr::mutate(
            sig = p_value < 0.05,
            # Get the base label (without asterisk)
            term_label_base = {
                idx <- match(term, names(.term_labels_gait))
                ifelse(is.na(idx), term, .term_labels_gait[idx])
            },
            # Create display label with asterisk if significant
            term_label_display = dplyr::case_when(
                sig ~ paste0(term_label_base, " *"),
                TRUE ~ term_label_base
            ),
            # Create factor with proper ordering (using base labels)
            term_label = factor(
                term_label_base,
                levels = rev(c(
                    unname(.term_labels_gait),
                    sort(unique(term_label_base[!(term %in% names(.term_labels_gait))]))
                ))
            )
        )
    
    p_coef <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = estimate, y = term_label,
                     xmin = conf_low, xmax = conf_high,
                     colour = sig)
    ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            colour = "grey60") +
        ggplot2::geom_errorbarh(height = 0.3, linewidth = 1) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_colour_manual(
            values = c(`TRUE` = "#92D050", `FALSE` = "#FF6666"),
            guide  = "none"
        ) +
        ggplot2::scale_y_discrete(
            expand = ggplot2::expansion(add = 0.1),
            labels = setNames(plot_df$term_label_display, plot_df$term_label_base)
        ) +
        ggplot2::labs(
            x = "Estimate (95 % CI)", 
            y = NULL, 
            title = title,
            caption = "* p < 0.05"
        ) +
        .theme_report_gait() +  # FIXED: was .theme_report()
        ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = 14),
            axis.text.x = ggplot2::element_text(size = 9),
            plot.margin = ggplot2::margin(2, 2, 2, 2)
        )
    
    # Fix panel height so rows are compact regardless of page size
    n_terms    <- nlevels(plot_df$term_label)
    coef_grob  <- ggplot2::ggplotGrob(p_coef)
    panel_row  <- which(coef_grob$layout$name == "panel")
    coef_grob$heights[coef_grob$layout$t[panel_row]] <-
        grid::unit(n_terms * 0.18, "in")
    
    # ── Numeric table ---------------------------------------------------------
    tbl_grob <- gridExtra::tableGrob(
        disp,
        rows  = NULL,
        theme = gridExtra::ttheme_minimal(
            base_size   = 7,
            core        = list(fg_params = list(hjust = 0, x = 0.05)),
            colhead     = list(fg_params = list(fontface = "bold", hjust = 0,
                                                x = 0.05))
        )
    )
    
    gridExtra::grid.arrange(coef_grob, tbl_grob, ncol = 2, widths = c(1.2, 1.8))
}

# ── Sandwich vs standard comparison page ------------------------------------

.sandwich_comparison_page_gait<- function(std_tidy, sand_tidy, title) {
    
    # Combine, exclude intercept
    combined <- dplyr::bind_rows(
        dplyr::mutate(std_tidy,  estimator = "Standard LMM"),
        dplyr::mutate(sand_tidy, estimator = "Sandwich (CR2)")
    ) |>
        dplyr::filter(term != "(Intercept)") |>
        dplyr::mutate(
            sig = p_value < 0.05,
            term_label = {
                idx <- match(term, names(.term_labels_gait))
                ifelse(is.na(idx), term, .term_labels_gait[idx])
            },
            term_label = factor(term_label,
                                levels = rev(unique(term_label))),
            estimator  = factor(estimator,
                                levels = c("Standard LMM", "Sandwich (CR2)"))
        )
    
    colours <- c("Standard LMM"   = .pal[8],
                 "Sandwich (CR2)" = .pal[2])
    shapes  <- c("Standard LMM"   = 16L,
                 "Sandwich (CR2)" = 17L)
    
    p <- ggplot2::ggplot(
        combined,
        ggplot2::aes(x      = estimate,
                     y      = term_label,
                     xmin   = conf_low,
                     xmax   = conf_high,
                     colour = estimator,
                     shape  = estimator)
    ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            colour = "grey60") +
        ggplot2::geom_errorbarh(
            ggplot2::aes(height = 0),
            linewidth = 0.8,
            position  = ggplot2::position_dodge(width = 0.5)
        ) +
        ggplot2::geom_point(
            size     = 2.5,
            position = ggplot2::position_dodge(width = 0.5)
        ) +
        ggplot2::scale_colour_manual(values = colours, name = "Estimator") +
        ggplot2::scale_shape_manual(values  = shapes,  name = "Estimator") +
        ggplot2::labs(
            x       = "Estimate (95 % CI)",
            y       = NULL,
            title   = paste("Robustness Check — Sandwich vs Standard:", title),
            caption = "CR2: cluster-robust (clubSandwich); clusters = subject ID"
        ) +
        .theme_report() +
        ggplot2::theme(
            legend.position = "bottom",
            axis.text.y     = ggplot2::element_text(size = 11)
        )
    
    # Numeric comparison table (delta SE, delta p)
    comp_tbl <- dplyr::inner_join(
        std_tidy  |> dplyr::select(term, estimate,
                                   std_error_std  = std_error,
                                   p_std          = p_value),
        sand_tidy |> dplyr::select(term,
                                   std_error_sand = std_error,
                                   p_sand         = p_value),
        by = "term"
    ) |>
        dplyr::mutate(
            estimate       = round(estimate, 3),
            std_error_std  = round(std_error_std,  3),
            std_error_sand = round(std_error_sand, 3),
            delta_se       = round(std_error_sand - std_error_std, 4),
            p_std  = dplyr::if_else(p_std  < 0.001, "<0.001",
                                    as.character(round(p_std,  3))),
            p_sand = dplyr::if_else(p_sand < 0.001, "<0.001",
                                    as.character(round(p_sand, 3)))
        ) |>
        dplyr::rename(
            Term        = term,
            Beta        = estimate,
            SE_std      = std_error_std,
            SE_sandwich = std_error_sand,
            `ΔSE`       = delta_se,
            p_std       = p_std,
            p_sandwich  = p_sand
        )
    
    tbl_grob <- gridExtra::tableGrob(
        comp_tbl,
        rows  = NULL,
        theme = gridExtra::ttheme_minimal(
            base_size = 7,
            core      = list(fg_params = list(hjust = 0, x = 0.05)),
            colhead   = list(fg_params = list(fontface = "bold",
                                              hjust = 0, x = 0.05))
        )
    )
    
    gridExtra::grid.arrange(p, tbl_grob, ncol = 2, widths = c(1.4, 1.6))
}



# ── Diagnostic plots ---------------------------------------------------------

.plot_diagnostics_gait <- function(model, label = "") {  # Already has _gait
    res_v  <- residuals(model)
    fit_v  <- fitted(model)
    df_d   <- data.frame(fitted    = fit_v,
                         residuals = res_v,
                         sqrt_abs  = sqrt(abs(res_v)))
    
    p_rd <- ggplot2::ggplot(df_d, ggplot2::aes(fitted, residuals)) +
        ggplot2::geom_point(colour = .pal_gait[8], alpha = 0.45, size = 1.2) +  # Changed: .pal_gait
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = .pal_gait[1]) +  # Changed: .pal_gait
        ggplot2::geom_smooth(method = "lm", se = TRUE, colour = .pal_gait[1],  # Changed: .pal_gait
                             fill = .pal_gait[4], alpha = 0.2, linewidth = 0.7) +  # Changed: .pal_gait
        ggplot2::labs(x = "Fitted", y = "Residuals",
                      title = paste("A  Residuals vs Fitted", label)) +
        .theme_report_gait()  # Changed: _gait
    
    p_sl <- ggplot2::ggplot(df_d, ggplot2::aes(fitted, sqrt_abs)) +
        ggplot2::geom_point(colour = .pal_gait[8], alpha = 0.45, size = 1.2) +  # Changed: .pal_gait
        ggplot2::geom_smooth(method = "lm", se = TRUE, colour = .pal_gait[1],  # Changed: .pal_gait
                             fill = .pal_gait[3], alpha = 0.2, linewidth = 0.7) +  # Changed: .pal_gait
        ggplot2::labs(x = "Fitted", y = expression(sqrt("|Residuals|")),
                      title = "B  Scale-Location") +
        .theme_report_gait()  # Changed: _gait
    
    p_qq <- ggplot2::ggplot(df_d, ggplot2::aes(sample = residuals)) +
        ggplot2::stat_qq(colour = .pal_gait[8], alpha = 0.45, size = 1.2) +  # Changed: .pal_gait
        ggplot2::stat_qq_line(colour = .pal_gait[1], linewidth = 0.7) +  # Changed: .pal_gait
        ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles",
                      title = "C  Q-Q") +
        .theme_report_gait()  # Changed: _gait
    
    patchwork::wrap_plots(p_rd, p_sl, p_qq, ncol = 3)
}


# ---------------------------------------------------------------------------
# 5.  MAIN REPORT FUNCTION
# ---------------------------------------------------------------------------

run_lmm_report_gait <- function(  # Already has _gait
    mids_object,
    outcome        = "gait_speed",
    outcome_fn     = identity,
    exposures      = tibble::tibble(
        exposure      = "dairy_100g",
        exposure_type = "linear",
        ref_level     = NA_character_
    ),
    covariate_sets = list(
        minimal = c("age_at_baseline_scaled", "BMI_category", "education_level",
                    "smoking_status", "pa_levels_tertile_f1",
                    "diabetes_status")
    ),
    random_slope   = TRUE,
    interaction    = FALSE,
    id_var         = "pt",
    time_var       = "time_since_baseline",
    out_dir        = "03_outputs/LMM_exploratory"
) {
    # ── Resolve outcome column name -------------------------------------------
    fn_name <- deparse(substitute(outcome_fn))
    if (grepl("^[a-zA-Z_][a-zA-Z0-9_.]*$", fn_name)) {
        # looks like a plain name — keep it
    } else {
        fn_name <- tryCatch(
            deparse(as.list(body(outcome_fn))[[1]]),
            error = function(e) "transformed"
        )
    }
    resp_col <- if (identical(outcome_fn, identity) || fn_name == "identity") {
        outcome
    } else {
        paste0(outcome, "_", fn_name)
    }
    
    # ── PDF setup -------------------------------------------------------------
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    pdf_path  <- file.path(out_dir,
                           paste0(resp_col, "_LMM_gait_", timestamp, ".pdf"))  # Added _gait to filename
    
    grDevices::pdf(pdf_path, width = 14, height = 9)
    on.exit(grDevices::dev.off(), add = TRUE)
    
    # ── Cover page -----------------------------------------------------------
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#1E466EFF", col = NA))
    grid::grid.text(paste0("LMM Report  —  GAIT SPEED  —  ", resp_col),  # Changed: added GAIT SPEED
                    x = 0.5, y = 0.60,
                    gp = grid::gpar(col = "white", fontsize = 24,
                                    fontface = "bold"))
    grid::grid.text(paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
                    x = 0.5, y = 0.48,
                    gp = grid::gpar(col = .pal_gait[5], fontsize = 12))  # Changed: .pal_gait
    info_lines <- c(
        paste0("Imputations: ", mids_object$m),
        paste0("Random slope: ", random_slope,
               "  |  Interaction: ", interaction),
        paste0("Exposures: ", nrow(exposures),
               "  |  Covariate sets: ", length(covariate_sets))
    )
    for (k in seq_along(info_lines))
        grid::grid.text(info_lines[k], x = 0.5, y = 0.40 - (k - 1) * 0.06,
                        gp = grid::gpar(col = .pal_gait[5], fontsize = 10))  # Changed: .pal_gait
    
    # ── Configuration summary ------------------------------------------------
    .text_page_gait(  # Changed: _gait
        c(paste("Outcome column:", resp_col),
          paste("Outcome transform:", fn_name),
          paste("Random slope:", random_slope),
          paste("Interaction (exposure × time):", interaction),
          "",
          "Exposures:",
          utils::capture.output(print(as.data.frame(exposures))),
          "",
          "Covariate sets:",
          unlist(lapply(names(covariate_sets), function(nm)
              c(paste0("  ", nm, ":"),
                paste0("    ", paste(covariate_sets[[nm]], collapse = ", ")),
                "")
          ))),
        title = "Model Configuration"
    )
    
    # ── Loop: covariate set × exposure --------------------------------------
    for (cov_nm in names(covariate_sets)) {
        covariates <- covariate_sets[[cov_nm]]
        
        .section_page_gait(  # Changed: _gait
            title    = paste0("Covariate set: ", cov_nm),
            subtitle = paste(covariates, collapse = "  ·  ")
        )
        
        for (i in seq_len(nrow(exposures))) {
            exp_row   <- exposures[i, ]
            exp_col   <- exp_row$exposure
            exp_type  <- exp_row$exposure_type
            ref_lev   <- if (!is.na(exp_row$ref_level)) exp_row$ref_level else NULL
            model_tag <- paste0(resp_col, " ~ ", exp_col,
                                " (", exp_type, ")  |  ", cov_nm)
            
            # ── Build formula ------------------------------------------------
            formula <- .build_formula_gait(  # Changed: _gait
                resp_col     = resp_col,
                exposure     = exp_col,
                exposure_type = exp_type,
                covariates   = covariates,
                time_var     = time_var,
                id_var       = id_var,
                random_slope = random_slope,
                interaction  = interaction
            )
            
            # ── Fit ----------------------------------------------------------
            fit <- tryCatch(
                fit_pooled_lmm_gait(
                    mids_object  = mids_object,
                    formula      = formula,
                    outcome      = outcome,
                    resp_col     = resp_col,
                    outcome_fn   = outcome_fn,
                    ref_col      = exp_col,
                    ref_lev      = ref_lev,
                    id_var       = id_var,
                    time_var     = time_var,
                    random_slope = random_slope
                ),
                error = function(e) {
                    .text_page_gait(  # Changed: _gait
                        c(paste("ERROR:", conditionMessage(e)),
                          "",
                          deparse(formula)),
                        title = paste("FAILED:", model_tag)
                    )
                    NULL
                }
            )
            if (is.null(fit)) next
            
            # ── Formula page -------------------------------------------------
            .text_page_gait(  # Changed: _gait
                c(deparse(fit$formula),
                  "",
                  paste("Imputations pooled:", fit$n_imp)),
                title = paste("Formula —", model_tag)
            )
            
            # ── Results: coefficient plot + table ----------------------------
            .results_page_gait(fit$pooled_tidy, title = model_tag)  # Changed: _gait
            
            # ── Sandwich robustness check ------------------------------------
            sand_tidy <- tryCatch(
                pool_sandwich_lmm(fit$models, id_var = id_var, cr_type = "CR2"),
                error = function(e) {
                    .text_page(
                        c(paste("Sandwich estimator failed:", conditionMessage(e))),
                        title = paste("Sandwich FAILED —", model_tag)
                    )
                    NULL
                }
            )
            if (!is.null(sand_tidy)) {
                .sandwich_comparison_page_gait(fit$pooled_tidy, sand_tidy,
                                          title = model_tag)
            }
            
            # ── Diagnostics (imputation 1) -----------------------------------
            p_diag <- tryCatch(
                .plot_diagnostics_gait(fit$first_model,
                                       label = paste0("[imp 1, ", model_tag, "]")),
                error = function(e) NULL
            )
            if (!is.null(p_diag)) print(p_diag)
        }
    }
    
    # ── Session info ---------------------------------------------------------
    .text_page_gait(  # Changed: _gait
        utils::capture.output(sessioninfo::session_info()),
        title = "Session Info"
    )
    
    message("PDF written to: ", pdf_path)
    invisible(pdf_path)
}