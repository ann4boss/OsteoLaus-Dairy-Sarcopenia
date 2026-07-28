# =============================================================================
# LMM Report — Gait Speed
#
# Gait speed uses lagged exposures and covariates (_lag suffix).
# Reuses shared helpers from 04_01_LMM_report.R:
#   .pal, .theme_report, .text_page, .section_page,
#   .build_formula, .plot_diagnostics, pool_sandwich_lmm
# =============================================================================


# ---------------------------------------------------------------------------
# 1.  GAIT-SPECIFIC TERM LABELS
# ---------------------------------------------------------------------------

.term_labels_gait <- c(
    dairy_100g_lag                               = "Dairy intake cum. avg. [100g/day]",
    fermented_100g_lag                           = "Fermented dairy cum. avg. [100g/day]",
    nonfermented_100g_lag                        = "Non-fermented dairy cum. avg. [100g/day]",
    highfat_100g_lag                             = "High-fat dairy cum. avg. [100g/day]",
    lowfat_100g_lag                              = "Low-fat dairy cum. avg. [100g/day]",
    dairy_quartile_baseline_lagQ2                = "Dairy intake Q2",
    dairy_quartile_baseline_lagQ3                = "Dairy intake Q3",
    dairy_quartile_baseline_lagQ4                = "Dairy intake Q4",
    `dairy_guidelines_port_lag>= 2 servings/day` = "Dairy intake ≥ 2 servings/day",
    time_since_baseline                          = "Time since baseline [year]",
    age_at_baseline_scaled_lag                   = "Age at T1 [year]",
    BMI_category_lagUnderweight                  = "BMI — Underweight",
    BMI_category_lagOverweight                   = "BMI — Overweight",
    BMI_category_lagObese                        = "BMI — Obese",
    `education_level_lagMedium (ISCED 3-4)`      = "Education — Medium (ISCED 3–4)",
    `education_level_lagHigh (ISCED 5-8)`        = "Education — High (ISCED 5–8)",
    smoking_status_lagFormer                     = "Smoking — Former",
    smoking_status_lagCurrent                    = "Smoking — Current",
    pa_levels_tertile_f1_lagMedium               = "Physical activity — Medium",
    pa_levels_tertile_f1_lagHigh                 = "Physical activity — High",
    pa_levels_who_f1_lagMedium                   = "Physical activity (WHO) — Medium",
    pa_levels_who_f1_lagHigh                     = "Physical activity (WHO) — High",
    pa_levels_who_f1_lag.Medium                      = "Physical activity (WHO) — Medium",
    pa_levels_who_f1_lag.High                      = "Physical activity (WHO) — High",
    diabetes_status_lagDiabetes                  = "Diabetes — Yes"
)


# ---------------------------------------------------------------------------
# 2.  PER-IMPUTATION DATA PREPARATION (gait speed)
# ---------------------------------------------------------------------------

.prep_gait_imputation <- function(
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
            age          = median(df$Age_lag,                             na.rm = TRUE),
            age_baseline_median  = median(df$age_at_baseline_lag, na.rm = TRUE),
            dairy        = median(df$dairy_total_gday_cumavg_lag,        na.rm = TRUE),
            fermented    = median(df$dairy_fermented_gday_cumavg_lag,    na.rm = TRUE),
            nonfermented = median(df$dairy_non_fermented_gday_cumavg_lag, na.rm = TRUE),
            highfat      = median(df$dairy_highfat_gday_cumavg_lag,      na.rm = TRUE),
            lowfat       = median(df$dairy_lowfat_gday_cumavg_lag,       na.rm = TRUE),
            time         = median(df[[time_var]],                        na.rm = TRUE)
        )
    }

    df[[resp_col]] <- outcome_fn(df[[outcome]])

    df <- df |>
        dplyr::mutate(
            # Current-visit Age centred on lag median, per 10 years
            age_decades           = as.numeric(scale(Age,
                                                     center = scale_centres$age,
                                                     scale  = 10)),
            age_at_baseline_scaled_lag = as.numeric(scale(age_at_baseline_lag, 
                                           center = scale_centres$age_baseline_median,    
                                           scale = 10)), 
            # Lagged dairy exposures, per 100 g/day
            dairy_100g_lag        = as.numeric(scale(dairy_total_gday_cumavg_lag,
                                                     center = scale_centres$dairy,
                                                     scale  = 100)),
            fermented_100g_lag    = as.numeric(scale(dairy_fermented_gday_cumavg_lag,
                                                     center = scale_centres$fermented,
                                                     scale  = 100)),
            nonfermented_100g_lag = as.numeric(scale(dairy_non_fermented_gday_cumavg_lag,
                                                     center = scale_centres$nonfermented,
                                                     scale  = 100)),
            highfat_100g_lag      = as.numeric(scale(dairy_highfat_gday_cumavg_lag,
                                                     center = scale_centres$highfat,
                                                     scale  = 100)),
            lowfat_100g_lag       = as.numeric(scale(dairy_lowfat_gday_cumavg_lag,
                                                     center = scale_centres$lowfat,
                                                     scale  = 100)),

            # Lagged categorical exposures
            dairy_quartile_baseline_lag = factor(dairy_quartile_baseline_lag,
                                                 levels  = c("Q1","Q2","Q3","Q4"),
                                                 ordered = FALSE) |>
                stats::relevel(ref = "Q1"),

            dairy_guidelines_port_lag   = factor(dairy_guidelines_port_lag,
                                                 levels  = c("< 2 servings/day",
                                                             ">= 2 servings/day"),
                                                 ordered = FALSE) |>
                stats::relevel(ref = "< 2 servings/day"),

            # Lagged categorical covariates
            BMI_category_lag          = factor(BMI_category_lag,
                                               levels  = c("Underweight","Normal",
                                                           "Overweight","Obese"),
                                               ordered = FALSE) |>
                stats::relevel(ref = "Normal"),

            education_level_lag       = factor(education_level_lag,
                                               levels  = c("Low (ISCED 0-2)",
                                                           "Medium (ISCED 3-4)",
                                                           "High (ISCED 5-8)"),
                                               ordered = FALSE) |>
                stats::relevel(ref = "Low (ISCED 0-2)"),

            smoking_status_lag        = factor(smoking_status_lag,
                                               levels  = c("Never","Former","Current"),
                                               ordered = FALSE) |>
                stats::relevel(ref = "Never"),

            pa_levels_tertile_f1_lag  = factor(pa_levels_tertile_f1_lag,
                                               levels  = c("Low","Medium","High"),
                                               ordered = FALSE) |>
                stats::relevel(ref = "Low"),

            pa_levels_who_f1_lag      = factor(pa_levels_who_f1_lag,
                                               levels  = c("Low","Medium","High"),
                                               ordered = FALSE) |>
                stats::relevel(ref = "Low"),

            diabetes_status_lag       = factor(diabetes_status_lag,
                                               levels  = c("No diabetes","Diabetes"),
                                               ordered = FALSE) |>
                stats::relevel(ref = "No diabetes"),

            dplyr::across(dplyr::all_of(id_var),
                          ~ factor(.x, ordered = FALSE))
        )

    list(df = df, scale_centres = scale_centres, resp_col = resp_col)
}


# ---------------------------------------------------------------------------
# 3.  POOLED MICE FIT (gait speed)
# ---------------------------------------------------------------------------

fit_pooled_lmm_gait <- function(
    mids_object,
    formula,
    outcome      = "gait_speed",
    resp_col     = outcome,
    outcome_fn   = identity,
    ref_col      = NULL,
    ref_lev      = NULL,
    id_var       = "pt",
    time_var     = "time_since_baseline",
    random_slope = FALSE
) {
    m    <- mids_object$m
    ctrl <- lme4::lmerControl(optimizer = "bobyqa",
                               optCtrl  = list(maxfun = 20000))

    df1     <- mice::complete(mids_object, 1)
    prep1   <- .prep_gait_imputation(df1,
                                     outcome    = outcome,
                                     resp_col   = resp_col,
                                     outcome_fn = outcome_fn,
                                     id_var     = id_var,
                                     time_var   = time_var)
    centres <- prep1$scale_centres

    models <- vector("list", m)
    for (i in seq_len(m)) {
        df_i   <- mice::complete(mids_object, i)
        prep_i <- .prep_gait_imputation(df_i,
                                         outcome       = outcome,
                                         resp_col      = resp_col,
                                         outcome_fn    = outcome_fn,
                                         id_var        = id_var,
                                         time_var      = time_var,
                                         scale_centres = centres)
        df_fit <- prep_i$df

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
            std_error = std.error,
            p_value   = p.value,
            conf_low  = `2.5 %`,
            conf_high = `97.5 %`
        ) |>
        dplyr::select(term, estimate, std_error, statistic, df,
                      p_value, conf_low, conf_high)

    list(
        pooled_tidy = pooled_tidy,

        models      = models,
        first_model = models[[1]],
        formula     = formula,
        n_imp       = m
    )
}


# ---------------------------------------------------------------------------
# 4.  PDF HELPERS (gait-specific: use .term_labels_gait)
# ---------------------------------------------------------------------------

.results_page_gait <- function(tidy_df, title, out_dir = NULL) {

    disp <- tidy_df |>
        dplyr::mutate(
            dplyr::across(c(estimate, std_error, statistic, conf_low, conf_high),
                          ~ round(.x, 3)),
            p_value = dplyr::if_else(p_value < 0.001, "<0.001",
                                     as.character(round(p_value, 3))),
            df = round(df, 1)
        )

    plot_df <- tidy_df |>
        dplyr::filter(term != "(Intercept)") |>
        dplyr::mutate(
            sig      = p_value < 0.05,
            is_dairy = grepl(
                "^(dairy|fermented|nonfermented|highfat|lowfat)",
                term
            ),
            point_colour = dplyr::case_when(
                is_dairy &  sig ~ "dairy_sig",
                is_dairy & !sig ~ "dairy_ns",
                TRUE            ~ "covariate"
            ),
            term_label_base = {
                idx <- match(term, names(.term_labels_gait))
                ifelse(is.na(idx), term, .term_labels_gait[idx])
            },
            term_order = factor(
                term_label_base,
                levels = rev(c(
                    unique(unname(.term_labels_gait)),
                    sort(unique(term_label_base[!(term %in% names(.term_labels_gait))]))
                ))
            ),
            term_label_display = dplyr::if_else(
                sig, paste0(term_label_base, " *"), term_label_base
            ),
            term_label = term_order
        )

    p_coef <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = estimate, y = term_label,
                     xmin = conf_low, xmax = conf_high,
                     colour = point_colour)
    ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            colour = "grey60") +
        ggplot2::geom_errorbarh(height = 0.3, linewidth = 1) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_colour_manual(
            values = c(dairy_sig = "#92D050", dairy_ns = "#FF6666",
                       covariate = "black"),
            guide  = "none"
        ) +
        ggplot2::scale_y_discrete(
            expand = ggplot2::expansion(add = 0.1),
            labels = setNames(plot_df$term_label_display,
                              plot_df$term_label_base)
        ) +
        ggplot2::labs(x = "Estimate (95 % CI)", y = NULL,
                      title = title, caption = "* p < 0.05") +
        .theme_report() +
        ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = 18),
            axis.text.x = ggplot2::element_text(size = 14),
            plot.margin = ggplot2::margin(2, 2, 2, 2)
        )

    n_terms   <- nlevels(plot_df$term_label)
    coef_grob <- ggplot2::ggplotGrob(p_coef)
    panel_row <- which(coef_grob$layout$name == "panel")
    coef_grob$heights[coef_grob$layout$t[panel_row]] <-
        grid::unit(n_terms * 0.14, "in")

    # ── Save forest plot as PNG and coefficients as CSV ----------------------
    if (!is.null(out_dir)) {
        stem <- gsub("[^A-Za-z0-9_-]", "_", title)
        ggplot2::ggsave(
            filename = file.path(out_dir, paste0(stem, "_forest.png")),
            plot     = p_coef,
            width    = 14,
            height   = max(3, n_terms * 0.22),
            dpi      = 300,
            bg       = "white"
        )
        readr::write_csv(disp, file.path(out_dir, paste0(stem, "_coefs.csv")))
    }

    tbl_grob <- gridExtra::tableGrob(
        disp, rows = NULL,
        theme = gridExtra::ttheme_minimal(
            base_size = 7,
            core    = list(fg_params = list(hjust = 0, x = 0.05)),
            colhead = list(fg_params = list(fontface = "bold",
                                            hjust = 0, x = 0.05))
        )
    )

    gridExtra::grid.arrange(coef_grob, tbl_grob, ncol = 2,
                            widths = c(1.2, 1.8))
}


.sandwich_comparison_page_gait <- function(std_tidy, sand_tidy, title,
                                            out_dir = NULL) {

    combined <- dplyr::bind_rows(
        dplyr::mutate(std_tidy,  estimator = "Standard LMM"),
        dplyr::mutate(sand_tidy, estimator = "Sandwich (CR2)")
    ) |>
        dplyr::filter(term != "(Intercept)") |>
        dplyr::mutate(
            term_label_base = {
                idx <- match(term, names(.term_labels_gait))
                ifelse(is.na(idx), term, .term_labels_gait[idx])
            },
            term_label = factor(
                term_label_base,
                levels = rev(c(
                    unique(unname(.term_labels_gait)),
                    sort(unique(term_label_base[!(term %in% names(.term_labels_gait))]))
                ))
            ),
            estimator = factor(estimator, levels = c("Standard LMM", "Sandwich (CR2)"))
        )

    colours <- c("Standard LMM" = .pal[8], "Sandwich (CR2)" = .pal[2])
    shapes  <- c("Standard LMM" = 16L,     "Sandwich (CR2)" = 17L)

    p_coef <- ggplot2::ggplot(
        combined,
        ggplot2::aes(x = estimate, y = term_label,
                     xmin = conf_low, xmax = conf_high,
                     colour = estimator, shape = estimator)
    ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
        ggplot2::geom_errorbarh(
            height   = 0.3, linewidth = 0.8,
            position = ggplot2::position_dodge(width = 0.5)
        ) +
        ggplot2::geom_point(
            size     = 2,
            position = ggplot2::position_dodge(width = 0.5)
        ) +
        ggplot2::scale_colour_manual(values = colours, name = "Estimator") +
        ggplot2::scale_shape_manual(values  = shapes,  name = "Estimator") +
        ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.1)) +
        ggplot2::labs(
            x       = "Estimate (95 % CI)",
            y       = NULL,
            title   = paste("Robustness Check — Sandwich vs Standard:", title),
            caption = "CR2: cluster-robust (clubSandwich); clusters = subject ID"
        ) +
        .theme_report() +
        ggplot2::theme(
            legend.position = "bottom",
            axis.text.y     = ggplot2::element_text(size = 18),
            axis.text.x     = ggplot2::element_text(size = 14)
        )

    n_terms   <- length(unique(combined$term_label))
    coef_grob <- ggplot2::ggplotGrob(p_coef)
    panel_row <- which(coef_grob$layout$name == "panel")
    coef_grob$heights[coef_grob$layout$t[panel_row]] <-
        grid::unit(n_terms * 0.18, "in")

    if (!is.null(out_dir)) {
        stem <- gsub("[^A-Za-z0-9_-]", "_", title)
        ggplot2::ggsave(
            filename = file.path(out_dir, paste0(stem, "_sandwich.png")),
            plot     = p_coef,
            width    = 10, height = max(4, n_terms * 0.3),
            dpi      = 300, bg = "white"
        )
    }

    comp_tbl <- dplyr::inner_join(
        std_tidy  |> dplyr::select(term, estimate,
                                    SE_std = std_error, p_std = p_value),
        sand_tidy |> dplyr::select(term,
                                    SE_sandwich = std_error, p_sandwich = p_value),
        by = "term"
    ) |>
        dplyr::mutate(
            estimate    = round(estimate,    3),
            SE_std      = round(SE_std,      3),
            SE_sandwich = round(SE_sandwich, 3),
            delta_SE    = round(SE_sandwich - SE_std, 4),
            p_std       = dplyr::if_else(p_std      < 0.001, "<0.001",
                                          as.character(round(p_std,      3))),
            p_sandwich  = dplyr::if_else(p_sandwich < 0.001, "<0.001",
                                          as.character(round(p_sandwich, 3)))
        )

    tbl_grob <- gridExtra::tableGrob(
        comp_tbl, rows = NULL,
        theme = gridExtra::ttheme_minimal(
            base_size = 7,
            core    = list(fg_params = list(hjust = 0, x = 0.05)),
            colhead = list(fg_params = list(fontface = "bold", hjust = 0, x = 0.05))
        )
    )

    gridExtra::grid.arrange(coef_grob, tbl_grob, ncol = 2, widths = c(1.2, 1.8))
}


# ---------------------------------------------------------------------------
# 5.  MAIN REPORT FUNCTION
# ---------------------------------------------------------------------------

#' Fit pooled gait-speed LMMs and write results + sandwich robustness to PDF.
#'
#' @param mids_object       A `mids` object for the gait speed outcome.
#' @param outcome           Raw outcome column (default `"gait_speed"`).
#' @param outcome_fn        Transformation function (default `identity`).
#' @param exposures         Tibble with columns `exposure`, `exposure_type`,
#'                          `ref_level`.  Use lagged column names (e.g.
#'                          `"dairy_100g_lag"`).
#' @param covariate_sets    Named list of character vectors.  Use lagged column
#'                          names (e.g. `"BMI_category_lag"`).
#' @param random_slope      Logical; default `FALSE` (two visits only).
#' @param interaction       Logical; add `exposure × time_var` term.
#' @param id_var            Subject ID column (default `"pt"`).
#' @param time_var          Time variable column (default
#'                          `"time_since_baseline"`).
#' @param out_dir           Directory to write the PDF into.
#' @return Invisibly, the path to the written PDF.

run_lmm_report_gait <- function(
    mids_object,
    outcome        = "gait_speed",
    outcome_fn     = identity,
    exposures      = tibble::tibble(
        exposure      = "dairy_100g_lag",
        exposure_type = "linear",
        ref_level     = NA_character_
    ),
    covariate_sets = list(
        minimal = c("age_decades", "BMI_category_lag", "education_level_lag",
                    "smoking_status_lag", "pa_levels_tertile_f1_lag",
                    "diabetes_status_lag")
    ),
    random_slope   = FALSE,
    interaction    = FALSE,
    id_var         = "pt",
    time_var       = "time_since_baseline",
    out_dir        = "03_outputs/LMM_exploratory"
) {
    # ── Resolve outcome column name -------------------------------------------
    fn_name <- deparse(substitute(outcome_fn))
    if (!grepl("^[a-zA-Z_][a-zA-Z0-9_.]*$", fn_name)) {
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
                           paste0(resp_col, "_LMM_gait_", timestamp, ".pdf"))

    grDevices::pdf(pdf_path, width = 14, height = 9)
    on.exit(grDevices::dev.off(), add = TRUE)

    # ── Cover page -----------------------------------------------------------
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#1E466EFF", col = NA))
    grid::grid.text(paste0("LMM Report  —  GAIT SPEED  —  ", resp_col),
                    x = 0.5, y = 0.60,
                    gp = grid::gpar(col = "white", fontsize = 22,
                                    fontface = "bold"))
    grid::grid.text(paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
                    x = 0.5, y = 0.48,
                    gp = grid::gpar(col = .pal[5], fontsize = 12))
    info_lines <- c(
        paste0("Imputations: ", mids_object$m),
        paste0("Random slope: ", random_slope,
               "  |  Interaction: ", interaction),
        paste0("Exposures: ", nrow(exposures),
               "  |  Covariate sets: ", length(covariate_sets))
    )
    for (k in seq_along(info_lines))
        grid::grid.text(info_lines[k], x = 0.5, y = 0.40 - (k - 1) * 0.06,
                        gp = grid::gpar(col = .pal[5], fontsize = 10))

    # ── Configuration summary ------------------------------------------------
    .text_page(
        c(paste("Outcome column:", resp_col),
          paste("Outcome transform:", fn_name),
          paste("Estimation method: REML = FALSE (Maximum Likelihood)"),
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

        .section_page(
            title    = paste0("Covariate set: ", cov_nm),
            subtitle = paste(covariates, collapse = "  ·  ")
        )

        dairy_summary_rows <- list()   # collect dairy estimates across exposures

        for (i in seq_len(nrow(exposures))) {
            exp_row   <- exposures[i, ]
            exp_col   <- exp_row$exposure
            exp_type  <- exp_row$exposure_type
            ref_lev   <- if (!is.na(exp_row$ref_level)) exp_row$ref_level else NULL
            model_tag <- paste0(resp_col, " ~ ", exp_col,
                                " (", exp_type, ")  |  ", cov_nm)

            # ── Build formula ------------------------------------------------
            formula <- .build_formula(
                resp_col      = resp_col,
                exposure      = exp_col,
                exposure_type = exp_type,
                covariates    = covariates,
                time_var      = time_var,
                id_var        = id_var,
                random_slope  = random_slope,
                interaction   = interaction
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
                    .text_page(
                        c(paste("ERROR:", conditionMessage(e)),
                          "", deparse(formula)),
                        title = paste("FAILED:", model_tag)
                    )
                    NULL
                }
            )
            if (is.null(fit)) next

            # ── Collect dairy rows for summary plot --------------------------
            dairy_summary_rows[[i]] <- fit$pooled_tidy |>
                dplyr::mutate(
                    exposure_label = paste0(exp_col, " (", exp_type, ")")
                )

            # ── Formula page -------------------------------------------------
            .text_page(
                c(deparse(fit$formula), "",
                  paste("Imputations pooled:", fit$n_imp)),
                title = paste("Formula —", model_tag)
            )

            # ── Results: coefficient plot + table ----------------------------
            .results_page_gait(fit$pooled_tidy, title = model_tag,
                               out_dir = if (cov_nm == "other_PA") NULL else out_dir)

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
                                               title   = model_tag,
                                               out_dir = if (cov_nm == "other_PA") NULL else out_dir)
            }

            # ── Variance components ------------------------------------------
            tryCatch(
                .variance_components_page(fit$first_model, title = model_tag,
                                          random_slope = random_slope),
                error = function(e) .text_page(
                    c(paste("Variance components failed:", conditionMessage(e))),
                    title = paste("Variance Components FAILED —", model_tag)
                )
            )

            # ── R² (Nakagawa) ------------------------------------------------
            tryCatch(
                .r2_page(fit$first_model, title = model_tag),
                error = function(e) .text_page(
                    c(paste("R² failed:", conditionMessage(e))),
                    title = paste("R² FAILED —", model_tag)
                )
            )

            # ── VIF ----------------------------------------------------------
            tryCatch(
                .vif_page(fit$first_model, title = model_tag),
                error = function(e) .text_page(
                    c(paste("VIF failed:", conditionMessage(e))),
                    title = paste("VIF FAILED —", model_tag)
                )
            )



            # ── Fixed-effect correlations ------------------------------------
            tryCatch(
                .fixed_corr_page(fit$first_model, title = model_tag),
                error = function(e) .text_page(
                    c(paste("Fixed-effect correlations failed:", conditionMessage(e))),
                    title = paste("Fixed Corr. FAILED —", model_tag)
                )
            )

            # ── Influential points -------------------------------------------
            inf_idx <- tryCatch(
                .influential_page(fit$first_model, title = model_tag),
                error = function(e) {
                    .text_page(
                        c(paste("Influential points check failed:", conditionMessage(e))),
                        title = paste("Influential FAILED —", model_tag)
                    )
                    NULL
                }
            )

            excl_tag <- paste0("[Excl. influential (top 1% Cook's D)]  ", model_tag)
            fit_excl <- NULL
            if (length(inf_idx) > 0L) {
                mf_imp1  <- model.frame(fit$first_model)
                excl_ids <- unique(mf_imp1[[id_var]][inf_idx])

                long_full <- mice::complete(mids_object, "long", include = TRUE)
                long_excl <- long_full |>
                    dplyr::filter(!.data[[id_var]] %in% excl_ids) |>
                    dplyr::group_by(.imp) |>
                    dplyr::mutate(.id = dplyr::row_number()) |>
                    dplyr::ungroup()
                mids_excl <- tryCatch(mice::as.mids(long_excl), error = function(e) NULL)

                if (!is.null(mids_excl)) {
                    fit_excl <- tryCatch(
                        fit_pooled_lmm_gait(
                            mids_object  = mids_excl,
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
                            .text_page(
                                c(paste("Cook's D exclusion model failed:",
                                        conditionMessage(e))),
                                title = paste("Excl. Cook's D FAILED —", model_tag)
                            )
                            NULL
                        }
                    )
                    if (!is.null(fit_excl)) {
                        .text_page(
                            c(paste("Excluded participants (top 1% Cook's D, imp 1):",
                                    length(excl_ids)),
                              paste("Remaining n:", length(unique(
                                  mice::complete(mids_excl, 1)[[id_var]]))),
                              "", deparse(fit_excl$formula)),
                            title = paste("Formula —", excl_tag)
                        )
                        .results_page_gait(fit_excl$pooled_tidy, title = excl_tag,
                                           out_dir = if (cov_nm == "other_PA") NULL else out_dir)
                    }
                }
            }

            # ── Sensitivity: exclude pts with dairy > 1000 g/day ------------
            sens_tag  <- paste0("[Sensitivity: dairy ≤ 1000 g/day]  ", model_tag)
            fit_s     <- NULL
            mids_sens <- tryCatch(
                .filter_mids_dairy_1000(mids_object, id_var,
                                        dairy_col = "dairy_total_gday_cumavg_lag"),
                error = function(e) {
                    .text_page(
                        c(paste("Sensitivity filter failed:", conditionMessage(e))),
                        title = paste("Sensitivity FAILED —", model_tag)
                    )
                    NULL
                }
            )

            if (!is.null(mids_sens)) {
                fit_s <- tryCatch(
                    fit_pooled_lmm_gait(
                        mids_object  = mids_sens,
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
                        .text_page(
                            c(paste("Sensitivity model failed:", conditionMessage(e))),
                            title = paste("Sensitivity FAILED —", model_tag)
                        )
                        NULL
                    }
                )

                if (!is.null(fit_s)) {
                    .text_page(
                        c(deparse(fit_s$formula), "",
                          paste("Imputations pooled:", fit_s$n_imp),
                          paste("Participants in sensitivity sample:",
                                length(unique(
                                    mice::complete(mids_sens, 1)[[id_var]]
                                ))),
                          paste("Threshold: dairy_total_gday_cumavg_lag ≤ 1000 g/day")),
                        title = paste("Formula —", sens_tag)
                    )
                    .results_page_gait(fit_s$pooled_tidy, title = sens_tag,
                                       out_dir = NULL)
                }
            }

            # ── Assumption checks: main / excl Cook's D / dairy sensitivity --
            diag_out <- if (cov_nm == "other_PA") NULL else out_dir

            p_diag <- tryCatch(
                .plot_diagnostics(fit$first_model,
                                  label     = paste0("Main model [imp 1]  ", model_tag),
                                  out_dir   = diag_out,
                                  file_stem = paste0(model_tag, "_main")),
                error = function(e) NULL
            )
            if (!is.null(p_diag)) print(p_diag)

            if (!is.null(fit_excl)) {
                p_excl <- tryCatch(
                    .plot_diagnostics(fit_excl$first_model,
                                      label     = paste0("Excl. Cook's D [imp 1]  ", model_tag),
                                      out_dir   = diag_out,
                                      file_stem = paste0(model_tag, "_excl_cooks")),
                    error = function(e) NULL
                )
                if (!is.null(p_excl)) print(p_excl)
            }

            if (!is.null(fit_s)) {
                p_diag_s <- tryCatch(
                    .plot_diagnostics(fit_s$first_model,
                                      label     = paste0("Dairy ≤ 1000 g/day [imp 1]  ", model_tag),
                                      out_dir   = diag_out,
                                      file_stem = paste0(model_tag, "_dairy_sens")),
                    error = function(e) NULL
                )
                if (!is.null(p_diag_s)) print(p_diag_s)
            }
        }

        # ── Dairy definitions summary forest (end of covariate set) ---------
        .dairy_summary_forest(
            rows_list   = dairy_summary_rows,
            cov_nm      = cov_nm,
            resp_col    = resp_col,
            term_labels = .term_labels_gait,
            out_dir     = if (cov_nm == "other_PA") NULL else out_dir
        )
    }

    # ── Session info ---------------------------------------------------------
    .text_page(
        utils::capture.output(sessioninfo::session_info()),
        title = "Session Info"
    )

    message("PDF written to: ", pdf_path)
    invisible(pdf_path)
}
