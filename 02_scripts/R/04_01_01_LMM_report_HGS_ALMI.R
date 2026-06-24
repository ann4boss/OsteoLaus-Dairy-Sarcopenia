# =============================================================================
# LMM Report — pooled MICE results printed to a dated PDF
#
# One outcome (user-specified transformation), multiple exposures ×
# multiple covariate sets.  Uses mice::with() + mice::pool() internally.
# =============================================================================


# ---------------------------------------------------------------------------
# 1.  PER-IMPUTATION DATA PREPARATION
# ---------------------------------------------------------------------------

#' Apply scaling and factor coding to one completed data frame.
#'
#' Scaling centres are derived from the supplied data frame (typically the
#' first imputation).  Pass `scale_centres` from a previous call to keep
#' centres consistent across imputations.
#'
#' @param df             A single completed (non-mids) data frame.
#' @param outcome        Raw outcome column name (e.g. `"HGS_MAX"`).
#' @param outcome_fn     Function to create the response; applied as
#'                       `response <- outcome_fn(df[[outcome]])`.
#'                       Use `identity` to leave the outcome untransformed.
#' @param id_var         Subject ID column.
#' @param time_var       Time variable column.
#' @param scale_centres  Named list with elements `age`, `sumtot`, `dairy`,
#'                       `time`.  Computed from `df` when `NULL`.
#' @return A list: `df` (prepared data frame) and `scale_centres`.

.prep_one_imputation <- function(
    df,
    outcome       = "HGS_MAX",
    resp_col      = outcome,  
    outcome_fn    = identity,
    id_var        = "pt",
    time_var      = "time_since_baseline",
    scale_centres = NULL
) {
    if (is.null(scale_centres)) {
        scale_centres <- list(
            age                  = median(df$Age, na.rm = TRUE),
            age_baseline_median  = median(df$age_at_baseline, na.rm = TRUE),
            sumtot               = median(df$sumtot1,                 na.rm = TRUE),
            dairy                = median(df$dairy_total_gday_cumavg, na.rm = TRUE),
            fermented            = median(df$dairy_fermented_gday_cumavg, na.rm = TRUE),
            nonfermented         = median(df$dairy_non_fermented_gday_cumavg, na.rm = TRUE),
            highfat              = median(df$dairy_highfat_gday_cumavg, na.rm = TRUE),
            lowfat               = median(df$dairy_lowfat_gday_cumavg, na.rm = TRUE),
            time                 = median(df[[time_var]],             na.rm = TRUE)
        )
    }

    df[[resp_col]] <- outcome_fn(df[[outcome]])

    df <- df |>
        dplyr::mutate(
            age_at_baseline_scaled = scale(age_at_baseline, 
                                 center = scale_centres$age_baseline_median,    
                                 scale = 10),  
            age_decades = scale(Age, 
                                center = scale_centres$age,    
                                scale = 10), 
            sumtot1_scaled = as.numeric(scale(sumtot1,
                                                center = scale_centres$sumtot,
                                                scale  = 1000)),
            dairy_100g       = as.numeric(scale(dairy_total_gday_cumavg,
                                                center = scale_centres$dairy,
                                                scale  = 100)),
            fermented_100g       = as.numeric(scale(dairy_fermented_gday_cumavg,
                                                center = scale_centres$fermented,
                                                scale  = 100)),
            nonfermented_100g       = as.numeric(scale(dairy_non_fermented_gday_cumavg,
                                                    center = scale_centres$nonfermented,
                                                    scale  = 100)),
            highfat_100g       = as.numeric(scale(dairy_highfat_gday_cumavg,
                                                    center = scale_centres$highfat,
                                                    scale  = 100)),
            lowfat_100g       = as.numeric(scale(dairy_lowfat_gday_cumavg,
                                                    center = scale_centres$lowfat,
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
            


            dplyr::across(dplyr::all_of(id_var),
                          ~ factor(.x, ordered = FALSE))
        )

    list(df = df, scale_centres = scale_centres, resp_col = resp_col)
}


# ---------------------------------------------------------------------------
# 2.  FORMULA BUILDER
# ---------------------------------------------------------------------------

.build_formula <- function(resp_col, exposure, exposure_type,
                           covariates, time_var, id_var,
                           random_slope, interaction) {
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

#' Fit one LMM across all imputations and return pooled results.
#'
#' @param mids_object   A `mids` object.
#' @param formula       Model formula (built by `.build_formula()`).
#' @param outcome       Raw outcome column name.
#' @param outcome_fn    Transformation function (e.g. `sqrt`, `log`,
#'                      `identity`).
#' @param id_var        Subject ID column.
#' @param time_var      Time variable column.
#' @param random_slope  Logical.
#' @return A list: `pooled_tidy` (tidy summary with beta/SE/p/CI),
#'   `first_model` (fitted lmer on imputation 1, for diagnostics),
#'   `formula`, `n_imp`.

fit_pooled_lmm <- function(
    mids_object,
    formula,
    outcome      = "HGS_MAX",
    resp_col     = outcome,   # pre-computed by run_lmm_report()
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
    prep1   <- .prep_one_imputation(df1,
                                    outcome      = outcome,
                                    resp_col     = resp_col,
                                    outcome_fn   = outcome_fn,
                                    id_var       = id_var,
                                    time_var     = time_var)
    centres <- prep1$scale_centres

    models <- vector("list", m)
    for (i in seq_len(m)) {
        df_i   <- mice::complete(mids_object, i)
        prep_i <- .prep_one_imputation(df_i,
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
        models      = models,          # all imputations — needed for sandwich
        first_model = models[[1]],
        formula     = formula,
        n_imp       = m
    )
}


# ---------------------------------------------------------------------------
# 4.  POOLED SANDWICH ESTIMATOR  (clubSandwich + Rubin's rules)
# ---------------------------------------------------------------------------

#' Pool cluster-robust (sandwich) SEs across imputations via Rubin's rules.
#'
#' For each imputation: fits the same lmer, computes a CR2 sandwich vcov via
#' `clubSandwich::vcovCR()`, extracts coefficients and their robust variances.
#' Rubin's rules are then applied to obtain pooled estimates, robust SEs,
#' and Barnard-Rubin degrees of freedom.
#'
#' @param models     List of fitted lmer objects (one per imputation), already
#'                   stored in `fit$models` from `fit_pooled_lmm()`.
#' @param id_var     Cluster variable name (passed to `clubSandwich::vcovCR()`).
#' @param cr_type    Sandwich type — `"CR2"` (default, small-sample corrected)
#'                   or any type accepted by clubSandwich.
#' @return A tibble with columns `term`, `estimate`, `std_error`, `statistic`,
#'   `df`, `p_value`, `conf_low`, `conf_high`.

pool_sandwich_lmm <- function(models, id_var = "pt", cr_type = "CR2") {

    m <- length(models)

    # Per-imputation: coefficients and diagonal of robust vcov
    coef_list <- vector("list", m)
    var_list  <- vector("list", m)

    for (i in seq_len(m)) {
        mod   <- models[[i]]
        cluster_vec <- lme4::getME(mod, "flist")[[id_var]]

        vcov_cr <- tryCatch(
            clubSandwich::vcovCR(mod, cluster = cluster_vec, type = cr_type),
            error = function(e) NULL
        )
        if (is.null(vcov_cr)) {
            warning("clubSandwich failed for imputation ", i, "; skipping.")
            next
        }

        coef_list[[i]] <- lme4::fixef(mod)
        var_list[[i]]  <- diag(as.matrix(vcov_cr))
    }

    # Drop any failed imputations
    ok        <- !vapply(coef_list, is.null, logical(1))
    coef_list <- coef_list[ok]
    var_list  <- var_list[ok]
    m_ok      <- sum(ok)

    if (m_ok == 0L)
        stop("clubSandwich failed for all imputations.")

    terms  <- names(coef_list[[1]])
    n_par  <- length(terms)

    # Rubin's rules (univariate, per-parameter)
    Q_mat <- do.call(rbind, coef_list)          # m_ok × n_par
    U_mat <- do.call(rbind, var_list)            # m_ok × n_par — within-imp var

    Q_bar <- colMeans(Q_mat)                     # pooled estimate
    U_bar <- colMeans(U_mat)                     # mean within-imp variance
    B     <- apply(Q_mat, 2, stats::var)         # between-imp variance

    T_var <- U_bar + (1 + 1 / m_ok) * B         # total variance
    se    <- sqrt(T_var)

    # Barnard-Rubin df
    r_L  <- (1 + 1 / m_ok) * B / U_bar          # relative increase in variance
    df_old <- (m_ok - 1) / r_L^2                # old df
    # Approximate complete-data df from first model (conservative)
    df_com <- nrow(lme4::getME(models[[which(ok)[1]]], "X")) - n_par
    df_adj <- df_old * df_com / (df_old + df_com)

    t_stat  <- Q_bar / se
    p_val   <- 2 * stats::pt(abs(t_stat), df = df_adj, lower.tail = FALSE)
    alpha   <- 0.05
    t_crit  <- stats::qt(1 - alpha / 2, df = df_adj)
    ci_lo   <- Q_bar - t_crit * se
    ci_hi   <- Q_bar + t_crit * se

    tibble::tibble(
        term      = terms,
        estimate  = Q_bar,
        std_error = se,
        statistic = t_stat,
        df        = df_adj,
        p_value   = p_val,
        conf_low  = ci_lo,
        conf_high = ci_hi
    )
}


# ---------------------------------------------------------------------------
# 5.  PDF HELPERS
# ---------------------------------------------------------------------------

.pal <- c("#E76254FF","#EF8A47FF","#F7AA58FF","#FFD06FFF",
          "#AADCE0FF","#72BCD5FF","#528FADFF","#376795FF","#1E466EFF")

.term_labels <- c(
    dairy_100g                            = "Dairy intake cum. avg. [100g/day]",
    fermented_100g                        = "Fermented Dairy intake cum. avg. [100g/day]",
    nonfermented_100g                     = "Non- Fermented Dairy intake cum. avg. [100g/day]",
    highfat_100g                          = "High Fat Dairy intake cum. avg. [100g/day]",
    lowfat_100g                           = "Low Fat Dairy intake cum. avg. [100g/day]",
    dairy_quartile_baselineQ2             = "Dairy intale Q2",
    dairy_quartile_baselineQ3             = "Dairy intake Q3",
    dairy_quartile_baselineQ4             = "Dairy intake Q4",
    `dairy_guidelines_port>= 2 servings/day` = "Dairy intake ≥ 2 servings/day",
    time_since_baseline                   = "Time since baseline [year]",
    age_at_baseline_scaled                = "Age at T1 [year]",
    BMI_categoryUnderweight               = "BMI - Underweight",
    BMI_categoryOverweight                = "BMI - Overweight",
    BMI_categoryObese                     = "BMI - Obese",
    `education_levelMedium (ISCED 3-4)` = "Education - Medium (ISCED 3-4)",
    `education_levelHigh (ISCED 5-8)`   = "Education - High (ISCED 5-8)",
    smoking_statusFormer                  = "Smoking - Former",
    smoking_statusCurrent                 = "Smoking - Current",
    pa_levels_tertile_f1Medium            = "Physical activity - Medium",
    pa_levels_tertile_f1High              = "Physical activity - High",
    diabetes_statusDiabetes               = "Diabetes - Yes",
    sumtot1_scaled                      = "Total calorie intake [kcal]"
)

.theme_report <- function() {
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

.section_page <- function(title, subtitle = NULL) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = .pal[9], col = NA))
    grid::grid.text(title, x = 0.5, y = if (is.null(subtitle)) 0.5 else 0.55,
                    gp = grid::gpar(col = "white", fontsize = 16,
                                    fontface = "bold"))
    if (!is.null(subtitle))
        grid::grid.text(subtitle, x = 0.5, y = 0.43,
                        gp = grid::gpar(col = .pal[5], fontsize = 11))
}

.text_page <- function(lines, title = NULL) {
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

# Tidy results as a ggplot table (coefficient plot + numeric table side-by-side)
.results_page <- function(tidy_df, title, out_dir = NULL) {
    
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
                idx <- match(term, names(.term_labels))
                ifelse(is.na(idx), term, .term_labels[idx])
            }
        ) |>
        dplyr::mutate(
            term_order = factor(
                term_label_base,
                levels = rev(c(
                    unname(.term_labels),
                    sort(unique(term_label_base[!(term %in% names(.term_labels))]))
                ))
            )
        ) |>
        dplyr::mutate(
            term_label_display = dplyr::case_when(
                sig ~ paste0(term_label_base, " *"),
                TRUE ~ term_label_base
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
            labels = setNames(plot_df$term_label_display, plot_df$term_label_base)
        ) +
        ggplot2::labs(
            x = "Estimate (95 % CI)", 
            y = NULL, 
            title = title,
            caption = "* p < 0.05"
        ) +
        .theme_report() +
        ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = 18),
            axis.text.x = ggplot2::element_text(size = 14),
            plot.margin = ggplot2::margin(2, 2, 2, 2)
        )
    
    # Fix panel height so rows are compact regardless of page size
    n_terms    <- nlevels(plot_df$term_label)
    coef_grob  <- ggplot2::ggplotGrob(p_coef)
    panel_row  <- which(coef_grob$layout$name == "panel")
    coef_grob$heights[coef_grob$layout$t[panel_row]] <-
        grid::unit(n_terms * 0.18, "in")

    # ── Save forest plot as PNG and coefficients as CSV ----------------------
    if (!is.null(out_dir)) {
        stem <- gsub("[^A-Za-z0-9_-]", "_", title)
        ggplot2::ggsave(
            filename = file.path(out_dir, paste0(stem, "_forest.png")),
            plot     = p_coef,
            width    = 10,
            height   = max(4, n_terms * 0.3),
            dpi      = 300,
            bg       = "white"
        )
        readr::write_csv(disp, file.path(out_dir, paste0(stem, "_coefs.csv")))
    }

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

.sandwich_comparison_page <- function(std_tidy, sand_tidy, title) {

    # Combine, exclude intercept
    combined <- dplyr::bind_rows(
        dplyr::mutate(std_tidy,  estimator = "Standard LMM"),
        dplyr::mutate(sand_tidy, estimator = "Sandwich (CR2)")
    ) |>
        dplyr::filter(term != "(Intercept)") |>
        dplyr::mutate(
            sig = p_value < 0.05,
            term_label = {
                idx <- match(term, names(.term_labels))
                ifelse(is.na(idx), term, .term_labels[idx])
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

.plot_diagnostics <- function(model, label = "") {
    res_v  <- residuals(model)
    fit_v  <- fitted(model)
    df_d   <- data.frame(fitted    = fit_v,
                          residuals = res_v,
                          sqrt_abs  = sqrt(abs(res_v)))

    slope_rd <- round(coef(lm(residuals ~ fitted, data = df_d))[["fitted"]], 4)

    p_rd <- ggplot2::ggplot(df_d, ggplot2::aes(fitted, residuals)) +
        ggplot2::geom_point(colour = .pal[8], alpha = 0.45, size = 1.2) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = .pal[1]) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, colour = .pal[1],
                             fill = .pal[4], alpha = 0.2, linewidth = 0.7) +
        ggplot2::annotate("text",
                          x    = -Inf, y = Inf,
                          hjust = -0.15, vjust = 1.6,
                          label = paste0("slope = ", slope_rd),
                          size  = 3.5, colour = .pal[1],
                          fontface = "italic") +
        ggplot2::labs(x = "Fitted", y = "Residuals",
                      title = paste("A  Residuals vs Fitted", label)) +
        .theme_report()

    p_sl <- ggplot2::ggplot(df_d, ggplot2::aes(fitted, sqrt_abs)) +
        ggplot2::geom_point(colour = .pal[8], alpha = 0.45, size = 1.2) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, colour = .pal[1],
                             fill = .pal[3], alpha = 0.2, linewidth = 0.7) +
        ggplot2::labs(x = "Fitted", y = expression(sqrt("|Residuals|")),
                      title = "B  Scale-Location") +
        .theme_report()

    p_qq <- ggplot2::ggplot(df_d, ggplot2::aes(sample = residuals)) +
        ggplot2::stat_qq(colour = .pal[8], alpha = 0.45, size = 1.2) +
        ggplot2::stat_qq_line(colour = .pal[1], linewidth = 0.7) +
        ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles",
                      title = "C  Q-Q") +
        .theme_report()

    patchwork::wrap_plots(p_rd, p_sl, p_qq, ncol = 3)
}


# ---------------------------------------------------------------------------
# 5.  MAIN REPORT FUNCTION
# ---------------------------------------------------------------------------

#' Fit pooled LMMs (mice::with + mice::pool) for every exposure × covariate-set
#' combination and write all results to a PDF.
#'
#' @param mids_object       A `mids` object (output of mice).
#' @param outcome           Raw outcome column name (e.g. `"HGS_MAX"`).
#' @param outcome_fn        Function to transform the outcome before modelling
#'                          (e.g. `sqrt`, `log`).  Use `identity` for no
#'                          transformation.  The response column will be named
#'                          `<outcome>_<fn_name>`, or `<outcome>` for identity.
#' @param exposures         Data frame / tibble with columns:
#'                          - `exposure`       column name of the exposure
#'                          - `exposure_type`  `"linear"`, `"categorical"`,
#'                                             `"rcs"`, or `"ns"`
#'                          - `ref_level`      reference level for categorical
#'                                             exposures; use `NA` otherwise
#' @param covariate_sets    Named list of character vectors.  One model is fit
#'                          for each list element.
#' @param random_slope      Logical; include `(1 + time_var | id_var)` when
#'                          `TRUE`, `(1 | id_var)` when `FALSE`.
#' @param interaction       Logical; add `exposure × time_var` term when `TRUE`.
#' @param id_var            Subject ID column (default `"pt"`).
#' @param time_var          Time variable column (default
#'                          `"time_since_baseline"`).
#' @param out_dir           Directory to write the PDF into.
#' @return Invisibly, the path to the written PDF.
#'
#' @examples
#' \dontrun{
#' run_lmm_report(
#'     mids_object    = mice_analysis$mids$HGS_MAX,
#'     outcome        = "HGS_MAX",
#'     outcome_fn     = sqrt,
#'     exposures      = exposure_definitions,
#'     covariate_sets = covariate_sets_hgs,
#'     random_slope   = TRUE,
#'     interaction    = FALSE
#' )
#' }

run_lmm_report <- function(
    mids_object,
    outcome        = "HGS_MAX",
    outcome_fn     = identity,
    exposures      = tibble::tibble(
        exposure      = "dairy_100g",
        exposure_type = "linear",
        ref_level     = NA_character_
    ),
    covariate_sets = list(
        minimal = c("age_decades", "BMI_category", "education_level",
                    "smoking_status", "pa_levels_tertile_f1",
                    "diabetes_status", "sumtot1_scaled")
    ),
    random_slope   = TRUE,
    interaction    = FALSE,
    id_var         = "pt",
    time_var       = "time_since_baseline",
    out_dir        = "03_outputs/LMM_exploratory"
) {
    # ── Resolve outcome column name -------------------------------------------
    # deparse(substitute()) captures the literal passed by the user (e.g. "sqrt").
    # If the user passes a variable holding a function, we fall back to the
    # function's own name via deparse(body()) — identity stays as-is.
    fn_name <- deparse(substitute(outcome_fn))
    if (grepl("^[a-zA-Z_][a-zA-Z0-9_.]*$", fn_name)) {
        # looks like a plain name — keep it
    } else {
        # complex expression: try to get the underlying function name
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
                           paste0(resp_col, "_LMM_", timestamp, ".pdf"))

    grDevices::pdf(pdf_path, width = 14, height = 9)
    on.exit(grDevices::dev.off(), add = TRUE)

    # ── Cover page -----------------------------------------------------------
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#1E466EFF", col = NA))
    grid::grid.text(paste0("LMM Report  —  ", resp_col),
                    x = 0.5, y = 0.60,
                    gp = grid::gpar(col = "white", fontsize = 24,
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

        for (i in seq_len(nrow(exposures))) {
            exp_row   <- exposures[i, ]
            exp_col   <- exp_row$exposure
            exp_type  <- exp_row$exposure_type
            ref_lev   <- if (!is.na(exp_row$ref_level)) exp_row$ref_level else NULL
            model_tag <- paste0(resp_col, " ~ ", exp_col,
                                " (", exp_type, ")  |  ", cov_nm)

            # ── Build formula ------------------------------------------------
            formula <- .build_formula(
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
                fit_pooled_lmm(
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
                          "",
                          deparse(formula)),
                        title = paste("FAILED:", model_tag)
                    )
                    NULL
                }
            )
            if (is.null(fit)) next

            # ── Formula page -------------------------------------------------
            .text_page(
                c(deparse(fit$formula),
                  "",
                  paste("Imputations pooled:", fit$n_imp)),
                title = paste("Formula —", model_tag)
            )

            # ── Results: coefficient plot + table ----------------------------
            .results_page(fit$pooled_tidy, title = model_tag, out_dir = out_dir)

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
                .sandwich_comparison_page(fit$pooled_tidy, sand_tidy,
                                          title = model_tag)
            }

            # ── Diagnostics (imputation 1) -----------------------------------
            p_diag <- tryCatch(
                .plot_diagnostics(fit$first_model,
                                  label = paste0("[imp 1, ", model_tag, "]")),
                error = function(e) NULL
            )
            if (!is.null(p_diag)) print(p_diag)
        }
    }

    # ── Session info ---------------------------------------------------------
    .text_page(
        utils::capture.output(sessioninfo::session_info()),
        title = "Session Info"
    )

    message("PDF written to: ", pdf_path)
    invisible(pdf_path)
}
