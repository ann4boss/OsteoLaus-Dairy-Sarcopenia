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
    resp_col      = outcome,   # pre-computed by caller; avoids substitute() issues
    outcome_fn    = identity,
    id_var        = "pt",
    time_var      = "time_since_baseline",
    scale_centres = NULL
) {
    if (is.null(scale_centres)) {
        scale_centres <- list(
            age    = median(df$age_at_baseline,         na.rm = TRUE),
            sumtot = median(df$sumtot1,                 na.rm = TRUE),
            dairy  = median(df$dairy_total_gday_cumavg, na.rm = TRUE),
            fermented = median(df$dairy_fermented_gday_cumavg, na.rm = TRUE),
            nonfermented = median(df$dairy_non_fermented_gday_cumavg, na.rm = TRUE),
            highfat = median(df$dairy_highfat_gday_cumavg, na.rm = TRUE),
            lowfat = median(df$dairy_lowfat_gday_cumavg, na.rm = TRUE),
            time   = median(df[[time_var]],             na.rm = TRUE)
        )
    }

    df[[resp_col]] <- outcome_fn(df[[outcome]])

    df <- df |>
        dplyr::mutate(
            age_decades      = as.numeric(scale(age_at_baseline,
                                                center = scale_centres$age,
                                                scale  = 10)),
            sumtot1_hundreds = as.numeric(scale(sumtot1,
                                                center = scale_centres$sumtot,
                                                scale  = 100)),
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
        first_model = models[[1]],
        formula     = formula,
        n_imp       = m
    )
}


# ---------------------------------------------------------------------------
# 4.  PDF HELPERS
# ---------------------------------------------------------------------------

.pal <- c("#E76254FF","#EF8A47FF","#F7AA58FF","#FFD06FFF",
          "#AADCE0FF","#72BCD5FF","#528FADFF","#376795FF","#1E466EFF")

.theme_report <- function() {
    ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.line  = ggplot2::element_line(colour = "black", linewidth = 0.4),
            axis.ticks = ggplot2::element_line(colour = "black"),
            axis.text  = ggplot2::element_text(colour = "black"),
            axis.title = ggplot2::element_text(colour = "black"),
            plot.title = ggplot2::element_text(face = "bold", size = 10)
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
.results_page <- function(tidy_df, title) {

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
            sig   = p_value < 0.05,
            term  = forcats::fct_rev(factor(term))
        )

    p_coef <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = estimate, y = term,
                     xmin = conf_low, xmax = conf_high,
                     colour = sig)
    ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            colour = "grey60") +
        ggplot2::geom_errorbarh(height = 0.3, linewidth = 0.7) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::scale_colour_manual(
            values = c(`TRUE` = .pal[9], `FALSE` = .pal[5]),
            guide  = "none"
        ) +
        ggplot2::labs(x = "Estimate (95 % CI)", y = NULL, title = title) +
        .theme_report()

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

    gridExtra::grid.arrange(p_coef, tbl_grob, ncol = 2, widths = c(1.2, 1.8))
}


# ── Diagnostic plots ---------------------------------------------------------

.plot_diagnostics <- function(model, label = "") {
    res_v  <- residuals(model)
    fit_v  <- fitted(model)
    df_d   <- data.frame(fitted    = fit_v,
                          residuals = res_v,
                          sqrt_abs  = sqrt(abs(res_v)))

    p_rd <- ggplot2::ggplot(df_d, ggplot2::aes(fitted, residuals)) +
        ggplot2::geom_point(colour = .pal[8], alpha = 0.45, size = 1.2) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = .pal[1]) +
        ggplot2::geom_smooth(method = "loess", se = TRUE, colour = .pal[1],
                             fill = .pal[4], alpha = 0.2, linewidth = 0.7) +
        ggplot2::labs(x = "Fitted", y = "Residuals",
                      title = paste("A  Residuals vs Fitted", label)) +
        .theme_report()

    p_sl <- ggplot2::ggplot(df_d, ggplot2::aes(fitted, sqrt_abs)) +
        ggplot2::geom_point(colour = .pal[8], alpha = 0.45, size = 1.2) +
        ggplot2::geom_smooth(method = "loess", se = TRUE, colour = .pal[1],
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
                    "diabetes_status", "sumtot1_hundreds")
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
            .results_page(fit$pooled_tidy, title = model_tag)

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
