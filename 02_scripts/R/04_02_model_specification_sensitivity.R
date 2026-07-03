# =============================================================================
# Alternative Model Specifications — HGS, ALMI, Gait Speed
#
# Each model type is fitted with BOTH random-effects structures:
#   "main" = reference RE (RS for HGS/ALMI, RI only for gait)
#   "alt"  = alternative RE (RI only for HGS/ALMI, RS for gait)
#
#   M0 / M0b   Reference (main / alt RE)
#   M1 / M1b   Natural spline ns(dairy, df=3)
#   M2 / M2b   Polynomial poly(dairy, 2)
#   M3a / M3a* Log-transformed outcome
#   M3b / M3b* Sqrt-transformed outcome
#   M4 / M4b   Dairy × time interaction
#   M5 / M5b   Height + Weight instead of BMI (main RE / always RI)
#   M6 / M6b   Within-/between-person (Mundlak) decomposition
#   M8 / M8b   Heteroscedastic residuals: nlme::lme + varPower()
#   M9 / M9b   GAMM: mgcv::gamm with s(dairy, k=5)
#   M10 / M10b Quadratic age
#
# AIC / BIC are evaluated on imputation 1 (IC is not poolable across
# imputations in a straightforward way).  A note to this effect is printed
# on the cover page.
#
# Assumption-check plots are written to the PDF and also saved as individual
# PNG files under out_dir/assumption_checks/<outcome>/.
#
# Shared helpers (.pal, .theme_report, .text_page, .section_page) are
# sourced from 04_01_01_LMM_report_HGS_ALMI.R via tar_source().
# =============================================================================


# ---------------------------------------------------------------------------
# 1.  EXTENDED DATA PREPARATION
# ---------------------------------------------------------------------------

#' Prepare one imputation for sensitivity-specification modelling.
#'
#' Calls the main prep function and extends it with:
#'   - `height_scaled`, `weight_scaled` for the body-size reparameterisation
#'   - `dairy_pmean_100g`  between-person (person mean) dairy intake
#'   - `dairy_within_100g` within-person deviation from own mean
#'
#' @param df            A single completed (non-mids) data frame.
#' @param outcome       Raw outcome column name.
#' @param resp_col      Column to store the (possibly transformed) response.
#' @param outcome_fn    Transformation function.
#' @param id_var        Subject ID column.
#' @param time_var      Time variable column.
#' @param is_gait       Logical; if TRUE calls .prep_gait_imputation and uses
#'                      lagged dairy / height / weight column names.
#' @return List: df, resp_col, sens_centres.

.prep_sens_imputation <- function(
    df,
    outcome    = "HGS_MAX",
    resp_col   = outcome,
    outcome_fn = identity,
    id_var     = "pt",
    time_var   = "time_since_baseline",
    is_gait    = FALSE
) {
    # ── Call main prep --------------------------------------------------------
    prep <- if (is_gait) {
        .prep_gait_imputation(df, outcome = outcome, resp_col = resp_col,
                              outcome_fn = outcome_fn, id_var = id_var,
                              time_var = time_var)
    } else {
        .prep_one_imputation(df, outcome = outcome, resp_col = resp_col,
                             outcome_fn = outcome_fn, id_var = id_var,
                             time_var = time_var)
    }
    df <- prep$df

    # ── Resolve height / weight column names ---------------------------------
    .try_col <- function(candidates, data) {
        candidates[candidates %in% names(data)][1]   # first match or NA
    }

    if (is_gait) {
        ht_col <- .try_col(c("height_lag", "Height_lag", "height", "Height"), df)
        wt_col <- .try_col(c("weight_lag", "Weight_lag", "weight", "Weight"), df)
    } else {
        ht_col <- .try_col(c("height", "Height"), df)
        wt_col <- .try_col(c("weight", "Weight"), df)
    }

    sens_centres <- list(
        height = if (!is.na(ht_col)) median(df[[ht_col]], na.rm = TRUE) else NA_real_,
        weight = if (!is.na(wt_col)) median(df[[wt_col]], na.rm = TRUE) else NA_real_
    )

    if (!is.na(ht_col)) {
        df[["height_scaled"]] <- as.numeric(
            scale(df[[ht_col]], center = sens_centres$height, scale = 10)
        )
    }
    if (!is.na(wt_col)) {
        df[["weight_scaled"]] <- as.numeric(
            scale(df[[wt_col]], center = sens_centres$weight, scale = 10)
        )
    }

    # ── Within-/between-person dairy decomposition (Mundlak) ----------------
    dairy_100g_col <- if (is_gait) "dairy_100g_lag" else "dairy_100g"

    df <- df |>
        dplyr::group_by(.data[[id_var]]) |>
        dplyr::mutate(
            dairy_pmean_100g  = mean(.data[[dairy_100g_col]], na.rm = TRUE),
            dairy_within_100g = .data[[dairy_100g_col]] - dairy_pmean_100g
        ) |>
        dplyr::ungroup()

    list(df = df, resp_col = resp_col, sens_centres = sens_centres)
}


# ---------------------------------------------------------------------------
# 2.  nlme + varPower FITTER
# ---------------------------------------------------------------------------

#' Fit nlme::lme with varPower heteroscedastic residuals (ML).
#'
#' The response column must already exist in `df` (i.e. outcome transformation
#' has been applied by the prep function).
#'
#' @param df              Prepared data frame.
#' @param fixed_formula   Fixed-effects formula (as.formula).
#' @param random_formula  Random-effects formula, e.g. ~ 1 + time | id.
#' @param id_var          Subject ID column (converted to factor).
#' @return An nlme::lme object, or NULL if fitting fails.

.fit_nlme_varpower <- function(df, fixed_formula, random_formula, id_var = "pt") {
    df[[id_var]] <- factor(df[[id_var]])
    tryCatch(
        nlme::lme(
            fixed   = fixed_formula,
            random  = random_formula,
            weights = nlme::varPower(),
            data    = df,
            method  = "ML",
            control = nlme::lmeControl(maxIter = 200, msMaxIter = 200,
                                       opt = "optim", returnObject = TRUE)
        ),
        error   = function(e) { warning("nlme varPower: ", conditionMessage(e)); NULL },
        warning = function(w) {
            # Attempt to suppress common convergence warnings while returning the model
            tryCatch(
                suppressWarnings(nlme::lme(
                    fixed   = fixed_formula,
                    random  = random_formula,
                    weights = nlme::varPower(),
                    data    = df,
                    method  = "ML",
                    control = nlme::lmeControl(maxIter = 200, msMaxIter = 200,
                                               opt = "optim", returnObject = TRUE)
                )),
                error = function(e2) NULL
            )
        }
    )
}


# ---------------------------------------------------------------------------
# 3.  IC EXTRACTION HELPER
# ---------------------------------------------------------------------------

#' Extract AIC, BIC, log-likelihood, and df from any supported model object.
#'
#' Handles lmer (lme4/lmerTest), lme (nlme), and gamm (mgcv, via the $lme
#' component).
#'
#' @param mod        Fitted model object.
#' @param model_name Character label for the model row.
#' @return One-row tibble: model, AIC, BIC, logLik, df_model, converged.

.extract_ic_sens <- function(mod, model_name) {
    na_row <- tibble::tibble(
        model = model_name, AIC = NA_real_, BIC = NA_real_,
        logLik = NA_real_, df_model = NA_integer_, converged = FALSE
    )

    if (is.null(mod)) return(na_row)

    # mgcv::gamm returns list(gam=..., lme=...) — compare on lme component
    if (is.list(mod) && !is.null(mod$lme) && inherits(mod$lme, "lme")) {
        mod <- mod$lme
    }

    converged <- TRUE
    if (inherits(mod, "merMod")) {
        msgs <- mod@optinfo$conv$lme4$messages
        if (length(msgs) > 0L) converged <- FALSE
    }

    aic_v  <- tryCatch(AIC(mod),    error = function(e) NA_real_)
    bic_v  <- tryCatch(BIC(mod),    error = function(e) NA_real_)
    ll_obj <- tryCatch(logLik(mod), error = function(e) NULL)
    ll_v   <- if (!is.null(ll_obj)) as.numeric(ll_obj) else NA_real_
    df_v   <- if (!is.null(ll_obj)) as.integer(attr(ll_obj, "df")) else NA_integer_

    tibble::tibble(model = model_name, AIC = aic_v, BIC = bic_v,
                   logLik = ll_v, df_model = df_v, converged = converged)
}


# ---------------------------------------------------------------------------
# 4.  AIC / BIC COMPARISON PLOT
# ---------------------------------------------------------------------------

.aic_bic_plot_sens <- function(ic_tbl, title,
                                reference_model = "M0  Reference") {
    ref_row <- ic_tbl[ic_tbl$model == reference_model, ]
    if (nrow(ref_row) == 0 || is.na(ref_row$AIC[1])) {
        .text_page("Reference AIC not available — Δ plot skipped.", title = title)
        return(invisible(NULL))
    }

    ref_aic <- ref_row$AIC[1]
    ref_bic <- ref_row$BIC[1]

    plot_df <- ic_tbl |>
        dplyr::filter(!is.na(AIC)) |>
        dplyr::mutate(
            delta_AIC = AIC - ref_aic,
            delta_BIC = BIC - ref_bic,
            model     = factor(model, levels = rev(model)),
            is_ref    = (model == reference_model)
        ) |>
        tidyr::pivot_longer(c(delta_AIC, delta_BIC),
                            names_to  = "metric",
                            values_to = "delta") |>
        dplyr::mutate(metric = dplyr::recode(metric,
                                              delta_AIC = "ΔAIC",
                                              delta_BIC = "ΔBIC"))

    p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = delta, y = model, colour = is_ref, shape = metric)
    ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            colour = "grey50") +
        ggplot2::geom_point(size = 3.2,
                            position = ggplot2::position_dodge(width = 0.55)) +
        ggplot2::scale_colour_manual(
            values = c(`TRUE` = .pal[9], `FALSE` = .pal[1]),
            labels = c(`TRUE` = "Reference", `FALSE` = "Alternative"),
            name   = NULL
        ) +
        ggplot2::scale_shape_manual(values = c(16L, 17L), name = NULL) +
        ggplot2::labs(
            x       = "Δ IC (alternative − reference; negative = better fit)",
            y       = NULL,
            title   = title,
            caption = "|ΔAIC| < 2: similar support; |ΔAIC| 2–10: moderate evidence; >10: strong evidence"
        ) +
        .theme_report() +
        ggplot2::theme(legend.position = "right")

    print(p)
    invisible(plot_df)
}


# ---------------------------------------------------------------------------
# 5.  DAIRY COEFFICIENT COMPARISON ACROSS SPECIFICATIONS
# ---------------------------------------------------------------------------

.coef_comparison_plot_sens <- function(model_list, model_names,
                                        dairy_pattern, title) {
    rows <- mapply(function(mod, nm) {
        if (is.null(mod)) return(NULL)

        # Use lme component for GAMM objects
        actual_mod <- if (is.list(mod) && !is.null(mod$lme)) mod$lme else mod

        tidy_df <- tryCatch(
            broom.mixed::tidy(actual_mod, effects = "fixed", conf.int = TRUE),
            error = function(e) NULL
        )

        if (is.null(tidy_df) || nrow(tidy_df) == 0) return(NULL)

        # Fall back to ±1.96 SE if conf.low absent
        if (!"conf.low" %in% names(tidy_df)) {
            tidy_df <- dplyr::mutate(tidy_df,
                                      conf.low  = estimate - 1.96 * std.error,
                                      conf.high = estimate + 1.96 * std.error)
        }

        tidy_df |>
            dplyr::filter(grepl(dairy_pattern, term)) |>
            dplyr::mutate(model = nm)
    }, model_list, model_names, SIMPLIFY = FALSE)

    plot_df <- dplyr::bind_rows(rows[!vapply(rows, is.null, logical(1))]) |>
        dplyr::filter(!is.na(estimate)) |>
        dplyr::mutate(model = factor(model, levels = rev(model_names)))

    if (nrow(plot_df) == 0) {
        .text_page("No linear dairy coefficients extractable.", title = title)
        return(invisible(NULL))
    }

    p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = estimate, y = model,
                     xmin = conf.low, xmax = conf.high)
    ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            colour = "grey50") +
        ggplot2::geom_errorbarh(height = 0.25, linewidth = 0.9,
                                colour = .pal[8]) +
        ggplot2::geom_point(size = 2.5, colour = .pal[8]) +
        ggplot2::facet_wrap(~ term, scales = "free_x") +
        ggplot2::labs(
            x       = "Estimate (95 % CI)",
            y       = NULL,
            title   = title,
            caption = "Single imputation (imp 1); models using spline / poly dairy excluded"
        ) +
        .theme_report()

    print(p)
    invisible(plot_df)
}


# ---------------------------------------------------------------------------
# 6.  GAMM SMOOTH PLOT
# ---------------------------------------------------------------------------

.gamm_smooth_page <- function(gamm_mod, dairy_col, outcome_label) {
    if (is.null(gamm_mod) || is.null(gamm_mod$gam)) {
        .text_page("GAMM smooth not available.", title = "GAMM smooth")
        return(invisible(NULL))
    }
    tryCatch({
        old_par <- graphics::par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
        on.exit(graphics::par(old_par), add = TRUE)
        plot(gamm_mod$gam, shade = TRUE, seWithMean = TRUE, rug = TRUE,
             xlab = paste0(dairy_col, " (g/day)"),
             ylab = paste0("s(", dairy_col, ")"),
             main = paste0("GAMM smooth  |  ", outcome_label))
        plot(gamm_mod$gam, shade = TRUE, seWithMean = TRUE,
             residuals = TRUE, rug = TRUE, pch = 16, cex = 0.3, col = "#55555540",
             xlab = paste0(dairy_col, " (g/day)"),
             ylab = paste0("s(", dairy_col, ")"),
             main = "With partial residuals")
    }, error = function(e) {
        .text_page(paste("GAMM smooth plot failed:", conditionMessage(e)),
                   title = "GAMM smooth FAILED")
    })
}


# ---------------------------------------------------------------------------
# 7.  WITHIN-/BETWEEN DETAIL PAGE
# ---------------------------------------------------------------------------

.wb_detail_page <- function(m6_model, outcome_label) {
    if (is.null(m6_model)) {
        .text_page("Within-/between model not available.", title = "W/B decomposition")
        return(invisible(NULL))
    }
    tidy_wb <- tryCatch(
        broom.mixed::tidy(m6_model, effects = "fixed", conf.int = TRUE),
        error = function(e) NULL
    )
    if (is.null(tidy_wb)) return(invisible(NULL))

    wb_terms <- tidy_wb |>
        dplyr::filter(grepl("dairy_(within|pmean)", term)) |>
        dplyr::mutate(
            dplyr::across(c(estimate, std.error, conf.low, conf.high),
                          ~ round(.x, 4)),
            p.value = dplyr::if_else(p.value < 0.001, "<0.001",
                                     as.character(round(p.value, 3)))
        )

    .text_page(
        c(
            paste("Outcome:", outcome_label),
            "",
            "dairy_within_100g : deviation from subject's own mean (within-person effect)",
            "dairy_pmean_100g  : subject-level mean relative to cohort median (between-person effect)",
            "",
            "A significant divergence between within and between estimates implies",
            "confounding by stable, unmeasured subject-level characteristics.",
            "",
            utils::capture.output(print(as.data.frame(wb_terms), row.names = FALSE))
        ),
        title = paste("Within-/Between-Person Dairy  — ", outcome_label)
    )
}


# ---------------------------------------------------------------------------
# 8.  ASSUMPTION CHECK PLOTS (all specifications)
# ---------------------------------------------------------------------------

#' Print residual diagnostic plots for a list of fitted models.
#'
#' Calls the existing .plot_diagnostics() helper (Residuals vs Fitted,
#' Scale-Location, Q-Q) for every non-NULL model in the list.
#'
#' Additional pages per model type:
#'   - nlme::lme  (M8 varPower): second row with *normalized* (Pearson)
#'     residuals so the variance-stabilisation can be verified visually.
#'   - mgcv::gamm (M9):  mgcv::gam.check() 2×2 panel for the gam component.
#'
#' @param model_list      Named list of fitted models (NULL entries skipped).
#' @param model_names     Character vector of display labels.
#' @param section_subtitle Short subtitle for the section divider page.

.assumption_checks_section <- function(model_list, model_names,
                                        section_subtitle = "",
                                        png_dir = NULL) {
    if (!is.null(png_dir))
        dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)

    .section_page("Assumption Checks", subtitle = section_subtitle)

    .text_page(
        c(
            "Each model: (A) Residuals vs Fitted,  (B) Scale-Location,  (C) Q-Q",
            "",
            "M8 varPower (nlme): raw residuals are shown first, then Pearson /",
            "  normalized residuals.  Normalized residuals should be homoscedastic",
            "  if the variance structure is correctly specified.",
            "",
            "M9 GAMM: standard diagnostics use the lme component (conditional",
            "  residuals at the subject level, comparable to lmer residuals).",
            "  mgcv::gam.check() is appended as an additional GAMM-specific check.",
            "",
            if (!is.null(png_dir)) paste("PNG files saved to:", png_dir) else ""
        ),
        title = "Assumption Checks — Notes"
    )

    for (i in seq_along(model_list)) {
        mod <- model_list[[i]]
        nm  <- model_names[i]
        if (is.null(mod)) next

        safe_nm  <- gsub("[^a-zA-Z0-9]", "_", nm)

        # GAMM: use lme component for conditional residuals
        diag_mod <- if (is.list(mod) && !is.null(mod$lme)) mod$lme else mod

        # ── Standard 3-panel diagnostic plot --------------------------------
        p <- tryCatch(
            .plot_diagnostics(diag_mod, label = nm),
            error = function(e) {
                .text_page(paste("Diagnostics failed:", conditionMessage(e)),
                           title = paste("Diagnostics FAILED —", nm))
                NULL
            }
        )
        if (!is.null(p)) {
            if (!is.null(png_dir))
                ggplot2::ggsave(
                    file.path(png_dir, paste0(safe_nm, "_diagnostics.png")),
                    plot = p, width = 14, height = 4.5, dpi = 150
                )
            print(p)
        }

        # ── Extra: normalized residuals for nlme + varPower ------------------
        if (inherits(mod, "lme")) {
            tryCatch({
                res_norm <- residuals(mod, type = "normalized")
                fit_v    <- fitted(mod)
                df_norm  <- data.frame(fitted    = fit_v,
                                       residuals = res_norm,
                                       sqrt_abs  = sqrt(abs(res_norm)))

                p_rd <- ggplot2::ggplot(
                    df_norm, ggplot2::aes(fitted, residuals)
                ) +
                    ggplot2::geom_point(colour = .pal[8], alpha = 0.4, size = 1.2) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                        colour = .pal[1]) +
                    ggplot2::geom_smooth(method = "loess", se = TRUE,
                                         colour = .pal[1], fill = .pal[4],
                                         alpha = 0.2, linewidth = 0.7) +
                    ggplot2::labs(x = "Fitted",
                                  y = "Normalized residuals",
                                  title = "Normalized Residuals vs Fitted [varPower check]") +
                    .theme_report()

                p_sl <- ggplot2::ggplot(
                    df_norm, ggplot2::aes(fitted, sqrt_abs)
                ) +
                    ggplot2::geom_point(colour = .pal[8], alpha = 0.4, size = 1.2) +
                    ggplot2::geom_smooth(method = "loess", se = TRUE,
                                         colour = .pal[1], fill = .pal[3],
                                         alpha = 0.2, linewidth = 0.7) +
                    ggplot2::labs(x = "Fitted",
                                  y = expression(sqrt("|Norm. Residuals|")),
                                  title = "Scale-Location (normalized)") +
                    .theme_report()

                p_qq <- ggplot2::ggplot(
                    df_norm, ggplot2::aes(sample = residuals)
                ) +
                    ggplot2::stat_qq(colour = .pal[8], alpha = 0.4, size = 1.2) +
                    ggplot2::stat_qq_line(colour = .pal[1], linewidth = 0.7) +
                    ggplot2::labs(x = "Theoretical quantiles",
                                  y = "Sample quantiles",
                                  title = "Q-Q (normalized residuals)") +
                    .theme_report()

                p_norm <- patchwork::wrap_plots(p_rd, p_sl, p_qq, ncol = 3) +
                    patchwork::plot_annotation(
                        title      = paste("Normalized Residuals (varPower) —", nm),
                        tag_levels = "A",
                        theme = ggplot2::theme(
                            plot.title = ggplot2::element_text(
                                face = "bold", size = 13,
                                margin = ggplot2::margin(b = 6)
                            ),
                            plot.tag = ggplot2::element_text(face = "bold", size = 10)
                        )
                    )
                if (!is.null(png_dir))
                    ggplot2::ggsave(
                        file.path(png_dir, paste0(safe_nm, "_norm_residuals.png")),
                        plot = p_norm, width = 14, height = 4.5, dpi = 150
                    )
                print(p_norm)
            }, error = function(e) {
                .text_page(
                    paste("Normalized residual plots failed:", conditionMessage(e)),
                    title = paste("Normalized residuals FAILED —", nm)
                )
            })
        }

        # ── Extra: gam.check() for GAMM -------------------------------------
        if (is.list(mod) && !is.null(mod$gam)) {
            tryCatch({
                if (!is.null(png_dir)) {
                    png_file <- file.path(png_dir, paste0(safe_nm, "_gamcheck.png"))
                    grDevices::png(png_file, width = 1800, height = 1800, res = 150)
                    old_par <- graphics::par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
                    tryCatch(mgcv::gam.check(mod$gam, rep = 500),
                             error = function(e2) NULL)
                    graphics::par(old_par)
                    grDevices::dev.off()
                }
                old_par <- graphics::par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
                on.exit(graphics::par(old_par), add = TRUE)
                mgcv::gam.check(mod$gam, rep = 500)
            }, error = function(e) {
                .text_page(
                    paste("gam.check() failed:", conditionMessage(e)),
                    title = paste("gam.check FAILED —", nm)
                )
            })
        }
    }
    invisible(NULL)
}


# ---------------------------------------------------------------------------
# 9.  MAIN FUNCTION — HGS / ALMI
# ---------------------------------------------------------------------------

#' Alternative model specifications for HGS or ALMI, written to a PDF.
#'
#' @param mids_object  A `mids` object.
#' @param outcome      Raw outcome column (e.g. `"HGS_MAX"`).
#' @param outcome_fn   Transformation used in the reference model (e.g. `log`,
#'                     `identity`).
#' @param covariates   Character vector of covariate column names (main set).
#' @param random_slope Logical; reference model uses random slope when TRUE.
#' @param id_var       Subject ID column.
#' @param time_var     Time variable column.
#' @param out_dir      Output directory.
#' @return Invisibly, path to the written PDF.

run_model_specification_sensitivity <- function(
    mids_object,
    outcome        = "HGS_MAX",
    outcome_fn     = identity,
    covariates     = c("age_at_baseline_scaled", "BMI_category",
                       "education_level", "smoking_status",
                       "pa_levels_tertile_f1", "diabetes_status",
                       "sumtot1_scaled"),
    random_slope   = TRUE,
    id_var         = "pt",
    time_var       = "time_since_baseline",
    out_dir        = "03_outputs/model_specification_sensitivity"
) {
    fn_name  <- deparse(substitute(outcome_fn))
    if (!grepl("^[a-zA-Z_][a-zA-Z0-9_.]*$", fn_name))
        fn_name <- tryCatch(deparse(as.list(body(outcome_fn))[[1]]),
                            error = function(e) "transformed")
    resp_col <- if (fn_name %in% c("identity", "")) outcome
               else paste0(outcome, "_", fn_name)

    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    ts       <- format(Sys.time(), "%Y%m%d_%H%M")
    pdf_path <- file.path(out_dir, paste0("modspec_", outcome, "_", ts, ".pdf"))

    grDevices::pdf(pdf_path, width = 14, height = 9)
    on.exit(grDevices::dev.off(), add = TRUE)

    # ── Cover page -----------------------------------------------------------
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = .pal[9], col = NA))
    grid::grid.text(
        paste0("Model Specification Sensitivity  —  ", outcome),
        x = 0.5, y = 0.62,
        gp = grid::gpar(col = "white", fontsize = 22, fontface = "bold")
    )
    for (k in seq_along(c(
        paste0("Reference transform: ", fn_name,
               "   |   Random slope: ", random_slope),
        paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
        "NOTE: AIC / BIC computed on imputation 1 only (IC is not poolable across imputations)."
    )))
        grid::grid.text(
            c(paste0("Reference transform: ", fn_name, "   |   Random slope: ", random_slope),
              paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
              "NOTE: AIC / BIC computed on imputation 1 only.")[k],
            x = 0.5, y = 0.50 - (k - 1) * 0.07,
            gp = grid::gpar(col = .pal[5], fontsize = 11)
        )

    # ── Prepare data (imputation 1) ------------------------------------------
    df1  <- mice::complete(mids_object, 1L)
    prep <- .prep_sens_imputation(df1,
                                   outcome    = outcome,
                                   resp_col   = resp_col,
                                   outcome_fn = outcome_fn,
                                   id_var     = id_var,
                                   time_var   = time_var,
                                   is_gait    = FALSE)
    df <- prep$df

    re_slope   <- paste0("(1 + ", time_var, " | ", id_var, ")")
    re_noSlope <- paste0("(1 | ", id_var, ")")
    re_term    <- if (random_slope) re_slope else re_noSlope
    re_alt     <- if (random_slope) re_noSlope else re_slope
    lbl_main   <- if (random_slope) "(RS)" else "(RI)"
    lbl_alt    <- if (random_slope) "(RI)" else "(RS)"
    ctrl <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

    .lmer <- function(f) {
        tryCatch(lmerTest::lmer(f, data = df, REML = FALSE, control = ctrl),
                 error = function(e) { warning(conditionMessage(e)); NULL })
    }

    # ── M0 / M0b: Reference — reference RE / alternative RE -----------------
    m0 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g", time_var, covariates, re_term), collapse = " + "))))
    m0b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g", time_var, covariates, re_alt), collapse = " + "))))

    # ── M1 / M1b: Natural spline on dairy (df = 3) --------------------------
    m1 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("splines::ns(dairy_100g, df = 3)", time_var, covariates, re_term),
              collapse = " + "))))
    m1b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("splines::ns(dairy_100g, df = 3)", time_var, covariates, re_alt),
              collapse = " + "))))

    # ── M2 / M2b: Quadratic polynomial on dairy -----------------------------
    m2 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("poly(dairy_100g, 2, raw = TRUE)", time_var, covariates, re_term),
              collapse = " + "))))
    m2b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("poly(dairy_100g, 2, raw = TRUE)", time_var, covariates, re_alt),
              collapse = " + "))))

    # ── M3a / M3b: Outcome transformations — reference RE -------------------
    # ── M3a_alt / M3b_alt:  same, alternative RE ----------------------------
    df[[paste0(outcome, "_log")]]  <- log(pmax(df[[outcome]], 1e-6))
    df[[paste0(outcome, "_sqrt")]] <- sqrt(pmax(df[[outcome]], 0))

    m3a     <- .lmer(stats::as.formula(paste(paste0(outcome, "_log"), "~",
        paste(c("dairy_100g", time_var, covariates, re_term), collapse = " + "))))
    m3a_alt <- .lmer(stats::as.formula(paste(paste0(outcome, "_log"), "~",
        paste(c("dairy_100g", time_var, covariates, re_alt), collapse = " + "))))
    m3b     <- .lmer(stats::as.formula(paste(paste0(outcome, "_sqrt"), "~",
        paste(c("dairy_100g", time_var, covariates, re_term), collapse = " + "))))
    m3b_alt <- .lmer(stats::as.formula(paste(paste0(outcome, "_sqrt"), "~",
        paste(c("dairy_100g", time_var, covariates, re_alt), collapse = " + "))))

    # ── M4 / M4b: Dairy × time interaction ----------------------------------
    m4 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g", time_var, covariates, re_term,
                paste0("dairy_100g:", time_var)), collapse = " + "))))
    m4b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g", time_var, covariates, re_alt,
                paste0("dairy_100g:", time_var)), collapse = " + "))))

    # ── M5 / M5b: Height + Weight instead of BMI ----------------------------
    # M5 uses the reference RE; M5b uses re_noSlope (RI only regardless of ref)
    has_hw <- all(c("height_scaled", "weight_scaled") %in% names(df))
    m5  <- NULL
    m5b <- NULL
    if (has_hw) {
        cov_hw <- c(setdiff(covariates, "BMI_category"), "weight_scaled")
        m5  <- .lmer(stats::as.formula(paste(resp_col, "~",
            paste(c("dairy_100g", time_var, cov_hw, re_term), collapse = " + "))))
        m5b <- .lmer(stats::as.formula(paste(resp_col, "~",
            paste(c("dairy_100g", time_var, cov_hw, re_alt), collapse = " + "))))
    } else {
        .text_page(
            c("height_scaled and/or weight_scaled not found in data.",
              "M5 / M5b (height + weight reparameterisation) skipped.",
              "",
              "Expected column names: 'height' or 'Height' (and 'weight' / 'Weight')."),
            title = paste("M5 / M5b skipped —", outcome)
        )
    }

    # ── M6 / M6b: Within-/between-person decomposition ----------------------
    m6 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_within_100g", "dairy_pmean_100g", time_var, covariates, re_term),
              collapse = " + "))))
    m6b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_within_100g", "dairy_pmean_100g", time_var, covariates, re_alt),
              collapse = " + "))))

    # ── M8 / M8b: nlme + varPower (reference RE / alternative RE) -----------
    fixed_nlme       <- stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g", time_var, covariates), collapse = " + ")))
    random_nlme_main <- if (random_slope)
        stats::as.formula(paste0("~ 1 + ", time_var, " | ", id_var))
    else
        stats::as.formula(paste0("~ 1 | ", id_var))
    random_nlme_alt  <- if (random_slope)
        stats::as.formula(paste0("~ 1 | ", id_var))
    else
        stats::as.formula(paste0("~ 1 + ", time_var, " | ", id_var))
    m8  <- .fit_nlme_varpower(df, fixed_nlme, random_nlme_main, id_var = id_var)
    m8b <- .fit_nlme_varpower(df, fixed_nlme, random_nlme_alt,  id_var = id_var)

    # ── M9 / M9b: GAMM with smooth on dairy ---------------------------------
    dairy_raw_col <- "dairy_total_gday_cumavg"
    m9  <- NULL
    m9b <- NULL
    if (dairy_raw_col %in% names(df)) {
        rand_form_main <- if (random_slope)
            stats::as.formula(paste0("~ 1 + ", time_var))
        else
            stats::as.formula("~ 1")
        rand_form_alt  <- if (random_slope)
            stats::as.formula("~ 1")
        else
            stats::as.formula(paste0("~ 1 + ", time_var))
        gam_formula <- stats::as.formula(paste(resp_col, "~",
            paste(c(paste0("s(", dairy_raw_col, ", k = 5, bs = 'cr')"),
                    time_var, covariates), collapse = " + ")))
        .fit_gamm <- function(rf) {
            tryCatch(
                mgcv::gamm(formula   = gam_formula,
                           random    = stats::setNames(list(rf), id_var),
                           data      = df,
                           na.action = na.omit),
                error = function(e) {
                    message("GAMM failed (", outcome, "): ", conditionMessage(e))
                    NULL
                }
            )
        }
        m9  <- .fit_gamm(rand_form_main)
        m9b <- .fit_gamm(rand_form_alt)
    }

    # ── M10 / M10b: Quadratic age (reference RE / alternative RE) -----------
    cov_no_age <- setdiff(covariates, "age_at_baseline_scaled")
    m10 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g", time_var,
                "poly(age_at_baseline_scaled, 2, raw = TRUE)",
                cov_no_age, re_term), collapse = " + "))))
    m10b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g", time_var,
                "poly(age_at_baseline_scaled, 2, raw = TRUE)",
                cov_no_age, re_alt), collapse = " + "))))

    # =========================================================================
    # AIC / BIC table + plot
    # =========================================================================
    .section_page("AIC / BIC Comparison", subtitle = outcome)

    ref_label <- paste("M0   Reference", lbl_main)
    ic_rows <- dplyr::bind_rows(
        .extract_ic_sens(m0,      paste("M0   Reference",   lbl_main)),
        .extract_ic_sens(m0b,     paste("M0b  Reference",   lbl_alt)),
        .extract_ic_sens(m1,      paste("M1   NS spline",   lbl_main)),
        .extract_ic_sens(m1b,     paste("M1b  NS spline",   lbl_alt)),
        .extract_ic_sens(m2,      paste("M2   Poly² dairy", lbl_main)),
        .extract_ic_sens(m2b,     paste("M2b  Poly² dairy", lbl_alt)),
        .extract_ic_sens(m3a,     paste("M3a  Log outcome", lbl_main)),
        .extract_ic_sens(m3a_alt, paste("M3a  Log outcome", lbl_alt)),
        .extract_ic_sens(m3b,     paste("M3b  Sqrt outcome", lbl_main)),
        .extract_ic_sens(m3b_alt, paste("M3b  Sqrt outcome", lbl_alt)),
        .extract_ic_sens(m4,      paste("M4   Dairy×time", lbl_main)),
        .extract_ic_sens(m4b,     paste("M4b  Dairy×time", lbl_alt)),
        .extract_ic_sens(m5,      paste("M5   Height+Weight", lbl_main)),
        .extract_ic_sens(m5b,     paste("M5b  Height+Weight", lbl_alt)),
        .extract_ic_sens(m6,      paste("M6   W/B dairy",   lbl_main)),
        .extract_ic_sens(m6b,     paste("M6b  W/B dairy",   lbl_alt)),
        .extract_ic_sens(m8,      paste("M8   varPower",    lbl_main)),
        .extract_ic_sens(m8b,     paste("M8b  varPower",    lbl_alt)),
        .extract_ic_sens(m9,      paste("M9   GAMM s(dairy,k=5)", lbl_main)),
        .extract_ic_sens(m9b,     paste("M9b  GAMM s(dairy,k=5)", lbl_alt)),
        .extract_ic_sens(m10,     paste("M10  Poly² age",  lbl_main)),
        .extract_ic_sens(m10b,    paste("M10b Poly² age",  lbl_alt))
    )

    ref_aic <- ic_rows$AIC[ic_rows$model == ref_label][1]
    ref_bic <- ic_rows$BIC[ic_rows$model == ref_label][1]

    ic_rows <- ic_rows |>
        dplyr::mutate(
            delta_AIC = round(AIC - ref_aic, 2),
            delta_BIC = round(BIC - ref_bic, 2),
            AIC       = round(AIC, 2),
            BIC       = round(BIC, 2),
            logLik    = round(logLik, 2)
        )

    tbl_grob <- gridExtra::tableGrob(
        ic_rows |> dplyr::select(model, AIC, BIC, delta_AIC, delta_BIC,
                                  df_model, converged),
        rows  = NULL,
        theme = gridExtra::ttheme_minimal(
            base_size = 8,
            core    = list(fg_params = list(hjust = 0, x = 0.02)),
            colhead = list(fg_params = list(fontface = "bold", hjust = 0, x = 0.02))
        )
    )
    grid::grid.newpage()
    gridExtra::grid.arrange(tbl_grob)

    readr::write_csv(
        ic_rows,
        file.path(out_dir, paste0("ic_comparison_", outcome, "_", ts, ".csv"))
    )

    .aic_bic_plot_sens(ic_rows,
                       title = paste0("ΔAIC / ΔBIC relative to reference  —  ",
                                      outcome),
                       reference_model = ref_label)

    # =========================================================================
    # Dairy coefficient comparison (linear-dairy models only)
    # =========================================================================
    .section_page("Dairy Coefficient Comparison", subtitle = outcome)

    .coef_comparison_plot_sens(
        model_list  = list(m0, m0b, m4, m4b, m5, m5b, m6, m6b, m8, m8b, m10, m10b),
        model_names = c(paste("M0   Reference",    lbl_main),
                        paste("M0b  Reference",    lbl_alt),
                        paste("M4   Dairy×time", lbl_main),
                        paste("M4b  Dairy×time", lbl_alt),
                        paste("M5   Height+Weight", lbl_main),
                        paste("M5b  Height+Weight", lbl_alt),
                        paste("M6   W/B dairy",    lbl_main),
                        paste("M6b  W/B dairy",    lbl_alt),
                        paste("M8   varPower",     lbl_main),
                        paste("M8b  varPower",     lbl_alt),
                        paste("M10  Poly² age", lbl_main),
                        paste("M10b Poly² age", lbl_alt)),
        dairy_pattern = "^dairy_100g$|^dairy_within_100g$",
        title = paste("Dairy estimate (95 % CI) across specifications  — ", outcome)
    )

    # =========================================================================
    # Assumption checks — PDF + PNG export
    # =========================================================================
    png_dir_assump <- file.path(out_dir, "assumption_checks", outcome)

    .assumption_checks_section(
        model_list  = list(m0, m0b, m1, m1b, m2, m2b,
                           m3a, m3a_alt, m3b, m3b_alt,
                           m4, m4b, m5, m5b, m6, m6b,
                           m8, m8b, m9, m9b, m10, m10b),
        model_names = c(paste("M0   Reference",       lbl_main),
                        paste("M0b  Reference",       lbl_alt),
                        paste("M1   NS spline dairy", lbl_main),
                        paste("M1b  NS spline dairy", lbl_alt),
                        paste("M2   Poly² dairy", lbl_main),
                        paste("M2b  Poly² dairy", lbl_alt),
                        paste("M3a  Log outcome",     lbl_main),
                        paste("M3a  Log outcome",     lbl_alt),
                        paste("M3b  Sqrt outcome",    lbl_main),
                        paste("M3b  Sqrt outcome",    lbl_alt),
                        paste("M4   Dairy×time", lbl_main),
                        paste("M4b  Dairy×time", lbl_alt),
                        paste("M5   Height+Weight",   lbl_main),
                        paste("M5b  Height+Weight",   lbl_alt),
                        paste("M6   W/B dairy",       lbl_main),
                        paste("M6b  W/B dairy",       lbl_alt),
                        paste("M8   varPower",        lbl_main),
                        paste("M8b  varPower",        lbl_alt),
                        paste("M9   GAMM",            lbl_main),
                        paste("M9b  GAMM",            lbl_alt),
                        paste("M10  Poly² age",  lbl_main),
                        paste("M10b Poly² age",  lbl_alt)),
        section_subtitle = outcome,
        png_dir = png_dir_assump
    )

    # =========================================================================
    # GAMM smooth
    # =========================================================================
    .section_page("GAMM Smooth",
                  subtitle = paste0("s(dairy_total_gday_cumavg, k=5)  |  ", outcome))

    for (.gm in list(list(m9,  lbl_main), list(m9b, lbl_alt))) {
        .gamm_smooth_page(.gm[[1]], dairy_raw_col,
                          paste(outcome, "GAMM", .gm[[2]]))
        if (!is.null(.gm[[1]])) {
            .text_page(utils::capture.output(summary(.gm[[1]]$gam)),
                       title = paste("GAMM summary —", outcome, .gm[[2]]))
            .text_page(utils::capture.output(summary(.gm[[1]]$lme)),
                       title = paste("GAMM (lme) summary —", outcome, .gm[[2]]))
        }
    }

    # =========================================================================
    # Within-/between detail
    # =========================================================================
    .section_page("Within- / Between-Person Decomposition",
                  subtitle = paste("Mundlak decomposition of dairy intake  |  ", outcome))
    .wb_detail_page(m6, outcome)

    # =========================================================================
    # varPower model summaries (M8 main + alt)
    # =========================================================================
    for (.vm in list(list(m8, lbl_main), list(m8b, lbl_alt))) {
        if (!is.null(.vm[[1]])) {
            .section_page("nlme + varPower Summary",
                          subtitle = paste(outcome, .vm[[2]]))
            .text_page(utils::capture.output(summary(.vm[[1]])),
                       title = paste("nlme + varPower —", outcome, .vm[[2]]))
        }
    }

    # =========================================================================
    # Session info
    # =========================================================================
    .text_page(utils::capture.output(sessioninfo::session_info()),
               title = "Session Info")

    message("PDF written to: ", pdf_path)
    message("Assumption-check PNGs saved to: ", png_dir_assump)
    invisible(pdf_path)
}


# ---------------------------------------------------------------------------
# 9.  GAIT SPEED VERSION
# ---------------------------------------------------------------------------

#' Alternative model specifications for gait speed, written to a PDF.
#'
#' Gait speed uses lagged covariates (`_lag` suffix), typically has no random
#' slope (only 2 visits).  M0b / "b" variants add a random slope.
#'
#' @param mids_object  A `mids` object for gait speed.
#' @param outcome      Raw outcome column (default `"gait_speed"`).
#' @param outcome_fn   Transformation for the reference model.
#' @param covariates   Character vector of lagged covariate column names.
#' @param random_slope Logical; reference model random slope (default FALSE).
#' @param id_var       Subject ID column.
#' @param time_var     Time variable column.
#' @param out_dir      Output directory.
#' @return Invisibly, path to the written PDF.

run_model_specification_sensitivity_gait <- function(
    mids_object,
    outcome        = "gait_speed",
    outcome_fn     = identity,
    covariates     = c("age_at_baseline_scaled_lag", "BMI_category_lag",
                       "education_level_lag", "smoking_status_lag",
                       "pa_levels_tertile_f1_lag", "diabetes_status_lag"),
    random_slope   = FALSE,
    id_var         = "pt",
    time_var       = "time_since_baseline",
    out_dir        = "03_outputs/model_specification_sensitivity"
) {
    fn_name  <- deparse(substitute(outcome_fn))
    if (!grepl("^[a-zA-Z_][a-zA-Z0-9_.]*$", fn_name))
        fn_name <- tryCatch(deparse(as.list(body(outcome_fn))[[1]]),
                            error = function(e) "transformed")
    resp_col <- if (fn_name %in% c("identity", "")) outcome
               else paste0(outcome, "_", fn_name)

    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    ts       <- format(Sys.time(), "%Y%m%d_%H%M")
    pdf_path <- file.path(out_dir, paste0("modspec_gait_speed_", ts, ".pdf"))

    grDevices::pdf(pdf_path, width = 14, height = 9)
    on.exit(grDevices::dev.off(), add = TRUE)

    # ── Cover page -----------------------------------------------------------
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = .pal[9], col = NA))
    grid::grid.text(
        "Model Specification Sensitivity  —  Gait Speed",
        x = 0.5, y = 0.62,
        gp = grid::gpar(col = "white", fontsize = 22, fontface = "bold")
    )
    for (k in seq_along(c(
        paste0("Reference transform: ", fn_name,
               "   |   Random slope: ", random_slope),
        paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
        "NOTE: AIC / BIC computed on imputation 1 only."
    )))
        grid::grid.text(
            c(paste0("Reference transform: ", fn_name, "   |   Random slope: ", random_slope),
              paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
              "NOTE: AIC / BIC computed on imputation 1 only.")[k],
            x = 0.5, y = 0.50 - (k - 1) * 0.07,
            gp = grid::gpar(col = .pal[5], fontsize = 11)
        )

    # ── Prepare data (imputation 1) ------------------------------------------
    df1  <- mice::complete(mids_object, 1L)
    prep <- .prep_sens_imputation(df1,
                                   outcome    = outcome,
                                   resp_col   = resp_col,
                                   outcome_fn = outcome_fn,
                                   id_var     = id_var,
                                   time_var   = time_var,
                                   is_gait    = TRUE)
    df <- prep$df

    re_slope   <- paste0("(1 + ", time_var, " | ", id_var, ")")
    re_noSlope <- paste0("(1 | ", id_var, ")")
    re_term    <- if (random_slope) re_slope else re_noSlope
    re_alt     <- if (random_slope) re_noSlope else re_slope
    lbl_main   <- if (random_slope) "(RS)" else "(RI)"
    lbl_alt    <- if (random_slope) "(RI)" else "(RS)"
    ctrl <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

    .lmer <- function(f) {
        tryCatch(lmerTest::lmer(f, data = df, REML = FALSE, control = ctrl),
                 error = function(e) { warning(conditionMessage(e)); NULL })
    }

    # ── M0 / M0b: Reference — reference RE / alternative RE -----------------
    m0 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_term), collapse = " + "))))
    m0b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_alt), collapse = " + "))))

    # ── M1 / M1b: Natural spline on lagged dairy ----------------------------
    m1 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("splines::ns(dairy_100g_lag, df = 3)", time_var, covariates, re_term),
              collapse = " + "))))
    m1b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("splines::ns(dairy_100g_lag, df = 3)", time_var, covariates, re_alt),
              collapse = " + "))))

    # ── M2 / M2b: Quadratic polynomial on lagged dairy ----------------------
    m2 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("poly(dairy_100g_lag, 2, raw = TRUE)", time_var, covariates, re_term),
              collapse = " + "))))
    m2b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("poly(dairy_100g_lag, 2, raw = TRUE)", time_var, covariates, re_alt),
              collapse = " + "))))

    # ── M3a / M3b: Outcome transformations — reference RE -------------------
    # ── M3a_alt / M3b_alt: same, alternative RE -----------------------------
    df[[paste0(outcome, "_log")]]  <- log(pmax(df[[outcome]], 1e-6))
    df[[paste0(outcome, "_sqrt")]] <- sqrt(pmax(df[[outcome]], 0))

    m3a     <- .lmer(stats::as.formula(paste(paste0(outcome, "_log"), "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_term), collapse = " + "))))
    m3a_alt <- .lmer(stats::as.formula(paste(paste0(outcome, "_log"), "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_alt), collapse = " + "))))
    m3b     <- .lmer(stats::as.formula(paste(paste0(outcome, "_sqrt"), "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_term), collapse = " + "))))
    m3b_alt <- .lmer(stats::as.formula(paste(paste0(outcome, "_sqrt"), "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_alt), collapse = " + "))))

    # ── M4 / M4b: Dairy × time interaction ----------------------------------
    m4 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_term,
                paste0("dairy_100g_lag:", time_var)), collapse = " + "))))
    m4b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_alt,
                paste0("dairy_100g_lag:", time_var)), collapse = " + "))))

    # ── M5 / M5b: Height + Weight instead of BMI ----------------------------
    # M5 uses reference RE; M5b uses alternative RE
    has_hw <- all(c("height_scaled", "weight_scaled") %in% names(df))
    m5  <- NULL
    m5b <- NULL
    if (has_hw) {
        cov_hw <- c(setdiff(covariates, "BMI_category_lag"),
                    "height_scaled", "weight_scaled")
        m5  <- .lmer(stats::as.formula(paste(resp_col, "~",
            paste(c("dairy_100g_lag", time_var, cov_hw, re_term), collapse = " + "))))
        m5b <- .lmer(stats::as.formula(paste(resp_col, "~",
            paste(c("dairy_100g_lag", time_var, cov_hw, re_alt), collapse = " + "))))
    } else {
        .text_page(
            c("height_scaled / weight_scaled not found.",
              "M5 / M5b skipped. Expected lagged columns: height_lag / weight_lag."),
            title = "M5 / M5b skipped — gait speed"
        )
    }

    # ── M6 / M6b: Within-/between-person decomposition ----------------------
    m6 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_within_100g", "dairy_pmean_100g", time_var, covariates, re_term),
              collapse = " + "))))
    m6b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_within_100g", "dairy_pmean_100g", time_var, covariates, re_alt),
              collapse = " + "))))

    # ── M8 / M8b: nlme + varPower (reference RE / alternative RE) -----------
    fixed_nlme       <- stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g_lag", time_var, covariates), collapse = " + ")))
    random_nlme_main <- if (random_slope)
        stats::as.formula(paste0("~ 1 + ", time_var, " | ", id_var))
    else
        stats::as.formula(paste0("~ 1 | ", id_var))
    random_nlme_alt  <- if (random_slope)
        stats::as.formula(paste0("~ 1 | ", id_var))
    else
        stats::as.formula(paste0("~ 1 + ", time_var, " | ", id_var))
    m8  <- .fit_nlme_varpower(df, fixed_nlme, random_nlme_main, id_var = id_var)
    m8b <- .fit_nlme_varpower(df, fixed_nlme, random_nlme_alt,  id_var = id_var)

    # ── M9 / M9b: GAMM with smooth on lagged dairy --------------------------
    dairy_raw_col <- "dairy_total_gday_cumavg_lag"
    m9  <- NULL
    m9b <- NULL
    if (dairy_raw_col %in% names(df)) {
        rand_form_main <- if (random_slope)
            stats::as.formula(paste0("~ 1 + ", time_var))
        else
            stats::as.formula("~ 1")
        rand_form_alt  <- if (random_slope)
            stats::as.formula("~ 1")
        else
            stats::as.formula(paste0("~ 1 + ", time_var))
        gam_formula <- stats::as.formula(paste(resp_col, "~",
            paste(c(paste0("s(", dairy_raw_col, ", k = 5, bs = 'cr')"),
                    time_var, covariates), collapse = " + ")))
        .fit_gamm <- function(rf) {
            tryCatch(
                mgcv::gamm(formula   = gam_formula,
                           random    = stats::setNames(list(rf), id_var),
                           data      = df,
                           na.action = na.omit),
                error = function(e) {
                    message("GAMM failed (gait_speed): ", conditionMessage(e))
                    NULL
                }
            )
        }
        m9  <- .fit_gamm(rand_form_main)
        m9b <- .fit_gamm(rand_form_alt)
    }

    # ── M10 / M10b: Quadratic age (reference RE / alternative RE) -----------
    cov_no_age <- setdiff(covariates, "age_at_baseline_scaled_lag")
    m10 <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g_lag", time_var,
                "poly(age_at_baseline_scaled_lag, 2, raw = TRUE)",
                cov_no_age, re_term), collapse = " + "))))
    m10b <- .lmer(stats::as.formula(paste(resp_col, "~",
        paste(c("dairy_100g_lag", time_var,
                "poly(age_at_baseline_scaled_lag, 2, raw = TRUE)",
                cov_no_age, re_alt), collapse = " + "))))

    # =========================================================================
    # AIC / BIC table + plot
    # =========================================================================
    .section_page("AIC / BIC Comparison", subtitle = "Gait speed")

    ref_label <- paste("M0   Reference", lbl_main)
    ic_rows <- dplyr::bind_rows(
        .extract_ic_sens(m0,      paste("M0   Reference",   lbl_main)),
        .extract_ic_sens(m0b,     paste("M0b  Reference",   lbl_alt)),
        .extract_ic_sens(m1,      paste("M1   NS spline",   lbl_main)),
        .extract_ic_sens(m1b,     paste("M1b  NS spline",   lbl_alt)),
        .extract_ic_sens(m2,      paste("M2   Poly² dairy", lbl_main)),
        .extract_ic_sens(m2b,     paste("M2b  Poly² dairy", lbl_alt)),
        .extract_ic_sens(m3a,     paste("M3a  Log outcome", lbl_main)),
        .extract_ic_sens(m3a_alt, paste("M3a  Log outcome", lbl_alt)),
        .extract_ic_sens(m3b,     paste("M3b  Sqrt outcome", lbl_main)),
        .extract_ic_sens(m3b_alt, paste("M3b  Sqrt outcome", lbl_alt)),
        .extract_ic_sens(m4,      paste("M4   Dairy×time", lbl_main)),
        .extract_ic_sens(m4b,     paste("M4b  Dairy×time", lbl_alt)),
        .extract_ic_sens(m5,      paste("M5   Height+Weight", lbl_main)),
        .extract_ic_sens(m5b,     paste("M5b  Height+Weight", lbl_alt)),
        .extract_ic_sens(m6,      paste("M6   W/B dairy",   lbl_main)),
        .extract_ic_sens(m6b,     paste("M6b  W/B dairy",   lbl_alt)),
        .extract_ic_sens(m8,      paste("M8   varPower",    lbl_main)),
        .extract_ic_sens(m8b,     paste("M8b  varPower",    lbl_alt)),
        .extract_ic_sens(m9,      paste("M9   GAMM s(dairy_lag,k=5)", lbl_main)),
        .extract_ic_sens(m9b,     paste("M9b  GAMM s(dairy_lag,k=5)", lbl_alt)),
        .extract_ic_sens(m10,     paste("M10  Poly² age",  lbl_main)),
        .extract_ic_sens(m10b,    paste("M10b Poly² age",  lbl_alt))
    )

    ref_aic <- ic_rows$AIC[ic_rows$model == ref_label][1]
    ref_bic <- ic_rows$BIC[ic_rows$model == ref_label][1]

    ic_rows <- ic_rows |>
        dplyr::mutate(
            delta_AIC = round(AIC - ref_aic, 2),
            delta_BIC = round(BIC - ref_bic, 2),
            AIC       = round(AIC, 2),
            BIC       = round(BIC, 2),
            logLik    = round(logLik, 2)
        )

    tbl_grob <- gridExtra::tableGrob(
        ic_rows |> dplyr::select(model, AIC, BIC, delta_AIC, delta_BIC,
                                  df_model, converged),
        rows  = NULL,
        theme = gridExtra::ttheme_minimal(
            base_size = 8,
            core    = list(fg_params = list(hjust = 0, x = 0.02)),
            colhead = list(fg_params = list(fontface = "bold", hjust = 0, x = 0.02))
        )
    )
    grid::grid.newpage()
    gridExtra::grid.arrange(tbl_grob)

    readr::write_csv(
        ic_rows,
        file.path(out_dir, paste0("ic_comparison_gait_speed_", ts, ".csv"))
    )

    .aic_bic_plot_sens(ic_rows,
                       title = "ΔAIC / ΔBIC relative to reference  —  Gait speed",
                       reference_model = ref_label)

    # =========================================================================
    # Dairy coefficient comparison
    # =========================================================================
    .section_page("Dairy Coefficient Comparison", subtitle = "Gait speed")

    .coef_comparison_plot_sens(
        model_list  = list(m0, m0b, m4, m4b, m5, m5b, m6, m6b, m8, m8b, m10, m10b),
        model_names = c(paste("M0   Reference",    lbl_main),
                        paste("M0b  Reference",    lbl_alt),
                        paste("M4   Dairy×time", lbl_main),
                        paste("M4b  Dairy×time", lbl_alt),
                        paste("M5   Height+Weight", lbl_main),
                        paste("M5b  Height+Weight", lbl_alt),
                        paste("M6   W/B dairy",    lbl_main),
                        paste("M6b  W/B dairy",    lbl_alt),
                        paste("M8   varPower",     lbl_main),
                        paste("M8b  varPower",     lbl_alt),
                        paste("M10  Poly² age", lbl_main),
                        paste("M10b Poly² age", lbl_alt)),
        dairy_pattern = "^dairy_100g_lag$|^dairy_within_100g$",
        title = "Dairy (lagged) estimate (95 % CI) across specifications  — Gait speed"
    )

    # =========================================================================
    # Assumption checks — PDF + PNG export
    # =========================================================================
    png_dir_assump <- file.path(out_dir, "assumption_checks", "gait_speed")

    .assumption_checks_section(
        model_list  = list(m0, m0b, m1, m1b, m2, m2b,
                           m3a, m3a_alt, m3b, m3b_alt,
                           m4, m4b, m5, m5b, m6, m6b,
                           m8, m8b, m9, m9b, m10, m10b),
        model_names = c(paste("M0   Reference",       lbl_main),
                        paste("M0b  Reference",       lbl_alt),
                        paste("M1   NS spline dairy", lbl_main),
                        paste("M1b  NS spline dairy", lbl_alt),
                        paste("M2   Poly² dairy", lbl_main),
                        paste("M2b  Poly² dairy", lbl_alt),
                        paste("M3a  Log outcome",     lbl_main),
                        paste("M3a  Log outcome",     lbl_alt),
                        paste("M3b  Sqrt outcome",    lbl_main),
                        paste("M3b  Sqrt outcome",    lbl_alt),
                        paste("M4   Dairy×time", lbl_main),
                        paste("M4b  Dairy×time", lbl_alt),
                        paste("M5   Height+Weight",   lbl_main),
                        paste("M5b  Height+Weight",   lbl_alt),
                        paste("M6   W/B dairy",       lbl_main),
                        paste("M6b  W/B dairy",       lbl_alt),
                        paste("M8   varPower",        lbl_main),
                        paste("M8b  varPower",        lbl_alt),
                        paste("M9   GAMM",            lbl_main),
                        paste("M9b  GAMM",            lbl_alt),
                        paste("M10  Poly² age",  lbl_main),
                        paste("M10b Poly² age",  lbl_alt)),
        section_subtitle = "Gait speed",
        png_dir = png_dir_assump
    )

    # =========================================================================
    # GAMM smooth + summaries
    # =========================================================================
    .section_page("GAMM Smooth",
                  subtitle = "s(dairy_total_gday_cumavg_lag, k=5)  |  Gait speed")

    for (.gm in list(list(m9, lbl_main), list(m9b, lbl_alt))) {
        .gamm_smooth_page(.gm[[1]], dairy_raw_col,
                          paste("Gait speed GAMM", .gm[[2]]))
        if (!is.null(.gm[[1]])) {
            .text_page(utils::capture.output(summary(.gm[[1]]$gam)),
                       title = paste("GAMM summary — Gait speed", .gm[[2]]))
            .text_page(utils::capture.output(summary(.gm[[1]]$lme)),
                       title = paste("GAMM (lme) summary — Gait speed", .gm[[2]]))
        }
    }

    # =========================================================================
    # Within-/between detail
    # =========================================================================
    .section_page("Within- / Between-Person Decomposition",
                  subtitle = "Mundlak decomposition  |  Gait speed")
    .wb_detail_page(m6, "gait_speed")

    # =========================================================================
    # varPower model summaries (M8 main + alt)
    # =========================================================================
    for (.vm in list(list(m8, lbl_main), list(m8b, lbl_alt))) {
        if (!is.null(.vm[[1]])) {
            .section_page("nlme + varPower Summary",
                          subtitle = paste("Gait speed", .vm[[2]]))
            .text_page(utils::capture.output(summary(.vm[[1]])),
                       title = paste("nlme + varPower — Gait speed", .vm[[2]]))
        }
    }

    .text_page(utils::capture.output(sessioninfo::session_info()),
               title = "Session Info")

    message("PDF written to: ", pdf_path)
    message("Assumption-check PNGs saved to: ", png_dir_assump)
    invisible(pdf_path)
}
