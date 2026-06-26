# =============================================================================
# Alternative Model Specifications — HGS, ALMI, Gait Speed
#
# Fits the following alternatives to the main LMM (M0 reference):
#   M1   Nonlinear dairy: natural spline ns(dairy, df=3)
#   M2   Nonlinear dairy: polynomial poly(dairy, 2)
#   M3a  Log-transformed outcome
#   M3b  Sqrt-transformed outcome
#   M4   Dairy × time interaction term
#   M5   Height + Weight instead of BMI categories
#   M6   Within-/between-person (Mundlak) decomposition of dairy
#   M7   Random intercept only  (HGS/ALMI: drop random slope)
#          / With random slope  (gait: reference has none)
#   M8   Heteroscedastic residuals: nlme::lme + varPower()
#   M9   GAMM: mgcv::gamm with s(dairy, k=5)
#   M10  Quadratic age: poly(age_at_baseline_scaled, 2)
#
# AIC / BIC are evaluated on imputation 1 (IC is not poolable across
# imputations in a straightforward way).  A note to this effect is printed
# on the cover page.
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
                                        section_subtitle = "") {
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
            "  mgcv::gam.check() is appended as an additional GAMM-specific check."
        ),
        title = "Assumption Checks — Notes"
    )

    for (i in seq_along(model_list)) {
        mod <- model_list[[i]]
        nm  <- model_names[i]
        if (is.null(mod)) next

        # GAMM: use lme component for conditional residuals (as requested)
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
        if (!is.null(p)) print(p)

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
                                  title = paste("A  Normalized Res. vs Fitted",
                                                "[varPower check]")) +
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
                                  title = "B  Scale-Location (normalized)") +
                    .theme_report()

                p_qq <- ggplot2::ggplot(
                    df_norm, ggplot2::aes(sample = residuals)
                ) +
                    ggplot2::stat_qq(colour = .pal[8], alpha = 0.4, size = 1.2) +
                    ggplot2::stat_qq_line(colour = .pal[1], linewidth = 0.7) +
                    ggplot2::labs(x = "Theoretical quantiles",
                                  y = "Sample quantiles",
                                  title = "C  Q-Q (normalized residuals)") +
                    .theme_report()

                print(patchwork::wrap_plots(p_rd, p_sl, p_qq, ncol = 3))
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
    ctrl <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

    # Helper: safe lmer
    .lmer <- function(f) {
        tryCatch(lmerTest::lmer(f, data = df, REML = FALSE, control = ctrl),
                 error = function(e) { warning(conditionMessage(e)); NULL })
    }

    # ── M0: Reference --------------------------------------------------------
    m0 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("dairy_100g", time_var, covariates, re_term), collapse = " + ")
    )))

    # ── M1: Natural spline on dairy (df = 3) --------------------------------
    m1 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("splines::ns(dairy_100g, df = 3)", time_var, covariates, re_term),
              collapse = " + ")
    )))

    # ── M2: Quadratic polynomial on dairy -----------------------------------
    m2 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("poly(dairy_100g, 2, raw = TRUE)", time_var, covariates, re_term),
              collapse = " + ")
    )))

    # ── M3a: Log outcome  /  M3b: Sqrt outcome ------------------------------
    df[[paste0(outcome, "_log")]]  <- log(pmax(df[[outcome]], 1e-6))
    df[[paste0(outcome, "_sqrt")]] <- sqrt(pmax(df[[outcome]], 0))

    m3a <- .lmer(stats::as.formula(paste(
        paste0(outcome, "_log"), "~",
        paste(c("dairy_100g", time_var, covariates, re_term), collapse = " + ")
    )))
    m3b <- .lmer(stats::as.formula(paste(
        paste0(outcome, "_sqrt"), "~",
        paste(c("dairy_100g", time_var, covariates, re_term), collapse = " + ")
    )))

    # ── M4: Dairy × time interaction ----------------------------------------
    m4 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("dairy_100g", time_var, covariates, re_term,
                paste0("dairy_100g:", time_var)),
              collapse = " + ")
    )))

    # ── M5 / M5b: Height + Weight instead of BMI categories -----------------
    has_hw <- all(c("height_scaled", "weight_scaled") %in% names(df))
    m5  <- NULL
    m5b <- NULL
    if (has_hw) {
        cov_hw <- c(setdiff(covariates, "BMI_category"),
                     "weight_scaled")

        # M5: same random effects structure as reference
        m5 <- .lmer(stats::as.formula(paste(
            resp_col, "~",
            paste(c("dairy_100g", time_var, cov_hw, re_term), collapse = " + ")
        )))

        # M5b: random intercept only (regardless of reference random effects)
        m5b <- .lmer(stats::as.formula(paste(
            resp_col, "~",
            paste(c("dairy_100g", time_var, cov_hw, re_noSlope), collapse = " + ")
        )))
    } else {
        .text_page(
            c("height_scaled and/or weight_scaled not found in data.",
              "M5 / M5b (height + weight reparameterisation) skipped.",
              "",
              "Expected column names: 'height' or 'Height' (and 'weight' / 'Weight')."),
            title = paste("M5 / M5b skipped —", outcome)
        )
    }

    # ── M6: Within-/between-person decomposition ----------------------------
    m6 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("dairy_within_100g", "dairy_pmean_100g",
                time_var, covariates, re_term),
              collapse = " + ")
    )))

    # ── M7: No random slope (only relevant when reference has random slope) --
    m7 <- NULL
    if (random_slope) {
        m7 <- .lmer(stats::as.formula(paste(
            resp_col, "~",
            paste(c("dairy_100g", time_var, covariates, re_noSlope), collapse = " + ")
        )))
    } else {
        .text_page(
            c("Reference model already uses random intercept only.",
              "M7 (drop random slope) is identical to M0 and is skipped."),
            title = paste("M7 skipped —", outcome)
        )
    }

    # ── M8: nlme + varPower --------------------------------------------------
    fixed_nlme  <- stats::as.formula(paste(
        resp_col, "~",
        paste(c("dairy_100g", time_var, covariates), collapse = " + ")
    ))
    random_nlme <- if (random_slope) {
        stats::as.formula(paste0("~ 1 + ", time_var, " | ", id_var))
    } else {
        stats::as.formula(paste0("~ 1 | ", id_var))
    }
    m8 <- .fit_nlme_varpower(df, fixed_nlme, random_nlme, id_var = id_var)

    # ── M9: GAMM with smooth on raw dairy intake ----------------------------
    dairy_raw_col <- "dairy_total_gday_cumavg"
    m9 <- NULL
    if (dairy_raw_col %in% names(df)) {
        rand_form <- if (random_slope)
            stats::as.formula(paste0("~ 1 + ", time_var))
        else
            stats::as.formula("~ 1")

        # na.action = na.omit: gamm defaults to na.fail; lmer silently drops NAs.
        gam_formula <- stats::as.formula(paste(
            resp_col, "~",
            paste(c(paste0("s(", dairy_raw_col, ", k = 5, bs = 'cr')"),
                    time_var, covariates),
                  collapse = " + ")
        ))
        m9 <- tryCatch(
            mgcv::gamm(
                formula   = gam_formula,
                random    = stats::setNames(list(rand_form), id_var),
                data      = df,
                na.action = na.omit
            ),
            error = function(e) {
                message("GAMM failed (", outcome, "): ", conditionMessage(e))
                NULL
            }
        )
    }

    # ── M10: Quadratic age --------------------------------------------------
    cov_no_age <- setdiff(covariates, "age_at_baseline_scaled")
    m10 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("dairy_100g", time_var,
                "poly(age_at_baseline_scaled, 2, raw = TRUE)",
                cov_no_age, re_term),
              collapse = " + ")
    )))

    # =========================================================================
    # AIC / BIC table + plot
    # =========================================================================
    .section_page("AIC / BIC Comparison", subtitle = outcome)

    ic_rows <- dplyr::bind_rows(
        .extract_ic_sens(m0,  "M0  Reference"),
        .extract_ic_sens(m1,  "M1  NS spline on dairy (df=3)"),
        .extract_ic_sens(m2,  "M2  Poly² on dairy"),
        .extract_ic_sens(m3a, "M3a Log outcome"),
        .extract_ic_sens(m3b, "M3b Sqrt outcome"),
        .extract_ic_sens(m4,  "M4  Dairy × time interaction"),
        .extract_ic_sens(m5,  "M5  Height + Weight"),
        .extract_ic_sens(m5b, "M5b Height + Weight, RI only"),
        .extract_ic_sens(m6,  "M6  Within-/between-person dairy"),
        .extract_ic_sens(m7,  "M7  Random intercept only"),
        .extract_ic_sens(m8,  "M8  varPower (nlme)"),
        .extract_ic_sens(m9,  "M9  GAMM dairy + s(dairy, k=5)"),
        .extract_ic_sens(m10, "M10 Poly² on age")
    )

    ref_aic <- ic_rows$AIC[ic_rows$model == "M0  Reference"][1]
    ref_bic <- ic_rows$BIC[ic_rows$model == "M0  Reference"][1]

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
                                      outcome))

    # =========================================================================
    # Dairy coefficient comparison (linear-dairy models only)
    # =========================================================================
    .section_page("Dairy Coefficient Comparison", subtitle = outcome)

    .coef_comparison_plot_sens(
        model_list   = list(m0, m4, m5, m5b, m6, m7, m8, m10),
        model_names  = c("M0  Reference",
                         "M4  Dairy × time",
                         "M5  Height + Weight",
                         "M5b Height + Weight, RI only",
                         "M6  Within-person dairy",
                         "M7  RI only",
                         "M8  varPower",
                         "M10 Poly² age"),
        dairy_pattern = "^dairy_100g$|^dairy_within_100g$",
        title = paste("Dairy estimate (95 % CI) across specifications  — ", outcome)
    )

    # =========================================================================
    # Assumption checks
    # =========================================================================
    .assumption_checks_section(
        model_list  = list(m0, m1, m2, m3a, m3b, m4, m5, m5b, m6, m7, m8, m9, m10),
        model_names = c("M0  Reference",
                        "M1  NS spline dairy",
                        "M2  Poly² dairy",
                        "M3a Log outcome",
                        "M3b Sqrt outcome",
                        "M4  Dairy × time",
                        "M5  Height + Weight",
                        "M5b Height + Weight, RI only",
                        "M6  Within-/between dairy",
                        "M7  Random intercept only",
                        "M8  varPower (nlme)",
                        "M9  GAMM",
                        "M10 Poly² age"),
        section_subtitle = outcome
    )

    # =========================================================================
    # GAMM smooth
    # =========================================================================
    .section_page("GAMM Smooth",
                  subtitle = paste0("s(dairy_total_gday_cumavg, k=5)  |  ", outcome))
    .gamm_smooth_page(m9, dairy_raw_col, outcome)

    if (!is.null(m9)) {
        .text_page(utils::capture.output(summary(m9$gam)),
                   title = paste("GAMM summary  — ", outcome))
        .text_page(utils::capture.output(summary(m9$lme)),
                   title = paste("GAMM (lme component) summary  — ", outcome))
    }

    # =========================================================================
    # Within-/between detail
    # =========================================================================
    .section_page("Within- / Between-Person Decomposition",
                  subtitle = paste("Mundlak decomposition of dairy intake  |  ", outcome))
    .wb_detail_page(m6, outcome)

    # =========================================================================
    # varPower model summary
    # =========================================================================
    if (!is.null(m8)) {
        .section_page("nlme + varPower Summary", subtitle = outcome)
        .text_page(utils::capture.output(summary(m8)),
                   title = paste("nlme + varPower  — ", outcome))
    }

    # =========================================================================
    # Session info
    # =========================================================================
    .text_page(utils::capture.output(sessioninfo::session_info()),
               title = "Session Info")

    message("PDF written to: ", pdf_path)
    invisible(pdf_path)
}


# ---------------------------------------------------------------------------
# 9.  GAIT SPEED VERSION
# ---------------------------------------------------------------------------

#' Alternative model specifications for gait speed, written to a PDF.
#'
#' Gait speed uses lagged covariates (`_lag` suffix), typically has no random
#' slope (only 2 visits).  M7 here tests the *addition* of a random slope.
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
    ctrl <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

    .lmer <- function(f) {
        tryCatch(lmerTest::lmer(f, data = df, REML = FALSE, control = ctrl),
                 error = function(e) { warning(conditionMessage(e)); NULL })
    }

    # ── M0: Reference --------------------------------------------------------
    m0 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_term), collapse = " + ")
    )))

    # ── M1: Natural spline on lagged dairy -----------------------------------
    m1 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("splines::ns(dairy_100g_lag, df = 3)", time_var, covariates, re_term),
              collapse = " + ")
    )))

    # ── M2: Quadratic polynomial on lagged dairy -----------------------------
    m2 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("poly(dairy_100g_lag, 2, raw = TRUE)", time_var, covariates, re_term),
              collapse = " + ")
    )))

    # ── M3a / M3b: Outcome transformations ----------------------------------
    df[[paste0(outcome, "_log")]]  <- log(pmax(df[[outcome]], 1e-6))
    df[[paste0(outcome, "_sqrt")]] <- sqrt(pmax(df[[outcome]], 0))

    m3a <- .lmer(stats::as.formula(paste(
        paste0(outcome, "_log"), "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_term), collapse = " + ")
    )))
    m3b <- .lmer(stats::as.formula(paste(
        paste0(outcome, "_sqrt"), "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_term), collapse = " + ")
    )))

    # ── M4: Dairy × time interaction ----------------------------------------
    m4 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("dairy_100g_lag", time_var, covariates, re_term,
                paste0("dairy_100g_lag:", time_var)),
              collapse = " + ")
    )))

    # ── M5 / M5b: Height + Weight instead of BMI categories -----------------
    has_hw <- all(c("height_scaled", "weight_scaled") %in% names(df))
    m5  <- NULL
    m5b <- NULL
    if (has_hw) {
        cov_hw <- c(setdiff(covariates, "BMI_category_lag"),
                    "height_scaled", "weight_scaled")

        # M5: same random effects as reference
        m5 <- .lmer(stats::as.formula(paste(
            resp_col, "~",
            paste(c("dairy_100g_lag", time_var, cov_hw, re_term), collapse = " + ")
        )))

        # M5b: random intercept only
        m5b <- .lmer(stats::as.formula(paste(
            resp_col, "~",
            paste(c("dairy_100g_lag", time_var, cov_hw, re_noSlope), collapse = " + ")
        )))
    } else {
        .text_page(
            c("height_scaled / weight_scaled not found.",
              "M5 / M5b skipped. Expected lagged columns: height_lag / weight_lag."),
            title = "M5 / M5b skipped — gait speed"
        )
    }

    # ── M6: Within-/between-person decomposition ----------------------------
    m6 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("dairy_within_100g", "dairy_pmean_100g",
                time_var, covariates, re_term),
              collapse = " + ")
    )))

    # ── M7: Add random slope (gait reference has none) ----------------------
    m7 <- NULL
    if (!random_slope) {
        m7 <- .lmer(stats::as.formula(paste(
            resp_col, "~",
            paste(c("dairy_100g_lag", time_var, covariates, re_slope),
                  collapse = " + ")
        )))
    } else {
        .text_page("Reference already has random slope; M7 skipped.",
                   title = "M7 skipped — gait speed")
    }

    # ── M8: nlme + varPower --------------------------------------------------
    fixed_nlme  <- stats::as.formula(paste(
        resp_col, "~",
        paste(c("dairy_100g_lag", time_var, covariates), collapse = " + ")
    ))
    random_nlme <- stats::as.formula(paste0("~ 1 | ", id_var))
    m8 <- .fit_nlme_varpower(df, fixed_nlme, random_nlme, id_var = id_var)

    # ── M9: GAMM -------------------------------------------------------------
    dairy_raw_col <- "dairy_total_gday_cumavg_lag"
    m9 <- NULL
    if (dairy_raw_col %in% names(df)) {
        # na.action = na.omit: gamm defaults to na.fail; lmer silently drops NAs.
        gam_formula <- stats::as.formula(paste(
            resp_col, "~",
            paste(c(paste0("s(", dairy_raw_col, ", k = 5, bs = 'cr')"),
                    time_var, covariates),
                  collapse = " + ")
        ))
        rand_form <- stats::as.formula("~ 1")
        m9 <- tryCatch(
            mgcv::gamm(
                formula   = gam_formula,
                random    = stats::setNames(list(rand_form), id_var),
                data      = df,
                na.action = na.omit
            ),
            error = function(e) {
                message("GAMM failed (gait_speed): ", conditionMessage(e))
                NULL
            }
        )
    }

    # ── M10: Quadratic age --------------------------------------------------
    cov_no_age <- setdiff(covariates, "age_at_baseline_scaled_lag")
    m10 <- .lmer(stats::as.formula(paste(
        resp_col, "~",
        paste(c("dairy_100g_lag", time_var,
                "poly(age_at_baseline_scaled_lag, 2, raw = TRUE)",
                cov_no_age, re_term),
              collapse = " + ")
    )))

    # =========================================================================
    # AIC / BIC table + plot
    # =========================================================================
    .section_page("AIC / BIC Comparison", subtitle = "Gait speed")

    ic_rows <- dplyr::bind_rows(
        .extract_ic_sens(m0,  "M0  Reference"),
        .extract_ic_sens(m1,  "M1  NS spline on dairy (df=3)"),
        .extract_ic_sens(m2,  "M2  Poly² on dairy"),
        .extract_ic_sens(m3a, "M3a Log outcome"),
        .extract_ic_sens(m3b, "M3b Sqrt outcome"),
        .extract_ic_sens(m4,  "M4  Dairy × time interaction"),
        .extract_ic_sens(m5,  "M5  Height + Weight"),
        .extract_ic_sens(m5b, "M5b Height + Weight, RI only"),
        .extract_ic_sens(m6,  "M6  Within-/between-person dairy"),
        .extract_ic_sens(m7,  "M7  With random slope"),
        .extract_ic_sens(m8,  "M8  varPower (nlme)"),
        .extract_ic_sens(m9,  "M9  GAMM dairy_lag + s(dairy_lag, k=5)"),
        .extract_ic_sens(m10, "M10 Poly² on age")
    )

    ref_aic <- ic_rows$AIC[ic_rows$model == "M0  Reference"][1]
    ref_bic <- ic_rows$BIC[ic_rows$model == "M0  Reference"][1]

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
                       title = "ΔAIC / ΔBIC relative to reference  —  Gait speed")

    # =========================================================================
    # Dairy coefficient comparison
    # =========================================================================
    .section_page("Dairy Coefficient Comparison", subtitle = "Gait speed")

    .coef_comparison_plot_sens(
        model_list   = list(m0, m4, m5, m5b, m6, m7, m8, m10),
        model_names  = c("M0  Reference",
                         "M4  Dairy × time",
                         "M5  Height + Weight",
                         "M5b Height + Weight, RI only",
                         "M6  Within-person dairy",
                         "M7  With random slope",
                         "M8  varPower",
                         "M10 Poly² age"),
        dairy_pattern = "^dairy_100g_lag$|^dairy_within_100g$",
        title = "Dairy (lagged) estimate (95 % CI) across specifications  — Gait speed"
    )

    # =========================================================================
    # Assumption checks
    # =========================================================================
    .assumption_checks_section(
        model_list  = list(m0, m1, m2, m3a, m3b, m4, m5, m5b, m6, m7, m8, m9, m10),
        model_names = c("M0  Reference",
                        "M1  NS spline dairy",
                        "M2  Poly² dairy",
                        "M3a Log outcome",
                        "M3b Sqrt outcome",
                        "M4  Dairy × time",
                        "M5  Height + Weight",
                        "M5b Height + Weight, RI only",
                        "M6  Within-/between dairy",
                        "M7  With random slope",
                        "M8  varPower (nlme)",
                        "M9  GAMM",
                        "M10 Poly² age"),
        section_subtitle = "Gait speed"
    )

    # =========================================================================
    # GAMM smooth + summaries
    # =========================================================================
    .section_page("GAMM Smooth",
                  subtitle = "s(dairy_total_gday_cumavg_lag, k=5)  |  Gait speed")
    .gamm_smooth_page(m9, dairy_raw_col, "gait_speed")

    if (!is.null(m9)) {
        .text_page(utils::capture.output(summary(m9$gam)),
                   title = "GAMM summary  —  Gait speed")
        .text_page(utils::capture.output(summary(m9$lme)),
                   title = "GAMM (lme component) summary  —  Gait speed")
    }

    # =========================================================================
    # Within-/between detail
    # =========================================================================
    .section_page("Within- / Between-Person Decomposition",
                  subtitle = "Mundlak decomposition  |  Gait speed")
    .wb_detail_page(m6, "gait_speed")

    # varPower summary
    if (!is.null(m8)) {
        .section_page("nlme + varPower Summary", subtitle = "Gait speed")
        .text_page(utils::capture.output(summary(m8)),
                   title = "nlme + varPower  —  Gait speed")
    }

    .text_page(utils::capture.output(sessioninfo::session_info()),
               title = "Session Info")

    message("PDF written to: ", pdf_path)
    invisible(pdf_path)
}
