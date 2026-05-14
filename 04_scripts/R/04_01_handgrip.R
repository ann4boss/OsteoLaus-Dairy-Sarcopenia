# Exploratory visualization and modular dairy/HGS modelling for the targets pipeline.
#
# Each function writes outputs to a timestamped folder labelled with route

safe_slug <- function(x) {
    x <- paste(x, collapse = "-")
    x <- gsub("[^A-Za-z0-9._-]+", "-", x)
    x <- gsub("^-+|-+$", "", x)
    ifelse(nchar(x) == 0, "none", x)
}

bt <- function(x) paste0("`", gsub("`", "", x), "`")

write_csv_file <- function(data, path) {
    utils::write.csv(data, path, row.names = FALSE, na = "")
    path
}

make_run_dir <- function(output_root, analysis_name, input_label, outcome, exposure,
                         covariates = NULL, imputed = FALSE) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    pieces <- c(
        timestamp,
        paste0("Hangrip_"),
        if (isTRUE(imputed)) "MICE" else "CC"
    )
    run_dir <- file.path(output_root, paste(pieces, collapse = "__"))
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    run_dir
}

get_analysis_frame <- function(analysis_object, imputed = FALSE,
                               data_element = NULL,
                               imp_col = ".imp",
                               exclude_imp0 = TRUE,
                               filter_exclusions = TRUE) {
    if (is.data.frame(analysis_object)) {
        data <- analysis_object
    } else if (!is.null(data_element) && data_element %in% names(analysis_object)) {
        data <- analysis_object[[data_element]]
    } else if (isTRUE(imputed) && "annotated_long" %in% names(analysis_object)) {
        data <- analysis_object$annotated_long
    } else if ("annotated" %in% names(analysis_object)) {
        data <- analysis_object$annotated
    } else {
        stop(
            "Could not find analysis data. Pass a data frame directly or an object with ",
            "$annotated / $annotated_long.",
            call. = FALSE
        )
    }
    
    data <- tibble::as_tibble(data)
    
    if (isTRUE(imputed) && imp_col %in% names(data) && isTRUE(exclude_imp0)) {
        data <- dplyr::filter(data, .data[[imp_col]] != 0)
    }
    
    if (isTRUE(filter_exclusions)) {
        excl_cols <- grep("^excl_", names(data), value = TRUE)
        if (length(excl_cols) > 0) {
            excluded <- vapply(excl_cols, function(col) {
                x <- data[[col]]
                if (is.logical(x)) return(x %in% TRUE)
                if (is.numeric(x) || is.integer(x)) return(!is.na(x) & x != 0)
                tolower(as.character(x)) %in% c("true", "t", "yes", "y", "1")
            }, logical(nrow(data)))
            if (is.null(dim(excluded))) {
                keep <- !excluded
            } else {
                keep <- rowSums(excluded, na.rm = TRUE) == 0
            }
            data <- data[keep, , drop = FALSE]
        }
    }
    
    data
}

validate_analysis_columns <- function(data, columns) {
    missing_columns <- setdiff(columns, names(data))
    if (length(missing_columns) > 0) {
        stop(
            "Analysis data is missing required column(s): ",
            paste(missing_columns, collapse = ", "),
            call. = FALSE
        )
    }
}

prepare_model_data <- function(data, outcome, exposure_continuous, exposure_quartile,
                               id_var, time_var, covariates) {
    required <- unique(c(outcome, exposure_continuous, exposure_quartile, id_var, time_var, covariates))
    validate_analysis_columns(data, required)
    
    data <- data |>
        dplyr::mutate(
            "..id_model" = factor(.data[[id_var]]),
            "..time_model" = if (is.numeric(.data[[time_var]])) {
                as.numeric(.data[[time_var]])
            } else {
                as.numeric(factor(.data[[time_var]], levels = unique(.data[[time_var]]), ordered = TRUE))
            }
        )
    
    data[[exposure_quartile]] <- factor(data[[exposure_quartile]])
    
    for (var in covariates) {
        if (is.character(data[[var]]) || is.logical(data[[var]])) {
            data[[var]] <- factor(data[[var]])
        }
    }
    
    complete_vars <- unique(c(outcome, exposure_continuous, exposure_quartile, "..id_model", "..time_model", covariates))
    data <- data[stats::complete.cases(data[, complete_vars, drop = FALSE]), , drop = FALSE]
    data
}

split_imputations <- function(data, imputed = FALSE, imp_col = ".imp") {
    if (!isTRUE(imputed)) {
        return(list(cc = data))
    }
    validate_analysis_columns(data, imp_col)
    imp_values <- sort(unique(data[[imp_col]]))
    out <- lapply(imp_values, function(i) data[data[[imp_col]] == i, , drop = FALSE])
    names(out) <- paste0("imp_", imp_values)
    out
}

pool_rubin <- function(estimates) {
    estimates |>
        dplyr::filter(!is.na(.data$estimate), !is.na(.data$std_error)) |>
        dplyr::group_by(.data$model, .data$term) |>
        dplyr::summarise(
            m = dplyr::n(),
            qbar = mean(.data$estimate),
            within_var = mean(.data$std_error^2),
            between_var = ifelse(m > 1, stats::var(.data$estimate), 0),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            estimate = .data$qbar,
            total_var = .data$within_var + (1 + 1 / .data$m) * .data$between_var,
            std_error = sqrt(.data$total_var),
            lambda = dplyr::if_else(.data$total_var > 0, ((1 + 1 / .data$m) * .data$between_var) / .data$total_var, 0),
            df = dplyr::if_else(.data$m > 1 & .data$lambda > 0, (.data$m - 1) / .data$lambda^2, Inf),
            statistic = .data$estimate / .data$std_error,
            p_value = 2 * stats::pt(abs(.data$statistic), df = .data$df, lower.tail = FALSE),
            conf_low = .data$estimate + stats::qt(0.025, df = .data$df) * .data$std_error,
            conf_high = .data$estimate + stats::qt(0.975, df = .data$df) * .data$std_error
        ) |>
        dplyr::select(-dplyr::all_of("qbar"))
}

extract_lmer_fixed <- function(fit, model_name, imputation) {
    tab <- as.data.frame(coef(summary(fit)))
    se_col <- if ("Std. Error" %in% names(tab)) "Std. Error" else grep("Std", names(tab), value = TRUE)[1]
    tibble::tibble(
        model = model_name,
        imputation = imputation,
        term = rownames(tab),
        estimate = tab[["Estimate"]],
        std_error = tab[[se_col]]
    )
}

extract_gam_parametric <- function(fit, model_name, imputation) {
    tab <- as.data.frame(summary(fit)$p.table)
    tibble::tibble(
        model = model_name,
        imputation = imputation,
        term = rownames(tab),
        estimate = tab[["Estimate"]],
        std_error = tab[["Std. Error"]]
    )
}

extract_gam_smooth <- function(fit, model_name, imputation) {
    tab <- as.data.frame(summary(fit)$s.table)
    if (nrow(tab) == 0) return(tibble::tibble())
    statistic_col <- grep("F|Chi.sq", names(tab), value = TRUE)[1]
    tibble::tibble(
        model = model_name,
        imputation = imputation,
        smooth = rownames(tab),
        edf = tab[["edf"]],
        ref_df = tab[["Ref.df"]],
        statistic = tab[[statistic_col]],
        p_value = tab[["p-value"]]
    )
}

make_lmer_formula <- function(outcome, exposure_term, covariates, interaction = FALSE) {
    exposure_part <- if (isTRUE(interaction)) {
        paste0(exposure_term, " * `..time_model`")
    } else {
        paste(exposure_term, "`..time_model`", sep = " + ")
    }
    stats::as.formula(
        paste(
            bt(outcome),
            "~",
            paste(c(exposure_part, vapply(covariates, bt, character(1)), "(1 | `..id_model`)"), collapse = " + ")
        )
    )
}

make_gam_formula <- function(outcome, exposure_continuous, covariates, interaction = FALSE, k = 5) {
    smooth_terms <- if (isTRUE(interaction)) {
        c(
            paste0("s(", bt(exposure_continuous), ", k = ", k, ")"),
            "`..time_model`",
            paste0("ti(", bt(exposure_continuous), ", `..time_model`, k = c(", k, ", ", min(k, 5), "))")
        )
    } else {
        c(paste0("s(", bt(exposure_continuous), ", k = ", k, ")"), "`..time_model`")
    }
    stats::as.formula(
        paste(
            bt(outcome),
            "~",
            paste(c(smooth_terms, vapply(covariates, bt, character(1)), "s(`..id_model`, bs = 're')"), collapse = " + ")
        )
    )
}

fit_one_imputation <- function(data, imputation, outcome, exposure_continuous,
                               exposure_quartile, covariates, rcs_df, gam_k) {
    quartile_formula <- make_lmer_formula(outcome, bt(exposure_quartile), covariates)
    linear_formula <- make_lmer_formula(outcome, bt(exposure_continuous), covariates)
    rcs_formula <- make_lmer_formula(
        outcome,
        paste0("splines::ns(", bt(exposure_continuous), ", df = ", rcs_df, ")"),
        covariates
    )
    gam_formula <- make_gam_formula(outcome, exposure_continuous, covariates, k = gam_k)
    
    list(
        imputation = imputation,
        n = nrow(data),
        data = data,
        formulas = list(
            quartile_lmm = quartile_formula,
            linear_lmm = linear_formula,
            rcs_lmm = rcs_formula,
            gamm = gam_formula
        ),
        fits = list(
            quartile_lmm = lme4::lmer(quartile_formula, data = data, REML = FALSE),
            linear_lmm = lme4::lmer(linear_formula, data = data, REML = FALSE),
            rcs_lmm = lme4::lmer(rcs_formula, data = data, REML = FALSE),
            gamm = mgcv::gam(gam_formula, data = data, method = "REML")
        )
    )
}

fit_indices_tbl <- function(fit_results) {
    purrr::map_dfr(fit_results, function(result) {
        purrr::imap_dfr(result$fits, function(fit, model_name) {
            tibble::tibble(
                imputation = result$imputation,
                model = model_name,
                n = result$n,
                aic = AIC(fit),
                bic = BIC(fit),
                logLik = as.numeric(logLik(fit))
            )
        })
    })
}

lrt_tbl <- function(fit_results, reduced, full, comparison) {
    purrr::map_dfr(fit_results, function(result) {
        tab <- stats::anova(result$fits[[reduced]], result$fits[[full]])
        tibble::tibble(
            comparison = comparison,
            imputation = result$imputation,
            reduced_model = reduced,
            full_model = full,
            chisq = tab$Chisq[2],
            df_diff = tab$`Chi Df`[2],
            p_value = tab$`Pr(>Chisq)`[2]
        )
    })
}

try_mice_d2_tbl <- function(fit_results, reduced, full, comparison) {
    out <- tryCatch(
        {
            d2 <- mice::D2(
                mice::as.mira(lapply(fit_results, function(x) x$fits[[full]])),
                mice::as.mira(lapply(fit_results, function(x) x$fits[[reduced]]))
            )
            tibble::as_tibble(d2) |>
                dplyr::mutate(comparison = comparison, method = "mice::D2") |>
                dplyr::relocate(dplyr::all_of(c("comparison", "method")))
        },
        error = function(e) {
            tibble::tibble(
                comparison = comparison,
                method = paste("mice::D2 failed:", conditionMessage(e)),
                statistic = NA_real_,
                p_value = NA_real_
            )
        }
    )
    out
}

reference_grid <- function(data, exposure_continuous, covariates, exposure_grid) {
    mode_value <- function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0) return(NA)
        names(sort(table(x), decreasing = TRUE))[1]
    }
    
    grid <- tibble::tibble(!!exposure_continuous := exposure_grid)
    grid[["..time_model"]] <- stats::median(data[["..time_model"]], na.rm = TRUE)
    
    for (var in covariates) {
        if (is.numeric(data[[var]])) {
            grid[[var]] <- stats::median(data[[var]], na.rm = TRUE)
        } else {
            value <- mode_value(data[[var]])
            grid[[var]] <- if (is.factor(data[[var]])) factor(value, levels = levels(data[[var]])) else value
        }
    }
    grid[["..id_model"]] <- factor(levels(data[["..id_model"]])[1], levels = levels(data[["..id_model"]]))
    grid
}

pooled_gamm_predictions <- function(fit_results, exposure_continuous, covariates) {
    all_data <- dplyr::bind_rows(lapply(fit_results, `[[`, "data"))
    exposure_grid <- seq(
        stats::quantile(all_data[[exposure_continuous]], 0.02, na.rm = TRUE),
        stats::quantile(all_data[[exposure_continuous]], 0.98, na.rm = TRUE),
        length.out = 100
    )
    
    pred_rows <- purrr::map_dfr(fit_results, function(result) {
        grid <- reference_grid(result$data, exposure_continuous, covariates, exposure_grid)
        pred <- predict(
            result$fits$gamm,
            newdata = grid,
            type = "link",
            se.fit = TRUE,
            exclude = "s(..id_model)"
        )
        grid |>
            dplyr::mutate(
                imputation = result$imputation,
                fit = as.numeric(pred$fit),
                std_error = as.numeric(pred$se.fit)
            )
    })
    
    pred_rows |>
        dplyr::group_by(.data[[exposure_continuous]]) |>
        dplyr::summarise(
            m = dplyr::n(),
            fit_mean = mean(.data$fit),
            within_var = mean(.data$std_error^2),
            between_var = ifelse(m > 1, stats::var(.data$fit), 0),
            total_var = within_var + (1 + 1 / m) * between_var,
            std_error = sqrt(total_var),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            fit = .data$fit_mean,
            conf_low = .data$fit - 1.96 * .data$std_error,
            conf_high = .data$fit + 1.96 * .data$std_error
        ) |>
        dplyr::select(-dplyr::all_of("fit_mean"))
}

fit_interaction <- function(result, best_model, outcome, exposure_continuous,
                            exposure_quartile, covariates, rcs_df, gam_k) {
    formula <- switch(
        best_model,
        quartile_lmm = make_lmer_formula(outcome, bt(exposure_quartile), covariates, interaction = TRUE),
        linear_lmm = make_lmer_formula(outcome, bt(exposure_continuous), covariates, interaction = TRUE),
        rcs_lmm = make_lmer_formula(
            outcome,
            paste0("splines::ns(", bt(exposure_continuous), ", df = ", rcs_df, ")"),
            covariates,
            interaction = TRUE
        ),
        gamm = make_gam_formula(outcome, exposure_continuous, covariates, interaction = TRUE, k = gam_k),
        stop("Unknown best_model: ", best_model, call. = FALSE)
    )
    
    fit <- if (best_model == "gamm") {
        mgcv::gam(formula, data = result$data, method = "REML")
    } else {
        lme4::lmer(formula, data = result$data, REML = FALSE)
    }
    
    list(imputation = result$imputation, formula = formula, fit = fit)
}

run_dairy_hgs_exploratory_visualization <- function(analysis_object,
                                                    analysis_name = "exploratory_visualization",
                                                    input_label = "cc_analysis",
                                                    imputed = FALSE,
                                                    data_element = NULL,
                                                    output_root = "05_outputs/dairy_hgs_models",
                                                    outcome = "HGS_MAX",
                                                    exposure_continuous = "dairy_total_gday",
                                                    imp_col = ".imp",
                                                    exclude_imp0 = TRUE,
                                                    filter_exclusions = TRUE,
                                                    loess_span = 0.75,
                                                    spline_df = 4) {
    data <- get_analysis_frame(
        analysis_object,
        imputed = imputed,
        data_element = data_element,
        imp_col = imp_col,
        exclude_imp0 = exclude_imp0,
        filter_exclusions = filter_exclusions
    )
    validate_analysis_columns(data, c(outcome, exposure_continuous))
    
    if (isTRUE(imputed) && imp_col %in% names(data)) {
        plot_imp <- sort(unique(data[[imp_col]]))[1]
        data <- dplyr::filter(data, .data[[imp_col]] == plot_imp)
        imp_label <- paste0(imp_col, "=", plot_imp)
    } else {
        imp_label <- "complete-case"
    }
    
    plot_data <- data |>
        dplyr::filter(!is.na(.data[[outcome]]), !is.na(.data[[exposure_continuous]]))
    if (nrow(plot_data) < 10) {
        stop("Fewer than 10 complete rows for exploratory visualization.", call. = FALSE)
    }
    
    run_dir <- make_run_dir(
        output_root = output_root,
        analysis_name = analysis_name,
        input_label = input_label,
        outcome = outcome,
        exposure = exposure_continuous,
        imputed = imputed
    )
    
    metadata <- c(
        paste("Analysis run:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        paste("Input target:", input_label),
        paste("Rows plotted:", nrow(plot_data)),
        paste("Outcome:", outcome),
        paste("Continuous exposure:", exposure_continuous),
        paste("Imputation shown:", imp_label),
        paste("Exclusion flags filtered:", filter_exclusions)
    )
    writeLines(metadata, file.path(run_dir, "run_metadata.txt"))
    capture.output(sessionInfo(), file = file.path(run_dir, "session_info.txt"))
    
    binned <- plot_data |>
        dplyr::mutate(exposure_bin = dplyr::ntile(.data[[exposure_continuous]], 20)) |>
        dplyr::group_by(.data$exposure_bin) |>
        dplyr::summarise(
            n = dplyr::n(),
            exposure_mean = mean(.data[[exposure_continuous]], na.rm = TRUE),
            exposure_median = stats::median(.data[[exposure_continuous]], na.rm = TRUE),
            outcome_mean = mean(.data[[outcome]], na.rm = TRUE),
            outcome_sd = stats::sd(.data[[outcome]], na.rm = TRUE),
            outcome_se = outcome_sd / sqrt(n),
            .groups = "drop"
        )
    
    binned_path <- write_csv_file(binned, file.path(run_dir, "binned_exposure_outcome_summary.csv"))
    
    loess_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[exposure_continuous]], y = .data[[outcome]])) +
        ggplot2::geom_point(alpha = 0.25, size = 1.1, color = "#2f4f4f") +
        ggplot2::geom_smooth(
            method = "loess",
            formula = y ~ x,
            span = loess_span,
            se = TRUE,
            color = "#bc4b51",
            fill = "#f4a6a6",
            linewidth = 1
        ) +
        ggplot2::labs(
            title = "Handgrip strength vs. dairy consumption",
            subtitle = paste0("LOESS smooth with 95% CI; ", imp_label, "; n = ", nrow(plot_data)),
            x = exposure_continuous,
            y = outcome
        ) +
        ggplot2::theme_minimal(base_size = 12)
    
    spline_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[exposure_continuous]], y = .data[[outcome]])) +
        ggplot2::geom_point(alpha = 0.20, size = 1.1, color = "#354f52") +
        ggplot2::geom_smooth(
            method = "lm",
            formula = y ~ splines::ns(x, df = spline_df),
            se = TRUE,
            color = "#2a9d8f",
            fill = "#b7e4d8",
            linewidth = 1
        ) +
        ggplot2::labs(
            title = "Handgrip strength vs. dairy consumption",
            subtitle = paste0("Natural cubic spline smooth, df = ", spline_df),
            x = exposure_continuous,
            y = outcome
        ) +
        ggplot2::theme_minimal(base_size = 12)
    
    binned_plot <- ggplot2::ggplot(binned, ggplot2::aes(x = .data$exposure_mean, y = .data$outcome_mean)) +
        ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data$outcome_mean - 1.96 * .data$outcome_se,
                         ymax = .data$outcome_mean + 1.96 * .data$outcome_se),
            width = 0,
            color = "#64748b",
            alpha = 0.8
        ) +
        ggplot2::geom_line(color = "#264653", linewidth = 0.8) +
        ggplot2::geom_point(ggplot2::aes(size = .data$n), color = "#e76f51", alpha = 0.85) +
        ggplot2::scale_size_continuous(name = "Bin n", range = c(2, 5)) +
        ggplot2::labs(
            title = "Binned mean handgrip strength by dairy consumption",
            subtitle = "Twenty quantile bins with approximate 95% CI",
            x = paste("Mean", exposure_continuous, "within bin"),
            y = paste("Mean", outcome, "within bin")
        ) +
        ggplot2::theme_minimal(base_size = 12)
    
    paths <- c(
        binned_summary = binned_path,
        loess_png = file.path(run_dir, "01_loess_hgs_vs_dairy.png"),
        loess_pdf = file.path(run_dir, "01_loess_hgs_vs_dairy.pdf"),
        spline_png = file.path(run_dir, "02_spline_hgs_vs_dairy.png"),
        binned_png = file.path(run_dir, "03_binned_hgs_vs_dairy.png")
    )
    
    ggplot2::ggsave(paths[["loess_png"]], loess_plot, width = 8, height = 6, dpi = 300)
    ggplot2::ggsave(paths[["loess_pdf"]], loess_plot, width = 8, height = 6)
    ggplot2::ggsave(paths[["spline_png"]], spline_plot, width = 8, height = 6, dpi = 300)
    ggplot2::ggsave(paths[["binned_png"]], binned_plot, width = 8, height = 6, dpi = 300)
    
    tibble::tibble(
        output_dir = run_dir,
        output_name = names(paths),
        path = unname(paths)
    )
}

run_dairy_hgs_modular_models <- function(analysis_object,
                                         analysis_name = "modular_models",
                                         input_label = "cc_analysis",
                                         imputed = FALSE,
                                         data_element = NULL,
                                         output_root = "05_outputs/dairy_hgs_models",
                                         outcome = "HGS_MAX",
                                         exposure_continuous = "dairy_total_gday",
                                         exposure_quartile = "dairy_quartile_baseline",
                                         id_var = "pt",
                                         time_var = ".visit_osteo",
                                         covariates = c(
                                             "Age", "Height", "Weight", "BMI", "BMI_category",
                                             "mrtsts2", "education_level",
                                             "smoking_status", "alcohol_category_conso",
                                             "pa_levels_tertile_f1",
                                             "diabetes_status", "hrt_status", "htn_status",
                                             "sumtot1"
                                         ),
                                         imp_col = ".imp",
                                         exclude_imp0 = TRUE,
                                         filter_exclusions = TRUE,
                                         rcs_df = 4,
                                         gam_k = 5,
                                         best_model = "auto") {
    raw_data <- get_analysis_frame(
        analysis_object,
        imputed = imputed,
        data_element = data_element,
        imp_col = imp_col,
        exclude_imp0 = exclude_imp0,
        filter_exclusions = filter_exclusions
    )
    
    required <- unique(c(outcome, exposure_continuous, exposure_quartile, id_var, time_var, covariates))
    validate_analysis_columns(raw_data, required)
    
    run_dir <- make_run_dir(
        output_root = output_root,
        analysis_name = analysis_name,
        input_label = input_label,
        outcome = outcome,
        exposure = exposure_continuous,
        covariates = covariates,
        imputed = imputed
    )
    
    writeLines(
        c(
            paste("Analysis run:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
            paste("Input target:", input_label),
            paste("Rows after exclusion filtering:", nrow(raw_data)),
            paste("Outcome:", outcome),
            paste("Continuous exposure:", exposure_continuous),
            paste("Quartile exposure:", exposure_quartile),
            paste("ID variable:", id_var),
            paste("Time variable:", time_var),
            "Internal model time variable: ..time_model",
            paste("Covariates:", paste(covariates, collapse = ", ")),
            paste("Imputed:", imputed),
            paste("Exclusion flags filtered:", filter_exclusions)
        ),
        file.path(run_dir, "run_metadata.txt")
    )
    capture.output(sessionInfo(), file = file.path(run_dir, "session_info.txt"))
    
    imp_data <- split_imputations(raw_data, imputed = imputed, imp_col = imp_col)
    imp_data <- lapply(
        imp_data,
        prepare_model_data,
        outcome = outcome,
        exposure_continuous = exposure_continuous,
        exposure_quartile = exposure_quartile,
        id_var = id_var,
        time_var = time_var,
        covariates = covariates
    )
    
    fit_results <- purrr::imap(
        imp_data,
        fit_one_imputation,
        outcome = outcome,
        exposure_continuous = exposure_continuous,
        exposure_quartile = exposure_quartile,
        covariates = covariates,
        rcs_df = rcs_df,
        gam_k = gam_k
    )
    
    formula_lines <- unlist(lapply(fit_results[[1]]$formulas, function(x) paste(deparse(x), collapse = " ")))
    writeLines(paste(names(formula_lines), formula_lines, sep = ": "), file.path(run_dir, "model_formulas_steps_1_to_3.txt"))
    
    fixed_by_imp <- purrr::map_dfr(fit_results, function(result) {
        dplyr::bind_rows(
            extract_lmer_fixed(result$fits$quartile_lmm, "step1_quartile_lmm", result$imputation),
            extract_lmer_fixed(result$fits$linear_lmm, "step2_linear_lmm", result$imputation),
            extract_lmer_fixed(result$fits$rcs_lmm, "step2_rcs_lmm", result$imputation),
            extract_gam_parametric(result$fits$gamm, "step3_gamm", result$imputation)
        )
    })
    fixed_pooled <- pool_rubin(fixed_by_imp)
    
    smooth_by_imp <- purrr::map_dfr(fit_results, function(result) {
        extract_gam_smooth(result$fits$gamm, "step3_gamm", result$imputation)
    })
    
    fit_indices <- fit_indices_tbl(fit_results)
    fit_summary <- fit_indices |>
        dplyr::group_by(.data$model) |>
        dplyr::summarise(
            mean_aic = mean(.data$aic, na.rm = TRUE),
            sd_aic = stats::sd(.data$aic, na.rm = TRUE),
            mean_bic = mean(.data$bic, na.rm = TRUE),
            mean_logLik = mean(.data$logLik, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::arrange(.data$mean_aic)
    
    rcs_lrt <- lrt_tbl(fit_results, "linear_lmm", "rcs_lmm", "linear_lmm_vs_rcs_lmm")
    rcs_d2 <- if (isTRUE(imputed)) {
        try_mice_d2_tbl(fit_results, "linear_lmm", "rcs_lmm", "linear_lmm_vs_rcs_lmm")
    } else {
        tibble::tibble(note = "D2 pooling not used for complete-case analysis.")
    }
    
    gamm_pred <- pooled_gamm_predictions(fit_results, exposure_continuous, covariates)
    gamm_plot <- ggplot2::ggplot(gamm_pred, ggplot2::aes(x = .data[[exposure_continuous]], y = .data$fit)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$conf_low, ymax = .data$conf_high), fill = "#9ecae1", alpha = 0.45) +
        ggplot2::geom_line(color = "#1d4e89", linewidth = 1) +
        ggplot2::labs(
            title = "GAMM smooth for dairy intake",
            subtitle = paste0("Pooled predictions with 95% CI; outcome = ", outcome),
            x = exposure_continuous,
            y = paste("Predicted", outcome)
        ) +
        ggplot2::theme_minimal(base_size = 12)
    
    selected_model <- if (best_model == "auto") fit_summary$model[1] else best_model
    interaction_results <- purrr::map(
        fit_results,
        fit_interaction,
        best_model = selected_model,
        outcome = outcome,
        exposure_continuous = exposure_continuous,
        exposure_quartile = exposure_quartile,
        covariates = covariates,
        rcs_df = rcs_df,
        gam_k = gam_k
    )
    
    interaction_formula <- paste(deparse(interaction_results[[1]]$formula), collapse = " ")
    writeLines(interaction_formula, file.path(run_dir, "model_formula_step_4_interaction.txt"))
    writeLines(
        c(
            paste("Best model selected:", selected_model),
            "Selection rule: lowest mean AIC across imputations unless best_model overrides this.",
            "Note: AIC comparison across LMM and GAMM families should be interpreted pragmatically."
        ),
        file.path(run_dir, "best_model_selection.txt")
    )
    
    interaction_fixed <- purrr::map_dfr(interaction_results, function(result) {
        if (selected_model == "gamm") {
            extract_gam_parametric(result$fit, "step4_interaction_gamm", result$imputation)
        } else {
            extract_lmer_fixed(result$fit, paste0("step4_interaction_", selected_model), result$imputation)
        }
    })
    interaction_pooled <- pool_rubin(interaction_fixed)
    
    interaction_lrt <- purrr::map2_dfr(fit_results, interaction_results, function(base_result, int_result) {
        if (selected_model == "gamm") {
            tab <- stats::anova(base_result$fits$gamm, int_result$fit, test = "Chisq")
            tibble::tibble(
                comparison = "best_model_vs_dairy_time_interaction",
                imputation = base_result$imputation,
                reduced_model = selected_model,
                full_model = paste0(selected_model, "_interaction"),
                chisq = tab$Deviance[2],
                df_diff = tab$Df[2],
                p_value = tab$`Pr(>Chi)`[2]
            )
        } else {
            tab <- stats::anova(base_result$fits[[selected_model]], int_result$fit)
            tibble::tibble(
                comparison = "best_model_vs_dairy_time_interaction",
                imputation = base_result$imputation,
                reduced_model = selected_model,
                full_model = paste0(selected_model, "_interaction"),
                chisq = tab$Chisq[2],
                df_diff = tab$`Chi Df`[2],
                p_value = tab$`Pr(>Chisq)`[2]
            )
        }
    })
    
    output_paths <- c(
        fixed_by_imputation = file.path(run_dir, "fixed_effects_by_imputation_steps_1_to_3.csv"),
        fixed_pooled = file.path(run_dir, "fixed_effects_pooled_rubin_steps_1_to_3.csv"),
        gamm_smooth_by_imputation = file.path(run_dir, "gamm_smooth_summary_by_imputation.csv"),
        model_fit_by_imputation = file.path(run_dir, "model_fit_indices_by_imputation.csv"),
        model_fit_summary = file.path(run_dir, "model_fit_indices_summary.csv"),
        rcs_lrt_by_imputation = file.path(run_dir, "rcs_vs_linear_lrt_by_imputation.csv"),
        rcs_d2 = file.path(run_dir, "rcs_vs_linear_pooled_D2_if_available.csv"),
        gamm_predictions = file.path(run_dir, "gamm_smooth_predictions_pooled_rubin.csv"),
        gamm_plot_png = file.path(run_dir, "step3_gamm_smooth_pooled.png"),
        gamm_plot_pdf = file.path(run_dir, "step3_gamm_smooth_pooled.pdf"),
        interaction_fixed_by_imputation = file.path(run_dir, "fixed_effects_by_imputation_step_4_interaction.csv"),
        interaction_fixed_pooled = file.path(run_dir, "fixed_effects_pooled_rubin_step_4_interaction.csv"),
        interaction_lrt = file.path(run_dir, "interaction_lrt_by_imputation.csv")
    )
    
    write_csv_file(fixed_by_imp, output_paths[["fixed_by_imputation"]])
    write_csv_file(fixed_pooled, output_paths[["fixed_pooled"]])
    write_csv_file(smooth_by_imp, output_paths[["gamm_smooth_by_imputation"]])
    write_csv_file(fit_indices, output_paths[["model_fit_by_imputation"]])
    write_csv_file(fit_summary, output_paths[["model_fit_summary"]])
    write_csv_file(rcs_lrt, output_paths[["rcs_lrt_by_imputation"]])
    write_csv_file(rcs_d2, output_paths[["rcs_d2"]])
    write_csv_file(gamm_pred, output_paths[["gamm_predictions"]])
    ggplot2::ggsave(output_paths[["gamm_plot_png"]], gamm_plot, width = 8, height = 6, dpi = 300)
    ggplot2::ggsave(output_paths[["gamm_plot_pdf"]], gamm_plot, width = 8, height = 6)
    write_csv_file(interaction_fixed, output_paths[["interaction_fixed_by_imputation"]])
    write_csv_file(interaction_pooled, output_paths[["interaction_fixed_pooled"]])
    write_csv_file(interaction_lrt, output_paths[["interaction_lrt"]])
    
    tibble::tibble(
        output_dir = run_dir,
        selected_model = selected_model,
        output_name = names(output_paths),
        path = unname(output_paths)
    )
}
