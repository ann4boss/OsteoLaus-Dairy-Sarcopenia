# =============================================================================
# R/cox_sarcopenia.R
# =============================================================================
# Cox Proportional Hazards Regression — Time to First Sarcopenia
#
# OVERVIEW
# --------
# Fits Cox regression models for the association between dairy intake and
# incident sarcopenia in the OsteoLaus cohort. Supports:
#
#   Sarcopenia definitions
#     "ewgsop2"  ewgsop2_sarcopenia_stage: any stage > "No sarcopenia"
#     "fnih"     fnih_sarcopenia == "Sarcopenia"
#
#   Covariate treatment
#     "fixed"         One row per pt. Time = Age at event / last follow-up.
#                     Covariates from the first OsteoLaus visit (.visit == "Baseline").
#     "time_dependent" One row per pt × visit (until event). Start/Stop = Age
#                     at interval boundaries. Covariates from each visit.
#
#   Dairy exposure
#     "continuous"   dairy_total_gday (numeric)
#     "categorical"  dairy_total_gday cut into tertiles; or pass a pre-existing
#                    factor column via dairy_cat_col
#
#   Analysis route
#     "cc"   Complete-case: single dataset from prepare_cc() / filter_ewgsop2()
#     "mice" Multiple imputation: long-format tibble with .imp column from
#            apply_mice_exclusions()
#
#   Optional interaction term
#     interaction_var  Character. Name of one additional covariate to interact
#                      with the dairy term. NULL = no interaction.
#
# INPUTS (passed to run_cox_sarcopenia())
# ----------------------------------------
#   data              Annotated tibble (CC route) OR long-format MICE tibble
#                     with .imp column. Must already have exclusion flags applied
#                     and rows filtered appropriately for the target analysis.
#   sarcopenia_def    "ewgsop2" or "fnih"
#   covariate_type    "fixed" or "time_dependent"
#   dairy_type        "continuous" or "categorical"
#   analysis_route    "cc" or "mice"
#   covariates        Character vector of adjustment covariates (beyond dairy).
#                     Default: see .DEFAULT_COVARIATES below.
#   dairy_col         Column name for continuous dairy intake. Default "dairy_total_gday".
#   dairy_cat_col     Column name for dairy factor (used when dairy_type = "categorical"
#                     and you supply a pre-existing factor). NULL = derive tertiles.
#   interaction_var   Character or NULL.
#   min_age_start     Minimum age for the time axis origin (passed to tmerge).
#                     Default NULL (uses participant minimum Age).
#   out_dir           Directory for output plots and summaries. NULL = skip saving.
#
# OUTPUTS (returned as a named list)
# ------------------------------------
#   $config           List of analysis configuration settings used.
#   $surv_data        Prepared survival dataset used for modelling.
#   $fit_unadj        Unadjusted Cox model object (coxph or mira).
#   $fit_adj          Adjusted Cox model object.
#   $results_unadj    Tidy tibble: HR, 95% CI, p-value (unadjusted).
#   $results_adj      Tidy tibble: HR, 95% CI, p-value (adjusted).
#   $ph_test          cox.zph() output (CC only; for MICE, run on first imp).
#   $ph_plot          Schoenfeld residuals plot object.
#   $vif              Variance inflation factors (adjusted model).
#   $km_plot          Kaplan-Meier plot by dairy category (categorical dairy only).
#
# ASSUMPTIONS CHECKED (§7.14–7.19, Nahhas 2026)
# -----------------------------------------------
#   PH (proportional hazards) — cox.zph() global test + per-covariate + Schoenfeld plots
#   Linearity (continuous predictors) — martingale residual plots
#   Influential observations — dfbeta plots
#   Collinearity — car::vif() / car::Anova() for categorical predictors
#
# RUBIN'S RULES FOR MICE
# -----------------------
# Uses mice::pool() which applies Rubin's rules automatically.
# For assumption checks (PH, linearity) the first imputed dataset is used as
# a representative sample. Model results are from pooled estimates.
#
# =============================================================================


# ── Default covariate set (update as needed) ----------------------------------
.DEFAULT_COVARIATES <- c(
    "Age", "BMI"
    #, "education_level", "smoking_status",
    #"pa_levels_tertile_f1", 
    #  "diabetes_status"
)


# =============================================================================
# Main entry point
# =============================================================================

#' Run Cox regression for time to first sarcopenia.
#'
#' @param data            Tibble. CC dataset or MICE long-format dataset.
#' @param sarcopenia_def  Character. "ewgsop2" or "fnih".
#' @param covariate_type  Character. "fixed" or "time_dependent".
#' @param dairy_type      Character. "continuous" or "categorical".
#' @param analysis_route  Character. "cc" or "mice".
#' @param covariates      Character vector of adjustment variables.
#' @param dairy_col       Character. Continuous dairy column name.
#' @param dairy_cat_col   Character or NULL. Pre-existing dairy factor column.
#' @param interaction_var Character or NULL. Covariate to interact with dairy.
#' @param out_dir         Character or NULL. Directory for output plots/CSVs.
#'
#' @return Named list — see file header.
run_cox_sarcopenia <- function(
        data,
        sarcopenia_def   = c("ewgsop2", "fnih"),
        covariate_type   = c("fixed", "time_dependent"),
        dairy_type       = c("continuous", "categorical"),
        analysis_route   = c("cc", "mice"),
        covariates       = .DEFAULT_COVARIATES,
        dairy_col        = "dairy_total_gday",
        dairy_cat_col    = "dairy_quartile_baseline",
        interaction_var  = NULL
) {
    # ── Argument matching --------------------------------------------------
    sarcopenia_def  <- match.arg(sarcopenia_def)
    covariate_type  <- match.arg(covariate_type)
    dairy_type      <- match.arg(dairy_type)
    analysis_route  <- match.arg(analysis_route)
    
    cli::cli_h1("Cox Regression — Sarcopenia Onset")
    cli::cli_inform(c(
        "i" = "Definition : {sarcopenia_def}",
        "i" = "Covariates : {covariate_type}",
        "i" = "Dairy      : {dairy_type}",
        "i" = "Route      : {analysis_route}",
        "i" = "Interaction: {interaction_var %||% 'none'}"
    ))
    
    # ── Timestamp --------------------------------------------------------------
    timestamp <- format(Sys.time(), "%d%m%Y_%H%M")
    
    # ── Configuration tag -----------------------------------------------------
    config_tag <- paste(
        analysis_route,
        sarcopenia_def,
        covariate_type,
        dairy_type,
        interaction_var,
        sep = "_"
    )
    
    # ── Output directory ------------------------------------------------------
    out_dir <- file.path(
        "03_outputs",
        "Cox",
        paste0(config_tag, "_", timestamp)
    )
    
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # ── Store config -------------------------------------------------------
    config <- list(
        sarcopenia_def  = sarcopenia_def,
        covariate_type  = covariate_type,
        dairy_type      = dairy_type,
        analysis_route  = analysis_route,
        covariates      = covariates,
        dairy_col       = dairy_col,
        dairy_cat_col   = dairy_cat_col,
        interaction_var = interaction_var,
        timestamp = timestamp,
        out_dir   = out_dir
    )
    
    # ── Route ---------------------------------------------------------------
    if (analysis_route == "cc") {
        
        surv_data <- .build_surv_data_cc(
            data            = data,
            sarcopenia_def  = sarcopenia_def,
            covariate_type  = covariate_type,
            dairy_type      = dairy_type,
            dairy_col       = dairy_col,
            dairy_cat_col   = dairy_cat_col,
            covariates      = covariates
        )
        
        
        result <- .fit_cox_cc(
            surv_data       = surv_data,
            covariate_type  = covariate_type,
            dairy_type      = dairy_type,
            covariates      = covariates,
            interaction_var = interaction_var,
            out_dir         = out_dir
        )
        
    } else {
        
        result <- .fit_cox_mice(
            data            = data,
            sarcopenia_def  = sarcopenia_def,
            covariate_type  = covariate_type,
            dairy_type      = dairy_type,
            dairy_col       = dairy_col,
            dairy_cat_col   = dairy_cat_col,
            covariates      = covariates,
            interaction_var = interaction_var,
            out_dir         = out_dir
        )
    
        surv_data <- result$surv_data  # representative (first imp) dataset
    }
    
    c(list(config = config, surv_data = surv_data), result[setdiff(names(result), "surv_data")])
}


# =============================================================================
# SECTION 1 — Survival dataset builders
# =============================================================================

# ── 1A: Complete-case dataset ------------------------------------------------

#' Build survival dataset for the complete-case route.
#' @keywords internal
.build_surv_data_cc <- function(
        data, sarcopenia_def, covariate_type,
        dairy_type, dairy_col, dairy_cat_col,
     covariates
) {
    cli::cli_h2("Building survival dataset (CC)")
    
    # ── Derive outcome indicator --------------------------------------------
    data <- .derive_event_indicator(data, sarcopenia_def)
    
    if (covariate_type == "fixed") {
        surv_data <- .build_fixed_dataset(
            data, dairy_type, dairy_col, dairy_cat_col,
         covariates
        )
    } else {
        surv_data <- .build_td_dataset(
            data, dairy_type, dairy_col, dairy_cat_col,
            covariates
        )
    }
    surv_data
}


# ── 1B: Derive binary event indicator ----------------------------------------

#' Add `event` (0/1) based on chosen sarcopenia definition.
#' @keywords internal
.derive_event_indicator <- function(data, sarcopenia_def) {
    
    if (sarcopenia_def == "ewgsop2") {
      required_col <- "ewgsop2_sarcopenia_stage"
      if (!required_col %in% names(data))
        cli::cli_abort("Column {.col {required_col}} not found.")
      
      data <- data |>
        dplyr::mutate(
          ewgsop2_sarcopenia_stage = factor(
            as.character(ewgsop2_sarcopenia_stage),  # drop ordered
            ordered = FALSE
          ),
          event = dplyr::if_else(
            !is.na(ewgsop2_sarcopenia_stage) &
              ewgsop2_sarcopenia_stage %in% c("Confirmed", "Severe"),
            1L, 0L
        )
      )
        
    } else {  # fnih
        required_col <- "fnih_sarcopenia"
        if (!required_col %in% names(data))
            cli::cli_abort("Column {.col {required_col}} not found.")
        
        data <- data |>
            dplyr::mutate(
                event = dplyr::if_else(
                    !is.na(fnih_sarcopenia) & fnih_sarcopenia == "Sarcopenia",
                    1L, 0L
                )
            )
    }
    
    n_events <- sum(data$event == 1L, na.rm = TRUE)
    n_total  <- dplyr::n_distinct(data$pt, na.rm = TRUE)
    cli::cli_inform("Events: {n_events} sarcopenia cases out of {n_total} participants")
    
    data
}


# ── 1C: Fixed covariates dataset (one row per pt) ----------------------------

#' One row per participant. Time = Age at first event or last follow-up.
#' Covariates taken from OsteoLaus Baseline visit.
#' @keywords internal
.build_fixed_dataset <- function(
        data, dairy_type, dairy_col, dairy_cat_col,
         covariates
) {
    cli::cli_h2("Fixed covariates dataset (one row per pt)")
    
    # ── Require columns --------------------------------
    required <- c("pt", "Age", "time_point", "event")
    .check_cols(data, required)
    
    # ── Baseline covariates (first OsteoLaus visit available per pt) --------
    baseline_covs <- data |>
        dplyr::arrange(pt, time_point) |>
        dplyr::group_by(pt) |>
        dplyr::slice(1L) |>
        dplyr::ungroup() |>
        dplyr::select(pt, dplyr::all_of(intersect(covariates, names(data))),
                      dplyr::any_of(c(dairy_col, dairy_cat_col)))
    
    # ── Event timing per pt -------------------------------------------------
    # For participants who develop sarcopenia: Age at first event visit.
    # For others: Age at last valid follow-up visit.
    event_tbl <- data |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            # Age at first event (NA if never)
            age_event   = suppressWarnings(min(Age[event == 1L], na.rm = TRUE)),
            age_last    = suppressWarnings(max(Age,              na.rm = TRUE)),
            ever_event  = any(event == 1L, na.rm = TRUE),
            .groups     = "drop"
        ) |>
        dplyr::mutate(
            age_event = dplyr::if_else(is.infinite(age_event), NA_real_, age_event),
            age_last  = dplyr::if_else(is.infinite(age_last),  NA_real_, age_last),
            # time = Age at event or censoring
            time  = dplyr::if_else(ever_event, age_event, age_last),
            event = dplyr::if_else(ever_event, 1L, 0L)
        )
    
    # ── Merge ---------------------------------------------------------------
    surv_data <- event_tbl |>
        dplyr::inner_join(baseline_covs, by = "pt")
    
    # ── Dairy variable preparation ------------------------------------------
    surv_data <- .prepare_dairy(
        data         = surv_data,
        dairy_type   = dairy_type,
        dairy_col    = dairy_col,
        dairy_cat_col = dairy_cat_col
    )
    
    # ── Drop rows with missing time or dairy --------------------------------
    surv_data <- surv_data |>
        dplyr::filter(!is.na(time), !is.na(event), !is.na(dairy_exposure))
    
    cli::cli_inform(c(
        "v" = "Fixed dataset: {nrow(surv_data)} rows | {sum(surv_data$event)} events"
    ))
    
    surv_data
}


# ── 1D: Time-dependent covariates dataset (one row per pt × visit) -----------

#' One row per pt × visit up to (and including) the event visit.
#' Interval = [Age_start, Age_stop). Covariates change at each visit.
#' Uses survival::tmerge() approach.
#' @keywords internal
.build_td_dataset <- function(
        data, dairy_type, dairy_col, dairy_cat_col,
        covariates
) {
    cli::cli_h2("Time-dependent covariates dataset (one row per pt × visit)")
    
    required <- c("pt", "Age", "time_point", "event")
    .check_cols(data, required)
    
    # ── Sort by pt and Age -------------------------------------------------
    data <- data |>
        dplyr::arrange(pt, Age)
    
    # ── Prepare dairy at the visit level first ----------------------------
    data <- .prepare_dairy(
        data          = data,
        dairy_type    = dairy_type,
        dairy_col     = dairy_col,
        dairy_cat_col = dairy_cat_col
    )
    
    # ── Build counting-process format using tmerge -------------------------
    # First: define the baseline dataset (one row per pt, with entry age and
    # event time + event indicator).
    baseline_per_pt <- data |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            tstart      = min(Age, na.rm = TRUE),   # Age at first visit
            age_event   = suppressWarnings(min(Age[event == 1L], na.rm = TRUE)),
            age_last    = suppressWarnings(max(Age,              na.rm = TRUE)),
            ever_event  = any(event == 1L, na.rm = TRUE),
            .groups     = "drop"
        ) |>
        dplyr::mutate(
            age_event  = dplyr::if_else(is.infinite(age_event), NA_real_, age_event),
            age_last   = dplyr::if_else(is.infinite(age_last),  NA_real_, age_last),
            tstop      = dplyr::if_else(ever_event, age_event, age_last),
            event_time = dplyr::if_else(ever_event, 1L, 0L)
        )
    
    # ── tmerge: base object -----------------------------------------------
    base_obj <- survival::tmerge(
        data1  = baseline_per_pt,
        data2  = baseline_per_pt,
        id     = pt,
        event  =  event(tstop, event_time)
    )
    
    # ── tmerge: add time-varying covariates --------------------------------
    # Each covariate is added as a tdc() (time-dependent covariate).
    # The value at each visit is assumed to apply until the next visit.
    td_cov_cols <- intersect(
        c(covariates, "dairy_exposure"),
        names(data)
    )
    
    td_obj <- base_obj
    
    for (col in td_cov_cols) {
        
        if (col %in% names(data)) {
            
            data2_tmp <- data |>
                dplyr::filter(!is.na(.data[[col]])) |>
                dplyr::select(pt, Age, value = dplyr::all_of(col))
            
            tmp <- survival::tmerge(
                data1   = td_obj,
                data2   = data2_tmp,
                id      = pt,
                tdc_tmp = tdc(Age, value)
            )
            
            names(tmp)[names(tmp) == "tdc_tmp"] <- col
            
            td_obj <- tmp
        }
    }
    
    # ── Rename tmerge time columns -----------------------------------------
    td_obj <- dplyr::rename(td_obj, age_start = tstart, age_stop = tstop)
    
    # ── Drop intervals with missing key variables --------------------------
    td_obj <- td_obj |>
        dplyr::filter(!is.na(dairy_exposure), !is.na(age_start), !is.na(age_stop),
                      age_start < age_stop)
    
    cli::cli_inform(c(
        "v" = "Time-dependent dataset: {nrow(td_obj)} intervals | {sum(td_obj$event)} events"
    ))
    
    td_obj
}


# =============================================================================
# SECTION 2 — Dairy exposure preparation
# =============================================================================

#' Prepare the dairy_exposure column (continuous or categorical).
#' @keywords internal
.prepare_dairy <- function(data, dairy_type, dairy_col, dairy_cat_col) {
  if (dairy_type == "continuous") {
    if (!dairy_col %in% names(data))
      cli::cli_abort("Dairy column {.col {dairy_col}} not found.")
    data$dairy_exposure <- data[[dairy_col]]
    
  } else {
    if (is.null(dairy_cat_col))
      cli::cli_abort(
        "dairy_type = 'categorical' but {.arg dairy_cat_col} is NULL."
      )
    if (!dairy_cat_col %in% names(data))
      cli::cli_abort("Categorical dairy column {.col {dairy_cat_col}} not found.")
    
    # Ensure it is an unordered factor
    data$dairy_exposure <- factor(
      as.character(data[[dairy_cat_col]]),
      ordered = FALSE
    )
    cli::cli_inform(
      "dairy_exposure levels: {.val {levels(data$dairy_exposure)}}"
    )
  }
  data
}


# =============================================================================
# SECTION 3 — Formula builder
# =============================================================================

#' Build the Cox model formula.
#' @keywords internal
.build_cox_formula <- function(
        covariate_type, dairy_type, covariates,
        interaction_var = NULL, adjusted = TRUE
) {
    # ── Time specification --------------------------------------------------
    if (covariate_type == "fixed") {
        surv_term <- "survival::Surv(time, event)"
    } else {
        surv_term <- "survival::Surv(age_start, age_stop, event)"
    }
    
    # ── Dairy term ----------------------------------------------------------
    dairy_term <- "dairy_exposure"
    
    # ── Interaction term ----------------------------------------------------
    if (!is.null(interaction_var)) {
        dairy_main <- glue::glue("{dairy_term} * {interaction_var}")
    } else {
        dairy_main <- dairy_term
    }
    
    # ── Unadjusted / adjusted ---------------------------------------------
    if (!adjusted || length(covariates) == 0L) {
        rhs <- dairy_main
    } else {
        adj_vars <- setdiff(covariates, interaction_var)  # avoid double entry
        rhs <- paste(c(dairy_main, adj_vars), collapse = " + ")
    }
    
    as.formula(glue::glue("{surv_term} ~ {rhs}"))
}


# =============================================================================
# SECTION 4 — Complete-case model fitting
# =============================================================================

#' Fit unadjusted and adjusted Cox models for the CC route.
#' @keywords internal
.fit_cox_cc <- function(
        surv_data, covariate_type, dairy_type,
        covariates, interaction_var, out_dir
) {
    cli::cli_h2("Fitting Cox models (CC)")
    
    # ── Formulae -----------------------------------------------------------
    formula_unadj <- .build_cox_formula(
        covariate_type = covariate_type,
        dairy_type     = dairy_type,
        covariates     = covariates,
        interaction_var = interaction_var,
        adjusted       = FALSE
    )
    formula_adj <- .build_cox_formula(
        covariate_type = covariate_type,
        dairy_type     = dairy_type,
        covariates     = covariates,
        interaction_var = interaction_var,
        adjusted       = TRUE
    )
    
    cli::cli_inform("Unadjusted formula: {deparse(formula_unadj)}")
    cli::cli_inform("Adjusted formula  : {deparse(formula_adj)}")
    
    # ── Fit models ---------------------------------------------------------
    fit_unadj <- survival::coxph(
        formula_unadj,
        data  = surv_data,
        ties  = "efron",
        model = TRUE,
        x     = TRUE
    )
    
    fit_adj <- survival::coxph(
        formula_adj,
        data  = surv_data,
        ties  = "efron",
        model = TRUE,
        x     = TRUE
    )
    
    # ── Tidy results -------------------------------------------------------
    results_unadj <- .tidy_cox(fit_unadj)
    results_adj   <- .tidy_cox(fit_adj)
    
    cli::cli_h2("Adjusted model results")
    print(results_adj)
    
    # ── Assumption checks --------------------------------------------------
    assumptions <- .check_cox_assumptions(
        fit        = fit_adj,
        surv_data  = surv_data,
        out_dir    = out_dir,
        label      = "cc_adjusted"
    )
    
    # ── KM plot (categorical+fixed dairy only) ----------------------------------
    km_plot <- NULL
    if (dairy_type == "categorical") {
      km_plot <- .km_plot_dairy(
        surv_data      = surv_data,
        covariate_type = covariate_type,
        out_dir        = out_dir,
        label          = "mice_imp1"
      )
    }
    # ── Save results -----------------------------------------------------------
    .save_model_results(results_unadj, results_adj, out_dir, label = "cc")
    
    list(
        fit_unadj           = fit_unadj,
        fit_adj             = fit_adj,
        results_unadj       = results_unadj,
        results_adj         = results_adj,
        ph_test             = assumptions$ph_test,
        ph_plot             = assumptions$ph_plot,
        vif                 = assumptions$vif,
        outlier_plot        = assumptions$outlier_plot,          
        outlier_flagged     = assumptions$outlier_flagged_rows,  
        dfbeta_plot         = assumptions$dfbeta_plot,
        dfbeta_flagged      = assumptions$dfbeta_flagged,        
        dfbeta_flag_detail  = assumptions$dfbeta_flag_detail,    
        km_plot             = km_plot
    )
}


# =============================================================================
# SECTION 5 — MICE (multiple imputation) route
# =============================================================================

#' Fit Cox models across all m imputed datasets and pool via Rubin's rules.
#' @keywords internal
.fit_cox_mice <- function(
        data, sarcopenia_def, covariate_type,
        dairy_type, dairy_col, dairy_cat_col,
         covariates, interaction_var, out_dir
) {
  
    cli::cli_h2("MICE route — pooling across imputed datasets")
    
    if (!".imp" %in% names(data))
        cli::cli_abort("MICE dataset must contain a {.col .imp} column.")
    
    imp_ids <- sort(unique(data$.imp))
    imp_ids <- imp_ids[imp_ids > 0L]
    m       <- length(imp_ids)
    
    cli::cli_inform("Detected {m} imputed datasets.")
    
    # ── Build formulae once -----------------------------------------------
    formula_unadj <- .build_cox_formula(covariate_type, dairy_type, covariates,
                                        interaction_var, adjusted = FALSE)
    formula_adj   <- .build_cox_formula(covariate_type, dairy_type, covariates,
                                        interaction_var, adjusted = TRUE)
    
    # ── Fit on each imputed dataset ----------------------------------------
    fits_unadj <- vector("list", m)
    fits_adj   <- vector("list", m)
    surv_data_list <- vector("list", m)
    
    for (i in seq_along(imp_ids)) {
        imp_i <- imp_ids[i]
        cli::cli_inform("  Fitting .imp = {imp_i} / {m} ...")
        
        df_i <- data |> dplyr::filter(.imp == imp_i) |> dplyr::select(-.imp)
        
        # Derive event indicator
        df_i <- .derive_event_indicator(df_i, sarcopenia_def)
        
        # Build survival dataset for this imputation
        surv_i <- if (covariate_type == "fixed") {
            .build_fixed_dataset(df_i, dairy_type, dairy_col, dairy_cat_col,
                                 covariates)
        } else {
            .build_td_dataset(df_i, dairy_type, dairy_col, dairy_cat_col,
                               covariates)
        }
        
        surv_data_list[[i]] <- surv_i
        
        fits_unadj[[i]] <- survival::coxph(
            formula_unadj,
            data  = surv_i,
            ties  = "efron",
            model = TRUE,
            x     = TRUE
        )
        
        fits_adj[[i]] <- survival::coxph(
            formula_adj,
            data  = surv_i,
            ties  = "efron",
            model = TRUE,
            x     = TRUE
        )
    }
    
    # ── Pool using mice::pool() -------------------------------------------
    # mice::pool() accepts a list of fitted models via as.mira()
    mira_unadj <- mice::as.mira(fits_unadj)
    mira_adj   <- mice::as.mira(fits_adj)
    
    pooled_unadj <- mice::pool(mira_unadj)
    pooled_adj   <- mice::pool(mira_adj)
    
    results_unadj <- .tidy_pooled(pooled_unadj)
    results_adj   <- .tidy_pooled(pooled_adj)
    
    cli::cli_h2("Pooled adjusted model results (Rubin's rules)")
    print(results_adj)
    
    # ── Assumption checks on first imputed dataset (representative) --------
    assumptions <- .check_cox_assumptions(
        fit       = fits_adj[[1L]],
        surv_data = surv_data_list[[1L]],
        out_dir   = out_dir,
        label     = "mice_imp1"
    )
    
    # ── KM plot on imputed datasets -----------------------------------
    km_plot <- NULL
    if (dairy_type == "categorical") {
      km_plot <- .km_plot_dairy(
        surv_data       = surv_data_list[[1L]],
        covariate_type  = covariate_type,
        out_dir         = out_dir,
        label           = "mice_pooled"
      )
    }
    .save_model_results(results_unadj, results_adj, out_dir, label = "mice")
    
    list(
      surv_data           = surv_data_list[[1L]], 
        fit_unadj           = mira_unadj,       
        fit_adj             = mira_adj,       
        results_unadj       = results_unadj,
        results_adj         = results_adj,
        ph_test             = assumptions$ph_test,
        ph_plot             = assumptions$ph_plot,
        vif                 = assumptions$vif,
        outlier_plot        = assumptions$outlier_plot,          
        outlier_flagged     = assumptions$outlier_flagged_rows,  
        dfbeta_plot         = assumptions$dfbeta_plot,
        dfbeta_flagged      = assumptions$dfbeta_flagged,        
        dfbeta_flag_detail  = assumptions$dfbeta_flag_detail,    
        km_plot             = km_plot
    )
}


# =============================================================================
# SECTION 6 — Results tidying
# =============================================================================

#' Tidy a coxph object into HR, 95% CI, p-value tibble.
#' @keywords internal
.tidy_cox <- function(fit) {
    broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE) |>
        dplyr::select(term, estimate, conf.low, conf.high, p.value) |>
        dplyr::rename(
            HR   = estimate,
            CI_low  = conf.low,
            CI_high = conf.high
        )
}

#' Tidy a pooled mice object into HR, 95% CI, p-value tibble.
#' @keywords internal
.tidy_pooled <- function(pooled) {
    s <- summary(pooled, conf.int = TRUE, exponentiate = TRUE)
    tibble::tibble(
        term    = as.character(s$term),
        HR      = s$estimate,
        CI_low  = s[["2.5 %"]],
        CI_high = s[["97.5 %"]],
        p.value = s$p.value
    )
}


# =============================================================================
# SECTION 7 — Assumption checks
# =============================================================================

#' Check all Cox regression assumptions and produce plots.
#'
#' Checks:
#'   PH assumption  — cox.zph() + Schoenfeld residual plots  (§7.16)
#'   Linearity      — Martingale residual plots               (§7.18)
#'   Influential obs — dfbeta plots                           (§7.19)
#'   Collinearity   — car::vif() for adjusted model           (§7.15)
#'
#' @keywords internal
.check_cox_assumptions <- function(fit, surv_data, out_dir, label) {
    
    cli::cli_h2("Assumption checks [{label}]")
    
    results <- list()
    
    # ── 1. Proportional hazards — cox.zph() --------------------------------
    cli::cli_h3("PH assumption (cox.zph)")
    tryCatch({
        ph_test <- survival::cox.zph(fit)
        results$ph_test <- ph_test
        
        cli::cli_inform(c(
            paste(capture.output(print(ph_test)), collapse = "\n"))
        )
        
        # Global test interpretation
        global_p <- ph_test$table["GLOBAL", "p"]
        if (global_p < 0.05) {
            cli::cli_warn(c(
                "!" = "Global PH test is significant (p = {round(global_p, 4)}).",
                "i" = "Consider: stratification, time × covariate interaction, or",
                "i" = "  fitting a model with tt() to allow time-varying coefficients."
            ))
        } else {
            cli::cli_inform(c("v" = "Global PH assumption not violated (p = {round(global_p, 4)})"))
        }
        
        # Schoenfeld residual plots
        ph_plot <- ggplot2::ggplot() +   # placeholder; survminer is used below
            ggplot2::ggtitle(glue::glue("Schoenfeld residuals [{label}]"))
        
        results$ph_plot <- survminer::ggcoxzph(ph_test)
        
        if (!is.null(out_dir)) {
            purrr::walk(seq_along(results$ph_plot), function(i) {
                fname <- file.path(out_dir, glue::glue("{label}_schoenfeld_{i}.png"))
                ggplot2::ggsave(fname, results$ph_plot[[i]], width = 8, height = 5, dpi = 150)
            })
            cli::cli_inform("Schoenfeld plots saved to {.path {out_dir}}")
        }
        
    }, error = function(e) {
        cli::cli_warn("cox.zph() failed: {conditionMessage(e)}")
        results$ph_test <<- NULL
        results$ph_plot <<- NULL
    })
    
    # ── 2. Outliers — Martingale AND deviance residuals (§7.19) ────────────────
    cli::cli_h3("Outliers (martingale + deviance residuals)")
    tryCatch({
        mart_resid <- residuals(fit, type = "martingale")
        dev_resid  <- residuals(fit, type = "deviance")
        obs_idx    <- seq_along(mart_resid)
        
        # Flag the most extreme negative martingale residuals (§7.19 rule of thumb)
        mart_sorted   <- sort(mart_resid)
        n_flag        <- min(5L, length(mart_sorted))
        flagged_mart  <- names(mart_sorted)[seq_len(n_flag)]
        
        cli::cli_inform(c(
            "i" = "Most negative martingale residuals:",
            paste(capture.output(print(round(mart_sorted[seq_len(n_flag)], 4))),
                  collapse = "\n")
        ))
        
        # Retrieve the flagged rows from surv_data so the analyst can inspect them
        flagged_rows <- surv_data[flagged_mart, , drop = FALSE]
        results$outlier_flagged_rows <- flagged_rows
        
        # ggplot version of base-R §7.19 two-panel plot
        df_resid <- data.frame(
            obs        = obs_idx,
            martingale = mart_resid,
            deviance   = dev_resid
        )
        
        p_mart <- ggplot2::ggplot(df_resid, ggplot2::aes(x = obs, y = martingale)) +
            ggplot2::geom_point(alpha = 0.4, size = 1.2) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
            ggplot2::labs(title = glue::glue("Martingale residuals [{label}]"),
                          x = "Observation", y = "Martingale residual") +
            ggplot2::theme_bw()
        
        p_dev <- ggplot2::ggplot(df_resid, ggplot2::aes(x = obs, y = deviance)) +
            ggplot2::geom_point(alpha = 0.4, size = 1.2) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
            ggplot2::labs(title = glue::glue("Deviance residuals [{label}]"),
                          x = "Observation", y = "Deviance residual") +
            ggplot2::theme_bw()
        
        results$outlier_plot <- list(martingale = p_mart, deviance = p_dev)
        
        if (!is.null(out_dir)) {
            ggplot2::ggsave(file.path(out_dir, glue::glue("{label}_martingale.png")),
                            p_mart, width = 7, height = 5, dpi = 150)
            ggplot2::ggsave(file.path(out_dir, glue::glue("{label}_deviance.png")),
                            p_dev,  width = 7, height = 5, dpi = 150)
            readr::write_csv(flagged_rows,
                             file.path(out_dir, glue::glue("{label}_outlier_flagged.csv")))
        }
    }, error = function(e) {
        cli::cli_warn("Outlier plots failed: {conditionMessage(e)}")
        results$outlier_plot         <<- NULL
        results$outlier_flagged_rows <<- NULL
    })
    
    # ── 3. Influential observations — standardised DFBetas (§7.20) ─────────────
    # Use type = "dfbetas" (standardised), not "dfbeta" (unstandardised).
    # Harrell (2015) cutoff: |dfbeta_std| > 0.2
    cli::cli_h3("Influential observations (standardised DFBetas, cutoff |x| > 0.2)")
    tryCatch({
        dfbetas_mat <- residuals(fit, type = "dfbetas")   # note the 's'
        cutoff      <- 0.2
        
        # One ggplot per coefficient, with reference lines at ±0.2
        dfb_plots <- purrr::map(seq_len(ncol(dfbetas_mat)), function(j) {
            coef_name <- colnames(dfbetas_mat)[j]
            df_plot   <- data.frame(obs = seq_len(nrow(dfbetas_mat)),
                                    dfbeta_std = dfbetas_mat[, j])
            n_influential <- sum(abs(df_plot$dfbeta_std) > cutoff)
            
            ggplot2::ggplot(df_plot, ggplot2::aes(x = obs, y = dfbeta_std)) +
                ggplot2::geom_bar(stat = "identity", fill = "#4e79a7", width = 0.6) +
                ggplot2::geom_hline(yintercept =  cutoff, linetype = "dashed",
                                    colour = "red", linewidth = 0.7) +
                ggplot2::geom_hline(yintercept = -cutoff, linetype = "dashed",
                                    colour = "red", linewidth = 0.7) +
                ggplot2::annotate("text", x = Inf, y = cutoff,
                                  label = paste0("n > cutoff: ", n_influential),
                                  hjust = 1.1, vjust = -0.4, size = 3) +
                ggplot2::labs(
                    title = glue::glue("Std. DFBeta: {coef_name} [{label}]"),
                    x = "Observation", y = "Standardised DFBeta"
                ) +
                ggplot2::theme_bw()
        })
        names(dfb_plots) <- colnames(dfbetas_mat)
        results$dfbeta_plot <- dfb_plots
        
        # Tabulate which observations exceed the cutoff and for which coefficients
        exceed_any <- apply(abs(dfbetas_mat) > cutoff, 1, any)
        flagged_dfb <- data.frame(
            obs       = which(exceed_any),
            surv_data[which(exceed_any), , drop = FALSE]
        )
        
        # Per-coefficient breakdown (mirrors §7.20 table() approach)
        dfb_flag_detail <- purrr::map_dfr(
            seq_len(ncol(dfbetas_mat)),
            function(j) {
                coef_name <- colnames(dfbetas_mat)[j]
                flagged   <- abs(dfbetas_mat[, j]) > cutoff
                if (!any(flagged)) return(NULL)
                surv_data[flagged, , drop = FALSE] |>
                    dplyr::mutate(coefficient = coef_name,
                                  dfbeta_std  = dfbetas_mat[flagged, j])
            }
        )
        
        results$dfbeta_flagged      <- flagged_dfb
        results$dfbeta_flag_detail  <- dfb_flag_detail
        
        cli::cli_inform(c(
            "i" = "{sum(exceed_any)} observations exceed |DFBeta_std| > {cutoff} for at least one coefficient."
        ))
        
        if (!is.null(out_dir)) {
            purrr::iwalk(dfb_plots, function(p, nm) {
                safe_nm <- gsub("[^A-Za-z0-9_]", "_", nm)
                ggplot2::ggsave(
                    file.path(out_dir, glue::glue("{label}_dfbetas_{safe_nm}.png")),
                    p, width = 7, height = 5, dpi = 150
                )
            })
            if (nrow(dfb_flag_detail) > 0L)
                readr::write_csv(dfb_flag_detail,
                                 file.path(out_dir, glue::glue("{label}_dfbetas_flagged.csv")))
        }
    }, error = function(e) {
        cli::cli_warn("DFBeta plot failed: {conditionMessage(e)}")
        results$dfbeta_plot        <<- NULL
        results$dfbeta_flagged     <<- NULL
        results$dfbeta_flag_detail <<- NULL
    })
    
    # ── 4. Collinearity — VIF ----------------------------------------------
    cli::cli_h3("Collinearity (VIF)")
    tryCatch({
        # car::vif() works for coxph
        vif_vals <- car::vif(fit)
        results$vif <- vif_vals
        
        cli::cli_inform(c(
            paste(capture.output(print(round(vif_vals, 3))), collapse = "\n"))
        )
        
        # Identify problematic VIFs
        # For categorical predictors car returns GVIFs; flag GVIF^(1/(2*Df)) > sqrt(5) ≈ 2.24
        if (is.matrix(vif_vals)) {
            gvif_scaled <- vif_vals[, "GVIF^(1/(2*Df))"]
            high_vif <- names(gvif_scaled[gvif_scaled > 2.24])
        } else {
            high_vif <- names(vif_vals[vif_vals > 5])
        }
        
        if (length(high_vif) > 0L) {
            cli::cli_warn(c(
                "!" = "High collinearity detected for: {.val {high_vif}}",
                "i" = "Consider removing or combining these predictors."
            ))
        } else {
            cli::cli_inform(c("v" = "No collinearity concerns (all VIF/GVIF within range)."))
        }
        
        if (!is.null(out_dir)) {
            readr::write_csv(
                tibble::as_tibble(vif_vals, rownames = "term"),
                file.path(out_dir, glue::glue("{label}_vif.csv"))
            )
        }
        
    }, error = function(e) {
        cli::cli_warn("VIF failed: {conditionMessage(e)}")
        results$vif <<- NULL
    })
    
    results
}




# =============================================================================
# Section 8 — Kaplan-Meier plot (pooled across imputations)
# =============================================================================

#' Kaplan-Meier survival curves stratified by dairy exposure category.
#' Accepts either a single surv_data data.frame (CC) or a list of data.frames
#' (MICE — one per imputation) and averages curves in the latter case.
#'
#' @param surv_data      data.frame (CC) or list of data.frames (MICE).
#' @param covariate_type "fixed" or "time_dependent".
#' @param out_dir        Output directory or NULL.
#' @param label          String label for file names.
#' @keywords internal
# =============================================================================
# SECTION 8 — Kaplan-Meier plot by dairy category
# =============================================================================

#' Kaplan-Meier survival curves stratified by dairy exposure category.
#' Uses a single dataset (CC dataset or first imputed dataset for MICE).
#' @keywords internal
.km_plot_dairy <- function(surv_data, covariate_type, out_dir, label) {
  
  cli::cli_h3("Kaplan-Meier plot by dairy category")
  
  if (!is.data.frame(surv_data) || !"dairy_exposure" %in% names(surv_data))
    return(NULL)
  
  km_formula <- if (covariate_type == "fixed") {
    survival::Surv(time, event) ~ dairy_exposure
  } else {
    survival::Surv(age_start, age_stop, event) ~ dairy_exposure
  }
  
  km_fit <- survival::survfit(km_formula, data = surv_data)
  
  km_plot <- survminer::ggsurvplot(
    fit          = km_fit,
    data         = surv_data,
    pval         = TRUE,
    conf.int     = TRUE,
    risk.table   = TRUE,
    xlab         = "Age (years)",
    ylab         = "Sarcopenia-free probability",
    legend.title = "Dairy intake",
    palette      = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2"),
    ggtheme      = ggplot2::theme_bw()
  )$plot
  
  if (!is.null(out_dir)) {
    ggplot2::ggsave(
      file.path(out_dir, glue::glue("{label}_km_dairy.png")),
      km_plot, width = 10, height = 7, dpi = 150
    )
    cli::cli_inform("KM plot saved to {.path {out_dir}}")
  }
  
  km_plot
}

# =============================================================================
# SECTION 9 — Interaction results helper
# =============================================================================

#' Estimate HR at each level of the interaction moderator.
#'
#' Wraps car::linearHypothesis() / emmeans approach. For a model with
#' `dairy_exposure * interaction_var`, returns the conditional HR for dairy
#' at each level of interaction_var (for categorical moderator) or at
#' specified values (for continuous moderator).
#'
#' @param fit           coxph object (CC route) or first model from MICE list.
#' @param interaction_var Character. Moderator variable name.
#' @param moderator_levels Optional. Levels or values of moderator at which to
#'                         evaluate the dairy HR. NULL = auto-detect from data.
#'
#' @return Tibble: moderator level, HR, 95% CI, p-value.
cox_interaction_hr <- function(fit, interaction_var, moderator_levels = NULL) {
    
    if (is.null(interaction_var))
        cli::cli_abort("No interaction_var specified.")
    
    cli::cli_h2("Conditional HR for dairy at each level of {interaction_var}")
    
    # Use emmeans for this if available (handles both continuous and categorical)
    if (requireNamespace("emmeans", quietly = TRUE)) {
        em <- emmeans::emmeans(fit, specs = "dairy_exposure",
                               by = interaction_var,
                               type = "response")
        contrasts_out <- emmeans::contrast(em, "pairwise")
        print(contrasts_out)
        return(invisible(contrasts_out))
    }
    
    # Fallback: manual coefficient extraction via model matrix
    cli::cli_warn(c(
        "!" = "Package 'emmeans' not available.",
        "i" = "Install emmeans for conditional HR estimation at each moderator level.",
        "i" = "Returning raw coefficient table instead."
    ))
    broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE)
}




# =============================================================================
# SECTION 10 — Utility helpers
# =============================================================================

#' Check that required columns exist in a data frame.
#' @keywords internal
.check_cols <- function(data, required) {
    missing <- setdiff(required, names(data))
    if (length(missing) > 0L)
        cli::cli_abort("Required columns not found: {.val {missing}}")
    invisible(TRUE)
}

#' Null coalescing operator (mirrors rlang::`%||%`).
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b



# =============================================================================
# Helper — save model results to CSV
# =============================================================================

#' Write unadjusted and adjusted HR tables to out_dir.
#' @keywords internal
.save_model_results <- function(results_unadj, results_adj, out_dir, label) {
  
  if (is.null(out_dir)) return(invisible(NULL))
  
  # Combine into one file with a model column for convenience
  combined <- dplyr::bind_rows(
    dplyr::mutate(results_unadj, model = "unadjusted"),
    dplyr::mutate(results_adj,   model = "adjusted")
  ) |>
    dplyr::select(model, term, HR, CI_low, CI_high, p.value) |>
    dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 4)))
  
  # Also add a formatted HR (95% CI) string for quick reading
  combined <- combined |>
    dplyr::mutate(
      HR_CI = glue::glue("{HR} ({CI_low}–{CI_high})")
    )
  
  path <- file.path(out_dir, glue::glue("{label}_model_results.csv"))
  readr::write_csv(combined, path)
  cli::cli_inform("Model results saved to {.path {path}}")
  
  invisible(combined)
}


