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
#            apply_mice_exclusions(), OR a mids object (auto-converted)
#            e.g. mice_analysis$mids$ewgsop2_sarcopenia_stage
#                 mice_analysis$mids$fnih_sarcopenia
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
#   $config_epv       Named numeric: n_events, n_coefs, epv.
#   $fit_unadj        Unadjusted Cox model object (coxph or mira).
#   $fit_adj          Adjusted Cox model object.
#   $results_unadj    Tidy tibble: HR, 95% CI, p-value (unadjusted).
#   $results_adj      Tidy tibble: HR, 95% CI, p-value (adjusted).
#   $ph_test          cox.zph() output (on first imputed dataset for MICE).
#   $ph_plot          Schoenfeld residuals plot list.
#   $linearity_plots  List of ggplots: martingale (null model) vs each continuous predictor.
#   $outlier_plot     List: martingale and deviance residual plots (|dev|>3 flagged in red).
#   $outlier_flagged  Rows from surv_data where |deviance residual| > 3.
#   $dfbeta_plot      List of ggplots: standardised DFBeta per coefficient.
#   $dfbeta_flagged   Rows from surv_data exceeding |DFBeta_std| > 0.2.
#   $dfbeta_flag_detail Per-coefficient flagged observations tibble.
#   $cox_snell_plot   ggplot: Cox-Snell residuals vs -log(KM) (overall fit).
#   $vif              Variance inflation factors (adjusted model).
#   $km_plot          Kaplan-Meier plot by dairy category (imp=1 for MICE).
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
    "BMI", "education_level", "smoking_status",
    "mvpa_min_day_f1", "diabetes_status"
)


# =============================================================================
# Main entry point
# =============================================================================

#' Run Cox regression for time to first sarcopenia.
#'
#' @param data            Tibble (CC or MICE long-format with .imp column), or a
#'                        \code{mids} object from the mice package (auto-converted).
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
        dairy_col        = "dairy_total_gday_cumavg",
        dairy_cat_col    = "dairy_quartile_baseline",
        interaction_var  = NULL,
        death_col        = NULL   # column name for all-cause death (0/1); enables Fine-Gray
) {
    # ── mids → long format conversion ----------------------------------------
    if (inherits(data, "mids")) {
        cli::cli_inform(c("i" = "mids object detected — converting to long format (include = FALSE)."))
        data <- mice::complete(data, action = "long", include = FALSE)
    }

    # ── Argument matching --------------------------------------------------
    sarcopenia_def  <- match.arg(sarcopenia_def)
    covariate_type  <- match.arg(covariate_type)
    dairy_type      <- match.arg(dairy_type)
    analysis_route  <- match.arg(analysis_route)

    # ── Output directory (computed before log capture starts) ──────────────
    timestamp  <- format(Sys.time(), "%d%m%Y_%H%M")
    config_tag <- paste(analysis_route, sarcopenia_def, covariate_type,
                        dairy_type, interaction_var, sep = "_")
    out_dir    <- file.path("03_outputs", "Cox", paste0(config_tag, "_", timestamp))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    # ── Capture all cli / message output → saved to PDF at end ─────────────
    log_lines <- character(0)

    result <- withCallingHandlers(
        {
            cli::cli_h1("Cox Regression — Sarcopenia Onset")
            cli::cli_inform(c(
                "i" = "Definition : {sarcopenia_def}",
                "i" = "Covariates : {covariate_type}",
                "i" = "Dairy      : {dairy_type}",
                "i" = "Route      : {analysis_route}",
                "i" = "Interaction: {interaction_var %||% 'none'}"
            ))

            config <- list(
                sarcopenia_def  = sarcopenia_def,
                covariate_type  = covariate_type,
                dairy_type      = dairy_type,
                analysis_route  = analysis_route,
                covariates      = covariates,
                dairy_col       = dairy_col,
                dairy_cat_col   = dairy_cat_col,
                interaction_var = interaction_var,
                death_col       = death_col,
                timestamp       = timestamp,
                out_dir         = out_dir
            )

            if (analysis_route == "cc") {

                surv_data <- .build_surv_data_cc(
                    data           = data,
                    sarcopenia_def = sarcopenia_def,
                    covariate_type = covariate_type,
                    dairy_type     = dairy_type,
                    dairy_col      = dairy_col,
                    dairy_cat_col  = dairy_cat_col,
                    covariates     = covariates
                )

                inner <- .fit_cox_cc(
                    surv_data       = surv_data,
                    covariate_type  = covariate_type,
                    dairy_type      = dairy_type,
                    dairy_col       = dairy_col,
                    covariates      = covariates,
                    interaction_var = interaction_var,
                    death_col       = death_col,
                    out_dir         = out_dir
                )

            } else {

                inner <- .fit_cox_mice(
                    data            = data,
                    sarcopenia_def  = sarcopenia_def,
                    covariate_type  = covariate_type,
                    dairy_type      = dairy_type,
                    dairy_col       = dairy_col,
                    dairy_cat_col   = dairy_cat_col,
                    covariates      = covariates,
                    interaction_var = interaction_var,
                    death_col       = death_col,
                    out_dir         = out_dir
                )

                surv_data <- inner$surv_data
            }

            c(list(config = config, surv_data = surv_data),
              inner[setdiff(names(inner), "surv_data")])
        },
        message = function(m) {
            line <- cli::ansi_strip(conditionMessage(m))
            # Filter out targets progress-bar noise (lines containing the targets
            # dispatcher pattern "→ N targets" or blocks of "?").
            if (!grepl("→.*target|\\.tar_make|\\?\\?\\?\\?", line)) {
                log_lines <<- c(log_lines, line)
            }
            invokeRestart("muffleMessage")
        }
    )

    # ── Write accumulated log to PDF ────────────────────────────────────────
    .write_log_pdf(log_lines, file.path(out_dir, "run_log.pdf"))
    cli::cli_inform(c("v" = "Run log saved to {.path {file.path(out_dir, 'run_log.pdf')}}"))

    result
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

            # Use .age_time as the tdc time column to avoid a name collision
            # when col == "Age" (dplyr would otherwise drop the Age column
            # while renaming it to value, leaving tdc() unable to find it).
            data2_tmp <- data |>
                dplyr::filter(!is.na(.data[[col]])) |>
                dplyr::mutate(.age_time = Age) |>
                dplyr::select(pt, .age_time, value = dplyr::all_of(col))

            tmp <- survival::tmerge(
                data1   = td_obj,
                data2   = data2_tmp,
                id      = pt,
                tdc_tmp = tdc(.age_time, value)
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
    
    person_years <- sum(td_obj$age_stop - td_obj$age_start, na.rm = TRUE)
    cli::cli_inform(c(
        "v" = "Time-dependent dataset: {nrow(td_obj)} intervals | {sum(td_obj$event)} events | {round(person_years, 1)} person-years"
    ))
    attr(td_obj, "person_years") <- person_years

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
# SECTION 3B — Events Per Variable (EPV)
# =============================================================================

#' Compute EPV and warn if below the recommended threshold.
#'
#' EPV = number of events / number of free model coefficients.
#' Uses \code{length(coef(fit))} so factor dummy coding is counted correctly.
#' Recommended: 10 (minimum), 20 (ideal).
#'
#' @param fit        Fitted \code{coxph} object (adjusted model).
#' @param n_events   Integer. Number of observed events.
#' @param label      Character. Label for messaging.
#' @return Named numeric: \code{n_events}, \code{n_coefs}, \code{epv}.
#' @keywords internal
.compute_epv <- function(fit, n_events, label = "") {
    n_coefs <- length(stats::coef(fit))
    epv     <- n_events / n_coefs

    cli::cli_h3("Events Per Variable (EPV) [{label}]")
    cli::cli_inform(c(
        "i" = "Events   : {n_events}",
        "i" = "Coefs    : {n_coefs}",
        "i" = "EPV      : {round(epv, 1)}"
    ))

    if (epv < 10) {
        cli::cli_warn(c(
            "!" = "EPV = {round(epv, 1)} < 10 — model likely overfit.",
            "i" = "Consider reducing predictors or using penalised regression."
        ))
    } else if (epv < 20) {
        cli::cli_inform(c("!" = "EPV = {round(epv, 1)} adequate (10–20). Aim for > 20 when possible."))
    } else {
        cli::cli_inform(c("v" = "EPV = {round(epv, 1)} — adequate covariate coverage."))
    }

    c(n_events = n_events, n_coefs = n_coefs, epv = epv)
}


# =============================================================================
# SECTION 3C — Global model tests (LR, Wald, Score)
# =============================================================================

#' Extract and report the three global tests from a fitted coxph model.
#'
#' For CC: directly from summary(fit).
#' For MICE: averaged across imputed models (LR and Score tests are not
#' formally poolable via Rubin's rules; averaging is an approximation).
#' The likelihood ratio test is preferred when available.
#'
#' @param fit   A \code{coxph} object or a list of \code{coxph} objects (MICE).
#' @param label Character label for messaging.
#' @return Named numeric: lr_chisq, lr_df, lr_p, wald_chisq, wald_df, wald_p,
#'         score_chisq, score_df, score_p.
#' @keywords internal
.global_model_tests <- function(fit, label = "") {

    cli::cli_h3("Global model tests [{label}]")

    fits <- if (is.list(fit) && !inherits(fit, "coxph")) fit else list(fit)

    extract_tests <- function(f) {
        s <- summary(f)
        c(
            lr_chisq    = unname(s$logtest["test"]),
            lr_df       = unname(s$logtest["df"]),
            lr_p        = unname(s$logtest["pvalue"]),
            wald_chisq  = unname(s$waldtest["test"]),
            wald_df     = unname(s$waldtest["df"]),
            wald_p      = unname(s$waldtest["pvalue"]),
            score_chisq = unname(s$sctest["test"]),
            score_df    = unname(s$sctest["df"]),
            score_p     = unname(s$sctest["pvalue"])
        )
    }

    vals <- do.call(rbind, lapply(fits, extract_tests))
    out  <- colMeans(vals, na.rm = TRUE)

    cli::cli_inform(c(
        "i" = "Likelihood ratio test : χ²({round(out['lr_df'])}) = {round(out['lr_chisq'],2)}, p = {signif(out['lr_p'],3)}  ← preferred",
        "i" = "Wald test             : χ²({round(out['wald_df'])}) = {round(out['wald_chisq'],2)}, p = {signif(out['wald_p'],3)}",
        "i" = "Score (log-rank) test : χ²({round(out['score_df'])}) = {round(out['score_chisq'],2)}, p = {signif(out['score_p'],3)}"
    ))

    out
}


# =============================================================================
# SECTION 3D — Harrell's C-index
# =============================================================================

#' Compute Harrell's concordance index (C-index) for a fitted Cox model.
#'
#' Uses \code{survival::concordance()} which correctly handles counting-process
#' (start–stop) data for time-dependent covariates. For MICE, the C-index is
#' computed on each imputed dataset and averaged (simple mean — pooling via
#' Rubin's rules is not defined for the C-index).
#'
#' @param fit      A \code{coxph} object (CC) or a list of \code{coxph} objects (MICE).
#' @param label    Character label for messaging.
#' @return Named numeric: \code{c_index}, \code{se}, \code{lower}, \code{upper}.
#' @keywords internal
.compute_cindex <- function(fit, label = "") {

    cli::cli_h3("C-index (Harrell's concordance) [{label}]")

    fits <- if (is.list(fit) && !inherits(fit, "coxph")) fit else list(fit)

    c_vals <- vapply(fits, function(f) {
        tryCatch(
            survival::concordance(f)$concordance,
            error = function(e) NA_real_
        )
    }, numeric(1L))

    se_vals <- vapply(fits, function(f) {
        tryCatch(
            sqrt(survival::concordance(f)$var),
            error = function(e) NA_real_
        )
    }, numeric(1L))

    c_mean  <- mean(c_vals,  na.rm = TRUE)
    se_mean <- mean(se_vals, na.rm = TRUE)
    lower   <- c_mean - 1.96 * se_mean
    upper   <- c_mean + 1.96 * se_mean

    cli::cli_inform(c(
        "i" = "C-index : {round(c_mean, 3)} (95% CI: {round(lower,3)}–{round(upper,3)})",
        "i" = "SE      : {round(se_mean, 4)}"
    ))

    if (c_mean < 0.6) {
        cli::cli_inform(c("!" = "C-index < 0.60 — limited discriminative ability (expected in epidemiology)."))
    } else if (c_mean < 0.7) {
        cli::cli_inform(c("i" = "C-index 0.60–0.70 — acceptable discrimination."))
    } else {
        cli::cli_inform(c("v" = "C-index >= 0.70 — good discrimination."))
    }

    c(c_index = c_mean, se = se_mean, lower_95 = lower, upper_95 = upper)
}


# =============================================================================
# SECTION 4 — Complete-case model fitting
# =============================================================================

#' Fit unadjusted and adjusted Cox models for the CC route.
#' @keywords internal
.fit_cox_cc <- function(
        surv_data, covariate_type, dairy_type, dairy_col,
        covariates, interaction_var, death_col = NULL, out_dir
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

    # ── EPV (uses actual coefficient count from fitted model) ──────────────
    n_events <- sum(surv_data$event == 1L, na.rm = TRUE)
    epv      <- .compute_epv(fit_adj, n_events, label = "cc")

    # ── C-index ────────────────────────────────────────────────────────────
    cindex <- .compute_cindex(fit_adj, label = "cc")

    # ── Global model tests ─────────────────────────────────────────────────
    global_tests <- .global_model_tests(fit_adj, label = "cc")

    # ── Person-years ───────────────────────────────────────────────────────
    person_years <- if (!is.null(attr(surv_data, "person_years"))) {
        attr(surv_data, "person_years")
    } else if ("time" %in% names(surv_data)) {
        sum(surv_data$time, na.rm = TRUE)
    } else if (all(c("age_start", "age_stop") %in% names(surv_data))) {
        sum(surv_data$age_stop - surv_data$age_start, na.rm = TRUE)
    } else NA_real_
    cli::cli_inform(c("i" = "Total person-years: {round(person_years, 1)}"))

    # ── Tidy results -------------------------------------------------------
    results_unadj <- .tidy_cox(fit_unadj)
    results_adj   <- .tidy_cox(fit_adj)

    cli::cli_h2("Adjusted model results")
    cli::cli_inform(paste(capture.output(print(results_adj)), collapse = "\n"))

    # ── Assumption checks --------------------------------------------------
    assumptions <- .check_cox_assumptions(
        fit        = fit_adj,
        surv_data  = surv_data,
        out_dir    = out_dir,
        label      = "cc_adjusted"
    )
    
    # ── KM plot — always produced; tertiles used when dairy is continuous ───────
    km_out <- .km_plot_dairy(
        surv_data      = surv_data,
        covariate_type = covariate_type,
        dairy_type     = dairy_type,
        dairy_col      = dairy_col,
        out_dir        = out_dir,
        label          = "cc"
    )
    km_plot    <- km_out$plot
    km_logrank <- km_out$logrank_p

    # ── Adjusted scenario curves (time-dependent only) ─────────────────────
    scenario_plot_cc <- if (covariate_type == "time_dependent") {
        .plot_scenario_survival(
            fit        = fit_adj,
            surv_data  = surv_data,
            dairy_col  = dairy_col,
            covariates = covariates,
            out_dir    = out_dir,
            label      = "cc"
        )
    } else NULL

    # ── Fine-Gray sensitivity analysis (competing risk of death) ──────────────
    fg_cc <- if (!is.null(death_col)) {
        .fit_finegray_cc(
            surv_data      = surv_data,
            covariate_type = covariate_type,
            dairy_type     = dairy_type,
            covariates     = covariates,
            interaction_var = interaction_var,
            death_col      = death_col,
            out_dir        = out_dir
        )
    } else NULL

    # ── Save results -----------------------------------------------------------
    .save_model_results(results_unadj, results_adj, out_dir, label = "cc")

    list(
        config_epv          = epv,
        c_index             = cindex,
        global_tests        = global_tests,
        person_years        = person_years,
        fit_unadj           = fit_unadj,
        fit_adj             = fit_adj,
        results_unadj       = results_unadj,
        results_adj         = results_adj,
        ph_test             = assumptions$ph_test,
        ph_plot             = assumptions$ph_plot,
        vif                 = assumptions$vif,
        linearity_plots     = assumptions$linearity_plots,
        outlier_plot        = assumptions$outlier_plot,
        outlier_flagged     = assumptions$outlier_flagged_rows,
        dfbeta_plot         = assumptions$dfbeta_plot,
        dfbeta_flagged      = assumptions$dfbeta_flagged,
        dfbeta_flag_detail  = assumptions$dfbeta_flag_detail,
        cox_snell_plot      = assumptions$cox_snell_plot,
        km_plot             = km_plot,
        km_logrank_p        = km_logrank,
        scenario_plot       = scenario_plot_cc,
        fg_results          = fg_cc$results,
        fg_fit              = fg_cc$fit,
        cif_plot            = fg_cc$cif_plot
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
        covariates, interaction_var, death_col = NULL, out_dir
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
    cli::cli_inform(paste(capture.output(print(results_adj)), collapse = "\n"))
    
    # ── EPV across MICE imputations ─────────────────────────────────────────
    # Events are identical across imputations (outcomes are not imputed).
    # Coefficient count comes from the first fitted model.
    n_events_mice <- sum(surv_data_list[[1L]]$event == 1L, na.rm = TRUE)
    epv           <- .compute_epv(fits_adj[[1L]], n_events_mice, label = "mice (all imputations)")

    # ── C-index (averaged across imputations) ───────────────────────────────
    cindex <- .compute_cindex(fits_adj, label = "mice (averaged)")

    # ── Global model tests (averaged across imputations) ────────────────────
    global_tests <- .global_model_tests(fits_adj, label = "mice (averaged)")

    # ── Person-years (summed across all imputed datasets, divided by m) ─────
    person_years <- {
        py_vals <- vapply(surv_data_list, function(d) {
            if (all(c("age_start", "age_stop") %in% names(d)))
                sum(d$age_stop - d$age_start, na.rm = TRUE)
            else NA_real_
        }, numeric(1L))
        mean(py_vals, na.rm = TRUE)
    }
    cli::cli_inform(c("i" = "Total person-years (mean across imputations): {round(person_years, 1)}"))

    # ── Assumption checks on first imputed dataset (representative) --------
    # KM also uses imp = 1 as the representative dataset (see §RUBIN'S RULES note).
    assumptions <- .check_cox_assumptions(
        fit       = fits_adj[[1L]],
        surv_data = surv_data_list[[1L]],
        out_dir   = out_dir,
        label     = "mice_imp1"
    )

    # ── KM plot on imp = 1 (representative dataset) — always produced ──────
    km_out <- .km_plot_dairy(
        surv_data      = surv_data_list[[1L]],   # imp = 1
        covariate_type = covariate_type,
        dairy_type     = dairy_type,
        dairy_col      = dairy_col,
        out_dir        = out_dir,
        label          = "mice_imp1"
    )
    km_plot    <- km_out$plot
    km_logrank <- km_out$logrank_p

    # ── Adjusted scenario curves (time-dependent only, on imp = 1) ────────────
    scenario_plot_mice <- if (covariate_type == "time_dependent") {
        .plot_scenario_survival(
            fit        = fits_adj[[1L]],
            surv_data  = surv_data_list[[1L]],
            dairy_col  = dairy_col,
            covariates = covariates,
            out_dir    = out_dir,
            label      = "mice_imp1"
        )
    } else NULL

    # ── Fine-Gray sensitivity analysis across imputations ─────────────────────
    fg_mice <- if (!is.null(death_col)) {
        .fit_finegray_mice(
            surv_data_list  = surv_data_list,
            covariate_type  = covariate_type,
            dairy_type      = dairy_type,
            covariates      = covariates,
            interaction_var = interaction_var,
            death_col       = death_col,
            out_dir         = out_dir
        )
    } else NULL

    .save_model_results(results_unadj, results_adj, out_dir, label = "mice")

    list(
        surv_data           = surv_data_list[[1L]],
        config_epv          = epv,
        c_index             = cindex,
        global_tests        = global_tests,
        person_years        = person_years,
        fit_unadj           = mira_unadj,
        fit_adj             = mira_adj,
        results_unadj       = results_unadj,
        results_adj         = results_adj,
        ph_test             = assumptions$ph_test,
        ph_plot             = assumptions$ph_plot,
        vif                 = assumptions$vif,
        linearity_plots     = assumptions$linearity_plots,
        outlier_plot        = assumptions$outlier_plot,
        outlier_flagged     = assumptions$outlier_flagged_rows,
        dfbeta_plot         = assumptions$dfbeta_plot,
        dfbeta_flagged      = assumptions$dfbeta_flagged,
        dfbeta_flag_detail  = assumptions$dfbeta_flag_detail,
        cox_snell_plot      = assumptions$cox_snell_plot,
        km_plot             = km_plot,
        km_logrank_p        = km_logrank,
        scenario_plot       = scenario_plot_mice,
        fg_results          = fg_mice$results,
        fg_fit              = fg_mice$fit,
        cif_plot            = fg_mice$cif_plot
    )
}


# =============================================================================
# SECTION 4B — Fine-Gray competing risk (sensitivity analysis)
# =============================================================================
#
# Fine-Gray subdistribution hazard model (Gray 1988; Fine & Gray 1999).
# Treats death as a competing event for sarcopenia.
#
# fstatus coding:
#   0 = censored (no sarcopenia, no death before end of follow-up)
#   1 = sarcopenia (event of interest)
#   2 = death (competing event)
#
# Uses survival::finegray() to expand the dataset into the pseudo-risk-set
# weights (fgwt), then fits a weighted coxph on the Fine-Gray pseudo-data.
#
# =============================================================================

#' Build the Fine-Gray pseudo-dataset from a survival data frame.
#'
#' @param surv_data  data.frame with columns: time (or age_stop for td),
#'                   event (sarcopenia 0/1), and death_col (death 0/1).
#' @param death_col  Character. Column name for all-cause death indicator.
#' @param covariate_type "fixed" or "time_dependent".
#' @return data.frame ready for weighted coxph (columns: fgstart, fgstop, fgstatus, fgwt, …).
#' @keywords internal
.build_finegray_data <- function(surv_data, death_col, covariate_type) {

    if (!death_col %in% names(surv_data))
        cli::cli_abort("death_col {.col {death_col}} not found in surv_data.")

    # For time-dependent data we use age_stop as the event time.
    time_col <- if (covariate_type == "time_dependent") "age_stop" else "time"

    if (!time_col %in% names(surv_data))
        cli::cli_abort("Time column {.col {time_col}} not found in surv_data.")

    # Build fstatus: 0 = censored, 1 = sarcopenia, 2 = death
    surv_data$fstatus <- dplyr::case_when(
        surv_data$event == 1L                             ~ 1L,
        !is.na(surv_data[[death_col]]) &
            surv_data[[death_col]] == 1L &
            surv_data$event == 0L                         ~ 2L,
        TRUE                                               ~ 0L
    )

    # Build the multi-state Surv object and expand via finegray
    fg_formula <- as.formula(
        paste0("survival::Surv(", time_col, ", factor(fstatus)) ~ 1")
    )

    tryCatch(
        survival::finegray(fg_formula, data = surv_data, etype = 1L),
        error = function(e) cli::cli_abort(
            "finegray() failed: {conditionMessage(e)}"
        )
    )
}


#' Fit Fine-Gray model for the complete-case route.
#' @keywords internal
.fit_finegray_cc <- function(
        surv_data, covariate_type, dairy_type, covariates,
        interaction_var, death_col, out_dir
) {
    cli::cli_h2("Fine-Gray competing risk — CC route")

    fg_data <- tryCatch(
        .build_finegray_data(surv_data, death_col, covariate_type),
        error = function(e) {
            cli::cli_warn("Fine-Gray data build failed: {conditionMessage(e)}")
            return(NULL)
        }
    )
    if (is.null(fg_data)) return(list(results = NULL, fit = NULL, cif_plot = NULL))

    # Carry over dairy_exposure from surv_data (finegray keeps original rows +
    # adds pseudo-obs; match on row index stored as id).
    # finegray returns a column "id" if the data had rownames; otherwise row order.
    dairy_exposure_vec <- surv_data$dairy_exposure
    if ("id" %in% names(fg_data)) {
        fg_data$dairy_exposure <- dairy_exposure_vec[fg_data$id]
    } else {
        fg_data$dairy_exposure <- dairy_exposure_vec[seq_len(nrow(fg_data))]
    }

    # Copy remaining covariates the same way
    for (cv in setdiff(covariates, names(fg_data))) {
        if (cv %in% names(surv_data)) {
            if ("id" %in% names(fg_data)) {
                fg_data[[cv]] <- surv_data[[cv]][fg_data$id]
            } else {
                fg_data[[cv]] <- surv_data[[cv]][seq_len(nrow(fg_data))]
            }
        }
    }

    # Fine-Gray formula: always uses fgstart/fgstop/fgstatus (produced by finegray)
    rhs <- .build_fg_rhs(dairy_type, covariates, interaction_var)
    fg_formula <- as.formula(
        paste0("survival::Surv(fgstart, fgstop, fgstatus) ~ ", rhs)
    )

    fg_fit <- tryCatch(
        survival::coxph(fg_formula, data = fg_data, weight = fgwt,
                        ties = "efron", model = TRUE, x = TRUE),
        error = function(e) {
            cli::cli_warn("Fine-Gray coxph failed: {conditionMessage(e)}")
            NULL
        }
    )

    if (is.null(fg_fit)) return(list(results = NULL, fit = NULL, cif_plot = NULL))

    fg_results <- .tidy_cox(fg_fit)
    cli::cli_h3("Fine-Gray results (SHR) — CC")
    cli::cli_inform(paste(capture.output(print(fg_results)), collapse = "\n"))

    cif_plot <- .plot_cif(surv_data, death_col, covariate_type,
                          dairy_type, out_dir, label = "cc")

    if (!is.null(out_dir)) {
        readr::write_csv(fg_results,
                         file.path(out_dir, "cc_finegray_results.csv"))
        cli::cli_inform("Fine-Gray CC results saved to {.path {out_dir}}")
    }

    list(results = fg_results, fit = fg_fit, cif_plot = cif_plot)
}


#' Fit Fine-Gray models across imputed datasets and pool.
#' @keywords internal
.fit_finegray_mice <- function(
        surv_data_list, covariate_type, dairy_type, covariates,
        interaction_var, death_col, out_dir
) {
    cli::cli_h2("Fine-Gray competing risk — MICE route")

    m <- length(surv_data_list)
    fg_fits <- vector("list", m)

    rhs <- .build_fg_rhs(dairy_type, covariates, interaction_var)
    fg_formula <- as.formula(
        paste0("survival::Surv(fgstart, fgstop, fgstatus) ~ ", rhs)
    )

    for (i in seq_len(m)) {
        cli::cli_inform("  Fine-Gray imp {i}/{m} ...")
        sd_i <- surv_data_list[[i]]

        fg_data <- tryCatch(
            .build_finegray_data(sd_i, death_col, covariate_type),
            error = function(e) {
                cli::cli_warn("  imp {i}: finegray failed — {conditionMessage(e)}")
                NULL
            }
        )
        if (is.null(fg_data)) next

        # Copy dairy_exposure and covariates into fg_data
        dairy_exposure_vec <- sd_i$dairy_exposure
        if ("id" %in% names(fg_data)) {
            fg_data$dairy_exposure <- dairy_exposure_vec[fg_data$id]
        } else {
            fg_data$dairy_exposure <- dairy_exposure_vec[seq_len(nrow(fg_data))]
        }
        for (cv in setdiff(covariates, names(fg_data))) {
            if (cv %in% names(sd_i)) {
                if ("id" %in% names(fg_data)) {
                    fg_data[[cv]] <- sd_i[[cv]][fg_data$id]
                } else {
                    fg_data[[cv]] <- sd_i[[cv]][seq_len(nrow(fg_data))]
                }
            }
        }

        fg_fits[[i]] <- tryCatch(
            survival::coxph(fg_formula, data = fg_data, weight = fgwt,
                            ties = "efron", model = TRUE, x = TRUE),
            error = function(e) {
                cli::cli_warn("  imp {i}: Fine-Gray coxph failed — {conditionMessage(e)}")
                NULL
            }
        )
    }

    fg_fits_ok <- Filter(Negate(is.null), fg_fits)
    if (length(fg_fits_ok) == 0L) {
        cli::cli_warn("All Fine-Gray imputations failed.")
        return(list(results = NULL, fit = NULL, cif_plot = NULL))
    }

    pooled_fg   <- mice::pool(mice::as.mira(fg_fits_ok))
    fg_results  <- .tidy_pooled(pooled_fg)

    cli::cli_h3("Fine-Gray pooled results (SHR) — MICE")
    cli::cli_inform(paste(capture.output(print(fg_results)), collapse = "\n"))

    # CIF plot on imp = 1 dataset
    cif_plot <- .plot_cif(surv_data_list[[1L]], death_col, covariate_type,
                          dairy_type, out_dir, label = "mice_imp1")

    if (!is.null(out_dir)) {
        readr::write_csv(fg_results,
                         file.path(out_dir, "mice_finegray_results.csv"))
        cli::cli_inform("Fine-Gray MICE results saved to {.path {out_dir}}")
    }

    list(results = fg_results, fit = fg_fits_ok[[1L]], cif_plot = cif_plot)
}


#' Build the RHS string for a Fine-Gray formula.
#' @keywords internal
.build_fg_rhs <- function(dairy_type, covariates, interaction_var) {
    dairy_term <- "dairy_exposure"
    dairy_main <- if (!is.null(interaction_var)) {
        paste0(dairy_term, " * ", interaction_var)
    } else {
        dairy_term
    }
    if (length(covariates) == 0L) return(dairy_main)
    adj_vars <- setdiff(covariates, interaction_var)
    paste(c(dairy_main, adj_vars), collapse = " + ")
}


#' Plot Cumulative Incidence Functions (CIF) for sarcopenia and death.
#'
#' Uses a multi-state survival object to estimate CIFs for each competing event.
#' Stratifies by dairy quartile (continuous) or category (categorical).
#'
#' @keywords internal
.plot_cif <- function(surv_data, death_col, covariate_type,
                      dairy_type, out_dir, label) {

    cli::cli_h3("CIF plot [{label}]")

    tryCatch({
        time_col <- if (covariate_type == "time_dependent") "age_stop" else "time"

        if (!all(c(time_col, "event", death_col) %in% names(surv_data))) {
            cli::cli_warn("CIF plot: missing required columns — skipping.")
            return(NULL)
        }

        surv_data$fstatus <- dplyr::case_when(
            surv_data$event == 1L                                         ~ 1L,
            !is.na(surv_data[[death_col]]) & surv_data[[death_col]] == 1L
            & surv_data$event == 0L                                        ~ 2L,
            TRUE                                                            ~ 0L
        )

        # Stratification group
        if (dairy_type == "categorical") {
            surv_data$cif_group <- surv_data$dairy_exposure
        } else {
            if (!"dairy_exposure" %in% names(surv_data)) {
                cli::cli_warn("CIF plot: dairy_exposure missing — skipping.")
                return(NULL)
            }
            breaks <- stats::quantile(surv_data$dairy_exposure,
                                      probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
            surv_data$cif_group <- cut(
                surv_data$dairy_exposure,
                breaks         = breaks,
                labels         = c("Q1 (low)", "Q2", "Q3", "Q4 (high)"),
                include.lowest = TRUE
            )
        }

        surv_data <- surv_data[!is.na(surv_data$cif_group), ]

        cif_formula <- as.formula(
            paste0("survival::Surv(", time_col, ", factor(fstatus)) ~ cif_group")
        )

        cif_fit <- survival::survfit(cif_formula, data = surv_data)
        p_cif <- plot(cif_fit, col = c("#E76254FF","#FFD06FFF","#72BCD5FF","#92D050"),
                      xlab = if (covariate_type == "time_dependent") "Age (years)" else "Time",
                      ylab = "Cumulative incidence",
                      main = paste0("CIF — sarcopenia vs death [", label, "]"),
                      lty  = 1)

        if (!is.null(out_dir)) {
            png(file.path(out_dir, paste0(label, "_cif.png")),
                width = 900, height = 600, res = 120)
            plot(cif_fit,
                 col  = c("#E76254FF","#FFD06FFF","#72BCD5FF","#92D050"),
                 xlab = if (covariate_type == "time_dependent") "Age (years)" else "Time",
                 ylab = "Cumulative incidence",
                 main = paste0("CIF [", label, "]"),
                 lty  = 1)
            legend("topleft",
                   legend = levels(surv_data$cif_group),
                   col    = c("#E76254FF","#FFD06FFF","#72BCD5FF","#92D050"),
                   lty    = 1, bty = "n", cex = 0.8)
            grDevices::dev.off()
            cli::cli_inform("CIF plot saved to {.path {out_dir}}")
        }

        invisible(cif_fit)

    }, error = function(e) {
        cli::cli_warn("CIF plot failed: {conditionMessage(e)}")
        NULL
    })
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
#' Assumption                   Method                              Pass criterion
#' ─────────────────────────────────────────────────────────────────────────────
#' Proportional Hazards         cox.zph() Schoenfeld residuals      Global p > 0.05
#' Linearity                    Martingale residuals (null model)   Smooth ≈ linear
#'                              vs each continuous predictor
#' Outliers                     Deviance residuals & dfbeta         |dev| ≤ 3; no extreme dfbeta
#' Overall Fit                  Cox-Snell residuals                 Points on 45° diagonal
#' Multicollinearity            VIF / GVIF                          VIF < 5 (< 10 relaxed)
#'
#' @keywords internal
.check_cox_assumptions <- function(fit, surv_data, out_dir, label) {

    cli::cli_h2("Assumption checks [{label}]")

    results <- list()

    # ── 1. Proportional hazards — cox.zph() + Schoenfeld plots ─────────────────
    cli::cli_h3("1. Proportional Hazards — cox.zph() | pass: global p > 0.05")
    tryCatch({
        ph_test <- survival::cox.zph(fit)
        results$ph_test <- ph_test

        cli::cli_inform(paste(capture.output(print(ph_test)), collapse = "\n"))

        global_p <- ph_test$table["GLOBAL", "p"]
        if (global_p < 0.05) {
            cli::cli_warn(c(
                "!" = "Global PH test SIGNIFICANT (p = {round(global_p, 4)}) — PH violated.",
                "i" = "Consider: stratification, time × covariate interaction, or tt()."
            ))
        } else {
            cli::cli_inform(c("v" = "PH assumption not violated (global p = {round(global_p, 4)})"))
        }

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

    # ── 2. Linearity — martingale residuals from null model vs continuous covariates
    # Plots martingale residuals of a null (intercept-only) Cox model against each
    # continuous predictor. A smoothed LOESS line that is approximately linear and
    # centred around 0 indicates no non-linearity.
    cli::cli_h3("2. Linearity — martingale (null model) vs continuous predictors | pass: smooth ≈ linear")
    tryCatch({
        # Build null formula as a string to avoid bare-symbol environment issues.
        # Use fit$model (stored because model = TRUE) to identify the time columns.
        mf_names  <- names(fit$model)
        is_surv   <- vapply(fit$model, inherits, logical(1L), "Surv")
        surv_name <- mf_names[is_surv][1L]   # e.g. "Surv(time, event)"

        null_formula_str <- if (grepl("age_start", surv_name, fixed = TRUE)) {
            "survival::Surv(age_start, age_stop, event) ~ 1"
        } else {
            "survival::Surv(time, event) ~ 1"
        }
        null_formula <- as.formula(null_formula_str)

        fit_null  <- survival::coxph(null_formula, data = surv_data, ties = "efron")
        mart_null <- residuals(fit_null, type = "martingale")

        # Identify continuous predictors using the stored model frame (avoids terms()).
        pred_names <- mf_names[!is_surv]
        cont_vars  <- pred_names[vapply(pred_names, function(v) {
            v %in% names(surv_data) && is.numeric(surv_data[[v]])
        }, logical(1L))]

        if (length(cont_vars) == 0L) {
            cli::cli_inform("No continuous predictors found for linearity plots.")
            results$linearity_plots <- NULL
        } else {
            lin_plots <- purrr::map(cont_vars, function(v) {
                df_lin <- data.frame(x = surv_data[[v]], mart = mart_null)
                df_lin <- df_lin[!is.na(df_lin$x), ]
                ggplot2::ggplot(df_lin, ggplot2::aes(x = x, y = mart)) +
                    ggplot2::geom_point(alpha = 0.35, size = 1) +
                    ggplot2::geom_smooth(method = "loess", se = TRUE,
                                        colour = "#e15759", linewidth = 1) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
                    ggplot2::labs(
                        title = glue::glue("Linearity: martingale vs {v} [{label}]"),
                        x = v, y = "Martingale residual (null model)"
                    ) +
                    ggplot2::theme_bw()
            })
            names(lin_plots) <- cont_vars
            results$linearity_plots <- lin_plots

            if (!is.null(out_dir)) {
                purrr::iwalk(lin_plots, function(p, nm) {
                    safe_nm <- gsub("[^A-Za-z0-9_]", "_", nm)
                    ggplot2::ggsave(
                        file.path(out_dir, glue::glue("{label}_linearity_{safe_nm}.png")),
                        p, width = 7, height = 5, dpi = 150
                    )
                })
                cli::cli_inform("Linearity plots saved to {.path {out_dir}}")
            }
        }
    }, error = function(e) {
        cli::cli_warn("Linearity plots failed: {conditionMessage(e)}")
        results$linearity_plots <<- NULL
    })

    # ── 3. Outliers — deviance residuals (|dev| > 3) + martingale ──────────────
    cli::cli_h3("3. Outliers — deviance & martingale residuals | pass: |dev| ≤ 3")
    tryCatch({
        mart_resid <- residuals(fit, type = "martingale")
        dev_resid  <- residuals(fit, type = "deviance")
        obs_idx    <- seq_along(dev_resid)

        n_extreme_dev <- sum(abs(dev_resid) > 3, na.rm = TRUE)
        cli::cli_inform(c(
            "i" = "{n_extreme_dev} observation(s) with |deviance residual| > 3."
        ))
        if (n_extreme_dev > 0L) {
            cli::cli_warn(c("!" = "{n_extreme_dev} potential outlier(s) — inspect {label}_outlier_flagged.csv."))
        } else {
            cli::cli_inform(c("v" = "No extreme deviance residuals (all |dev| ≤ 3)."))
        }

        # Flag rows with |dev| > 3 as the primary outlier criterion
        outlier_flag <- abs(dev_resid) > 3
        flagged_rows <- surv_data[which(outlier_flag), , drop = FALSE]
        results$outlier_flagged_rows <- flagged_rows

        df_resid <- data.frame(obs = obs_idx, martingale = mart_resid, deviance = dev_resid)

        p_mart <- ggplot2::ggplot(df_resid, ggplot2::aes(x = obs, y = martingale)) +
            ggplot2::geom_point(alpha = 0.4, size = 1.2) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
            ggplot2::labs(title = glue::glue("Martingale residuals [{label}]"),
                          x = "Observation", y = "Martingale residual") +
            ggplot2::theme_bw()

        df_resid$outlier <- abs(df_resid$deviance) > 3

        p_dev <- ggplot2::ggplot(df_resid,
                                 ggplot2::aes(x = obs, y = deviance, colour = outlier)) +
            ggplot2::geom_point(alpha = 0.4, size = 1.2) +
            ggplot2::scale_colour_manual(values = c("FALSE" = "grey40", "TRUE" = "#e15759"),
                                         guide = "none") +
            ggplot2::geom_hline(yintercept =  3, linetype = "dashed", colour = "red") +
            ggplot2::geom_hline(yintercept = -3, linetype = "dashed", colour = "red") +
            ggplot2::geom_hline(yintercept =  0, linetype = "dotted") +
            ggplot2::labs(title = glue::glue("Deviance residuals [{label}] — red: |dev| > 3"),
                          x = "Observation", y = "Deviance residual") +
            ggplot2::theme_bw()

        results$outlier_plot <- list(martingale = p_mart, deviance = p_dev)

        if (!is.null(out_dir)) {
            ggplot2::ggsave(file.path(out_dir, glue::glue("{label}_martingale.png")),
                            p_mart, width = 7, height = 5, dpi = 150)
            ggplot2::ggsave(file.path(out_dir, glue::glue("{label}_deviance.png")),
                            p_dev,  width = 7, height = 5, dpi = 150)
            if (nrow(flagged_rows) > 0L)
                readr::write_csv(flagged_rows,
                                 file.path(out_dir, glue::glue("{label}_outlier_flagged.csv")))
        }
    }, error = function(e) {
        cli::cli_warn("Outlier plots failed: {conditionMessage(e)}")
        results$outlier_plot         <<- NULL
        results$outlier_flagged_rows <<- NULL
    })

    # ── 4. Influential observations — standardised DFBetas ──────────────────────
    # Harrell cutoff: |dfbeta_std| > 0.2
    cli::cli_h3("4. Influential observations — std. DFBetas | pass: no extreme spikes")
    tryCatch({
        dfbetas_mat <- residuals(fit, type = "dfbetas")
        cutoff      <- 0.2

        dfb_plots <- purrr::map(seq_len(ncol(dfbetas_mat)), function(j) {
            coef_name     <- colnames(dfbetas_mat)[j]
            df_plot       <- data.frame(
                obs        = seq_len(nrow(dfbetas_mat)),
                dfbeta_std = dfbetas_mat[, j],
                flagged    = abs(dfbetas_mat[, j]) > cutoff
            )
            n_influential <- sum(df_plot$flagged)

            ggplot2::ggplot(df_plot,
                            ggplot2::aes(x = obs, y = dfbeta_std, fill = flagged)) +
                ggplot2::geom_bar(stat = "identity", width = 0.6) +
                ggplot2::scale_fill_manual(
                    values = c("FALSE" = "#4e79a7", "TRUE" = "#e15759"),
                    guide  = "none"
                ) +
                ggplot2::geom_hline(yintercept =  cutoff, linetype = "dashed",
                                    colour = "red", linewidth = 0.7) +
                ggplot2::geom_hline(yintercept = -cutoff, linetype = "dashed",
                                    colour = "red", linewidth = 0.7) +
                ggplot2::annotate("text", x = Inf, y = cutoff,
                                  label = paste0("n > |0.2|: ", n_influential),
                                  hjust = 1.1, vjust = -0.4, size = 3) +
                ggplot2::labs(
                    title = glue::glue("Std. DFBeta: {coef_name} [{label}]"),
                    x = "Observation", y = "Standardised DFBeta"
                ) +
                ggplot2::theme_bw()
        })
        names(dfb_plots) <- colnames(dfbetas_mat)
        results$dfbeta_plot <- dfb_plots

        exceed_any <- apply(abs(dfbetas_mat) > cutoff, 1, any)
        flagged_dfb <- data.frame(obs = which(exceed_any),
                                  surv_data[which(exceed_any), , drop = FALSE])

        dfb_flag_detail <- purrr::map_dfr(seq_len(ncol(dfbetas_mat)), function(j) {
            coef_name <- colnames(dfbetas_mat)[j]
            flagged   <- abs(dfbetas_mat[, j]) > cutoff
            if (!any(flagged)) return(NULL)
            surv_data[flagged, , drop = FALSE] |>
                dplyr::mutate(coefficient = coef_name,
                              dfbeta_std  = dfbetas_mat[flagged, j])
        })

        results$dfbeta_flagged     <- flagged_dfb
        results$dfbeta_flag_detail <- dfb_flag_detail

        cli::cli_inform(c("i" = "{sum(exceed_any)} obs exceed |DFBeta_std| > {cutoff}."))

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

    # ── 5. Overall Fit — Cox-Snell residuals plot ───────────────────────────────
    # Cox-Snell residuals r_i = -log(S_hat(t_i)). Under a correctly specified model
    # these follow Exp(1), so a KM curve of 1 - KM(r_i) plotted against r_i should
    # fall on the 45° diagonal (y = x line).
    cli::cli_h3("5. Overall Fit — Cox-Snell residuals | pass: points on 45° diagonal")
    tryCatch({
        # Build Cox-Snell residuals and put them in a data frame so survfit
        # can resolve names without bare-symbol environment issues.
        cs_df  <- data.frame(
            cs_resid = surv_data$event - residuals(fit, type = "martingale"),
            status   = surv_data$event
        )
        cs_df  <- cs_df[!is.na(cs_df$cs_resid), ]

        km_cs <- survival::survfit(survival::Surv(cs_resid, status) ~ 1, data = cs_df)
        df_cs <- data.frame(
            cs   = km_cs$time,
            surv = km_cs$surv
        )

        p_cs <- ggplot2::ggplot(df_cs, ggplot2::aes(x = cs, y = -log(surv))) +
            ggplot2::geom_step(colour = "#4e79a7", linewidth = 0.8) +
            ggplot2::geom_abline(intercept = 0, slope = 1,
                                 linetype = "dashed", colour = "red") +
            ggplot2::labs(
                title    = glue::glue("Cox-Snell residuals [{label}]"),
                subtitle = "Dashed: 45° reference (ideal fit). Steps should follow the line.",
                x        = "Cox-Snell residual",
                y        = expression(-log(hat(S)(r[i])))
            ) +
            ggplot2::theme_bw()

        results$cox_snell_plot <- p_cs

        if (!is.null(out_dir)) {
            ggplot2::ggsave(file.path(out_dir, glue::glue("{label}_cox_snell.png")),
                            p_cs, width = 7, height = 5, dpi = 150)
            cli::cli_inform("Cox-Snell plot saved to {.path {out_dir}}")
        }

    }, error = function(e) {
        cli::cli_warn("Cox-Snell plot failed: {conditionMessage(e)}")
        results$cox_snell_plot <<- NULL
    })

    # ── 6. Collinearity — VIF / GVIF ────────────────────────────────────────────
    # Pass: VIF < 5 (strict) or < 10 (relaxed). For categorical predictors
    # car returns GVIF^(1/(2*Df)); flag if > sqrt(5) ≈ 2.24.
    cli::cli_h3("6. Multicollinearity — VIF | pass: VIF < 5 (< 10 relaxed)")
    tryCatch({
        vif_vals <- car::vif(fit)
        results$vif <- vif_vals

        cli::cli_inform(paste(capture.output(print(round(vif_vals, 3))), collapse = "\n"))

        if (is.matrix(vif_vals)) {
            gvif_scaled <- vif_vals[, "GVIF^(1/(2*Df))"]
            high_vif <- names(gvif_scaled[gvif_scaled > 2.24])
        } else {
            high_vif <- names(vif_vals[vif_vals > 5])
        }

        if (length(high_vif) > 0L) {
            cli::cli_warn(c(
                "!" = "High collinearity: {.val {high_vif}}",
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

#' Kaplan-Meier survival curves stratified by dairy exposure.
#'
#' For categorical dairy: uses the existing \code{dairy_exposure} factor.
#' For continuous dairy: creates tertiles of \code{dairy_col} for display
#' (the Cox model itself still uses the continuous variable).
#'
#' @param surv_data      data.frame with event and time columns.
#' @param covariate_type "fixed" or "time_dependent".
#' @param dairy_type     "continuous" or "categorical".
#' @param dairy_col      Column name for continuous dairy (used to make tertiles).
#' @param out_dir        Output directory or NULL.
#' @param label          String label for file names.
#' @keywords internal
.km_plot_dairy <- function(surv_data, covariate_type, dairy_type, dairy_col,
                           out_dir, label) {

    cli::cli_h3("Kaplan-Meier plot by dairy category [{label}]")

    if (!is.data.frame(surv_data)) return(NULL)

    # ── Derive KM stratification variable -----------------------------------
    if (dairy_type == "categorical") {
        if (!"dairy_exposure" %in% names(surv_data)) {
            cli::cli_warn("dairy_exposure column not found — skipping KM plot.")
            return(NULL)
        }
        surv_data$km_group <- surv_data$dairy_exposure
        legend_title       <- "Dairy intake"

    } else {
        # Continuous dairy: tertiles for KM visualisation only
        if (!dairy_col %in% names(surv_data)) {
            cli::cli_warn("{.col {dairy_col}} not found — skipping KM plot.")
            return(NULL)
        }
        breaks <- stats::quantile(surv_data[[dairy_col]],
                                  probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        surv_data$km_group <- cut(
            surv_data[[dairy_col]],
            breaks         = breaks,
            labels         = c("Q1 (low)", "Q2", "Q3", "Q4 (high)"),
            include.lowest = TRUE
        )
        legend_title <- glue::glue(
            "Dairy quartile ({dairy_col})\n",
            "Q1 ≤{round(breaks[2],1)}  ",
            "Q2 ≤{round(breaks[3],1)}  ",
            "Q3 ≤{round(breaks[4],1)}  ",
            "Q4 >{round(breaks[4],1)} g/day"
        )
        cli::cli_inform(c("i" = "Continuous dairy split into quartiles for KM visualisation."))
    }

    surv_data <- surv_data[!is.na(surv_data$km_group), ]
    if (nrow(surv_data) == 0L) return(NULL)

    # For the time-dependent route, reduce to one row per participant using the
    # first interval. KM curves require fixed group membership — the cumulative-
    # average dairy used as the time-varying exposure in the Cox model changes
    # per visit, so we stratify by its baseline (first-interval) value instead.
    # The Cox model itself is unaffected; this is visualisation only.
    if (covariate_type == "time_dependent") {
        if (!"pt" %in% names(surv_data)) {
            cli::cli_warn("KM (td): no 'pt' column — skipping plot.")
            return(NULL)
        }
        # Baseline dairy quartile (first interval) — fixed group for KM strata.
        baseline_group <- surv_data |>
            dplyr::arrange(pt, age_start) |>
            dplyr::group_by(pt) |>
            dplyr::slice(1L) |>
            dplyr::ungroup() |>
            dplyr::select(pt, km_group)

        # Event status and final time come from the LAST interval per participant.
        # In counting-process format, event = 1 only appears on the last row.
        outcome_per_pt <- surv_data |>
            dplyr::arrange(pt, age_start) |>
            dplyr::group_by(pt) |>
            dplyr::slice_tail(n = 1L) |>
            dplyr::ungroup() |>
            dplyr::select(pt, time = age_stop, event)

        surv_data <- dplyr::inner_join(baseline_group, outcome_per_pt, by = "pt")
        cli::cli_inform(c("i" = "KM (td): baseline dairy quartile × final event status ({sum(surv_data$event)} events in {nrow(surv_data)} pts)."))
    }

    # Formula must use bare symbols (not as.formula(string_var)) so that
    # ggsurvplot can re-evaluate km_fit$call$formula from data = surv_data
    # without needing any local variable in scope.
    km_fit <- survival::survfit(survival::Surv(time, event) ~ km_group, data = surv_data)

    # ── Quartile range caption (placed below x-axis) ──────────────────────────
    if (dairy_type == "continuous") {
        quartile_caption <- glue::glue(
            "Q1 ≤{round(breaks[2],1)} g/day  |  ",
            "Q2 ≤{round(breaks[3],1)} g/day  |  ",
            "Q3 ≤{round(breaks[4],1)} g/day  |  ",
            "Q4 >{round(breaks[4],1)} g/day"
        )
    } else {
        quartile_caption <- NULL
    }

    # ── Helvetica theme ───────────────────────────────────────────────────────
    km_theme <- ggplot2::theme_bw(base_family = "Helvetica") +
        ggplot2::theme(
            # Axis text
            axis.text.x  = ggplot2::element_text(size = 14),
            axis.text.y  = ggplot2::element_text(size = 14),
            # Axis titles
            axis.title.x = ggplot2::element_text(size = 20, margin = ggplot2::margin(t = 6)),
            axis.title.y = ggplot2::element_text(size = 20, margin = ggplot2::margin(r = 6)),
            # Legend
            legend.title = ggplot2::element_text(size = 20),
            legend.text  = ggplot2::element_text(size = 20),
            legend.position = "right",
            # Caption for quartile ranges
            plot.caption = ggplot2::element_text(size = 11, hjust = 0.5,
                                                  family = "Helvetica")
        )

    km_obj <- survminer::ggsurvplot(
        fit              = km_fit,
        data             = surv_data,
        pval             = TRUE,
        pval.method      = FALSE,          # show p-value only, not method label
        conf.int         = TRUE,
        censor           = FALSE,          # remove censor marks
        risk.table       = TRUE,
        risk.table.y.text = FALSE,         # coloured bars instead of text labels
        risk.table.fontsize = 4,
        tables.theme     = ggplot2::theme_bw(base_family = "Helvetica") +
            ggplot2::theme(
                axis.text.x  = ggplot2::element_text(size = 14),
                axis.text.y  = ggplot2::element_text(size = 12),
                axis.title.y = ggplot2::element_text(size = 14)
            ),
        xlim             = c(50, 90),
        break.x.by      = 5,              # 5-year steps: 50, 55, 60 … 90
        ylim             = c(0, 1),
        break.y.by      = 0.2,            # 20% steps: 0, 0.2 … 1.0
        xlab             = "Age (years)",
        ylab             = "Sarcopenia-free probability",
        legend.title     = "Dairy quartile",
        legend.labs      = c("Q1 (low)", "Q2", "Q3", "Q4 (high)"),
        palette          = c("#E76254FF", "#FFD06FFF", "#72BCD5FF", "#92D050"),
        ggtheme          = km_theme,
        # Line thickness
        size             = 1.2
    )

    # ── Add quartile-range caption below x-axis ───────────────────────────────
    if (!is.null(quartile_caption)) {
        km_obj$plot <- km_obj$plot +
            ggplot2::labs(caption = quartile_caption)
    }

    # ── Combine plot + risk table ─────────────────────────────────────────────
    km_combined <- survminer::arrange_ggsurvplots(
        list(km_obj),
        print = FALSE,
        ncol  = 1, nrow = 1
    )

    # ── Log-rank test p-value (returned separately) ───────────────────────────
    logrank     <- survival::survdiff(survival::Surv(time, event) ~ km_group,
                                      data = surv_data)
    logrank_p   <- 1 - stats::pchisq(logrank$chisq, df = length(logrank$n) - 1L)
    cli::cli_inform(c("i" = "Log-rank p-value: {signif(logrank_p, 3)}"))

    if (!is.null(out_dir)) {
        ggplot2::ggsave(
            file.path(out_dir, glue::glue("{label}_km_dairy.png")),
            km_combined, width = 12, height = 9, dpi = 150
        )
        cli::cli_inform("KM plot saved to {.path {out_dir}}")
    }

    list(plot = km_obj, logrank_p = logrank_p)
}

# =============================================================================
# SECTION 8B — Adjusted scenario survival curves (time-dependent route)
# =============================================================================
#
# For a time-dependent Cox model, standard KM is not appropriate for showing
# the exposure effect. Instead we predict survival for two hypothetical
# covariate trajectories using survfit(fit, newdata, id):
#
#   "Always low dairy"  — exposure fixed at the median of Q1 throughout
#   "Always high dairy" — exposure fixed at the median of Q4 throughout
#
# All other covariates are held at their reference values (mean for continuous,
# modal level for factors). This is analogous to the Simon-Makuch approach but
# model-based, so it is adjusted for all covariates simultaneously.
#
# Only runs when covariate_type == "time_dependent".
# =============================================================================

#' Compute reference (mean/mode) values for all adjustment covariates.
#' @keywords internal
.reference_covariates <- function(surv_data, covariates) {
    ref <- list()
    for (cv in covariates) {
        if (!cv %in% names(surv_data)) next
        x <- surv_data[[cv]]
        if (is.numeric(x)) {
            ref[[cv]] <- mean(x, na.rm = TRUE)
        } else {
            tbl <- sort(table(x), decreasing = TRUE)
            ref[[cv]] <- names(tbl)[1L]
            # Restore original class (factor levels must match)
            if (is.factor(x)) ref[[cv]] <- factor(ref[[cv]], levels = levels(x))
        }
    }
    ref
}


#' Predict adjusted survival curves for fixed dairy exposure scenarios.
#'
#' Creates hypothetical counting-process datasets for Q1 and Q4 dairy
#' trajectories (all other covariates at reference), then calls
#' \code{survfit(fit, newdata, id)} to obtain adjusted survival curves.
#'
#' Only meaningful when \code{covariate_type == "time_dependent"}.
#'
#' @param fit          Fitted \code{coxph} object (adjusted, with \code{x = TRUE}).
#' @param surv_data    The survival dataset used to fit the model.
#' @param dairy_col    Character. Continuous dairy column name.
#' @param covariates   Character vector of adjustment covariates.
#' @param out_dir      Output directory or NULL.
#' @param label        String label for file names.
#' @return A ggplot, or NULL on failure.
#' @keywords internal
.plot_scenario_survival <- function(fit, surv_data, dairy_col, covariates,
                                    out_dir, label) {

    cli::cli_h3("Adjusted scenario survival curves (time-dependent) [{label}]")

    tryCatch({

        if (!all(c("age_start", "age_stop", "event") %in% names(surv_data)))
            cli::cli_abort("surv_data must have age_start, age_stop, event columns.")

        # ── Reference covariate values ──────────────────────────────────────
        ref <- .reference_covariates(surv_data, covariates)

        # ── Dairy scenario values: median of Q1 and Q4 ──────────────────────
        q_breaks <- stats::quantile(surv_data[[dairy_col]],
                                    probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        dairy_low  <- stats::median(
            surv_data[[dairy_col]][surv_data[[dairy_col]] <= q_breaks[2L]], na.rm = TRUE)
        dairy_high <- stats::median(
            surv_data[[dairy_col]][surv_data[[dairy_col]] >= q_breaks[4L]], na.rm = TRUE)

        cli::cli_inform(c(
            "i" = "Scenario dairy values: low = {round(dairy_low,1)} g/day (Q1 median),",
            "i" = "                       high = {round(dairy_high,1)} g/day (Q4 median)."
        ))

        # ── Build newdata: one row per scenario (constant trajectory) ────────
        # For a constant dairy exposure throughout follow-up, a single interval
        # spanning the observed age range is sufficient. survfit.coxph with id
        # evaluates the covariate at each event time against the baseline hazard.
        age_min <- min(surv_data$age_start, na.rm = TRUE)
        age_max <- max(surv_data$age_stop,  na.rm = TRUE)

        .make_scenario_row <- function(dairy_val, scenario_id) {
            df <- data.frame(
                .id       = scenario_id,
                age_start = age_min,
                age_stop  = age_max,
                event     = 0L
            )
            df$dairy_exposure <- dairy_val
            for (cv in names(ref)) df[[cv]] <- ref[[cv]]
            df
        }

        nd <- dplyr::bind_rows(
            .make_scenario_row(dairy_low,  1L),
            .make_scenario_row(dairy_high, 2L)
        )

        # ── Predict survival via survfit(fit, newdata, id) ───────────────────
        # id must be passed as a vector (nd$.id), not a bare symbol.
        sf <- survival::survfit(fit, newdata = nd, id = nd$.id)

        # ── Tidy into a plottable data frame ─────────────────────────────────
        scenario_labels <- c(
            glue::glue("Always low  (Q1 median: {round(dairy_low,1)} g/day)"),
            glue::glue("Always high (Q4 median: {round(dairy_high,1)} g/day)")
        )
        strata_lengths <- as.integer(sf$strata)
        sf_df <- data.frame(
            time     = sf$time,
            surv     = sf$surv,
            upper    = sf$upper,
            lower    = sf$lower,
            scenario = rep(scenario_labels, times = strata_lengths)
        )

        # ── Plot ──────────────────────────────────────────────────────────────
        p <- ggplot2::ggplot(sf_df, ggplot2::aes(
                x = time, y = surv,
                colour = scenario, fill = scenario)) +
            ggplot2::geom_step(linewidth = 0.9) +
            ggplot2::geom_ribbon(
                ggplot2::aes(ymin = lower, ymax = upper),
                alpha = 0.15, colour = NA
            ) +
            ggplot2::scale_colour_manual(
                values = c("#E76254FF", "#72BCD5FF"),
                name   = "Dairy trajectory"
            ) +
            ggplot2::scale_fill_manual(
                values = c("#E76254FF", "#72BCD5FF"),
                name   = "Dairy trajectory"
            ) +
            ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
            ggplot2::scale_x_continuous(limits = c(50, NA)) +
            ggplot2::labs(
                title    = glue::glue("Adjusted survival curves — dairy scenarios [{label}]"),
                subtitle = glue::glue(
                    "Covariates held at reference values. ",
                    "Low: {round(dairy_low,1)} g/day | High: {round(dairy_high,1)} g/day."
                ),
                x = "Age (years)",
                y = "Sarcopenia-free probability"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "bottom")

        if (!is.null(out_dir)) {
            ggplot2::ggsave(
                file.path(out_dir, glue::glue("{label}_scenario_survival.png")),
                p, width = 8, height = 6, dpi = 150
            )
            cli::cli_inform("Scenario survival plot saved to {.path {out_dir}}")
        }

        p

    }, error = function(e) {
        cli::cli_warn("Scenario survival plot failed: {conditionMessage(e)}")
        NULL
    })
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
# Helper — write captured log lines to a PDF
# =============================================================================

#' Write a character vector of log lines to a multi-page PDF.
#'
#' Strips ANSI colour codes (already done upstream via cli::ansi_strip).
#' Pages hold \code{lines_per_page} lines in monospace 7.5pt on US-letter
#' landscape (11 × 8.5 in).
#'
#' @param lines          Character vector of log lines.
#' @param path           Full file path for the PDF.
#' @param lines_per_page Integer. Lines per page (default 55).
#' @keywords internal
.write_log_pdf <- function(lines, path, lines_per_page = 55L) {
    if (length(lines) == 0L) return(invisible(NULL))

    # Strip any residual ANSI sequences
    lines <- cli::ansi_strip(lines)
    # Replace non-ASCII chars that PDF might reject
    lines <- iconv(lines, to = "ASCII//TRANSLIT", sub = "?")

    grDevices::pdf(path, width = 11, height = 8.5)
    on.exit(grDevices::dev.off(), add = TRUE)

    pages <- split(lines, ceiling(seq_along(lines) / lines_per_page))

    for (pg in pages) {
        grid::grid.newpage()
        grid::grid.text(
            label = paste(pg, collapse = "\n"),
            x     = 0.02,
            y     = 0.98,
            just  = c("left", "top"),
            gp    = grid::gpar(fontfamily = "mono", fontsize = 7.5, lineheight = 1.25)
        )
    }

    invisible(path)
}



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


