# =============================================================================
# R/model_specs.R
# =============================================================================
# Central definition of covariate tiers used by all model files.
#
# DESIGN RATIONALE — Four-tier hierarchy
# ----------------------------------------
# All model tiers are estimated in the SAME analytical sample (determined by
# freeze_dataset() hard exclusions, which require complete data on all M3
# covariates). This ensures that the progression M0 → M3 reflects covariate
# adjustment only, not a change in sample composition.
#
# M0  Crude         dairy × time only; no covariate adjustment
# M1  Minimal       age, anthropometric (BMI or Height), energy intake
#                   — the three confounders always required by nutritional
#                   epidemiology irrespective of research question
# M2  Lifestyle     + protein density, physical activity, smoking, alcohol
#                   — behavioural confounders of both diet and muscle
# M3  Full          + education, diabetes, HTN, HRT, corticosteroids,
#                   Ca/VitD/bisphosphonate
#                   — primary reported model; all pre-specified confounders
#
# Using tiers allows reviewers to see coefficient stability (robustness check)
# and makes it trivial to add sensitivity models in _targets.R.
#
# INTERACTION
# -----------
# The dairy × time interaction tests *trajectory modification* (does higher
# dairy slow the rate of decline?). This is a SECONDARY hypothesis.
# The PRIMARY hypothesis is the average association of dairy with the outcome
# across the follow-up period, obtained without the interaction term.
# All fit functions accept add_interaction = FALSE (default = primary model)
# and add_interaction = TRUE (secondary / supplementary).
#
# EXPOSURE METRIC SENSITIVITY
# ----------------------------
# Three exposure metrics are carried in the model datasets as audit columns:
#   dairy_cumavg      — cumulative average up to and including the current wave
#                       [PRIMARY: best proxy for habitual long-term intake]
#   dairy_total_lag1  — dairy intake at the *previous* wave (1-wave lag)
#                       [SENSITIVITY S1: tests whether prior intake predicts
#                        current outcome; helps address reverse causation]
#   dairy_total_gday  — dairy intake at the *current* wave (instantaneous)
#                       [SENSITIVITY S2: simplest exposure, no accumulation
#                        assumption; useful as a lower-bound of attenuation]
#
# Set the `exposure` argument to switch between them.
# =============================================================================


# =============================================================================
# Covariate tier definitions
# =============================================================================

.COVARIATE_TIERS <- list(
    
    # ── LME models: grip strength & gait speed (BMI as anthropometric) ──────────
    grip = list(
        M0 = character(0),
        M1 = c(
            "Age", "BMI", "energy_kcal"
        ),
        M2 = c(
            "Age", "BMI", "energy_kcal",
            "protein_pct",
            #"pa_levels", 
            "sbsmk", "alcohol_category"
        ),
        M3 = c(
            "Age", "BMI", "energy_kcal",
            "protein_pct",
            "pa_levels", "sbsmk", "alcohol_category",
            "education_level",
            "diabetes_status", "HTN_status", "hrt_status",
            "corticoids_status", "calcium_status",
            "vitD_status", "bisphosphonate_status"
        )
    ),
    
    # ── LME model: ALMI (Height replaces BMI — see collinearity note below) ─────
    # BMI = weight / height²; ALMI = ALM / height². Because lean mass is the
    # dominant component of weight, BMI and ALMI share a common denominator AND
    # a correlated numerator. Including BMI in an ALMI model inflates VIF for
    # both terms and can produce sign reversal. Height is the orthogonal
    # anthropometric adjustment.
    alm = list(
        M0 = character(0),
        M1 = c(
            "Age", "Height", "energy_kcal"
        ),
        M2 = c(
            "Age", "Height", "energy_kcal",
            "protein_pct",
            "pa_levels", "sbsmk", "alcohol_category"
        ),
        M3 = c(
            "Age", "Height", "energy_kcal",
            "protein_pct",
            "pa_levels", "sbsmk", "alcohol_category",
            "education_level",
            "diabetes_status", "HTN_status", "hrt_status",
            "corticoids_status", "calcium_status",
            "vitD_status", "bisphosphonate_status"
        )
    ),
    
    # ── Cox model: incident sarcopenia (Baseline values only) ───────────────────
    cox = list(
        M0 = character(0),
        M1 = c(
            "baseline_osteo_age", "baseline_bmi", "energy_kcal"
        ),
        M2 = c(
            "baseline_osteo_age", "baseline_bmi", "energy_kcal",
            "protein_pct",
            "pa_levels", "sbsmk", "alcohol_category"
        ),
        M3 = c(
            "baseline_osteo_age", "baseline_bmi", "energy_kcal",
            "protein_pct",
            "pa_levels", "sbsmk", "alcohol_category",
            "education_level",
            "diabetes_status", "HTN_status", "hrt_status",
            "corticoids_status", "calcium_status",
            "vitD_status", "bisphosphonate_status"
        )
    )
)


# =============================================================================
# Exposure options
# =============================================================================

# Valid exposure column names for sensitivity analyses (all present in
# model data tibbles after build_*_model_data()).
.EXPOSURE_OPTIONS <- c(
    primary = "dairy_cumavg",       # cumulative average [DEFAULT]
    lag1    = "dairy_total_lag1",   # one-wave lag
    current = "dairy_total_gday"    # same-wave instantaneous value
)


# =============================================================================
# Shared factor reference-level helper
# =============================================================================

#' Apply consistent factor reference levels to any model dataset.
#'
#' Called inside every build_*_model_data() function to keep reference
#' levels identical across all model tiers and sensitivity runs.
#'
#' @param data A tibble containing one or more of the named factor columns.
#' @return The same tibble with reference levels set where columns exist.
set_reference_levels <- function(data) {
    data |>
        dplyr::mutate(
            dplyr::across(
                dplyr::any_of("education_level"),
                ~ forcats::fct_relevel(.x, "Low")
            ),
            dplyr::across(
                dplyr::any_of("sbsmk"),
                ~ forcats::fct_relevel(.x, "Never")
            ),
            dplyr::across(
                dplyr::any_of("alcohol_category"),
                ~ forcats::fct_relevel(.x, "Non-drinker")
            ),
            dplyr::across(
                dplyr::any_of("pa_levels"),
                ~ forcats::fct_relevel(.x, levels(.x)[1])
            ),
            dplyr::across(
                dplyr::any_of(c(
                    "diabetes_status", "HTN_status", "hrt_status",
                    "corticoids_status", "calcium_status",
                    "vitD_status", "bisphosphonate_status"
                )),
                ~ forcats::fct_relevel(.x, "No")
            )
        )
}


# =============================================================================
# Covariate completeness reporting helper
# =============================================================================

#' Report and return a completeness log for the analytical sample.
#'
#' Counts participants lost to listwise deletion under a given covariate tier
#' and returns a named list suitable for downstream reporting.
#'
#' @param model_data  Tibble after row- and participant-level exclusions.
#' @param covariates  Character vector of covariate names for this tier.
#' @param n_before    Participant count before covariate completeness filter.
#' @return Named list: n_before, n_complete, n_lost_cov, pct_missing per cov.
report_completeness <- function(model_data, covariates, n_before) {
    present_covs <- intersect(covariates, names(model_data))
    
    miss_pct <- if (length(present_covs) > 0) {
        model_data |>
            dplyr::summarise(
                dplyr::across(
                    dplyr::all_of(present_covs),
                    ~ round(mean(is.na(.x)) * 100, 1)
                )
            ) |>
            tidyr::pivot_longer(
                dplyr::everything(),
                names_to  = "covariate",
                values_to = "pct_missing"
            ) |>
            dplyr::filter(pct_missing > 0)
    } else {
        tibble::tibble(covariate = character(), pct_missing = numeric())
    }
    
    n_complete <- if (length(present_covs) > 0) {
        sum(stats::complete.cases(model_data[, present_covs]))
    } else {
        nrow(model_data)
    }
    
    n_lost_cov <- dplyr::n_distinct(model_data$pt) - n_complete
    
    if (nrow(miss_pct) > 0) {
        cli::cli_warn(c(
            "!" = "Covariates with missing values (listwise deletion in lmer/coxph):",
            "*" = paste(
                glue::glue("{miss_pct$covariate}: {miss_pct$pct_missing}%"),
                collapse = "\n"
            )
        ))
    } else {
        cli::cli_inform(c("v" = "No missing values in selected covariates."))
    }
    
    list(
        n_before    = n_before,
        n_complete  = n_complete,
        n_lost_cov  = n_lost_cov,
        miss_detail = miss_pct
    )
}




# =============================================================================
# Pre-fit data preparation helper
# =============================================================================

#' Drop empty factor levels, filter exposure-NA rows, re-apply min obs, and
#' remove degenerate covariates before fitting.
#'
#' Called at the top of every fit_*_model() function. Handles four issues in
#' order:
#'
#'   1. Exposure-NA row filter — the build functions keep all waves where
#'      dairy_cumavg is non-NA, retaining lag/current columns as audit
#'      columns. Those columns are NA at waves where they could not be
#'      computed (dairy_total_lag1 is always NA at Baseline). When a
#'      sensitivity exposure is used, lmer would silently drop those rows
#'      via listwise deletion, potentially leaving participants with < 2
#'      observations and making the random slope unidentifiable
#'      (n_obs < n_random_effect_parameters). Fix: drop explicitly before
#'      fitting.
#'
#'   2. Minimum observations per participant (LME only) — after Step 1,
#'      re-apply the >= min_obs_lme criterion. Participants who fall below
#'      are removed from the fitting dataset only; the build output is
#'      unchanged.
#'
#'   3. droplevels() — removes factor levels with zero observations in the
#'      remaining data.
#'
#'   4. Degenerate covariate removal — any column that is constant in this
#'      subsample (single-level factor, or numeric/logical with no variation)
#'      is removed from the covariate vector so the formula can be rebuilt
#'      without it. lmer and coxph both error on constant predictors.
#'
#' @param data        Model-ready tibble from build_*_model_data().
#' @param covariates  Character vector of covariate names for the tier.
#' @param caller      String for warning messages (function name + tier).
#' @param exposure    Exposure column being used. Default "dairy_cumavg".
#'   When this differs from "dairy_cumavg", Step 1 is triggered.
#' @param min_obs_lme Integer or NULL. Minimum valid rows per participant
#'   required after Step 1. Pass 2L for LME models; leave NULL for Cox.
#' @return Named list:
#'   $data       — prepared data frame ready for lmer / coxph
#'   $covariates — cleaned covariate vector (degenerate columns removed)
#'   $dropped    — character vector of columns removed in Step 4
prepare_fit_data <- function(data, covariates, caller = "fit_model",
                             exposure        = "dairy_cumavg",
                             min_obs_lme     = NULL,
                             m3_covariates   = NULL,
                             scale_covariates = TRUE) {
    
    # ------------------------------------------------------------------
    # Step 1: Restrict to M3 complete-case sample
    #
    # WHY THIS MATTERS: lmer does listwise deletion silently. Without this
    # step, M0 runs on ~3579 rows but M2/M3 run on ~1600-1846 rows because
    # clinical covariates (pa_levels, diabetes_status etc.) have missing
    # values. The stability table then confounds adjustment with sample
    # composition — the coefficient change from M0 to M3 partly reflects
    # a different (healthier, better-measured) subpopulation rather than
    # covariate adjustment alone.
    #
    # Fix: before fitting ANY tier, drop rows that are incomplete on the
    # FULL M3 covariate set. All four tiers then run on the same rows and
    # the stability table is a clean comparison of adjustment only.
    # ------------------------------------------------------------------
    if (!is.null(m3_covariates) && length(m3_covariates) > 0L) {
        m3_present   <- intersect(m3_covariates, names(data))
        complete_rows <- stats::complete.cases(data[, m3_present, drop = FALSE])
        n_dropped_cc  <- sum(!complete_rows)
        if (n_dropped_cc > 0L) {
            cli::cli_inform(c(
                "i" = "{caller}: restricting to M3 complete-case sample — \\
               {n_dropped_cc} rows dropped with missing M3 covariates. \\
               All tiers run on the same {sum(complete_rows)} observations."
            ))
        }
        data <- data[complete_rows, ]
    }
    
    # ------------------------------------------------------------------
    # Step 2: drop rows where the sensitivity exposure is NA
    # ------------------------------------------------------------------
    if (exposure != "dairy_cumavg" && exposure %in% names(data)) {
        n_before  <- nrow(data)
        data      <- dplyr::filter(data, !is.na(.data[[exposure]]))
        n_dropped <- n_before - nrow(data)
        if (n_dropped > 0L) {
            cli::cli_inform(c(
                "i" = "{caller}: {n_dropped} rows dropped where {exposure} is NA \\
               (expected — e.g. Baseline has no lag-1 value)."
            ))
        }
    }
    
    # ------------------------------------------------------------------
    # Step 3: re-apply minimum observations per participant (LME only)
    # ------------------------------------------------------------------
    if (!is.null(min_obs_lme) && "pt" %in% names(data)) {
        pt_counts    <- dplyr::count(data, pt, name = "n_valid")
        keep_pts     <- pt_counts$pt[pt_counts$n_valid >= min_obs_lme]
        n_dropped_pt <- dplyr::n_distinct(data$pt) - length(keep_pts)
        if (n_dropped_pt > 0L) {
            cli::cli_warn(c(
                "!" = "{caller}: {n_dropped_pt} participant(s) removed — fewer than \\
               {min_obs_lme} valid rows after filtering on {exposure}.",
                "i" = "Expected for lag-1 / concurrent sensitivities. \\
               Fitting on {length(keep_pts)} participants."
            ))
        }
        data <- dplyr::filter(data, pt %in% keep_pts)
    }
    
    # ------------------------------------------------------------------
    # Step 4: drop empty factor levels
    # ------------------------------------------------------------------
    data <- droplevels(data)
    
    # ------------------------------------------------------------------
    # Step 5: warn about small factor cells (< 10 observations per level)
    #
    # A factor level with very few observations produces unstable, wide-CI
    # coefficients that can look anomalous (e.g. insulin-treated diabetes
    # showing +5 kg grip). This does not remove the level but flags it so
    # the analyst can decide whether to collapse the category.
    # ------------------------------------------------------------------
    present_factors <- purrr::keep(
        intersect(covariates, names(data)),
        ~ is.factor(data[[.x]])
    )
    
    purrr::walk(present_factors, function(col) {
        counts <- table(data[[col]])
        small  <- names(counts)[counts < 10L & counts > 0L]
        if (length(small) > 0L) {
            cli::cli_warn(c(
                "!" = "{caller}: factor '{col}' has level(s) with < 10 observations: \\
               {paste(small, collapse = ', ')}.",
                "i" = "Coefficients for these levels will be unstable. \\
               Consider collapsing to a binary Yes/No variable."
            ))
        }
    })
    
    # ------------------------------------------------------------------
    # Step 6: remove degenerate covariates (constant after above filtering)
    # ------------------------------------------------------------------
    present    <- intersect(covariates, names(data))
    degenerate <- purrr::keep(present, function(col) {
        x <- data[[col]]
        x <- x[!is.na(x)]
        if (length(x) == 0L)                  return(TRUE)
        if (is.factor(x))                     return(nlevels(droplevels(x)) < 2L)
        if (is.logical(x) || is.character(x)) return(length(unique(x)) < 2L)
        return(diff(range(x)) == 0)
    })
    
    if (length(degenerate) > 0L) {
        cli::cli_warn(c(
            "!" = "{caller}: {length(degenerate)} covariate(s) constant — \\
             removed from model formula:",
            "*" = paste(degenerate, collapse = ", "),
            "i" = "Common in small subsamples. Interpret results with caution."
        ))
    }
    
    clean_covariates <- setdiff(covariates, degenerate)
    
    # ------------------------------------------------------------------
    # Step 7: standardise continuous covariates (z-score, mean 0 SD 1)
    #
    # WHY: lmer's BOBYQA optimiser can converge poorly when predictors
    # span very different scales (e.g. energy_kcal ~1800 vs protein_pct
    # ~17 vs Age ~70). Standardising removes this without changing any
    # inference — only the covariate coefficients change scale, not the
    # exposure (dairy) coefficient which stays on its original g/day scale.
    #
    # NOTE: this modifies the data copy only. The original dataset from
    # build_*_model_data() is unchanged (R copies on assignment).
    # Covariate beta values in the output will be in SD units.
    # ------------------------------------------------------------------
    if (scale_covariates) {
        continuous_covs <- purrr::keep(
            intersect(clean_covariates, names(data)),
            ~ is.numeric(data[[.x]]) && !(.x %in% c(exposure, "pt",
                                                    "osteo_wave_num",
                                                    "time_since_bsl_yr",
                                                    "surv_time"))
        )
        if (length(continuous_covs) > 0L) {
            data <- dplyr::mutate(
                data,
                dplyr::across(
                    dplyr::all_of(continuous_covs),
                    ~ as.numeric(scale(.x))
                )
            )
        }
    }
    
    list(
        data       = data,
        covariates = clean_covariates,
        dropped    = degenerate
    )
}


# =============================================================================
# Safe label list helper for gtsummary::tbl_regression()
# =============================================================================

#' Build a gtsummary label list filtered to terms present in the model.
#'
#' tbl_regression() errors if the `label` argument references a term that does
#' not exist in the fitted model (e.g. the interaction term when the model was
#' fitted without add_interaction = TRUE, or a covariate that was dropped by
#' prepare_fit_data()). This helper constructs the full candidate label list
#' and silently drops any entry whose term is absent from the model.
#'
#' @param fit_reml   A fitted lmerMod or coxph object.
#' @param candidates A named list of label formulas, e.g.
#'   list(dairy_cumavg ~ "Cumulative dairy (g/day)", ...).
#'   Names are taken from the left-hand side of each formula.
#' @return A filtered list safe to pass to tbl_regression(label = ...).
safe_labels <- function(fit_reml, candidates) {
    
    # Extract the term names that actually appear in the model
    present_terms <- tryCatch(
        names(stats::coef(fit_reml)),            # works for both lmerMod and coxph
        error = function(e) character(0)
    )
    
    # Also accept lmerMod fixed-effect names
    if (inherits(fit_reml, "lmerMod") || inherits(fit_reml, "lmerModLmerTest")) {
        present_terms <- names(lme4::fixef(fit_reml))
    }
    
    # Extract the LHS variable name from each candidate formula
    # e.g.  `dairy_cumavg:time_since_bsl_yr` ~ "Dairy x Time"  ->  the backtick name
    lhs_name <- function(f) {
        lhs <- f[[2L]]
        # deparse handles both plain names and backtick-quoted operator expressions
        deparse(lhs, backtick = FALSE)
    }
    
    purrr::keep(candidates, function(f) lhs_name(f) %in% present_terms)
}