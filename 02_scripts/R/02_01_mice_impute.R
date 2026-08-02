# =============================================================================
# R/02_01_mice_impute.R
# =============================================================================
# MICE (multiple imputation by chained equations) for CoLaus and OsteoLaus,
# using the FCS-1L-wide approach: each cohort's long-format tibble is
# reshaped to one row per participant (wide, one column per visit x
# variable), imputed with mice::mice(), then reshaped back to long format
# and wrapped as a mids object for downstream derive()/select_analysis_columns().
#
# Functions:
#   .save_diagnostics()       — writes convergence/density/strip PDFs for a mids
#   .investigate_donor_pool() — reports non-NA donor counts per imputed variable
#   impute_mice_colaus()      — wide-reshape, impute, reshape back (CoLaus)
#   impute_mice_osteo()       — wide-reshape, impute, reshape back (OsteoLaus)
#   post_imputation_checks()  — thin wrapper around .save_diagnostics(),
#                                kept for back-compat call sites
# =============================================================================


#' Save convergence (trace) plot and per-variable density/strip plots to PDF.
#'
#' Convergence is assessed via the standard mice trace plot: mean and SD of
#' each imputed variable per iteration, across chains (imputed datasets). When
#' the lines mix and show no trend, imputation has converged.
#'
#' @param mids_obj  A mids object returned by mice::mice().
#' @param out_dir   Directory to write PDFs into.
#' @param label     Short label prepended to filenames ("colaus" or "osteo").
#' @return Invisibly, a list with the three PDF paths: convergence, density, strip.
# -----------------------------------------------------------------------------
# .save_diagnostics()
# -----------------------------------------------------------------------------
.save_diagnostics <- function(mids_obj, out_dir, label = "imputation") {
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  vars_imputed <- names(mids_obj$method)[mids_obj$method != ""]
  
  cli::cli_h3("Saving MICE diagnostics to {out_dir}")
  
  # -------------------------------------------------------------------------
  # Helper: safe PDF wrapper
  # -------------------------------------------------------------------------
  save_pdf <- function(path, expr, width = 10, height = 7) {
    grDevices::pdf(path, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
    expr
  }
  
  # -------------------------------------------------------------------------
  # 1. Convergence plots
  # -------------------------------------------------------------------------
  conv_path <- file.path(out_dir, paste0(label, "_convergence.pdf"))
  
  save_pdf(conv_path, {
    for (v in vars_imputed) {
      tryCatch({
        print(
          plot(
            mids_obj,
            y = v,
            main = paste0("Convergence: ", v)
          )
        )
      }, error = function(e) NULL)
    }
  })
  
  # -------------------------------------------------------------------------
  # 2. Density plots (observed vs imputed)
  # -------------------------------------------------------------------------
  dens_path <- file.path(out_dir, paste0(label, "_density.pdf"))
  
  save_pdf(dens_path, {
    for (v in vars_imputed) {
      tryCatch({
        print(
          mice::densityplot(
            mids_obj,
            as.formula(paste("~", v)),
            main = v
          )
        )
      }, error = function(e) NULL)
    }
  })
  
  # -------------------------------------------------------------------------
  # 3. Strip plots (imputed vs observed)
  # -------------------------------------------------------------------------
  strip_path <- file.path(out_dir, paste0(label, "_stripplot.pdf"))
  
  save_pdf(strip_path, {
    for (v in vars_imputed) {
      tryCatch({
        print(
          mice::stripplot(
            mids_obj,
            as.formula(paste(v, "~ .imp")),
            pch = c(21, 20),
            cex = c(1, 1.2),
            main = v
          )
        )
      }, error = function(e) NULL)
    }
  })
  
  cli::cli_inform(c(
    "v" = "Convergence -> {conv_path}",
    "v" = "Density      -> {dens_path}",
    "v" = "Strip        -> {strip_path}"
  ))
  
  invisible(list(
    convergence = conv_path,
    density = dens_path,
    strip = strip_path
  ))
}


# =============================================================================
# Donor pool investigation — non-NA counts for all imputed variables
# =============================================================================

# -----------------------------------------------------------------------------
# .investigate_donor_pool()
# -----------------------------------------------------------------------------
#' Report non-NA donor counts for every variable slated for imputation.
#'
#' Flags variables with too few observed values to reliably drive PMM/logreg/
#' polr imputation ("CRITICAL" < 5 donors, "WARNING" < `min_donors`).
#' Diagnostic only — called manually / commented out in the pipeline, not
#' part of the default impute_mice_*() flow.
#'
#' @param df_mice     Wide imputation input data frame.
#' @param all_imputed Character vector of wide column names to check.
#' @param min_donors  Minimum non-NA count below which a variable is flagged
#'   "WARNING". Default 20L.
#' @return Invisibly, a tibble with one row per variable: `n_donors`,
#'   `n_missing`, `n_total`, `pct_obs`, `flag`.
.investigate_donor_pool <- function(df_mice, all_imputed, min_donors = 20L) {
  
  donor_counts <- all_imputed |>
    purrr::map_dfr(function(var) {
      if (!var %in% names(df_mice)) {
        return(tibble::tibble(
          variable  = var,
          n_donors  = NA_integer_,
          n_missing = NA_integer_,
          n_total   = NA_integer_,
          pct_obs   = NA_real_,
          flag      = "NOT IN DATA"
        ))
      }
      x <- df_mice[[var]]
      tibble::tibble(
        variable  = var,
        n_donors  = sum(!is.na(x)),
        n_missing = sum( is.na(x)),
        n_total   = length(x),
        pct_obs   = round(100 * mean(!is.na(x)), 1),
        flag      = dplyr::case_when(
          sum(!is.na(x)) == 0        ~ "NO DONORS — cannot impute",
          sum(!is.na(x)) < 5L        ~ "CRITICAL   < 5  donors",
          sum(!is.na(x)) < min_donors ~ paste0("WARNING    < ", min_donors, " donors"),
          TRUE                        ~ "ok"
        )
      )
    })
  
  # ── Console summary ──────────────────────────────────────────────────────
  cli::cli_h1("Donor pool investigation")
  cli::cli_inform("Total imputed variables: {nrow(donor_counts)}")
  cli::cli_inform("Min donor threshold: {min_donors}")
  
  flagged <- dplyr::filter(donor_counts, flag != "ok")
  
  if (nrow(flagged) == 0L) {
    cli::cli_alert_success("All variables have >= {min_donors} donors.")
  } else {
    cli::cli_alert_warning("{nrow(flagged)} variable(s) below threshold:")
    print(flagged, n = Inf)
  }
  
  # ── Full table sorted by n_donors ascending ──────────────────────────────
  cli::cli_h2("Full donor pool table (sorted by n_donors)")
  print(dplyr::arrange(donor_counts, n_donors), n = Inf)
  
  invisible(donor_counts)
}


# =============================================================================
# CoLaus imputation
# =============================================================================

#' Impute missing CoLaus covariates and exposures using MICE (pre-derivation).
#'
#' Imputation targets raw harmonised columns so that derived variables
#' (smoking_status, alcohol_category, education_level, etc.) are computed from
#' imputed primitives via derive_colaus(), not imputed directly.
#'
#' Diagnostics produced (in-session and on disk):
#' \itemize{
#'   \item Pre- and post-imputation missingness tables (cli output + returned list).
#'   \item NA-free assertion across all m datasets for every imputed variable.
#'   \item Convergence (trace) PDF — one page per variable, all chains overlaid.
#'   \item Density and strip-plot PDFs for observed-vs-imputed comparison.
#' }
#'
#' @param df Harmonised stacked CoLaus tibble from stack_visits().
#' @param m        Number of imputed datasets. Default 20L.
#' @param maxit    MICE iterations. Default 20L.
#' @param seed     Random seed. Default 2024L.
#' @param out_dir  Directory for diagnostic PDFs. Default "output/mice_diagnostics/colaus".
#' @param ...      Extra arguments forwarded to mice::mice().
#' @return List: mids, long, m, seed, imputed_vars, diag_paths.
# -----------------------------------------------------------------------------
# impute_mice_colaus()
# -----------------------------------------------------------------------------
impute_mice_colaus <- function(df,
                               m       = 20L,
                               maxit   = 20L,
                               seed    = 2024L,
                               out_dir = "03_outputs/mice_diagnostics/colaus") {
  
  cli::cli_h1("MICE Imputation — CoLaus (FCS-1L-wide)")
  
  # =============================================================================
  # Variable group definitions — updated
  # =============================================================================
  
  # Numeric variables: imputed with pmm
  # NOTE: Age used as predictor only, never imputed
  .CL_PMM_VARS <- c(
    "Height", "Weight", "conso_hebdo", "sumalco",
    "PAFQ_MPA", "PAFQ_VPA", "sumtot1", "sumprot1", "sumgluc1",
    "sumlipi1",
    "esthrpage",   
    "HGS_MAX",
    paste0("FFQ", c(1:8, 52, 53, 63, 68, 71, 82:86), "amount"),
    paste0("freqFFQ", c(1:8, 52, 53, 63, 68, 71, 82:86)),
    paste0("FFQp",    c(1:8, 52, 53, 63, 68, 71, 82:86))
  )
  
  # Binary factor variables: imputed with logreg
  .CL_LOGREG_VARS <- c(
    "dbtld", "DIAB", "DIAB_Hb", "esthrp", "antiHTA",
    "crbpmed",      
    "HTA", "miac", "strk", "chf", "cad", "angn", "cmp", "hdc",
    "hdv", "artm", "vslg", "ccth", "cabg", "pcin"
  )
  
  # Ordered factor variables: imputed with polr
  .CL_POLR_VARS <- c(
    "sbsmk",   # smoking (Never / Former / Current)
    "edtyp4",  # education (4-level ordered)
    "mrtsts2"
  )
  
  
  
  # ---------------------------------------------------------------------------
  # 1. Wide reshape
  # ---------------------------------------------------------------------------
  
  visit_levels <- levels(df$.visit)
  observed_visits <- df |>
    dplyr::distinct(pt, .visit)
  pt_constants <- df |>
    dplyr::distinct(pt, .cohort)
  
  df <- df |>
    dplyr::mutate(
      dplyr::across(where(is.list), ~ unlist(.x))
    )
  
  wide <- df |> 
    dplyr::select(pt, .visit, dplyr::everything()) |> 
    dplyr::mutate(exam_date_num = as.numeric(as.Date(exam_date_iso))) |> 
    dplyr::select(-exam_date_iso) |> 
    tidyr::pivot_wider( id_cols = "pt", 
                        names_from = ".visit", 
                        values_from = -c(pt, .visit), 
                        names_sep = "_" )
  
  # exclude NA columns
  df_mice <- wide[, colSums(!is.na(wide)) > 0, drop = FALSE]
  
  # ---------------------------------------------------------------------------
  # 2. Variable groups (wide names)
  # ---------------------------------------------------------------------------
  make_wide <- function(vars) {
    as.vector(outer(vars, visit_levels, paste, sep = "_"))
  }
  
  pmm_vars    <- intersect(make_wide(.CL_PMM_VARS),    names(df_mice))
  logreg_vars <- intersect(make_wide(.CL_LOGREG_VARS), names(df_mice))
  polr_vars   <- intersect(make_wide(.CL_POLR_VARS),   names(df_mice))
  
  # Age: in df_mice as predictor column but NOT in all_imputed
  # → it will stay in df_mice, method = "", pred column = 1 where needed
  age_vars  <- intersect(make_wide("Age"), names(df_mice))
  
  id_vars   <- "pt"
  time_vars <- intersect(paste0("exam_date_num_", visit_levels), names(df_mice))
  
  all_imputed <- c(pmm_vars, logreg_vars, polr_vars)
  # Age kept in df_mice as predictor-only — include it in the kept columns
  all_kept    <- unique(c(id_vars, time_vars, age_vars, all_imputed))
  
  df_mice <- df_mice[, intersect(all_kept, names(df_mice)), drop = FALSE]
  
  # Re-attach .cohort so it survives into the mids and is available after
  # mice::complete() in downstream pipeline steps.
  df_mice <- dplyr::left_join(df_mice, pt_constants, by = "pt")
  
  #donor_summary <- .investigate_donor_pool(df_mice, all_imputed, min_donors = 20L)
  
  # ---------------------------------------------------------------------------
  # 3. Where matrix
  # ---------------------------------------------------------------------------
  where <- mice::make.where(df_mice, keyword = "missing")
  
  # sumalco at Baseline is structurally missing (not measured),
  # not random missingness → do NOT impute those cells
  sumalco_baseline <- intersect("sumalco_Baseline", colnames(where))
  if (length(sumalco_baseline))
    where[, sumalco_baseline] <- FALSE
  
  where[age_vars[age_vars %in% rownames(where)], ] <- FALSE
  where[,"crbpmed_F2"] <- FALSE
  # ---------------------------------------------------------------------------
  # 4. Method vector
  # ---------------------------------------------------------------------------
  meth <- mice::make.method(df_mice)
  meth[] <- ""
  meth[intersect(pmm_vars,    names(meth))] <- "pmm"
  meth[intersect(logreg_vars, names(meth))] <- "logreg"
  meth[intersect(polr_vars,   names(meth))] <- "polr"
  # Age stays "" — predictor only, never imputed
  meth[age_vars[age_vars %in% names(meth)]] <- ""
  meth["crbpmed_F2"] <- ""
  
  # ---------------------------------------------------------------------------
  # 5. Predictor matrix
  # ---------------------------------------------------------------------------
  pred <- mice::make.predictorMatrix(df_mice)
  pred[,] <- 0L
  
  VISITS <- visit_levels
  
  wv <- function(vars, visits = VISITS) {
    as.vector(outer(vars, visits, paste, sep = "_"))
  }
  
  set_pred <- function(pred, targets, predictors) {
    targets    <- intersect(targets,    rownames(pred))
    predictors <- intersect(predictors, colnames(pred))
    pred[targets, predictors] <- 1L
    pred
  }
  
  
  # =============================================================================
  # 1. HEIGHT
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv("Height"),
                   predictors = wv(c(
                     "Age",               
                     "Weight"
                   )))
  
  # =============================================================================
  # 2. WEIGHT
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv("Weight"),
                   predictors = wv(c(
                     "Age", 
                     "Height",
                     "sbsmk", "edtyp4",
                     "DIAB", "HTA",
                     "PAFQ_MPA", "conso_hebdo"
                   )))
  
  # =============================================================================
  # 3. ALCOHOL  —  conso_hebdo, sumalco
  # sumalco_Baseline stays NA (structural), but sumalco F1/F2/F3 are imputed
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv(c("conso_hebdo", "sumalco")),
                   predictors = wv(c(
                     "conso_hebdo", "sumalco",
                     "Age",
                     "sbsmk", "edtyp4", "mrtsts2",
                     "DIAB", "HTA"
                   )))
  
  # =============================================================================
  # 4. PHYSICAL ACTIVITY  —  PAFQ_MPA, PAFQ_VPA
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv(c("PAFQ_MPA", "PAFQ_VPA")),
                   predictors = wv(c(
                     "PAFQ_MPA", "PAFQ_VPA",
                     "Age", "Weight",
                     "sbsmk", "edtyp4",
                     "HGS_MAX",
                     "cad", "chf"
                   )))
  
  # =============================================================================
  # 5. DIET TOTALS  —  sumtot1, sumprot1, sumgluc1, sumlipi1
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv(c("sumtot1","sumprot1","sumgluc1","sumlipi1")),
                   predictors = wv(c(
                     "Age", "Weight", "edtyp4",
                     "sbsmk",
                     "DIAB", "conso_hebdo"
                   )))
  
  # =============================================================================
  # 6. HAND-GRIP STRENGTH  —  HGS_MAX
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv("HGS_MAX"),
                   predictors = wv(c(
                     "HGS_MAX",
                     "Height", "Weight",
                     "PAFQ_MPA",
                     "DIAB", "HTA",
                     "sbsmk", "edtyp4"
                   )))
  
  # =============================================================================
  # 7. FFQ — per-item: amount, frequency, proportion
  # =============================================================================
  FFQ_ITEMS <- c(1:8, 52, 53, 63, 68, 71, 82:86)
  
  for (item in FFQ_ITEMS) {
    amt_var  <- paste0("FFQ",     item, "amount")
    freq_var <- paste0("freqFFQ", item)
    prop_var <- paste0("FFQp",    item)
    
    item_pred <- c(
      "sumtot1",
      "Age", "Weight",
      "sbsmk"
    )
    
    pred <- set_pred(pred,
                     targets    = wv(c(amt_var, freq_var, prop_var)),
                     predictors = wv(item_pred))
  }
  
  # =============================================================================
  # 8. DIABETES CLUSTER  —  dbtld, DIAB, DIAB_Hb
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv(c("dbtld","DIAB","DIAB_Hb")),
                   predictors = wv(c(
                     "dbtld", "DIAB", "DIAB_Hb",
                     "Weight",
                     "HTA", "sbsmk",
                     "sumgluc1", "conso_hebdo"
                   )))
  
  # =============================================================================
  # 9. HYPERTENSION  —  HTA, antiHTA
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv(c("HTA","antiHTA")),
                   predictors = wv(c(
                     "HTA", "antiHTA",
                     "Age", "Weight",
                     "DIAB", "sbsmk",
                     "cad", "strk",
                     "conso_hebdo"
                   )))
  
  # =============================================================================
  # 10. HRT  —  esthrp, esthrpage 
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv("esthrp"),
                   predictors = wv(c(
                     "Age", "mrtsts2",
                     "sbsmk", "edtyp4",
                     "HTA", "DIAB",
                     "cad"
                   )))
  
  pred <- set_pred(pred,
                   targets    = wv("esthrpage"),
                   predictors = wv(c(
                     "Age", "mrtsts2",
                     "sbsmk",
                     "HTA", "DIAB"
                   )))
  
  # =============================================================================
  # 11. crbpmed  —  (added, binary logreg)
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv("crbpmed"),
                   predictors = wv(c(
                     "crbpmed",
                     "Weight",
                     "HTA", "DIAB",
                     "antiHTA", "sbsmk",
                     "cad", "strk"
                   )))
  
  
  # =============================================================================
  # 12. HARD CVD EVENTS  —  miac, strk, chf, cad, angn
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv(c("miac","strk","chf","cad","angn")),
                   predictors = wv(c(
                     "miac", "strk", "chf", "cad", "angn",
                     "Age", "DIAB",
                     "HTA", "sbsmk"
                   )))
  
  # =============================================================================
  # 13. PROCEDURES  —  cmp, hdc, hdv, artm, vslg, ccth, cabg, pcin
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv(c("cmp","hdc","hdv","artm","vslg","ccth","cabg","pcin")),
                   predictors = wv(c(
                     "cmp", "hdc", "cabg",
                     "cad", "miac", "chf",
                     "Age", "DIAB", "HTA"
                   )))
  
  # =============================================================================
  # 14. SMOKING  —  sbsmk (polr)
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv("sbsmk"),
                   predictors = wv(c(
                     "sbsmk",
                     "Age", "edtyp4", "mrtsts2",
                     "conso_hebdo",
                     "HTA", "cad",
                     "PAFQ_MPA"
                   )))
  
  # =============================================================================
  # 15. EDUCATION  —  edtyp4 (polr, quasi time-invariant)
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv("edtyp4"),
                   predictors = wv(c(
                     "edtyp4",
                     "Age", "mrtsts2",
                     "sbsmk",
                     "HGS_MAX",
                     "PAFQ_MPA", "conso_hebdo"
                   )))
  
  # =============================================================================
  # 16. MARITAL STATUS  —  mrtsts2 (polr)
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = wv("mrtsts2"),
                   predictors = wv(c(
                     "mrtsts2",
                     "Age", "edtyp4",
                     "sbsmk",
                     "HTA", "DIAB",
                     "strk", "chf"
                   )))
  
  # ---------------------------------------------------------------------------
  # RESTRICT: Age as predictor → same visit only
  # First blank every Age_<visit> predictor column set above (some of the
  # per-topic blocks above included "Age" without a visit suffix filter), then
  # re-enable exactly one Age column per imputed target: the Age from that
  # target's own visit (Age_F1 predicts *_F1, Age_F2 predicts *_F2, etc.),
  # by pairing each target's row index with its same-visit Age column index.
  # ---------------------------------------------------------------------------

  pred[, intersect(age_vars, colnames(pred))] <- 0L

  own_age_targets <- intersect(all_imputed, rownames(pred))
  own_age_cols    <- paste0("Age_", sub("^.*_", "", own_age_targets))
  matched         <- own_age_cols %in% colnames(pred)

  pred[cbind(match(own_age_targets[matched], rownames(pred)),
             match(own_age_cols[matched],    colnames(pred)))] <- 1L

  # ---------------------------------------------------------------------------
  # ENFORCE: Age rows never imputed → rows stay 0
  # ENFORCE: diagonal = 0
  # ---------------------------------------------------------------------------
  pred[intersect(age_vars, rownames(pred)), ] <- 0L
  diag(pred) <- 0L
  
  
  
  cat("\nPredictor matrix created successfully!\n")
  cat("Dimensions:", nrow(pred), "x", ncol(pred), "\n")
  cat("Density:", sum(pred) / (nrow(pred)^2 - nrow(pred)), "\n")
  # ---------------------------------------------------------------------------
  # 7. Visit sequence
  # ---------------------------------------------------------------------------
  
  visit_seq <- mice::make.visitSequence(data = df_mice)
  
  post <- mice::make.post(df_mice)
  
  sumtot1_wide_cols <- intersect(
    paste0("sumtot1_", visit_levels),
    names(df_mice)
  )
  
  
  for (col in sumtot1_wide_cols) {
    post[col] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(500, 3500))"
  }

  
  
  # ---------------------------------------------------------------------------
  # 8. Run MICE
  # ---------------------------------------------------------------------------
  
  cli::cli_h2("Running MICE (m = {m}, maxit = {maxit}, seed = {seed})")
  
  mids_obj <- mice::mice(
    data            = df_mice,
    m               = m,
    maxit           = maxit,
    seed            = seed,
    method          = meth,
    predictorMatrix = pred,
    where           = where,
    visitSequence   = visit_seq,
    post            = post,
    ridge           = 1e-5, 
    eps             = 1e-4,
    printFlag       = FALSE,
    donors = 10L
  )
  
  # ---------------------------------------------------------------------------
  # 9. Extract LONG format correctly
  # ---------------------------------------------------------------------------
  visit_pat <- paste(visit_levels, collapse = "|") 
  long_df <- mice::complete(mids_obj, action = "long", include = TRUE) |>
    tibble::as_tibble() |>
    tidyr::pivot_longer( cols = -c(.imp, .id, pt), names_to = c(".value", ".visit"), 
                         names_pattern = paste0("^(.+)_(", visit_pat, ")$") ) |>
    dplyr::mutate( .visit = factor(.visit, levels = visit_levels), 
                   exam_date_iso = as.Date(exam_date_num, origin = "1970-01-01") ) |>
    dplyr::select(-dplyr::starts_with("exam_date_num")) |>
    # After pivot_longer, .id is the original wide-row index and is no longer
    # unique within each imputation (each pt now has n_visits rows with the
    # same .id). Reassign to a unique sequential index per .imp so that
    # mice::as.mids() can reconstruct the long-format mids without duplicate
    # row name errors.
    dplyr::group_by(.imp) |>
    dplyr::mutate(.id = dplyr::row_number()) |>
    dplyr::ungroup() 
  
  # delete visit rows that were introduced and did not occur in real life
  long_df <- long_df |>
    dplyr::inner_join(
      observed_visits,
      by = c("pt", ".visit")
    )
  
  
  key_cols <- c("pt", ".visit") 
  orig_key <- dplyr::select(df, pt, .visit, dplyr::everything()) 
  existing_cols <- names(long_df) 
  extra_cols <- setdiff(names(orig_key), c(existing_cols, key_cols)) 
  if (length(extra_cols) > 0) { 
    long_df <- long_df |> 
      dplyr::left_join( 
        dplyr::select(orig_key, pt, .visit, dplyr::all_of(extra_cols)), 
        by = c("pt", ".visit") 
      ) 
  }
  
  
  # ---------------------------------------------------------------------------
  # 10. Post-imputation diagnostics
  # ---------------------------------------------------------------------------
  
  # Base variable names (strip visit suffix) for long-format NA check
  base_imputed <- unique(c(.CL_PMM_VARS, .CL_LOGREG_VARS, .CL_POLR_VARS))
  
  
  # Convergence + density + strip plots saved to disk
  diag_paths <- .save_diagnostics(mids_obj, out_dir, label = "colaus")
  
  # ---------------------------------------------------------------------------
  # 10b. Rebuild mids in long format
  # ---------------------------------------------------------------------------
  # mids_obj is wide (one row per pt, columns like BMI_F1 / BMI_F2 / BMI_F3).
  # Downstream pipeline steps call mice::complete() expecting long format
  # (one row per pt x visit). Rebuild the mids from long_df so that every
  # subsequent mice::complete() returns long data directly.
  # The wide mids_obj is kept as mids_wide for diagnostics.
  long_mids <- mice::as.mids(long_df)
  
  # ---------------------------------------------------------------------------
  # 11. Output
  # ---------------------------------------------------------------------------
  
  list(
    df_wide      = df_mice,
    mids         = long_mids,
    mids_wide    = mids_obj,
    long         = long_df,
    m            = m,
    seed         = seed,
    imputed_vars = all_imputed,
    diag_paths   = diag_paths
  )
}


# =============================================================================
# OsteoLaus imputation
# =============================================================================

#' Impute missing OsteoLaus outcomes using MICE — FCS-1L-wide approach.
#'
#' Diagnostics produced (in-session and on disk):
#' \itemize{
#'   \item Pre- and post-imputation missingness tables (cli output + returned list).
#'   \item NA-free assertion across all m datasets for every imputed variable.
#'   \item Convergence (trace) PDF — one page per variable, all chains overlaid.
#'   \item Density and strip-plot PDFs for observed-vs-imputed comparison.
#' }
#'
#' @param df Harmonised stacked OsteoLaus tibble from stack_visits().
#' @param m        Number of imputed datasets. Default 20L.
#' @param maxit    MICE iterations. Default 20L.
#' @param seed     Random seed. Default 2024L.
#' @param out_dir  Directory for diagnostic PDFs. Default "output/mice_diagnostics/osteo".
#' @param ...      Extra arguments forwarded to mice::mice().
#' @return List: mids, long, m, seed, imputed_vars, diag_paths.
# -----------------------------------------------------------------------------
# impute_mice_osteo()
# -----------------------------------------------------------------------------
impute_mice_osteo <- function(df,
                              m       = 20L,
                              maxit   = 20L,
                              seed    = 2024L,
                              out_dir = "03_outputs/mice_diagnostics/osteo") {
  
  cli::cli_h1("MICE Imputation — OsteoLaus (FCS-1L-wide)")
  
  # ---------------------------------------------------------------------------
  # 1. Wide reshape
  # ---------------------------------------------------------------------------
  
  observed_visits <- df |>
    dplyr::distinct(pt, .visit)
  pt_constants <- df |>
    dplyr::distinct(pt, .cohort)
  visit_levels <- levels(df$.visit)
  
  df <- df |>
    dplyr::mutate(dplyr::across(where(is.list), ~ unlist(.x, use.names = FALSE)))
  
  wide <- df |>
    dplyr::select(pt, .visit, dplyr::everything()) |>
    dplyr::mutate(exam_date_num = as.numeric(as.Date(exam_date_iso))) |>
    dplyr::select(-exam_date_iso) |>
    tidyr::pivot_wider(
      id_cols     = "pt",
      names_from  = ".visit",
      values_from = -c(pt, .visit),
      names_sep   = "_"
    )
  
  df_mice <- wide[, colSums(!is.na(wide)) > 0, drop = FALSE]
  
  # ---------------------------------------------------------------------------
  # 2. Define variable groups
  # ---------------------------------------------------------------------------
  
  weight_vars  <- intersect(paste0("Weight_",          visit_levels),      names(df_mice))
  height_vars  <- intersect(paste0("Height_",          visit_levels),      names(df_mice))
  larm12_vars  <- intersect(paste0("LARM_LEAN_MASS_",  visit_levels[1:2]), names(df_mice))
  larm35_vars  <- intersect(paste0("LARM_LEAN_MASS_",  visit_levels[3:5]), names(df_mice))
  rarm12_vars  <- intersect(paste0("RARM_LEAN_MASS_",  visit_levels[1:2]), names(df_mice))
  rarm35_vars  <- intersect(paste0("RARM_LEAN_MASS_",  visit_levels[3:5]), names(df_mice))
  lleg12_vars  <- intersect(paste0("LLEG_LEAN_MASS_",  visit_levels[1:2]), names(df_mice))
  lleg35_vars  <- intersect(paste0("LLEG_LEAN_MASS_",  visit_levels[3:5]), names(df_mice))
  rleg12_vars  <- intersect(paste0("RLEG_LEAN_MASS_",  visit_levels[1:2]), names(df_mice))
  rleg35_vars  <- intersect(paste0("RLEG_LEAN_MASS_",  visit_levels[3:5]), names(df_mice))
  gs_vars      <- intersect(paste0("gait_speed_",      visit_levels[4:5]), names(df_mice))
  hgs_vars     <- intersect(paste0("HGS_MAX_",         visit_levels[5]),   names(df_mice))
  time_vars    <- intersect(paste0("exam_date_num_",   visit_levels),      names(df_mice))
  id_vars      <- "pt"
  
  all_imputed <- c(weight_vars, height_vars,
                   larm12_vars, larm35_vars, rarm12_vars, rarm35_vars,
                   lleg12_vars, lleg35_vars, rleg12_vars, rleg35_vars,
                   gs_vars, hgs_vars)
  
  df_mice <- df_mice[, intersect(c(id_vars, time_vars, all_imputed), names(df_mice)), drop = FALSE]
  
  # Re-attach .cohort so it survives into the mids.
  df_mice <- dplyr::left_join(df_mice, pt_constants, by = "pt")
  
  #donor_summary <- .investigate_donor_pool(df_mice, all_imputed, min_donors = 20L)
  
  # ---------------------------------------------------------------------------
  # 4. Where matrix
  # ---------------------------------------------------------------------------
  
  where <- mice::make.where(df_mice, keyword = "missing")
  
  # ---------------------------------------------------------------------------
  # 5. Method vector
  # ---------------------------------------------------------------------------
  
  meth                              <- mice::make.method(df_mice)
  meth[]                            <- ""
  meth[intersect(all_imputed, names(meth))] <- "pmm"
  # Constants: never imputed
  meth[intersect(c(".cohort"), names(meth))] <- ""
  
  # ---------------------------------------------------------------------------
  # 6. Predictor matrix
  # ---------------------------------------------------------------------------

  pred <- mice::make.predictorMatrix(df_mice)
  pred[,] <- 0L

  set_pred <- function(pred, targets, predictors) {
    targets    <- intersect(targets,    rownames(pred))
    predictors <- intersect(predictors, colnames(pred))
    pred[targets, predictors] <- 1L
    pred
  }

  add_longitudinal <- function(pred, vars) {
    vars <- intersect(vars, colnames(pred))
    if (length(vars) > 1L) {
      pred[vars, vars] <- 1L
      idx <- match(vars, colnames(pred))
      pred[cbind(idx, idx)] <- 0L
    }
    pred
  }

  anthro     <- c(weight_vars, height_vars)
  alm35_vars <- c(larm35_vars, rarm35_vars, lleg35_vars, rleg35_vars)

  # =============================================================================
  # 1. WEIGHT
  # =============================================================================
  pred <- set_pred(pred, targets = weight_vars, predictors = height_vars)

  # =============================================================================
  # 2. HEIGHT
  # =============================================================================
  pred <- set_pred(pred,
                   targets    = height_vars,
                   predictors = c(weight_vars, alm35_vars, gs_vars, hgs_vars))

  # =============================================================================
  # 3. LIMB LEAN MASS — same scanner-era visits imputed together
  #    Hologic (Baseline, V2) and Lunar (V3, V4, V5) are kept as two
  #    separate longitudinal blocks because the two scanners are on
  #    different measurement scales.
  # =============================================================================
  pred <- add_longitudinal(pred, larm12_vars); pred <- add_longitudinal(pred, larm35_vars)
  pred <- add_longitudinal(pred, rarm12_vars); pred <- add_longitudinal(pred, rarm35_vars)
  pred <- add_longitudinal(pred, lleg12_vars); pred <- add_longitudinal(pred, lleg35_vars)
  pred <- add_longitudinal(pred, rleg12_vars); pred <- add_longitudinal(pred, rleg35_vars)

  pred <- set_pred(pred, targets = larm12_vars, predictors = c(anthro, rarm12_vars))
  pred <- set_pred(pred, targets = larm35_vars, predictors = c(anthro, rarm35_vars))
  pred <- set_pred(pred, targets = rarm12_vars, predictors = c(anthro, larm12_vars))
  pred <- set_pred(pred, targets = rarm35_vars, predictors = c(anthro, larm35_vars))
  pred <- set_pred(pred, targets = lleg12_vars, predictors = c(anthro, rleg12_vars))
  pred <- set_pred(pred, targets = lleg35_vars, predictors = c(anthro, rleg35_vars))
  pred <- set_pred(pred, targets = rleg12_vars, predictors = c(anthro, lleg12_vars))
  pred <- set_pred(pred, targets = rleg35_vars, predictors = c(anthro, lleg35_vars))

  # =============================================================================
  # 4. GAIT SPEED (V4, V5) & HAND-GRIP STRENGTH (V5)
  # =============================================================================
  pred <- set_pred(pred, targets = gs_vars,  predictors = c(anthro, alm35_vars))
  pred <- set_pred(pred, targets = hgs_vars, predictors = c(anthro, alm35_vars))

  # ---------------------------------------------------------------------------
  # Never impute id/time columns; constants are neither imputed nor predictors
  # ---------------------------------------------------------------------------
  pred[intersect(c(id_vars, time_vars), rownames(pred)), ] <- 0L

  const_cols <- intersect(c(".cohort"), colnames(pred))
  if (length(const_cols)) {
    pred[, const_cols] <- 0L
    pred[const_cols, ] <- 0L
  }
  
  cat("\nPredictor matrix created successfully!\n")
  cat("Dimensions:", nrow(pred), "x", ncol(pred), "\n")
  cat("Density:", sum(pred) / (nrow(pred)^2 - nrow(pred)), "\n")
  
  # ---------------------------------------------------------------------------
  # 7. Visit sequence
  # ---------------------------------------------------------------------------
  
  visit_seq <- mice::make.visitSequence(data = df_mice)
  
  # ---------------------------------------------------------------------------
  # 8. Run MICE
  # ---------------------------------------------------------------------------
  
  cli::cli_h2("Running MICE (m = {m}, maxit = {maxit}, seed = {seed})")
  
  mids_obj <- mice::mice(
    data            = df_mice,
    m               = m,
    maxit           = maxit,
    seed            = seed,
    method          = meth,
    predictorMatrix = pred,
    where           = where,
    visitSequence   = visit_seq,
    ridge           = 1e-4, 
    eps             = 1e-4,
    printFlag       = FALSE,
    donors = 10L
  )
  
  # ---------------------------------------------------------------------------
  # 9a. Back to long
  # ---------------------------------------------------------------------------
  
  visit_pat <- paste(visit_levels, collapse = "|")
  
  long_df <- mice::complete(mids_obj, action = "long", include = TRUE) |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      cols = -c(.imp, .id, pt),
      names_to  = c(".value", ".visit"),
      names_pattern = paste0("^(.+)_(", visit_pat, ")$")
    ) |>
    dplyr::mutate(
      .visit        = factor(.visit, levels = visit_levels),
      exam_date_iso = as.Date(exam_date_num, origin = "1970-01-01")
    ) |>
    dplyr::select(-dplyr::starts_with("exam_date_num")) |>
    # Reassign .id to be unique within each .imp after pivot_longer.
    dplyr::group_by(.imp) |>
    dplyr::mutate(.id = dplyr::row_number()) |>
    dplyr::ungroup()
  
  # ---------------------------------------------------------------------------
  # 9b. Reattach non-imputed variables
  # ---------------------------------------------------------------------------
  
  # delete visit rows that were introduced and did not occur in real life
  long_df <- long_df |>
    dplyr::inner_join(
      observed_visits,
      by = c("pt", ".visit")
    )
  
  
  key_cols      <- c("pt", ".visit")
  orig_key      <- dplyr::select(df, pt, .visit, dplyr::everything())
  existing_cols <- names(long_df)
  extra_cols    <- setdiff(names(orig_key), c(existing_cols, key_cols))
  
  if (length(extra_cols) > 0) {
    long_df <- long_df |>
      dplyr::left_join(
        dplyr::select(orig_key, pt, .visit, dplyr::all_of(extra_cols)),
        by = c("pt", ".visit")
      )
  }
  
  # ---------------------------------------------------------------------------
  # 10. Post-imputation diagnostics
  # ---------------------------------------------------------------------------
  
  # Base variable names (strip visit suffix) for long-format NA check
  base_imputed <- c("Weight", "Height",
                    "LARM_LEAN_MASS", "RARM_LEAN_MASS",
                    "LLEG_LEAN_MASS", "RLEG_LEAN_MASS",
                    "gait_speed", "HGS_MAX")
  
  diag_paths <- .save_diagnostics(mids_obj, out_dir, label = "osteo")
  
  # ---------------------------------------------------------------------------
  # 10b. Rebuild mids in long format
  # ---------------------------------------------------------------------------
  long_mids <- mice::as.mids(long_df)
  
  # ---------------------------------------------------------------------------
  # 11. Output
  # ---------------------------------------------------------------------------
  
  list(
    df_wide      = df_mice,
    mids         = long_mids,
    mids_wide    = mids_obj,
    long         = long_df,
    m            = m,
    seed         = seed,
    imputed_vars = all_imputed,
    diag_paths   = diag_paths
  )
}


# =============================================================================
# Internal: post-imputation diagnostics (standalone helper, kept for back-compat)
# =============================================================================

# -----------------------------------------------------------------------------
# post_imputation_checks()
# -----------------------------------------------------------------------------
#' Write convergence/density/strip diagnostic PDFs for a mids object.
#'
#' Thin wrapper around `.save_diagnostics()`, called from
#' `build_analysis_dataset()` (R/02_build_analysis_dataset.R) after each
#' `impute_mice_*()` call.
#'
#' @param mids_obj A mids object returned by mice::mice().
#' @param out_dir  Directory to write PDFs into.
#' @return Invisibly, a list with the three PDF paths (see `.save_diagnostics()`).
post_imputation_checks <- function(mids_obj, out_dir) {
  .save_diagnostics(mids_obj, out_dir, label = "imputation")
}