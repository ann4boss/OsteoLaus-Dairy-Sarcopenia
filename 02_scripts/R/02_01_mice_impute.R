# =============================================================================
# R/mice_impute.R
# =============================================================================
# Numeric variables: imputed with pmm 
.CL_PMM_VARS <- c( "Age", "BMI", "Height", "Weight", "conso_hebdo", "sumalco", 
                   "PAFQ_MPA","PAFQ_VPA", "sumtot1", "sumprot1", "sumgluc1", 
                   "sumlipi1", "esthrpage", "HGS_MAX",
                   paste0("FFQ", c(1:8, 52, 53, 63, 68, 71, 82:86), "amount") ) 
# Binary factor variables: imputed with logreg 
.CL_LOGREG_VARS <- c( "dbtld", "DIAB", "DIAB_Hb", "esthrp", "antiHTA", "crbpmed", 
                      "HTA", "miac", "strk", "chf", "cad", "angn", "cmp", "hdc", 
                      "hdv", "artm", "vslg", "ccth", "cabg", "pcin" ) 
# Ordered factor variables: imputed with polr 
.CL_POLR_VARS <- c( "sbsmk", # smoking (Never / Former / Current) 
                    "edtyp4" # education (4-level ordered) 
                    ) 
# Predictor-only: included in mice data, never imputed 
.CL_AUX_VARS <- c( "pt", ".visit", "exam_date_iso", "alcuse", "metab_synd", 
                   "antiDIAB", "hctld", "hypolip", "lateralite", "WHR", "PAFQ_SE_pct", 
                   "PAFQ_LPA_pct", "PAFQ_MPA_pct", "PAFQ_VPA_pct", "mnwlk", "phyact",
                   "sumvitd1", "PAFQ_SE", "PAFQ_LPA", "bmpsc", "mrtsts2", "dbdrg" )


# =============================================================================
# CoLaus imputation
# =============================================================================

#' Impute missing CoLaus covariates and exposures using MICE (pre-derivation).
#'
#' Imputation targets raw harmonised columns so that derived variables
#' (smoking_status, alcohol_category, education_level, etc.) are computed from
#' imputed primitives via derive_colaus(), not imputed directly.
#'
#' Visit restrictions from .CL_IMPUTE_AT are applied via a `where` matrix
#' (mice::make.where). Cells outside allowed visits are set to FALSE —
#' mice will never impute those cells, even if the value is NA.
#' This is the correct approach per the mice documentation and avoids the
#' previous bug of silently skipping entire variables (logreg/polr) because
#' they were excluded from .CL_IMPUTED_VARS.
#'
#' @param df Harmonised stacked CoLaus tibble from stack_visits().
#' @param m        Number of imputed datasets. Default 20L.
#' @param maxit    MICE iterations. Default 20L.
#' @param seed     Random seed. Default 2024L.
#' @param mincor   quickpred |correlation| threshold. Default 0.10.
#' @param n_cores  Parallel workers (requires future). Default 1L.
#' @param min_class_n Minimum observed cases per class for logreg/polr. Default 5L.
#' @param ...      Extra arguments forwarded to mice::mice().
#' @return List: mids, long, m, seed, imputed_vars.
impute_mice_colaus <- function(df,
                               m     = 20L,
                               maxit = 20L,
                               seed  = 2024L) {
  
  cli::cli_h1("MICE Imputation — CoLaus (FCS-1L-wide)")
  
  # ---------------------------------------------------------------------------
  # 1. Wide reshape
  # ---------------------------------------------------------------------------
  
  visit_levels <- levels(df$.visit)
  
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
  # 2. Variable groups (wide names)
  # ---------------------------------------------------------------------------
  
  make_wide <- function(vars) {
    as.vector(outer(vars, visit_levels, paste, sep = "_"))
  }
  
  pmm_vars    <- intersect(make_wide(.CL_PMM_VARS), names(df_mice))
  logreg_vars <- intersect(make_wide(.CL_LOGREG_VARS), names(df_mice))
  polr_vars   <- intersect(make_wide(.CL_POLR_VARS), names(df_mice))
  aux_vars    <- intersect(make_wide(.CL_AUX_VARS), names(df_mice))
  
  id_vars   <- "pt"
  time_vars <- intersect(paste0("exam_date_num_", visit_levels), names(df_mice))
  
  all_imputed <- c(pmm_vars, logreg_vars, polr_vars)
  
  df_mice <- df_mice[, intersect(c(id_vars, time_vars, all_imputed, aux_vars), names(df_mice)), drop = FALSE]
  
  # ---------------------------------------------------------------------------
  # 3. Where matrix
  # ---------------------------------------------------------------------------
  
  where <- mice::make.where(df_mice, keyword = "missing")
  
  # Never impute auxiliary variables
  where[, intersect(aux_vars, colnames(where))] <- FALSE
  
  # ---------------------------------------------------------------------------
  # 4. Method vector
  # ---------------------------------------------------------------------------
  
  meth <- mice::make.method(df_mice)
  meth[] <- ""
  
  meth[intersect(pmm_vars, names(meth))]    <- "pmm"
  meth[intersect(logreg_vars, names(meth))] <- "logreg"
  meth[intersect(polr_vars, names(meth))]   <- "polr"
  
  # ---------------------------------------------------------------------------
  # 5. Predictor matrix
  # ---------------------------------------------------------------------------
  
  pred <- mice::make.predictorMatrix(df_mice)
  pred[,] <- 0L
  
  # ID + time predictors
  aux_pred <- intersect(c(id_vars, time_vars), colnames(pred))
  rows     <- intersect(all_imputed, rownames(pred))
  pred[rows, aux_pred] <- 1L
  
  # Longitudinal structure (same variable across visits)
  base_vars <- unique(c(.CL_PMM_VARS, .CL_LOGREG_VARS, .CL_POLR_VARS))
  
  add_longitudinal <- function(base) {
    vars <- intersect(paste0(base, "_", visit_levels), colnames(pred))
    if (length(vars) > 1L) {
      pred[vars, vars] <<- 1L
      idx <- match(vars, colnames(pred))
      pred[cbind(idx, idx)] <<- 0L
    }
  }
  
  lapply(base_vars, add_longitudinal)
  
  # Temporal restriction (no future → past)
  restrict_time <- function(base) {
    vars <- intersect(paste0(base, "_", visit_levels), colnames(pred))
    
    if (length(vars) < 2) return(NULL)
    
    idx <- match(vars, colnames(pred))
    
    for (k in seq_along(idx)) {
      later <- idx[(k+1):length(idx)]
      later <- later[!is.na(later)]
      
      if (length(later)) {
        pred[idx[k], later] <<- 0L
      }
    }
  }
  
  lapply(base_vars, restrict_time)
  
  # Auxiliary variables act as predictors only
  aux_cols <- intersect(aux_vars, colnames(pred))
  rows <- intersect(rows, rownames(pred))
  aux_pred <- intersect(aux_pred, colnames(pred))
  pred[aux_cols, ] <- 0L
  
  # ---------------------------------------------------------------------------
  # 6. Visit sequence
  # ---------------------------------------------------------------------------
  
  visit_seq <- mice::make.visitSequence(data = df_mice)
  
  # ---------------------------------------------------------------------------
  # 7. Run MICE
  # ---------------------------------------------------------------------------
  
  cli::cli_h2("Running MICE")
  
  mids_obj <- mice::mice(
    data            = df_mice,
    m               = m,
    maxit           = maxit,
    seed            = seed,
    method          = meth,
    predictorMatrix = pred,
    where           = where,
    visitSequence   = visit_seq,
    printFlag       = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # 8a. Back to long
  # ---------------------------------------------------------------------------
  
  visit_pat <- paste(visit_levels, collapse = "|")
  
  long_df <- mice::complete(mids_obj, action = "long", include = TRUE) |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      cols = -c(.imp, .id, pt),
      names_to = c(".value", ".visit"),
      names_pattern = paste0("^(.+)_(", visit_pat, ")$")
    ) |>
    dplyr::mutate(
      .visit        = factor(.visit, levels = visit_levels),
      exam_date_iso = as.Date(exam_date_num, origin = "1970-01-01")
    ) |>
    dplyr::select(-dplyr::starts_with("exam_date_num"))
  
  # ---------------------------------------------------------------------------
  # 8b. Reattach non-imputed variables
  # ---------------------------------------------------------------------------
  
  # Columns that should never be used as join payload
  key_cols <- c("pt", ".visit")
  
  # Original data in long format
  orig_long <- df
  
  # Keep only key + extra variables
  orig_key <- orig_long |>
    dplyr::select(pt, .visit, dplyr::everything())
  
  # Identify variables already present in long_df
  existing_cols <- names(long_df)
  
  # Select ONLY columns that are not already in long_df and not keys
  extra_cols <- setdiff(names(orig_key), c(existing_cols, key_cols))
  
  if (length(extra_cols) > 0) {
    
    orig_key <- orig_key |>
      dplyr::select(pt, .visit, dplyr::all_of(extra_cols))
    
    long_df <- long_df |>
      dplyr::left_join(orig_key, by = c("pt", ".visit"))
  }
  
  # ---------------------------------------------------------------------------
  # 9. Output
  # ---------------------------------------------------------------------------
  
  list(
    mids         = mids_obj,
    long         = long_df,
    m            = m,
    seed         = seed,
    imputed_vars = all_imputed
  )
}
# =============================================================================
# OsteoLaus imputation
# =============================================================================

#' Impute missing OsteoLaus outcomes using MICE — FCS-1L-wide approach.
#'
#' Implements the FCS-1L-wide strategy recommended by Wijesuriya et al. (2025,
#' Stat Med e10274) for longitudinal data: reshape to wide format (one row per
#' participant, one column per variable × visit), impute, then reshape back to
#' long. This captures within-person correlation across visits while remaining
#' simple to implement and computationally efficient.
#'
#' Visit-specific imputation constraints enforced:
#' \itemize{
#'   \item \strong{Age, Weight, Height}: imputed at all visits.
#'   \item \strong{ALM}: imputed at all visits, but ALM columns from visits
#'     using one DXA method (Baseline/V2) are blocked from predicting ALM
#'     columns from visits using a different method (V3–V5) and vice versa.
#'     The DXA method groups are detected automatically from the \code{DXA_method}
#'     column in the data.
#'   \item \strong{gait_speed}: imputed only at V4 and V5 (missing by design
#'     at earlier visits — all-NA wide columns are dropped before imputation).
#'   \item \strong{HGS_MAX}: imputed only at V5 (missing by design elsewhere).
#' }
#'
#' @param df Harmonised stacked OsteoLaus tibble from stack_visits().
#' @param m        Number of imputed datasets. Default 20L.
#' @param maxit    MICE iterations. Default 20L.
#' @param seed     Random seed. Default 2024L.
#' @param mincor   quickpred |correlation| threshold. Default 0.10.
#' @param n_cores  Parallel workers (requires future). Default 1L.
#' @param ...      Extra arguments forwarded to mice::mice().
#' @return List: mids (wide-format mids object), long (long-format tibble),
#'   m, seed, imputed_vars.

impute_mice_osteo <- function(df,
                              m     = 20L,
                              maxit = 20L,
                              seed  = 2024L) {
  
  cli::cli_h1("MICE Imputation — OsteoLaus (FCS-1L-wide)")
  
  # ---------------------------------------------------------------------------
  # 1. Wide reshape
  # ---------------------------------------------------------------------------
  
  visit_levels <- levels(df$.visit)
  
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
  
  age_vars    <- intersect(paste0("Age_",        visit_levels), names(df_mice))
  weight_vars <- intersect(paste0("Weight_",     visit_levels), names(df_mice))
  height_vars <- intersect(paste0("Height_",     visit_levels), names(df_mice))
  larm12_vars  <- intersect(paste0("LARM_LEAN_MASS_",        visit_levels[1:2]), names(df_mice))
  larm35_vars  <- intersect(paste0("LARM_LEAN_MASS_",        visit_levels[3:5]), names(df_mice))
  rarm12_vars  <- intersect(paste0("RARM_LEAN_MASS_",        visit_levels[1:2]), names(df_mice))
  rarm35_vars  <- intersect(paste0("RARM_LEAN_MASS_",        visit_levels[3:5]), names(df_mice))
  lleg12_vars  <- intersect(paste0("LLEG_LEAN_MASS_",        visit_levels[1:2]), names(df_mice))
  lleg35_vars  <- intersect(paste0("LLEG_LEAN_MASS_",        visit_levels[3:5]), names(df_mice))
  rleg12_vars  <- intersect(paste0("RLEG_LEAN_MASS_",        visit_levels[1:2]), names(df_mice))
  rleg35_vars  <- intersect(paste0("RLEG_LEAN_MASS_",        visit_levels[3:5]), names(df_mice))
  gs_vars     <- intersect(paste0("gait_speed_", visit_levels[4:5]), names(df_mice))
  hgs_vars    <- intersect(paste0("HGS_MAX_",    visit_levels[5]),   names(df_mice))
  time_vars   <- intersect(paste0("exam_date_num_", visit_levels),   names(df_mice))
  id_vars     <- "pt"
  
  all_imputed <- c(age_vars, weight_vars, height_vars,
                   larm12_vars, larm35_vars,rarm12_vars, rarm35_vars,
                   lleg12_vars, lleg35_vars, rleg12_vars, rleg35_vars,
                   gs_vars, hgs_vars)
  
  df_mice <- df_mice[, intersect(c(id_vars, time_vars, all_imputed), names(df_mice)), drop = FALSE]
  
  # ---------------------------------------------------------------------------
  # 3. Where matrix
  # ---------------------------------------------------------------------------
  
  where <- mice::make.where(df_mice, keyword = "missing")
  
  # ---------------------------------------------------------------------------
  # 4. Method vector
  # ---------------------------------------------------------------------------
  
  meth           <- mice::make.method(df_mice)
  meth[]         <- ""
  meth[intersect(all_imputed, names(meth))] <- "pmm"
  
  # ---------------------------------------------------------------------------
  # 5. Predictor matrix
  # ---------------------------------------------------------------------------
  
  pred <- mice::make.predictorMatrix(df_mice)
  pred[,] <- 0L
  
  # ID + time predictors
  aux  <- intersect(c(id_vars, time_vars), colnames(pred))
  rows <- intersect(all_imputed, rownames(pred))
  pred[rows, aux] <- 1L
  
  # helper
  add_longitudinal <- function(vars) {
    vars <- intersect(vars, colnames(pred))
    if (length(vars) > 1L) {
      pred[vars, vars] <<- 1L
      idx <- match(vars, colnames(pred))
      pred[cbind(idx, idx)] <<- 0L
    }
  }
  
  add_longitudinal(age_vars)
  add_longitudinal(weight_vars)
  add_longitudinal(height_vars)
  add_longitudinal(larm12_vars)
  add_longitudinal(larm35_vars)
  add_longitudinal(rarm12_vars)
  add_longitudinal(rarm35_vars)
  add_longitudinal(lleg12_vars)
  add_longitudinal(lleg35_vars)
  add_longitudinal(rleg12_vars)
  add_longitudinal(rleg35_vars)
  
  # Cross-variable structure
  anthro <- c(age_vars, weight_vars, height_vars)
  
  alm12_vars <- c(larm12_vars, rarm12_vars, lleg12_vars, rleg12_vars)
  
  alm35_vars <- c(larm35_vars, rarm35_vars, lleg35_vars, rleg35_vars)
  
  pred[intersect(larm12_vars, rownames(pred)),
       intersect(c(anthro, rarm12_vars) , colnames(pred))] <- 1L
  
  pred[intersect(larm35_vars, rownames(pred)),
       intersect(c(anthro, rarm35_vars) , colnames(pred))] <- 1L
  
  pred[intersect(rarm12_vars, rownames(pred)),
       intersect(c(anthro, larm12_vars) , colnames(pred))] <- 1L
  
  pred[intersect(rarm35_vars, rownames(pred)),
       intersect(c(anthro, larm35_vars) , colnames(pred))] <- 1L
  
  pred[intersect(lleg12_vars, rownames(pred)),
       intersect(c(anthro, rleg12_vars) , colnames(pred))] <- 1L
  
  pred[intersect(lleg35_vars, rownames(pred)),
       intersect(c(anthro, rleg35_vars) , colnames(pred))] <- 1L
  
  pred[intersect(rleg12_vars, rownames(pred)),
       intersect(c(anthro, lleg12_vars) , colnames(pred))] <- 1L
  
  pred[intersect(rleg35_vars, rownames(pred)),
       intersect(c(anthro, lleg35_vars) , colnames(pred))] <- 1L
  
  
  pred[intersect(gs_vars, rownames(pred)),
       intersect(c(anthro, alm35_vars), colnames(pred))] <- 1L
  
  pred[intersect(hgs_vars, rownames(pred)),
       intersect(c(anthro, alm35_vars), colnames(pred))] <- 1L
  
  # Temporal restriction
  restrict_time <- function(vars) {
    vars <- intersect(vars, colnames(pred))
    for (i in seq_along(vars)) {
      for (j in seq_along(vars)) {
        if (j > i) {
          r <- vars[i]
          c <- vars[j]
          if (r %in% rownames(pred) && c %in% colnames(pred)) {
            pred[r, c] <<- 0L
          }
        }
      }
    }
  }
  
  restrict_time(age_vars)
  restrict_time(weight_vars)
  restrict_time(height_vars)
  restrict_time(larm12_vars)
  restrict_time(larm35_vars)
  restrict_time(rarm12_vars)
  restrict_time(rarm35_vars)
  restrict_time(lleg12_vars)
  restrict_time(lleg35_vars)
  restrict_time(rleg12_vars)
  restrict_time(rleg35_vars)

  
  pred[intersect(c(id_vars, time_vars), rownames(pred)), ] <- 0L
  
  # ---------------------------------------------------------------------------
  # 6. Visit sequence
  # ---------------------------------------------------------------------------
  
  visit_seq <- mice::make.visitSequence(data = df_mice)
  
  # ---------------------------------------------------------------------------
  # 7. Diagnostics
  # ---------------------------------------------------------------------------
  
  cli::cli_h2("Missingness summary (pre-imputation)")
  mice::md.pattern(df_mice, plot = FALSE) |>
    utils::capture.output() |>
    paste(collapse = "\n") |>
    cli::cli_inform()
  
  cli::cli_inform(c(
    "i" = "Active imputation targets: {length(all_imputed)}",
    "i" = "Cells flagged for imputation: {sum(where)}"
  ))
  
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
    printFlag       = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # 9a. Back to long
  # ---------------------------------------------------------------------------
  
  visit_pat <- paste(visit_levels, collapse = "|")
  
  long_df <- mice::complete(mids_obj, action = "long", include = TRUE) |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      cols = -c(.imp, .id, pt),
      names_to = c(".value", ".visit"),
      names_pattern = paste0("^(.+)_(", visit_pat, ")$")
    ) |>
    dplyr::mutate(
      .visit        = factor(.visit, levels = visit_levels),
      exam_date_iso = as.Date(exam_date_num, origin = "1970-01-01")
    ) |>
    dplyr::select(-dplyr::starts_with("exam_date_num"))
  
  
  # ---------------------------------------------------------------------------
  # 9b. Reattach non-imputed variables
  # ---------------------------------------------------------------------------
  
  # Columns that should never be used as join payload
  key_cols <- c("pt", ".visit")
  
  # Original data in long format
  orig_long <- df
  
  # Keep only key + extra variables
  orig_key <- orig_long |>
    dplyr::select(pt, .visit, dplyr::everything())
  
  # Identify variables already present in long_df
  existing_cols <- names(long_df)
  
  # Select ONLY columns that are not already in long_df and not keys
  extra_cols <- setdiff(names(orig_key), c(existing_cols, key_cols))
  
  if (length(extra_cols) > 0) {
    
    orig_key <- orig_key |>
      dplyr::select(pt, .visit, dplyr::all_of(extra_cols))
    
    long_df <- long_df |>
      dplyr::left_join(orig_key, by = c("pt", ".visit"))
  }
  
  # ---------------------------------------------------------------------------
  # 10. Output
  # ---------------------------------------------------------------------------
  
  list(
    mids         = mids_obj,
    long         = long_df,
    m            = m,
    seed         = seed,
    imputed_vars = all_imputed
  )
}


# =============================================================================
# Internal: Post-imputation diagnostics
# =============================================================================

post_imputation_checks <- function(mids_obj, out_dir) {
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  cli::cli_h2("Post-imputation diagnostics")
  
  vars <- names(mids_obj$long)
  
  # -------------------------------------------------------------------------
  # 1. Trace plots (multi-page)
  # -------------------------------------------------------------------------
  pdf(file.path(out_dir, "traceplots.pdf"), width = 10, height = 7)
  
  for (v in vars) {
    try({
      plot(mids_obj, v)
    }, silent = TRUE)
  }
  
  dev.off()
  
  # -------------------------------------------------------------------------
  # 2. Density plots (multi-page)
  # -------------------------------------------------------------------------
  pdf(file.path(out_dir, "densityplots.pdf"), width = 10, height = 7)
  
  for (v in vars) {
    try({
      print(mice::densityplot(mids_obj, as.formula(paste("~", v))))
    }, silent = TRUE)
  }
  
  dev.off()
  
  # -------------------------------------------------------------------------
  # 3. Strip plots (multi-page)
  # -------------------------------------------------------------------------
  pdf(file.path(out_dir, "stripplots.pdf"), width = 10, height = 7)
  
  for (v in vars) {
    try({
      print(mice::stripplot(mids_obj, as.formula(paste(v, "~ .imp")),
                            pch = 20, cex = 0.5))
    }, silent = TRUE)
  }
  
  dev.off()
  
  # -------------------------------------------------------------------------
  # 4. Basic plausibility checks
  # -------------------------------------------------------------------------
  comp1 <- mice::complete(mids_obj$mids, 1)
  
  num_vars <- sapply(comp1, is.numeric)
  
  summary_df <- data.frame(
    variable = names(comp1)[num_vars],
    min = sapply(comp1[num_vars], min, na.rm = TRUE),
    max = sapply(comp1[num_vars], max, na.rm = TRUE)
  )
  
  write.csv(
    summary_df,
    file.path(out_dir, "range_check.csv"),
    row.names = FALSE
  )
  
  cli::cli_inform("Post-imputation diagnostics saved to {out_dir}")
}
