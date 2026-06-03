# =============================================================================
# R/mice_impute.R
# =============================================================================
# Numeric variables: imputed with pmm 
.CL_PMM_VARS <- c( "BMI", "Height", "Weight", "conso_hebdo", "sumalco", 
                   "PAFQ_MPA","PAFQ_VPA", "sumtot1", "sumprot1", "sumgluc1", 
                   "sumlipi1", "esthrpage", "HGS_MAX",
                   paste0("FFQ", 1:100, "amount"),
                   paste0("freqFFQ", c(1:8, 85, 86)),
                   paste0("FFQp",    c(1:8, 85, 86))
                   ) 
# Binary factor variables: imputed with logreg 
.CL_LOGREG_VARS <- c( "dbtld", "DIAB", "DIAB_Hb", "esthrp", "antiHTA", "crbpmed", 
                      "HTA", "miac", "strk", "chf", "cad", "angn", "cmp", "hdc", 
                      "hdv", "artm", "vslg", "ccth", "cabg", "pcin" ) 
# Ordered factor variables: imputed with polr 
.CL_POLR_VARS <- c( "sbsmk", # smoking (Never / Former / Current) 
                    "edtyp4", # education (4-level ordered) 
                    "mrtsts2"
) 
# Predictor-only: included in mice data, never imputed 
.CL_AUX_VARS <- c(
  "alcuse", "metab_synd",
  "antiDIAB", "hctld", "hypolip",
  "lateralite",
  "PAFQ_SE_pct", "PAFQ_LPA_pct",
  "PAFQ_MPA_pct", "PAFQ_VPA_pct",
  "sumvitd1",
  "bmpsc", "dbdrg"
)

 
# =============================================================================
# Internal helpers
# =============================================================================

#' Summarise missingness for a set of variables in a wide data frame.
#'
#' Returns a tibble with columns: variable, n_miss, pct_miss.
#'
#' @param df      Wide data frame.
#' @param vars    Character vector of column names to check.
#' @param label   Short label used in the cli header ("Pre" or "Post").
.missingness_summary <- function(df, vars, label = "Pre") {
  vars_present <- intersect(vars, names(df))
  
  tbl <- tibble::tibble(
    variable = vars_present,
    n_miss   = sapply(vars_present, function(v) sum(is.na(df[[v]]))),
    n_total  = nrow(df),
    pct_miss = round(n_miss / n_total * 100, 1)
  )
  
  cli::cli_h3("{label}-imputation missingness ({nrow(tbl)} variables)")
  
  any_miss <- tbl[tbl$n_miss > 0, ]
  
  if (nrow(any_miss) == 0) {
    cli::cli_inform(c("v" = "No missing values in imputation targets."))
  } else {
    cli::cli_inform(c(
      "i" = "{nrow(any_miss)} variable(s) with missing values:"
    ))
    # Print top offenders (up to 20)
    top <- any_miss[order(-any_miss$pct_miss), ][seq_len(min(20, nrow(any_miss))), ]
    for (i in seq_len(nrow(top))) {
      cli::cli_inform(
        "  {top$variable[i]}: {top$n_miss[i]} / {top$n_total[i]} ({top$pct_miss[i]}%)"
      )
    }
    if (nrow(any_miss) > 20) {
      cli::cli_inform("  ... and {nrow(any_miss) - 20} more.")
    }
  }
  
  invisible(tbl)
}


#' Assert that all imputed variables are NA-free across all m datasets.
#'
#' Emits a cli warning for each variable that still has NAs; otherwise
#' confirms success.
#'
#' @param long_df  Long-format tibble from mice::complete(..., action = "long").
#' @param vars     Character vector of (wide) column names that were imputed.
#'                 Variables are matched against long_df by exact name after
#'                 stripping the visit suffix.
.assert_no_na_after_imputation <- function(long_df, imputed_base_vars) {
  cli::cli_h3("Post-imputation NA check")
  
  # In long format the columns are the base variable names (no visit suffix)
  long_imputed <- intersect(imputed_base_vars, names(long_df))
  
  # Only check imputed datasets (.imp > 0)
  check_df <- long_df[long_df$.imp > 0, long_imputed, drop = FALSE]
  
  n_miss <- sapply(long_imputed, function(v) sum(is.na(check_df[[v]])))
  problems <- n_miss[n_miss > 0]
  
  if (length(problems) == 0) {
    cli::cli_inform(c("v" = "All {length(long_imputed)} imputed variable(s) are NA-free across all {max(long_df$.imp)} dataset(s)."))
  } else {
    cli::cli_warn(c(
      "!" = "{length(problems)} imputed variable(s) still contain NAs:",
      setNames(paste0(names(problems), ": ", problems, " NA(s)"), rep("*", length(problems)))
    ))
  }
  
  invisible(n_miss)
}


#' Save convergence (trace) plot and per-variable density/strip plots to PDF.
#'
#' Convergence is assessed via the standard mice trace plot: mean and SD of
#' each imputed variable per iteration, across chains (imputed datasets). When
#' the lines mix and show no trend, imputation has converged.
#'
#' @param mids_obj  A mids object returned by mice::mice().
#' @param out_dir   Directory to write PDFs into.
#' @param label     Short label prepended to filenames ("colaus" or "osteo").
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
#' @return List: mids, long, m, seed, imputed_vars, miss_pre, miss_post, diag_paths.
impute_mice_colaus <- function(df,
                               m       = 20L,
                               maxit   = 20L,
                               seed    = 2024L,
                               out_dir = "03_outputs/mice_diagnostics/colaus") {
  
  cli::cli_h1("MICE Imputation — CoLaus (FCS-1L-wide)")
  
  # ---------------------------------------------------------------------------
  # 1. Wide reshape
  # ---------------------------------------------------------------------------
  
  visit_levels <- levels(df$.visit)
  
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
  aux_vars    <- intersect(make_wide(.CL_AUX_VARS),    names(df_mice))
  
  id_vars   <- "pt"
  time_vars <- intersect(paste0("exam_date_num_", visit_levels), names(df_mice))
  
  all_imputed <- c(pmm_vars, logreg_vars, polr_vars)
  
  df_mice <- df_mice[, intersect(c(id_vars, time_vars, all_imputed, aux_vars), names(df_mice)), drop = FALSE]
  
  # ---------------------------------------------------------------------------
  # 3. Pre-imputation missingness
  # ---------------------------------------------------------------------------
  
  miss_pre <- .missingness_summary(df_mice, all_imputed, label = "Pre")
  
  # ---------------------------------------------------------------------------
  # 4. Where matrix
  # ---------------------------------------------------------------------------
  
  where <- mice::make.where(df_mice, keyword = "missing")
  where[, intersect(aux_vars, colnames(where))] <- FALSE
  
  # ---------------------------------------------------------------------------
  # 5. Method vector
  # ---------------------------------------------------------------------------
  
  meth <- mice::make.method(df_mice)
  meth[] <- ""
  meth[intersect(pmm_vars,    names(meth))] <- "pmm"
  meth[intersect(logreg_vars, names(meth))] <- "logreg"
  meth[intersect(polr_vars,   names(meth))] <- "polr"
  
  # ---------------------------------------------------------------------------
  # 6. Predictor matrix
  # ---------------------------------------------------------------------------
  
  # Design principles:
  # 1. No full connectivity (avoids singular matrices)
  # 2. Within-variable longitudinal structure only
  # 3. Within-domain blocks for correlated predictors
  # 4. Auxiliary variables used only as predictors (not imputed)
  # 5. Strict separation of roles (no variable appears in multiple roles)
  # 6. No forward-time leakage across visits
  # ---------------------------------------------------------------------------
  
  pred <- mice::make.predictorMatrix(df_mice)
  
  # Start from empty matrix (manual control of all dependencies)
  pred[,] <- 0L
  diag(pred) <- 0L
  
  
  # ---------------------------------------------------------------------------
  # Helper: create symmetric within-block prediction
  # ---------------------------------------------------------------------------
  add_block <- function(vars) {
    vars <- intersect(vars, colnames(pred))
    if (length(vars) > 1) {
      pred[vars, vars] <<- 1L
      diag(pred[vars, vars]) <<- 0L
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Longitudinal structure (same variable across visits)
  # ---------------------------------------------------------------------------
  base_vars <- c(.CL_PMM_VARS, .CL_LOGREG_VARS, .CL_POLR_VARS)
  
  add_longitudinal <- function(base) {
    vars <- intersect(paste0(base, "_", visit_levels), colnames(pred))
    add_block(vars)
  }
  
  lapply(base_vars, add_longitudinal)
  
  
  # ---------------------------------------------------------------------------
  # Domain-based predictor blocks (cross-sectional structure)
  # ---------------------------------------------------------------------------
  
  # Anthropometrics (keep tightly linked)
  add_block(c("BMI", "Weight", "Height"))
  
  # Physical activity block
  add_block(c("PAFQ_MPA", "PAFQ_VPA", "PAFQ_SE", "mnwlk", "phyact"))
  
  # Clinical binary disease block
  add_block(.CL_LOGREG_VARS)
  
  # Ordinal variables
  add_block(.CL_POLR_VARS)
  
  # Dietary FFQ block (high-dimensional, but internally consistent)
  ffq_vars <- c(
    paste0("FFQ", 1:100, "amount"),
    paste0("freqFFQ", c(1:8, 85, 86)),
    paste0("FFQp", c(1:8, 85, 86))
  )
  
  add_block(ffq_vars)
  
  
  # ---------------------------------------------------------------------------
  # Auxiliary variables (predictors only, NOT imputed)
  # ---------------------------------------------------------------------------
  aux_cols <- intersect(.CL_AUX_VARS, colnames(pred))
  
  # Allow aux variables to predict everything
  pred[, aux_cols] <- 1L
  
  # Prevent imputation of auxiliary variables themselves
  pred[aux_cols, ] <- 0L
  
  
  # ---------------------------------------------------------------------------
  # ID and time variables (structural predictors only)
  # ---------------------------------------------------------------------------
  # These help stabilize models but are never imputed
  pred[, id_vars] <- 1L
  pred[, time_vars] <- 1L
  
  
  # ---------------------------------------------------------------------------
  # Prevent forward-time leakage within longitudinal variables
  # ---------------------------------------------------------------------------
  restrict_time <- function(base) {
    vars <- intersect(paste0(base, "_", visit_levels), colnames(pred))
    if (length(vars) < 2) return(NULL)
    
    idx <- match(vars, colnames(pred))
    
    for (k in seq_along(idx)) {
      later <- idx[(k + 1):length(idx)]
      later <- later[!is.na(later)]
      
      if (length(later)) {
        pred[idx[k], later] <<- 0L
      }
    }
  }
  
  lapply(base_vars, restrict_time)
  
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
    printFlag       = FALSE
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
    dplyr::select(-dplyr::starts_with("exam_date_num")) 
  
  
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
  
  # Missingness in completed dataset (imp == 1 as representative)
  comp1     <- mice::complete(mids_obj, 1)
  miss_post <- .missingness_summary(comp1, all_imputed, label = "Post")
  
  # Assert no NAs remain in imputed variables across all datasets
  .assert_no_na_after_imputation(long_df, base_imputed)
  
  # Convergence + density + strip plots saved to disk
  diag_paths <- .save_diagnostics(mids_obj, out_dir, label = "colaus")
  
  # ---------------------------------------------------------------------------
  # 11. Output
  # ---------------------------------------------------------------------------
  
  list(
    mids         = mids_obj,
    long         = long_df,
    m            = m,
    seed         = seed,
    imputed_vars = all_imputed,
    miss_pre     = miss_pre,
    miss_post    = miss_post,
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
#' @return List: mids, long, m, seed, imputed_vars, miss_pre, miss_post, diag_paths.
impute_mice_osteo <- function(df,
                              m       = 20L,
                              maxit   = 20L,
                              seed    = 2024L,
                              out_dir = "03_outputs/mice_diagnostics/osteo") {
  
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
  
  # ---------------------------------------------------------------------------
  # 3. Pre-imputation missingness
  # ---------------------------------------------------------------------------
  
  miss_pre <- .missingness_summary(df_mice, all_imputed, label = "Pre")
  
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
  
  # ---------------------------------------------------------------------------
  # 6. Predictor matrix
  # ---------------------------------------------------------------------------
  
  pred <- mice::make.predictorMatrix(df_mice)
  pred[,] <- 0L
  
  aux  <- intersect(c(time_vars), colnames(pred))
  rows <- intersect(all_imputed, rownames(pred))
  pred[rows, aux] <- 1L
  
  add_longitudinal <- function(vars) {
    vars <- intersect(vars, colnames(pred))
    if (length(vars) > 1L) {
      pred[vars, vars] <<- 1L
      idx <- match(vars, colnames(pred))
      pred[cbind(idx, idx)] <<- 0L
    }
  }
  
  add_longitudinal(weight_vars)
  add_longitudinal(height_vars)
  add_longitudinal(larm12_vars); add_longitudinal(larm35_vars)
  add_longitudinal(rarm12_vars); add_longitudinal(rarm35_vars)
  add_longitudinal(lleg12_vars); add_longitudinal(lleg35_vars)
  add_longitudinal(rleg12_vars); add_longitudinal(rleg35_vars)
  
  anthro     <- c(weight_vars, height_vars)
  alm35_vars <- c(larm35_vars, rarm35_vars, lleg35_vars, rleg35_vars)
  
  for (grp in list(
    list(larm12_vars, c(anthro, rarm12_vars)),
    list(larm35_vars, c(anthro, rarm35_vars)),
    list(rarm12_vars, c(anthro, larm12_vars)),
    list(rarm35_vars, c(anthro, larm35_vars)),
    list(lleg12_vars, c(anthro, rleg12_vars)),
    list(lleg35_vars, c(anthro, rleg35_vars)),
    list(rleg12_vars, c(anthro, lleg12_vars)),
    list(rleg35_vars, c(anthro, lleg35_vars)),
    list(gs_vars,     c(anthro, alm35_vars)),
    list(hgs_vars,    c(anthro, alm35_vars))
  )) {
    r <- intersect(grp[[1]], rownames(pred))
    c <- intersect(grp[[2]], colnames(pred))
    if (length(r) && length(c)) pred[r, c] <- 1L
  }
  
  restrict_time <- function(vars) {
    vars <- intersect(vars, colnames(pred))
    for (i in seq_along(vars)) {
      for (j in seq_along(vars)) {
        if (j > i) {
          r <- vars[i]; cc <- vars[j]
          if (r %in% rownames(pred) && cc %in% colnames(pred))
            pred[r, cc] <<- 0L
        }
      }
    }
  }
  
  restrict_time(weight_vars); restrict_time(height_vars)
  restrict_time(larm12_vars); restrict_time(larm35_vars)
  restrict_time(rarm12_vars); restrict_time(rarm35_vars)
  restrict_time(lleg12_vars); restrict_time(lleg35_vars)
  restrict_time(rleg12_vars); restrict_time(rleg35_vars)
  
  pred[intersect(c(id_vars, time_vars), rownames(pred)), ] <- 0L
  
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
      names_to  = c(".value", ".visit"),
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
  
  comp1     <- mice::complete(mids_obj, 1)
  miss_post <- .missingness_summary(comp1, all_imputed, label = "Post")
  
  .assert_no_na_after_imputation(long_df, base_imputed)
  
  diag_paths <- .save_diagnostics(mids_obj, out_dir, label = "osteo")
  
  # ---------------------------------------------------------------------------
  # 11. Output
  # ---------------------------------------------------------------------------
  
  list(
    mids         = mids_obj,
    long         = long_df,
    m            = m,
    seed         = seed,
    imputed_vars = all_imputed,
    miss_pre     = miss_pre,
    miss_post    = miss_post,
    diag_paths   = diag_paths
  )
}


# =============================================================================
# Internal: post-imputation diagnostics (standalone helper, kept for back-compat)
# =============================================================================

post_imputation_checks <- function(mids_obj, out_dir) {
  .save_diagnostics(mids_obj, out_dir, label = "imputation")
}