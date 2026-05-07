# =============================================================================
# R/05_qc_variables.R
# =============================================================================
# Variable-level quality control on the stacked long tibble produced by
# 04_stack_visits.R.
#
# Checks performed:
#   1. Missingness summary (n_miss, pct_miss) per variable per cohort/visit
#   2. Descriptive statistics for numeric variables
#      (min, max, median, mean, SD, IQR, n_valid)
#   3. Histograms for all numeric variables (saved to <out_dir>/histograms/)
#   4. BMI present despite missing Height or Weight
#   5. Implausible value counts for pre-defined variables
#
# Usage:
#   source("05_qc_variables.R")
#   qc_vars_result <- qc_variables(stacked_df, out_dir = "output/qc_variables")
#
# Returns a named list:
#   $missingness    — long tibble: variable × cohort/visit missingness
#   $numeric_stats  — long tibble: descriptive stats for numeric columns
#   $bmi_anomaly    — tibble: rows where BMI present but Height or Weight missing
#   $implausible    — tibble: counts of implausible values per variable/rule
#
# =============================================================================


# =============================================================================
# Implausible-range rules
# =============================================================================
# Add or edit entries here to extend checks.
# Each entry:
#   variable  : column name (post-harmonisation base name)
#   min / max : inclusive plausible range (use NA to skip that bound)
#   label     : human-readable description shown in output/report
# =============================================================================

IMPLAUSIBLE_RULES <- tibble::tribble(
  ~variable,          ~min,    ~max,   ~label,
  # ── Anthropometry ─────────────────────────────────────────────────────────
  "Age",               34,      100,   "Age out of 34–100 y",
  "Height",           100,      230,   "Height out of 100–230 cm",
  "Weight",            20,      200,   "Weight out of 20–200 kg",
  "BMI",               10,       70,   "BMI out of 10–70 kg/m²",
  "WHR",                0.4,      1.6, "WHR out of 0.4–1.6",
  # ── Grip strength ─────────────────────────────────────────────────────────
  "HGS_MAX",            1,      100,   "HGS_MAX below 1 kg (implausible)",
  "HGS_R1",             1,      100,   "HGS_R1 below 1 kg",
  "HGS_R2",             1,      100,   "HGS_R2 below 1 kg",
  "HGS_R3",             1,      100,   "HGS_R3 below 1 kg",
  "HGS_L1",             1,      100,   "HGS_L1 below 1 kg",
  "HGS_L2",             1,      100,   "HGS_L2 below 1 kg",
  "HGS_L3",             1,      100,   "HGS_L3 below 1 kg",
  # ── DXA body composition ──────────────────────────────────────────────────
  "ALM",                1,       50,   "ALM out of 1–50 kg",
  "ALM_HT2",            1,       20,   "ALM_HT2 out of 1–20 kg/m²",
  "ALM_BMI",            0,       10,   "ALM_BMI out of 0–10",
  "WBTOT_LEAN_MASS",    5,      100,   "WBTOT_LEAN_MASS out of 5–100 kg",
  # ── Gait / functional ─────────────────────────────────────────────────────
  "gait_speed",         0,        5,   "gait_speed out of 0–5 m/s",
  "TUG_TIME",           0,      120,   "TUG_TIME out of 0–120 s",
  "SARCF_TOTAL",        0,       10,   "SARCF_TOTAL out of 0–10",
  # ── Diet (energy intake) ──────────────────────────────────────────────────
  "sumtot1",          400,     3500,   "sumtot1 (kcal, incl. alcohol) out of 400–3500",
  "sumtot3",          400,     3500,   "sumtot3 (kcal, excl. alcohol) out of 400–3500",
  # ── Alcohol ───────────────────────────────────────────────────────────────
  "sumalco",            0,      400,   "sumalco out of 0–400 g/day",
  "conso_hebdo",        0,      80,   "conso_hebdo out of 0–80 units/week"
)


# =============================================================================
# Helper: round numerics in a tibble for display
# =============================================================================

.round_df <- function(df, digits = 3) {
  dplyr::mutate(df, dplyr::across(where(is.numeric), ~ round(.x, digits)))
}


# =============================================================================
# 1. Missingness
# =============================================================================

#' Compute missingness per variable, stratified by cohort and visit.
#'
#' @param df Stacked long tibble from stack_visits().
#' @return Tibble with columns: cohort, visit, variable, n_total, n_miss, pct_miss.
.qc_missingness <- function(df) {
  
  cli::cli_h2("1 / 5  Missingness")
  
  # Work on collected tibble
  df <- dplyr::collect(df)
  
  groups <- df |>
    dplyr::group_by(.cohort, .visit) |>
    dplyr::group_split()
  
  purrr::map_dfr(groups, function(g) {
    cohort <- as.character(unique(g$.cohort))
    visit  <- as.character(unique(g$.visit))
    n_rows <- nrow(g)
    
    purrr::map_dfr(names(g), function(col) {
      n_miss <- sum(is.na(g[[col]]))
      tibble::tibble(
        cohort   = cohort,
        visit    = visit,
        variable = col,
        n_total  = n_rows,
        n_miss   = n_miss,
        pct_miss = round(100 * n_miss / n_rows, 1)
      )
    })
  }) |>
    dplyr::arrange(cohort, visit, dplyr::desc(pct_miss))
}


# =============================================================================
# 2. Numeric descriptive statistics
# =============================================================================

#' Compute min/max/median/mean/SD/IQR for all numeric columns.
#'
#' @param df Stacked long tibble.
#' @return Tibble with one row per variable × cohort × visit.
.qc_numeric_stats <- function(df) {
  
  cli::cli_h2("2 / 5  Numeric descriptive statistics")
  
  df <- dplyr::collect(df)
  
  num_cols <- names(df)[sapply(df, is.numeric)]
  # exclude internal id/flag columns
  num_cols <- setdiff(num_cols, c("pt", "visit_num"))
  
  if (length(num_cols) == 0) {
    cli::cli_warn("No numeric columns found.")
    return(tibble::tibble())
  }
  
  groups <- df |>
    dplyr::group_by(.cohort, .visit) |>
    dplyr::group_split()
  
  purrr::map_dfr(groups, function(g) {
    cohort <- as.character(unique(g$.cohort))
    visit  <- as.character(unique(g$.visit))
    
    purrr::map_dfr(num_cols, function(col) {
      x <- g[[col]]
      x_valid <- x[!is.na(x)]
      
      tibble::tibble(
        cohort   = cohort,
        visit    = visit,
        variable = col,
        n_valid  = length(x_valid),
        n_miss   = sum(is.na(x)),
        min      = if (length(x_valid)) min(x_valid)    else NA_real_,
        max      = if (length(x_valid)) max(x_valid)    else NA_real_,
        mean     = if (length(x_valid)) mean(x_valid)   else NA_real_,
        median   = if (length(x_valid)) stats::median(x_valid) else NA_real_,
        sd       = if (length(x_valid)) stats::sd(x_valid)     else NA_real_,
        iqr      = if (length(x_valid)) stats::IQR(x_valid)    else NA_real_
      )
    })
  }) |>
    .round_df() |>
    dplyr::arrange(cohort, visit, variable)
}


# =============================================================================
# 3. Histograms
# =============================================================================

#' Save one histogram per numeric variable, faceted by cohort × visit.
#'
#' @param df        Stacked long tibble.
#' @param out_dir   Directory to write PNG files into.
#' @param width,height  Plot dimensions in inches.
.qc_histograms <- function(df, out_dir, width = 8, height = 5) {
  
  cli::cli_h2("3 / 5  Histograms")
  
  hist_dir <- file.path(out_dir, "histograms")
  dir.create(hist_dir, showWarnings = FALSE, recursive = TRUE)
  
  df <- dplyr::collect(df)
  
  num_cols <- names(df)[sapply(df, is.numeric)]
  num_cols <- setdiff(num_cols, c("pt", "visit_num"))
  
  cohorts <- unique(as.character(df$.cohort))
  n_plots  <- length(cohorts) * length(num_cols)
  cli::cli_inform("Saving up to {n_plots} histogram(s) to {.path {hist_dir}} ({length(cohorts)} cohort(s) × {length(num_cols)} variable(s))")
  
  for (cohort in cohorts) {
    
    df_cohort <- df |> dplyr::filter(.cohort == cohort)
    
    for (col in num_cols) {
      
      # Skip variables not present (or all-NA) for this cohort
      if (!col %in% names(df_cohort)) next
      
      plot_df <- df_cohort |>
        dplyr::select(dplyr::all_of(c(".cohort", ".visit", col))) |>
        dplyr::filter(!is.na(.data[[col]])) |>
        dplyr::mutate(visit_label = paste0(.visit))
      
      if (nrow(plot_df) == 0) next
      
      # Add implausible-range lines if rule exists for this variable
      rule <- IMPLAUSIBLE_RULES |> dplyr::filter(variable == col)
      
      p <- ggplot2::ggplot(plot_df,
                           ggplot2::aes(x = .data[[col]])) +
        ggplot2::geom_histogram(bins = 50, fill = "#4e79a7", colour = "white", linewidth = 0.2) +
        ggplot2::facet_wrap(~ visit_label, scales = "free_y") +
        ggplot2::labs(
          title    = paste0(col, "  [", cohort, "]"),
          subtitle = cohort,
          x        = col,
          y        = "Count"
        ) +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(
          strip.background = ggplot2::element_rect(fill = "#dce9f5"),
          plot.title       = ggplot2::element_text(face = "bold"),
          plot.subtitle    = ggplot2::element_text(colour = "grey40")
        )
      
      # Overlay plausible-range bounds
      if (nrow(rule) > 0) {
        if (!is.na(rule$min)) {
          p <- p + ggplot2::geom_vline(xintercept = rule$min,
                                       colour = "#e15759", linetype = "dashed", linewidth = 0.7)
        }
        if (!is.na(rule$max)) {
          p <- p + ggplot2::geom_vline(xintercept = rule$max,
                                       colour = "#e15759", linetype = "dashed", linewidth = 0.7)
        }
        p <- p + ggplot2::labs(
          caption = paste0("Red dashed lines: plausible range [",
                           rule$min, ", ", rule$max, "]")
        )
      }
      
      # Filename: <cohort>_<variable>.png — cohort prefix prevents overwrites
      fname <- file.path(hist_dir, paste0(cohort, "_", col, ".png"))
      ggplot2::ggsave(fname, plot = p, width = width, height = height, dpi = 150)
    }
  }
  
  cli::cli_inform(c("v" = "Histograms saved."))
}


# =============================================================================
# 4. BMI present despite missing Height or Weight
# =============================================================================

#' Flag rows where BMI is non-missing but Height or Weight is NA.
#'
#' These rows are suspicious: BMI cannot be validated from its components.
#'
#' @param df Stacked long tibble.
#' @return Tibble of offending rows (key columns only), or empty tibble.
.qc_bmi_anomaly <- function(df) {
  
  cli::cli_h2("4 / 5  BMI present despite missing Height or Weight")
  
  df <- dplyr::collect(df)
  
  required <- c("BMI", "Height", "Weight")
  present  <- intersect(required, names(df))
  
  if (!all(required %in% present)) {
    missing_cols <- setdiff(required, present)
    cli::cli_warn(
      "Cannot run BMI anomaly check — column(s) not found: {.val {missing_cols}}"
    )
    return(tibble::tibble())
  }
  
  anomaly <- df |>
    dplyr::filter(
      !is.na(BMI) & (is.na(Height) | is.na(Weight))
    ) |>
    dplyr::select(
      dplyr::any_of(c("pt", ".cohort", ".visit", "visit_num",
                      "Age", "Height", "Weight", "BMI"))
    )
  
  n <- nrow(anomaly)
  
  if (n == 0) {
    cli::cli_inform(c("v" = "No rows with BMI present but Height/Weight missing."))
  } else {
    cli::cli_warn(c(
      "!" = "{n} row(s) have BMI recorded despite missing Height or Weight.",
      "i" = "Inspect `$bmi_anomaly` in the returned list."
    ))
  }
  
  return(anomaly)
}


# =============================================================================
# 5. Implausible values
# =============================================================================

#' Count values outside plausible ranges defined in IMPLAUSIBLE_RULES.
#'
#' @param df Stacked long tibble.
#' @return Tibble with columns: variable, label, cohort, visit,
#'   n_implausible, n_valid, pct_implausible.
.qc_implausible <- function(df) {
  
  cli::cli_h2("5 / 5  Implausible values")
  
  df <- dplyr::collect(df)
  
  rules_present <- IMPLAUSIBLE_RULES |>
    dplyr::filter(variable %in% names(df))
  
  if (nrow(rules_present) == 0) {
    cli::cli_warn("No IMPLAUSIBLE_RULES variables found in the data.")
    return(tibble::tibble())
  }
  
  purrr::map_dfr(seq_len(nrow(rules_present)), function(i) {
    
    rule <- rules_present[i, ]
    col  <- rule$variable
    
    groups <- df |>
      dplyr::group_by(.cohort, .visit) |>
      dplyr::group_split()
    
    purrr::map_dfr(groups, function(g) {
      x       <- g[[col]]
      x_valid <- x[!is.na(x)]
      n_valid <- length(x_valid)
      
      # Build implausible flag based on available bounds
      implaus <- rep(FALSE, length(x_valid))
      if (!is.na(rule$min)) implaus <- implaus | (x_valid < rule$min)
      if (!is.na(rule$max)) implaus <- implaus | (x_valid > rule$max)
      
      n_impl <- sum(implaus)
      
      tibble::tibble(
        variable       = col,
        label          = rule$label,
        cohort         = as.character(unique(g$.cohort)),
        visit          = as.character(unique(g$.visit)),
        n_valid        = n_valid,
        n_implausible  = n_impl,
        pct_implausible = if (n_valid > 0) round(100 * n_impl / n_valid, 2) else NA_real_
      )
    })
  }) |>
    dplyr::arrange(dplyr::desc(n_implausible), variable, cohort, visit)
}


# =============================================================================
# Main entry point
# =============================================================================

#' Run all variable-level QC checks on the stacked dataset.
#'
#' @param df      Stacked long tibble produced by stack_visits(). May be a
#'                regular tibble or a dtplyr lazy_dt — both are handled.
#' @param out_dir Directory for histogram PNGs and CSV summaries. Created if
#'                absent. Pass NULL to skip file output.
#' @param save_csv Logical. Write CSV summaries to out_dir? Default TRUE.
#' @param save_hist Logical. Write histogram PNGs? Default TRUE.
#'
#' @return Named list:
#'   $missingness   — missingness per variable × cohort × visit
#'   $numeric_stats — descriptive stats for numeric variables
#'   $bmi_anomaly   — rows where BMI present but Height/Weight missing
#'   $implausible   — counts of values outside plausible ranges
qc_variables <- function(df,
                         cohort = NULL,
                         out_dir   = "06_outputs/qc_variables",
                         save_csv  = TRUE,
                         save_hist = TRUE) {
  
  cli::cli_h1("Variable-level QC")
  
  
  
  # Ensure plain tibble ─────────────────────────────────────────────────────
  if (inherits(df, "dtplyr_step")) df <- dplyr::as_tibble(df)
  
  # Output directory ────────────────────────────────────────────────────────
  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # ── Run checks ────────────────────────────────────────────────────────────
  miss_tbl  <- .qc_missingness(df)
  stats_tbl <- .qc_numeric_stats(df)
  bmi_anom  <- .qc_bmi_anomaly(df)
  impl_tbl  <- .qc_implausible(df)
  
  # ── Histograms ────────────────────────────────────────────────────────────
  if (save_hist && !is.null(out_dir)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      cli::cli_warn("ggplot2 not available — skipping histograms.")
    } else {
      .qc_histograms(df, out_dir)
    }
  }
  
  # ── CSV export ────────────────────────────────────────────────────────────
  if (save_csv && !is.null(out_dir)) {
    cli::cli_h2("Saving CSV summaries")
    
    prefix <- if (!is.null(cohort)) paste0(cohort, "_") else ""
    
    readr::write_csv(miss_tbl,  file.path(out_dir, paste0(prefix, "qc_missingness.csv")))
    readr::write_csv(stats_tbl, file.path(out_dir, paste0(prefix, "qc_numeric_stats.csv")))
    readr::write_csv(bmi_anom,  file.path(out_dir, paste0(prefix, "qc_bmi_anomaly.csv")))
    readr::write_csv(impl_tbl,  file.path(out_dir, paste0(prefix, "qc_implausible.csv")))
    
    cli::cli_inform(c("v" = "CSV files written to {.path {out_dir}}"))
  }
  
  # ── Console summary of implausible hits ───────────────────────────────────
  impl_hits <- impl_tbl |> dplyr::filter(n_implausible > 0)
  
  if (nrow(impl_hits) == 0) {
    cli::cli_inform(c("v" = "No implausible values detected."))
  } else {
    cli::cli_warn(c(
      "!" = "{nrow(impl_hits)} variable × cohort × visit combination(s) contain implausible values:"
    ))
    
  }
  
  # ── Console summary of missingness extremes ───────────────────────────────
  high_miss <- miss_tbl |>
    dplyr::filter(pct_miss > 80, !variable %in% c("pt", ".cohort", ".visit", "visit_num"))
  
  if (nrow(high_miss) > 0) {
    cli::cli_warn(c(
      "!" = "{nrow(high_miss)} variable(s) exceed 80% missingness in at least one visit:"
    ))
    
  }
  
  cli::cli_h1("Variable QC complete")
  
  # ── Return ────────────────────────────────────────────────────────────────
  invisible(list(
    missingness   = miss_tbl,
    numeric_stats = stats_tbl,
    bmi_anomaly   = bmi_anom,
    implausible   = impl_tbl
  ))
}