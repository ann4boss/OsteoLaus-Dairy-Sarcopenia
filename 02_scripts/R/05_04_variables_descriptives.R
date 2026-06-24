# =============================================================================
# R/variables_descriptives.R
# Describe variables of the shared dataset: missingness, distributions,
# categorical summaries, and longitudinal change.
# Accepts data.frame or mids objects.
# =============================================================================

# ── Constants ──────────────────────────────────────────────────────────────────

PALETTE <- c(
  "#E76254FF", "#EF8A47FF", "#F7AA58FF", "#FFD06FFF", "#FFE6B7FF",
  "#AADCE0FF", "#72BCD5FF", "#528FADFF", "#376795FF", "#1E466EFF"
)

FONT <- "Helvetica"

VAR_LABELS <- c(
  Age                      = "Age (yr)",
  Height                   = "Height (cm)",
  Weight                   = "Weight (kg)",
  BMI                      = "BMI (kg/m²)",
  BMI_category             = "BMI Category",
  HGS_MAX                  = "Grip strength (kg)",
  ALM_HT2_harmonised       = "ALMI (kg/m²)",
  gait_speed               = "Gait speed (m/s)",
  sumtot1                  = "Physical activity score",
  dairy_total_gday         = "Total dairy (g/day)",
  dairy_fermented_gday     = "Fermented dairy (g/day)",
  dairy_non_fermented_gday = "Non-fermented dairy (g/day)",
  dairy_lowfat_gday        = "Low-fat dairy (g/day)",
  dairy_highfat_gday       = "High-fat dairy (g/day)",
  dairy_total_gday_cumvavg         = "Total dairy cumulative average (g/day)",
  dairy_fermented_gday_cumvavg     = "Fermented dairy cumulative average (g/day)",
  dairy_non_fermented_gday_cumvavg = "Non-fermented dairy cumulative average(g/day)",
  dairy_lowfat_gday_cumvavg        = "Low-fat dairy cumulative average(g/day)",
  dairy_highfat_gday_cumvavg      = "High-fat dairy cumulative average(g/day)",
  dairy_quartile_baseline. = "Dairy quartile",
  dairy_guidelines_port    = "Dairy Swiss serving guidelines",
  days_colaus_minus_osteo  = "Days CoLaus – OsteoLaus",
  BMI_category             = "BMI category",
  smoking_status           = "Smoking status",
  alcohol_category_conso   = "Alcohol (conso.)",
  alcohol_category_sumalco = "Alcohol (sumalco.)",
  pa_levels_tertile_f1     = "PA tertile (F1)",
  pa_levels_tertile_f2     = "PA tertile (F2)",
  pa_levels_who_f1         = "PA WHO category (F1)",
  pa_levels_who_f2         = "PA WHO category (F2)",
  diabetes_status          = "Diabetes",
  htn_status               = "Hypertension",
  hrt_status               = "HRT status",
  hypolip_drug_status      = "Hypolipidemic drugs",
  corticoids_status        = "Corticosteroids",
  vitD_status              = "Vitamin D supplement",
  calcium_status           = "Calcium supplement",
  benzo_status             = "Benzodiazepines",
  mrtsts2                  = "Marital status",
  ewgsop2_sarcopenia_stage = "EWGSOP2 sarcopenia",
  fnih_sarcopenia          = "FNIH sarcopenia"
)

CONTINUOUS_VARS <- c(
  "Age", "Height", "Weight", "BMI",
  "HGS_MAX", "ALM_HT2_harmonised", "gait_speed", "sumtot1",
  "dairy_total_gday", "dairy_fermented_gday",
  "dairy_non_fermented_gday", "dairy_lowfat_gday",
  "dairy_highfat_gday", "days_colaus_minus_osteo",
  "dairy_total_gday_cumavg", "dairy_fermented_gday_cumavg",
  "dairy_non_fermented_gday_cumavg", "dairy_lowfat_gday_cumavg",
  "dairy_highfat_gday_cumavg"
)

CATEGORICAL_VARS <- c(
  "BMI_category", "smoking_status",
  "alcohol_category_conso", "alcohol_category_sumalco",
  "pa_levels_tertile_f1", "pa_levels_tertile_f2",
  "pa_levels_who_f1", "pa_levels_who_f2",
  "diabetes_status", "htn_status", "hrt_status",
  "hypolip_drug_status", "corticoids_status",
  "vitD_status", "calcium_status", "benzo_status",
  "mrtsts2", "ewgsop2_sarcopenia_stage", "fnih_sarcopenia",
  "dairy_quartile_baseline", "dairy_guidelines_port"
)

# ── Helpers ────────────────────────────────────────────────────────────────────

extract_data <- function(data) {
  if (inherits(data, "mids")) data$data else data
}

label_var <- function(var) {
  ifelse(var %in% names(VAR_LABELS), VAR_LABELS[var], gsub("_", " ", var))
}

# Sequential palette: evenly spaced across full gradient (for ordered visits).
.pal_seq <- function(n) {
  idx <- round(seq(1, length(PALETTE), length.out = max(n, 2)))
  if (n <= length(PALETTE)) PALETTE[idx[seq_len(n)]]
  else grDevices::colorRampPalette(PALETTE)(n)
}

# Categorical palette: interleave warm/cool ends for maximum contrast.
.pal_cat <- function(n) {
  warm <- PALETTE[c(1, 2, 3, 4, 5)]
  cool <- PALETTE[c(10, 9, 8, 7, 6)]
  interleaved <- c(rbind(warm, cool))   # red, navy, orange, steel, ...
  if (n <= 10) interleaved[seq_len(n)]
  else grDevices::colorRampPalette(interleaved)(n)
}

theme_proj <- function() {
  ggplot2::theme_minimal(base_family = FONT) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

# ── 1. Missingness heatmap ─────────────────────────────────────────────────────

plot_missing_heatmap <- function(data,
                                 vars     = NULL,
                                 time_col = "time_point") {
  df   <- extract_data(data)
  vars <- intersect(vars %||% setdiff(names(df), time_col), names(df))

  miss <- df |>
    dplyr::group_by(.data[[time_col]]) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(vars), ~ mean(is.na(.)) * 100),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(-dplyr::all_of(time_col),
                        names_to = "variable", values_to = "pct") |>
    dplyr::mutate(
      vlabel = label_var(variable),
      vlabel = forcats::fct_reorder(vlabel, pct, .fun = mean)
    )

  ggplot2::ggplot(miss, ggplot2::aes(
    x    = factor(.data[[time_col]]),
    y    = vlabel,
    fill = pct
  )) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.0f%%", pct)),
      size = 3, family = FONT
    ) +
    ggplot2::scale_fill_gradient2(
      low = PALETTE[6], mid = PALETTE[5], high = PALETTE[1],
      midpoint = 50, limits = c(0, 100), name = "% missing"
    ) +
    theme_proj() +
    ggplot2::labs(
      title = "Missingness by variable and time point",
      x = "Time point", y = NULL
    )
}

# ── 2. Continuous distributions (density overlay per visit) ───────────────────

plot_continuous <- function(data,
                            vars     = NULL,
                            time_col = "time_point") {
  df     <- extract_data(data)
  vars   <- intersect(vars %||% CONTINUOUS_VARS, names(df))
  visits <- sort(unique(df[[time_col]]))
  cols   <- .pal_seq(length(visits))

  purrr::map(vars, function(v) {
    pd <- df |>
      dplyr::select(dplyr::all_of(c(time_col, v))) |>
      dplyr::filter(!is.na(.data[[v]])) |>
      dplyr::mutate(visit = factor(.data[[time_col]], levels = visits))

    ggplot2::ggplot(pd, ggplot2::aes(x = .data[[v]], fill = visit, color = visit)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        alpha = 0.25, position = "identity", bins = 40, linewidth = 0.2
      ) +
      ggplot2::geom_density(alpha = 0.15, linewidth = 0.7) +
      ggplot2::scale_fill_manual(values  = cols, name = "Visit") +
      ggplot2::scale_color_manual(values = cols, name = "Visit") +
      theme_proj() +
      ggplot2::labs(title = label_var(v), x = label_var(v), y = "Density")
  }) |> rlang::set_names(vars)
}

# ── 3. Continuous box plots per visit ──────────────────────────────────────────

plot_boxplots <- function(data,
                          vars     = NULL,
                          time_col = "time_point") {
  df     <- extract_data(data)
  vars   <- intersect(vars %||% CONTINUOUS_VARS, names(df))
  visits <- sort(unique(df[[time_col]]))
  cols   <- .pal_seq(length(visits))

  purrr::map(vars, function(v) {
    pd <- df |>
      dplyr::select(dplyr::all_of(c(time_col, v))) |>
      dplyr::filter(!is.na(.data[[v]])) |>
      dplyr::mutate(visit = factor(.data[[time_col]], levels = visits))

    ggplot2::ggplot(pd, ggplot2::aes(x = visit, y = .data[[v]], fill = visit)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.size = 0.8, outlier.alpha = 0.4) +
      ggplot2::scale_fill_manual(values = cols, name = "Visit") +
      theme_proj() +
      ggplot2::labs(title = label_var(v), x = "Time point", y = label_var(v))
  }) |> rlang::set_names(vars)
}

# ── 4. Categorical stacked bar charts per visit ───────────────────────────────

plot_categorical <- function(data,
                             vars     = NULL,
                             time_col = "time_point") {
  df   <- extract_data(data)
  vars <- intersect(vars %||% CATEGORICAL_VARS, names(df))

  purrr::map(vars, function(v) {
    pd <- df |>
      dplyr::select(dplyr::all_of(c(time_col, v))) |>
      dplyr::filter(!is.na(.data[[v]]), !is.na(.data[[time_col]])) |>
      dplyr::count(.data[[time_col]], .data[[v]]) |>
      dplyr::group_by(.data[[time_col]]) |>
      dplyr::mutate(pct = n / sum(n) * 100) |>
      dplyr::ungroup() |>
      dplyr::mutate(cat = as.factor(.data[[v]]), visit = as.factor(.data[[time_col]]))

    n_cats <- dplyr::n_distinct(pd$cat)

    ggplot2::ggplot(pd, ggplot2::aes(x = visit, y = pct, fill = cat)) +
      ggplot2::geom_col(alpha = 0.85, position = "stack") +
      ggplot2::scale_fill_manual(values = .pal_cat(n_cats), name = label_var(v)) +
      theme_proj() +
      ggplot2::labs(title = label_var(v), x = "Time point", y = "%")
  }) |> rlang::set_names(vars)
}

# ── 5. Alluvial: categorical change over time ──────────────────────────────────

plot_alluvial <- function(data,
                          variable,
                          id_var     = "pt",
                          time_col   = "time_point",
                          min_visits = 2) {
  if (!requireNamespace("ggalluvial", quietly = TRUE))
    stop("Package 'ggalluvial' is required.")

  df <- extract_data(data) |>
    dplyr::select(dplyr::all_of(c(id_var, time_col, variable))) |>
    dplyr::filter(!is.na(.data[[time_col]])) |>
    dplyr::group_by(.data[[id_var]], .data[[time_col]]) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  keep <- df |>
    dplyr::filter(!is.na(.data[[variable]])) |>
    dplyr::count(.data[[id_var]]) |>
    dplyr::filter(n >= min_visits) |>
    dplyr::pull(.data[[id_var]])

  df <- df |>
    dplyr::filter(.data[[id_var]] %in% keep) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(time_col), ~ factor(.x, levels = sort(unique(.x)))),
      dplyr::across(dplyr::all_of(variable), as.factor)
    )

  n_cats <- dplyr::n_distinct(df[[variable]])

  ggplot2::ggplot(df, ggplot2::aes(
    x        = .data[[time_col]],
    stratum  = .data[[variable]],
    alluvium = .data[[id_var]],
    fill     = .data[[variable]],
    label    = .data[[variable]]
  )) +
    ggalluvial::geom_flow(alpha = 0.45) +
    ggalluvial::geom_stratum(alpha = 0.9) +
    ggplot2::geom_text(stat = "stratum", size = 3, family = FONT) +
    ggplot2::scale_fill_manual(values = .pal_cat(n_cats), name = label_var(variable)) +
    theme_proj() +
    ggplot2::labs(
      title = paste(label_var(variable), "over time"),
      x = "Visit", y = "Participants"
    )
}

# ── 6. Exposure vs outcome scatter with LOESS per time point ──────────────────

plot_exposure_outcome <- function(data,
                                  x,
                                  y,
                                  time_col = "time_point",
                                  span     = 0.75) {
  df     <- extract_data(data)
  visits <- sort(unique(df[[time_col]]))
  cols   <- .pal_seq(length(visits))
  combos <- expand.grid(x = x, y = y, stringsAsFactors = FALSE)

  purrr::pmap(combos, function(x, y) {
    pd <- df |>
      dplyr::select(dplyr::all_of(c(time_col, x, y))) |>
      dplyr::filter(!is.na(.data[[x]]), !is.na(.data[[y]])) |>
      dplyr::mutate(visit = factor(.data[[time_col]], levels = visits))

    ggplot2::ggplot(pd, ggplot2::aes(
      x = .data[[x]], y = .data[[y]], color = visit, fill = visit
    )) +
      ggplot2::geom_point(alpha = 0.35, size = 1.2) +
      ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                           span = span, se = TRUE, alpha = 0.15, linewidth = 0.8) +
      ggplot2::facet_wrap(~ visit, scales = "free") +
      ggplot2::scale_color_manual(values = cols, guide = "none") +
      ggplot2::scale_fill_manual(values  = cols, guide = "none") +
      theme_proj() +
      ggplot2::labs(
        title = paste(label_var(y), "~", label_var(x)),
        x = label_var(x), y = label_var(y)
      )
  }) |> rlang::set_names(paste(combos$y, "vs", combos$x))
}

# ── Main entry point ───────────────────────────────────────────────────────────

#' Produce all variable description plots, alluvials, and CSV summaries.
#'
#' @param data             data.frame or mids object.
#' @param time_col         Column name for visit/time stratification.
#' @param id_var           Participant ID column (used for alluvials).
#' @param continuous_vars  Variables for density and boxplot panels (default: CONTINUOUS_VARS).
#' @param categorical_vars Variables for bar and alluvial panels (default: CATEGORICAL_VARS).
#' @param missing_vars     Variables shown in missingness heatmap (default: all).
#' @param scatter_pairs    Named list of lists, each with `x` (exposure vars) and `y`
#'                         (outcome vars). Allows different exposure sets per outcome group,
#'                         e.g. lagged exposures for gait speed. NULL skips scatter plots.
#'                         Example: list(hgs_alm = list(x = c("dairy_total_gday"), y = c("HGS_MAX")),
#'                                       gait    = list(x = c("dairy_100g_lag"),   y = "gait_speed"))
#' @param loess_span       LOESS smoothing span passed to plot_exposure_outcome().
#' @param out_dir          Directory for PNG and CSV output; NULL to skip saving.
#' @param width,height,dpi Plot dimensions.
#' @return Named list: $missing, $densities, $boxplots, $bars, $alluvials, $scatter.
describe_variables <- function(data,
                               time_col         = "time_point",
                               id_var           = "pt",
                               continuous_vars  = NULL,
                               categorical_vars = NULL,
                               missing_vars     = NULL,
                               scatter_pairs    = NULL,
                               loess_span       = 0.75,
                               out_dir          = NULL,
                               width = 8, height = 5, dpi = 150) {
  df     <- extract_data(data)
  cont_v <- intersect(continuous_vars  %||% CONTINUOUS_VARS,               names(df))
  cat_v  <- intersect(categorical_vars %||% CATEGORICAL_VARS,              names(df))
  miss_v <- intersect(missing_vars     %||% setdiff(names(df), time_col),  names(df))

  if (!is.null(out_dir))
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # ── Plots ──────────────────────────────────────────────────────────────────
  plots <- list(
    missing   = plot_missing_heatmap(df, vars = miss_v,  time_col = time_col),
    densities = plot_continuous(df,     vars = cont_v,   time_col = time_col),
    boxplots  = plot_boxplots(df,       vars = cont_v,   time_col = time_col),
    bars      = plot_categorical(df,    vars = cat_v,    time_col = time_col),
    alluvials = purrr::map(cat_v, ~ plot_alluvial(
                  df, variable = .x, id_var = id_var, time_col = time_col
                )) |> rlang::set_names(cat_v)
  )

  if (!is.null(scatter_pairs)) {
    plots$scatter <- purrr::imap(scatter_pairs, function(pair, nm) {
      pair_df <- extract_data(pair$data %||% df)
      plot_exposure_outcome(pair_df, x = pair$x, y = pair$y,
                            time_col = time_col, span = loess_span)
    }) |> purrr::list_flatten(name_spec = "{outer}_{inner}")
  }

  # ── CSV summaries ──────────────────────────────────────────────────────────
  if (!is.null(out_dir)) {

    # Missingness
    df |>
      dplyr::group_by(.data[[time_col]]) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(miss_v), ~ mean(is.na(.)) * 100),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(-dplyr::all_of(time_col),
                          names_to = "variable", values_to = "pct_missing") |>
      dplyr::mutate(label = label_var(variable), pct_missing = round(pct_missing, 1)) |>
      readr::write_csv(file.path(out_dir, "data_missing.csv"))

    # Continuous summary stats
    purrr::map_dfr(cont_v, function(v) {
      df |>
        dplyr::select(dplyr::all_of(c(time_col, v))) |>
        dplyr::filter(!is.na(.data[[v]])) |>
        dplyr::group_by(.data[[time_col]]) |>
        dplyr::summarise(
          variable = v, label = label_var(v),
          n        = dplyr::n(),
          mean     = round(mean(.data[[v]]),             3),
          sd       = round(stats::sd(.data[[v]]),        3),
          median   = round(stats::median(.data[[v]]),    3),
          q25      = round(stats::quantile(.data[[v]], 0.25), 3),
          q75      = round(stats::quantile(.data[[v]], 0.75), 3),
          min      = round(min(.data[[v]]),              3),
          max      = round(max(.data[[v]]),              3),
          .groups  = "drop"
        )
    }) |> readr::write_csv(file.path(out_dir, "data_continuous_stats.csv"))

    # Categorical counts and percentages
    purrr::map_dfr(cat_v, function(v) {
      df |>
        dplyr::select(dplyr::all_of(c(time_col, v))) |>
        dplyr::filter(!is.na(.data[[v]]), !is.na(.data[[time_col]])) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(v), as.character)) |>
        dplyr::count(.data[[time_col]], .data[[v]]) |>
        dplyr::group_by(.data[[time_col]]) |>
        dplyr::mutate(pct = round(n / sum(n) * 100, 1)) |>
        dplyr::ungroup() |>
        dplyr::rename(category = dplyr::all_of(v)) |>
        dplyr::mutate(variable = v, label = label_var(v))
    }) |> readr::write_csv(file.path(out_dir, "data_categorical_counts.csv"))

    # Scatter correlations (Pearson + Spearman per visit, across all pairs)
    if (!is.null(scatter_pairs)) {
      purrr::map_dfr(scatter_pairs, function(pair) {
        pair_df <- extract_data(pair$data %||% df)
        expand.grid(x = pair$x, y = pair$y, stringsAsFactors = FALSE) |>
          purrr::pmap_dfr(function(x, y) {
            pair_df |>
              dplyr::select(dplyr::all_of(c(time_col, x, y))) |>
              dplyr::filter(!is.na(.data[[x]]), !is.na(.data[[y]])) |>
              dplyr::group_by(.data[[time_col]]) |>
              dplyr::summarise(
                exposure   = x, outcome = y,
                n          = dplyr::n(),
                r_pearson  = round(cor(.data[[x]], .data[[y]], method = "pearson"),  3),
                r_spearman = round(cor(.data[[x]], .data[[y]], method = "spearman"), 3),
                .groups    = "drop"
              )
          })
      }) |> readr::write_csv(file.path(out_dir, "data_scatter_correlations.csv"))
    }

    # ── Save plots ────────────────────────────────────────────────────────────
    ggplot2::ggsave(file.path(out_dir, "missing_heatmap.png"),
                    plots$missing, width = 10, height = 7, dpi = dpi)

    save_list <- function(lst, prefix) {
      purrr::iwalk(lst, ~ ggplot2::ggsave(
        file.path(out_dir, paste0(prefix, "_", .y, ".png")),
        .x, width = width, height = height, dpi = dpi
      ))
    }
    save_list(plots$densities, "density")
    save_list(plots$boxplots,  "boxplot")
    save_list(plots$bars,      "bar")
    save_list(plots$alluvials, "alluvial")
    if (!is.null(plots$scatter))
      save_list(plots$scatter, "scatter")

    cli::cli_inform(c("v" = "Plots and CSVs saved to {.path {out_dir}}"))
  }

  invisible(plots)
}
