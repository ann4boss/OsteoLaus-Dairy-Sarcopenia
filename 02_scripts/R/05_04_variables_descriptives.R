# =============================================================================
# R/05_04_variables_descriptives.R
# =============================================================================
# Describe variables of the shared dataset: missingness, distributions,
# categorical summaries, and longitudinal change.
# Accepts data.frame or mids objects.
#
# Functions:
#   extract_data()            — unwraps a mids object to its plain $data
#   label_var()                — raw column name -> display label (VAR_LABELS)
#   .pal_seq() / .pal_cat() / .pal_alluvial() — palette helpers (sequential /
#                                categorical / alluvial-specific)
#   compute_quartile_cuts()    — Q25/Q50/Q75 per variable (mids-averaged)
#   theme_proj()                — shared ggplot theme
#   plot_continuous()           — density overlay per visit, continuous vars
#   plot_boxplots()             — box plots per visit, continuous vars
#   plot_categorical()          — stacked bar charts per visit, categorical vars
#   plot_alluvial()              — alluvial plot of categorical change over time
#   plot_exposure_outcome()      — exposure-vs-outcome scatter + LOESS, by visit
#   describe_variables()         — main entry point: all plots + CSV summaries
#
# Shared by R/05_04_01_missingness_analysis.R and R/05_05_age_trajectories.R
# via tar_source(): PALETTE, FONT, extract_data(), label_var(), theme_proj(),
# .pal_seq(), .pal_cat().
# =============================================================================

# ── Constants ──────────────────────────────────────────────────────────────────

PALETTE <- c(
  "#E76254FF", "#EF8A47FF", "#F7AA58FF", "#FFD06FFF", "#FFE6B7FF",
  "#AADCE0FF", "#72BCD5FF", "#528FADFF", "#376795FF", "#1E466EFF"
)

FONT <- "Helvetica"

VAR_LABELS <- c(
  Age                      = "Age [year]",
  Height                   = "Height [cm]",
  Weight                   = "Weight [kg]",
  BMI                      = "BMI [kg/m²]",
  BMI_category             = "BMI Category",
  HGS_MAX                  = "Handgrip strength [kg]",
  ALM_HT2_harmonised       = "ALMI [kg/m²]",
  ALM_BMI_harmonised       = "ALM/BMI [kg/(kg/m²)]",
  gait_speed               = "Gait speed [m/s]",
  sumtot1                  = "Total calorie intake [kcal/day]",
  dairy_total_gday         = "Total dairy [g/day]",
  dairy_fermented_gday     = "Fermented dairy [g/day",
  dairy_non_fermented_gday = "Non-fermented dairy [g/day",
  dairy_lowfat_gday        = "Low-fat dairy [g/day",
  dairy_highfat_gday       = "High-fat dairy [g/day",
  dairy_total_gday_cumavg              = "Cumulative total dairy intake [g/day]",
  dairy_fermented_gday_cumavg          = "Fermented dairy cumulative average [g/day]",
  dairy_non_fermented_gday_cumavg      = "Non-fermented dairy cumulative average [g/day]",
  dairy_lowfat_gday_cumavg             = "Low-fat dairy cumulative average [g/day]",
  dairy_highfat_gday_cumavg            = "High-fat dairy cumulative average [g/day]",
  dairy_total_gday_cumavg_lag          = "Lagged Cumulative total dairy intake [g/day]",
  dairy_fermented_gday_cumavg_lag      = "Lagged Fermented dairy cumulative average [g/day]",
  dairy_non_fermented_gday_cumavg_lag  = "Lagged Non-fermented dairy cumulative average [g/day]",
  dairy_lowfat_gday_cumavg_lag         = "Lagged Low-fat dairy cumulative average [g/day]",
  dairy_highfat_gday_cumavg_lag        = "Lagged High-fat dairy cumulative average [g/day]",
  dairy_quartile_baseline  = "Dairy quartile",
  dairy_guidelines_port    = "Dairy Swiss serving guidelines",
  days_colaus_minus_osteo  = "Days CoLaus – OsteoLaus",
  education_level          = "Education level",
  smoking_status           = "Smoking status",
  alcohol_category_conso   = "Alcohol intake (conso_hebdo)",
  alcohol_category_sumalco = "Alcohol (sumalco)",
  pa_levels_tertile_f1     = "Physical activity tertile",
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

# extract_data(): unwraps a mids object to its incomplete $data (observed
# values with NAs intact); returns plain data frames unchanged.
extract_data <- function(data) {
  if (inherits(data, "mids")) data$data else data
}

# label_var(): raw column name -> display label via VAR_LABELS, falling back
# to the column name with underscores replaced by spaces.
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

# Alluvial category palette: fixed, mutually distinguishable colors.
.pal_alluvial <- function(n) {
  base <- c("#EF8A47", "#FFE6B7", "#72BCD5", "#1E466E")
  if (n <= length(base)) base[seq_len(n)]
  else grDevices::colorRampPalette(base)(n)
}

# Compute Q25/Q50/Q75 cut-points per exposure variable.
# For mids objects, averages quantiles over all m complete datasets.
compute_quartile_cuts <- function(data, vars, probs = c(0.25, 0.5, 0.75)) {
  if (inherits(data, "mids")) {
    purrr::map(vars, function(v) {
      qs <- purrr::map(seq_len(data$m), function(i) {
        d <- mice::complete(data, i)
        if (v %in% names(d)) stats::quantile(d[[v]], probs = probs, na.rm = TRUE)
        else NULL
      })
      qs <- purrr::compact(qs)
      if (length(qs) == 0) return(NULL)
      rowMeans(do.call(cbind, qs))
    }) |> rlang::set_names(vars) |> purrr::compact()
  } else {
    purrr::map(vars, function(v) {
      if (!v %in% names(data)) return(NULL)
      stats::quantile(data[[v]], probs = probs, na.rm = TRUE)
    }) |> rlang::set_names(vars) |> purrr::compact()
  }
}

# theme_proj(): shared minimal ggplot theme for all descriptive plots.
theme_proj <- function() {
  ggplot2::theme_minimal(base_family = FONT) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

# -----------------------------------------------------------------------------
# plot_continuous()
# -----------------------------------------------------------------------------
# ── 1. Continuous distributions (density overlay per visit) ───────────────────

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

# -----------------------------------------------------------------------------
# plot_boxplots()
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# plot_categorical()
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# plot_alluvial()
# -----------------------------------------------------------------------------
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
    dplyr::ungroup() |>
    dplyr::group_by(.data[[time_col]]) |>
    dplyr::filter(!all(is.na(.data[[variable]]))) |>
    dplyr::ungroup()

  # Missingness at any visit is shown as its own "Missing" stratum rather
  # than dropped, so participants are still visible in the flow.
  df <- df |>
    dplyr::mutate(
      !!variable := dplyr::if_else(
        is.na(.data[[variable]]), "Missing", as.character(.data[[variable]])
      )
    )

  keep <- df |>
    dplyr::filter(!is.na(.data[[variable]])) |>
    dplyr::count(.data[[id_var]]) |>
    dplyr::filter(n >= min_visits) |>
    dplyr::pull(.data[[id_var]])

  df <- df |>
    dplyr::filter(.data[[id_var]] %in% keep) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(time_col), ~ factor(.x, levels = sort(unique(.x)))),
      dplyr::across(
        dplyr::all_of(variable),
        ~ factor(.x, levels = c(setdiff(sort(unique(.x)), "Missing"), "Missing"))
      )
    )

  cat_levels  <- levels(df[[variable]])
  real_levels <- setdiff(cat_levels, "Missing")
  pal <- stats::setNames(.pal_alluvial(length(real_levels)), real_levels)
  if ("Missing" %in% cat_levels) pal <- c(pal, Missing = "grey75")

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
    ggplot2::scale_fill_manual(values = pal, name = label_var(variable)) +
    theme_proj() +
    ggplot2::labs(x = "Visit", y = "Participants")
}

# -----------------------------------------------------------------------------
# plot_exposure_outcome()
# -----------------------------------------------------------------------------
# ── 6. Exposure vs outcome scatter with LOESS per time point ──────────────────

plot_exposure_outcome <- function(data,
                                  x,
                                  y,
                                  time_col      = "time_point",
                                  span          = 0.75,
                                  quartile_cuts = NULL) {
  df     <- extract_data(data)
  visits <- sort(unique(df[[time_col]]))
  # Single light blue for all time points (facet labels already distinguish visits)
  col_pt <- "#376795"
  # Warm-to-cool gradient across quartiles (low -> high dairy)
  band_colors <- c(Q1 = "#E76254", Q2 = "#F7AA58", Q3 = "#72BCD5", Q4 = "#1E466E")
  combos <- expand.grid(x = x, y = y, stringsAsFactors = FALSE)

  purrr::pmap(combos, function(x, y) {
    pd <- df |>
      dplyr::select(dplyr::all_of(c(time_col, x, y))) |>
      dplyr::filter(!is.na(.data[[x]]), !is.na(.data[[y]])) |>
      dplyr::mutate(visit = factor(.data[[time_col]], levels = visits))

    rect_layers <- NULL
    line_layer  <- NULL
    text_layer  <- NULL

    if (!is.null(quartile_cuts) && x %in% names(quartile_cuts)) {
      cuts <- sort(quartile_cuts[[x]])

      # Facets use scales = "free_x", so band edges must be computed per
      # panel; the y-axis is shared, so y_top is computed once globally.
      ranges_df <- pd |>
        dplyr::group_by(visit) |>
        dplyr::summarise(
          x_min = min(.data[[x]], na.rm = TRUE), x_max = max(.data[[x]], na.rm = TRUE),
          .groups = "drop"
        )

      y_rng <- range(pd[[y]], na.rm = TRUE)
      y_top <- y_rng[2] + diff(y_rng) * 0.08

      bands_df <- purrr::pmap_dfr(ranges_df, function(visit, x_min, x_max) {
        bounds <- c(x_min, cuts, x_max)
        tibble::tibble(
          visit  = visit,
          xmin   = bounds[-length(bounds)],
          xmax   = bounds[-1],
          qlabel = paste0("Q", seq_len(length(bounds) - 1))
        )
      }) |>
        dplyr::mutate(xmid = (xmin + xmax) / 2, y_top = y_top)

      rect_layers <- purrr::map(names(band_colors), function(q) {
        ggplot2::geom_rect(
          data = dplyr::filter(bands_df, qlabel == q),
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          fill = band_colors[[q]], alpha = 0.10, inherit.aes = FALSE
        )
      })

      cuts_df <- tidyr::expand_grid(visit = ranges_df$visit, xintercept = cuts)

      line_layer <- ggplot2::geom_vline(
        data = cuts_df,
        ggplot2::aes(xintercept = xintercept),
        linetype = "dashed", color = "grey30", linewidth = 0.5, alpha = 0.8,
        inherit.aes = FALSE
      )

      text_layer <- ggplot2::geom_text(
        data = bands_df,
        ggplot2::aes(x = xmid, y = y_top, label = qlabel),
        vjust = 0, hjust = 0.5,
        size = 2.5, family = FONT, color = "grey30",
        inherit.aes = FALSE
      )
    }

    p <- ggplot2::ggplot(pd, ggplot2::aes(
      x = .data[[x]], y = .data[[y]], color = visit, fill = visit
    )) +
      rect_layers +
      line_layer +
      ggplot2::geom_point(alpha = 0.35, size = 1.2) +
      ggplot2::geom_smooth(
        ggplot2::aes(x = .data[[x]], y = .data[[y]]),
        method = "loess", formula = y ~ x, span = span, se = TRUE,
        colour = "black", fill = "grey40", alpha = 0.15, linewidth = 0.8,
        inherit.aes = FALSE
      ) +
      text_layer +
      ggplot2::facet_wrap(~ visit, scales = "free_x") +
      ggplot2::scale_color_manual(values = rep(col_pt, length(visits)), guide = "none") +
      ggplot2::scale_fill_manual(values  = rep(col_pt, length(visits)), guide = "none") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.14))) +
      theme_proj() +
      ggplot2::theme(
        axis.line = ggplot2::element_line(color = "black", linewidth = 0.4)
      ) +
      ggplot2::labs(x = label_var(x), y = label_var(y))

    p
  }) |> rlang::set_names(paste(combos$y, "vs", combos$x))
}

# -----------------------------------------------------------------------------
# describe_variables()
# -----------------------------------------------------------------------------
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
                               time_col           = "time_point",
                               id_var             = "pt",
                               continuous_vars    = NULL,
                               categorical_vars   = NULL,
                               missing_vars       = NULL,
                               scatter_pairs      = NULL,
                               loess_span         = 0.75,
                               quartile_cuts_file = NULL,
                               quartile_dataset   = "MICE",
                               out_dir            = NULL,
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
    # Pre-load baseline cuts from saved CSV (MICE-averaged, F1/T1 only).
    # These match the derivation cuts used in dairy_quartile_baseline.
    # Applied to dairy_total_gday_cumavg and its lagged variant; all other
    # exposure variables keep their data-derived cuts.
    file_baseline_cuts <- NULL
    if (!is.null(quartile_cuts_file) && file.exists(quartile_cuts_file)) {
      raw <- readr::read_csv(quartile_cuts_file, show_col_types = FALSE)
      q_rows <- raw |>
        dplyr::filter(dataset == quartile_dataset, quartile_var == "baseline") |>
        dplyr::arrange(boundary) |>
        dplyr::pull(cut_g_day)
      if (length(q_rows) == 3)
        file_baseline_cuts <- stats::setNames(q_rows, c("25%", "50%", "75%"))
    }

    # Compute cuts first (keeps mids object before extract_data strips it)
    all_cuts <- purrr::imap(scatter_pairs, function(pair, nm) {
      cuts <- compute_quartile_cuts(pair$data %||% data, pair$x)
      # Override total-dairy cuts with file-based baseline derivation cuts
      if (!is.null(file_baseline_cuts)) {
        total_dairy_vars <- c("dairy_total_gday_cumavg", "dairy_total_gday_cumavg_lag")
        for (v in intersect(pair$x, total_dairy_vars))
          cuts[[v]] <- file_baseline_cuts
      }
      cuts
    })
    plots$scatter <- purrr::imap(scatter_pairs, function(pair, nm) {
      pair_df <- extract_data(pair$data %||% data)
      plot_exposure_outcome(pair_df, x = pair$x, y = pair$y,
                            time_col = time_col, span = loess_span,
                            quartile_cuts = all_cuts[[nm]])
    }) |> purrr::list_flatten(name_spec = "{outer}_{inner}")
    plots$quartile_cuts <- all_cuts
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

      # Quartile cut-points (one row per pair × variable)
      purrr::imap_dfr(all_cuts, function(cuts, pair_nm) {
        purrr::imap_dfr(cuts, function(q, var_nm) {
          tibble::tibble(pair     = pair_nm,
                         variable = var_nm,
                         label    = label_var(var_nm),
                         q25      = round(q[[1]], 3),
                         q50      = round(q[[2]], 3),
                         q75      = round(q[[3]], 3))
        })
      }) |> readr::write_csv(file.path(out_dir, "data_dairy_quartile_cuts.csv"))
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
