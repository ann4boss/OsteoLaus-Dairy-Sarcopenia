# =============================================================================
# R/05_03_missingness_analysis.R
# Missingness analysis on the pre-exclusion merged dataset.
# Run before exclusions so the full missing data structure is visible.
# Accepts data.frame or mids objects.
# =============================================================================

# ── 1. Missingness heatmap (% missing per variable × time point) ──────────────

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

# ── 2. Missingness pattern plot (which variable combinations are missing) ──────

plot_missing_patterns <- function(data,
                                  vars     = NULL,
                                  time_col = "time_point",
                                  max_vars = 30) {
  df   <- extract_data(data)
  vars <- intersect(vars %||% setdiff(names(df), time_col), names(df))

  # Limit to variables with any missingness, capped at max_vars by % missing
  miss_pct <- colMeans(is.na(df[vars])) * 100
  vars <- names(sort(miss_pct[miss_pct > 0], decreasing = TRUE))
  if (length(vars) == 0) {
    cli::cli_inform("No missing values found — pattern plot skipped.")
    return(invisible(NULL))
  }
  vars <- utils::head(vars, max_vars)

  pattern <- df |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), is.na)) |>
    dplyr::count(dplyr::across(dplyr::everything()), name = "n_rows") |>
    dplyr::mutate(pattern_id = dplyr::row_number()) |>
    tidyr::pivot_longer(-c(n_rows, pattern_id),
                        names_to = "variable", values_to = "is_missing") |>
    dplyr::mutate(
      vlabel = label_var(variable),
      vlabel = factor(vlabel, levels = label_var(vars))
    )

  ggplot2::ggplot(pattern, ggplot2::aes(
    x    = vlabel,
    y    = factor(pattern_id),
    fill = is_missing
  )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      data = dplyr::distinct(pattern, pattern_id, n_rows),
      ggplot2::aes(x = length(vars) + 0.5, y = factor(pattern_id),
                   label = n_rows, fill = NULL),
      hjust = 0, size = 3, family = FONT
    ) +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = PALETTE[6], "TRUE" = PALETTE[1]),
      labels = c("Present", "Missing"),
      name   = NULL
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0, 2))) +
    theme_proj() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 40, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Missing data patterns",
      subtitle = "Each row is a unique missingness pattern; n = number of participants",
      x = NULL, y = "Pattern"
    )
}

# ── 3. Per-variable missingness bar chart (sorted, coloured by visit) ──────────

plot_missing_by_var <- function(data,
                                vars     = NULL,
                                time_col = "time_point") {
  df     <- extract_data(data)
  vars   <- intersect(vars %||% setdiff(names(df), time_col), names(df))
  visits <- sort(unique(df[[time_col]]))

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
      vlabel = forcats::fct_reorder(vlabel, pct, .fun = mean),
      visit  = factor(.data[[time_col]], levels = visits)
    )

  ggplot2::ggplot(miss, ggplot2::aes(
    x    = pct,
    y    = vlabel,
    fill = visit,
    color = visit
  )) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::scale_fill_manual(values  = .pal_seq(length(visits)), name = "Visit") +
    ggplot2::scale_color_manual(values = .pal_seq(length(visits)), name = "Visit") +
    ggplot2::scale_x_continuous(limits = c(0, 100), labels = scales::label_percent(scale = 1)) +
    theme_proj() +
    ggplot2::labs(
      title = "Missingness per variable by time point",
      x = "% missing", y = NULL
    )
}

# ── 4. Comparison: missingness across named datasets ──────────────────────────
#
# Designed to compare e.g.:
#   "Before imputation" (mice_merged_derived$data)  — NAs present
#   "After MICE"        (mice::complete(mids, 1))   — imputed vars → 0%
#   "CC"                (cc_analysis$data_shared)   — complete cases, 0% but fewer rows
#
# Only variables with any missingness in at least one dataset are shown.
# Variables absent from a dataset are treated as 0% missing.

plot_missing_comparison <- function(data_list,
                                    vars     = NULL,
                                    time_col = "time_point") {
  miss <- purrr::imap_dfr(data_list, function(data, nm) {
    df    <- extract_data(data)
    v_use <- intersect(vars %||% setdiff(names(df), time_col), names(df))
    tibble::tibble(
      dataset  = nm,
      variable = v_use,
      label    = label_var(v_use),
      pct      = colMeans(is.na(df[v_use])) * 100
    )
  })

  # Variables with any missingness in any dataset
  any_miss <- miss |>
    dplyr::group_by(variable) |>
    dplyr::filter(any(pct > 0)) |>
    dplyr::pull(variable) |>
    unique()

  if (length(any_miss) == 0) {
    cli::cli_inform("No missing values found — comparison plot skipped.")
    return(invisible(NULL))
  }

  # Sort variables by missingness in the reference (first) dataset
  ref_order <- miss |>
    dplyr::filter(dataset == names(data_list)[1], variable %in% any_miss) |>
    dplyr::arrange(pct) |>
    dplyr::pull(label)

  miss <- miss |>
    dplyr::filter(variable %in% any_miss) |>
    dplyr::mutate(
      label   = factor(label, levels = ref_order),
      dataset = factor(dataset, levels = names(data_list))
    )

  ggplot2::ggplot(miss, ggplot2::aes(x = pct, y = label, fill = dataset)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
    ggplot2::scale_fill_manual(values = .pal_cat(length(data_list)), name = NULL) +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      labels = scales::label_percent(scale = 1)
    ) +
    theme_proj() +
    ggplot2::labs(
      title    = "Missingness comparison across datasets",
      subtitle = "Only variables with missingness in at least one dataset shown",
      x = "% missing", y = NULL
    )
}

# ── Main entry point ───────────────────────────────────────────────────────────

#' Full missingness report on the pre-exclusion dataset.
#'
#' @param data     data.frame or mids object (use pre-exclusion merged data).
#'                 For mids, $data (original unimputed data) is used automatically.
#' @param compare  Named list of additional datasets to compare against in
#'                 plot_missing_comparison(). Typical usage:
#'                 list("After MICE" = mice::complete(mids, 1),
#'                      "CC"         = cc_analysis$data_shared)
#'                 Each entry can be a data.frame or mids (extract_data() applied).
#' @param vars     Variables to assess; default: all non-time columns.
#' @param time_col Column name for visit/time stratification.
#' @param out_dir  Directory for PNG and CSV output; NULL to skip saving.
#' @param width,height,dpi Plot dimensions.
#' @return Named list: $heatmap, $patterns, $by_var, $comparison, $data.
describe_missingness <- function(data,
                                 compare  = NULL,
                                 vars     = NULL,
                                 time_col = "time_point",
                                 out_dir  = NULL,
                                 width = 10, height = 7, dpi = 180) {
  df   <- extract_data(data)
  vars <- intersect(vars %||% setdiff(names(df), time_col), names(df))

  if (!is.null(out_dir))
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  plots <- list(
    heatmap  = plot_missing_heatmap(df,  vars = vars, time_col = time_col),
    patterns = plot_missing_patterns(df, vars = vars, time_col = time_col),
    by_var   = plot_missing_by_var(df,   vars = vars, time_col = time_col)
  )

  if (!is.null(compare)) {
    plots$comparison <- plot_missing_comparison(
      data_list = c(list("Before imputation" = df), compare),
      vars      = vars,
      time_col  = time_col
    )
  }

  # Summary tibble (original data only)
  miss_data <- df |>
    dplyr::group_by(.data[[time_col]]) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(vars), ~ mean(is.na(.)) * 100),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(-dplyr::all_of(time_col),
                        names_to = "variable", values_to = "pct_missing") |>
    dplyr::mutate(label = label_var(variable), pct_missing = round(pct_missing, 1)) |>
    dplyr::arrange(dplyr::desc(pct_missing))

  if (!is.null(out_dir)) {
    purrr::iwalk(
      purrr::compact(plots),
      ~ ggplot2::ggsave(
        file.path(out_dir, paste0("missing_", .y, ".png")),
        .x, width = width, height = height, dpi = dpi
      )
    )
    readr::write_csv(miss_data, file.path(out_dir, "missing_summary.csv"))

    if (!is.null(compare)) {
      data_list <- c(list("Before imputation" = df), compare)
      purrr::imap_dfr(data_list, function(data, nm) {
        d     <- extract_data(data)
        v_use <- intersect(vars, names(d))
        tibble::tibble(
          dataset  = nm,
          variable = v_use,
          label    = label_var(v_use),
          pct      = round(colMeans(is.na(d[v_use])) * 100, 1)
        )
      }) |>
        tidyr::pivot_wider(names_from = dataset, values_from = pct,
                           values_fill = 0) |>
        dplyr::mutate(
          reduction_pct = .data[["Before imputation"]] -
                          .data[[names(compare)[1]]]
        ) |>
        dplyr::filter(.data[["Before imputation"]] > 0) |>
        dplyr::arrange(dplyr::desc(.data[["Before imputation"]])) |>
        readr::write_csv(file.path(out_dir, "missing_imputation_summary.csv"))
    }

    cli::cli_inform(c("v" = "Missingness report saved to {.path {out_dir}}"))
  }

  invisible(c(plots, list(data = miss_data)))
}
