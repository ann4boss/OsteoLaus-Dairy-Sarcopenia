# =============================================================================
# R/results_summary.R
# =============================================================================
# Manuscript-facing result numbers and flow diagram helpers.
# =============================================================================

.default_outcome_labels <- c(
  HGS = "Handgrip strength",
  ALM = "ALMI",
  ALM_Lunar = "ALMI (Lunar)",
  gait = "Gait speed",
  sarcopenia = "Incident sarcopenia"
)

.count_participants_by_imp <- function(data, pt_col = "pt", imp_col = ".imp") {
  if (imp_col %in% names(data)) {
    data |>
      dplyr::filter(.data[[imp_col]] > 0) |>
      dplyr::group_by(.data[[imp_col]]) |>
      dplyr::summarise(n = dplyr::n_distinct(.data[[pt_col]]), .groups = "drop") |>
      dplyr::rename(imp = dplyr::all_of(imp_col))
  } else {
    tibble::tibble(imp = NA_integer_, n = dplyr::n_distinct(data[[pt_col]]))
  }
}

.format_imp_n <- function(counts) {
  counts <- dplyr::filter(counts, !is.na(.data$n))
  if (nrow(counts) == 0L) {
    return(list(n = NA_integer_, text = "NA", min = NA_integer_, max = NA_integer_))
  }

  n_min <- min(counts$n)
  n_max <- max(counts$n)
  n_mid <- stats::median(counts$n)

  if (n_min == n_max) {
    list(n = n_min, text = as.character(n_min), min = n_min, max = n_max)
  } else {
    list(
      n = n_mid,
      text = sprintf("%.0f (range %s-%s across imputations)", n_mid, n_min, n_max),
      min = n_min,
      max = n_max
    )
  }
}

.participant_sets_by_imp <- function(data, pt_col = "pt", imp_col = ".imp") {
  if (imp_col %in% names(data)) {
    data |>
      dplyr::filter(.data[[imp_col]] > 0) |>
      dplyr::group_by(.data[[imp_col]]) |>
      dplyr::summarise(pt = list(unique(.data[[pt_col]])), .groups = "drop") |>
      dplyr::rename(imp = dplyr::all_of(imp_col))
  } else {
    tibble::tibble(imp = NA_integer_, pt = list(unique(data[[pt_col]])))
  }
}

.common_participants_by_imp <- function(analysis_results, pt_col = "pt", imp_col = ".imp") {
  sets <- purrr::imap(analysis_results, function(x, nm) {
    .participant_sets_by_imp(x$data, pt_col = pt_col, imp_col = imp_col) |>
      dplyr::mutate(outcome = nm)
  })

  dplyr::bind_rows(sets) |>
    dplyr::group_by(.data$imp) |>
    dplyr::summarise(
      n = length(Reduce(intersect, .data$pt)),
      .groups = "drop"
    )
}

.summarise_consort_counts <- function(consort_counts, imp_col = ".imp") {
  if (imp_col %in% names(consort_counts)) {
    consort_counts |>
      dplyr::filter(.data[[imp_col]] > 0) |>
      dplyr::group_by(.data$stage) |>
      dplyr::summarise(
        n_participants = round(mean(.data$n_participants, na.rm = TRUE)),
        n_rows = round(mean(.data$n_rows, na.rm = TRUE)),
        .groups = "drop"
      )
  } else {
    consort_counts
  }
}

#' Build manuscript-ready sample size summaries.
make_result_numbers <- function(general_analysis,
                                outcome_analyses,
                                outcome_labels = .default_outcome_labels,
                                pt_col = "pt",
                                imp_col = ".imp") {
  outcome_labels <- outcome_labels[names(outcome_analyses)]

  outcome_n <- purrr::imap_dfr(outcome_analyses, function(x, nm) {
    formatted <- .format_imp_n(.count_participants_by_imp(x$data, pt_col, imp_col))
    tibble::tibble(
      outcome = nm,
      label = unname(outcome_labels[[nm]]),
      n = formatted$n,
      n_text = formatted$text,
      n_min = formatted$min,
      n_max = formatted$max
    )
  })

  list(
    n_general = .format_imp_n(.count_participants_by_imp(general_analysis$data, pt_col, imp_col)),
    n_all_outcomes = .format_imp_n(.common_participants_by_imp(outcome_analyses, pt_col, imp_col)),
    outcome_n = outcome_n,
    common_flow_counts = .summarise_consort_counts(general_analysis$consort_counts, imp_col)
  )
}

.flow_box_data <- function(result_numbers) {
  common <- result_numbers$common_flow_counts |>
    dplyr::mutate(
      type = "common",
      label = paste0(.data$stage, "\n", .data$n_participants, " participants"),
      x = 0,
      y = dplyr::row_number()
    )

  outcomes <- result_numbers$outcome_n |>
    dplyr::mutate(
      type = "outcome",
      label = paste0(.data$label, "\n", .data$n_text, " participants"),
      x = seq_len(dplyr::n()) - mean(seq_len(dplyr::n())),
      y = max(common$y) + 2
    )

  list(common = common, outcomes = outcomes)
}

#' Plot one shared exclusion flow with branches to outcome-specific samples.
plot_analysis_flow <- function(result_numbers) {
  boxes <- .flow_box_data(result_numbers)
  common <- boxes$common
  outcomes <- boxes$outcomes
  final_common <- dplyr::slice_tail(common, n = 1)

  common_rects <- common |>
    dplyr::mutate(
      xmin = -1.45, xmax = 1.45,
      ymin = .data$y - 0.34, ymax = .data$y + 0.34
    )

  outcome_rects <- outcomes |>
    dplyr::mutate(
      xmin = .data$x - 0.48, xmax = .data$x + 0.48,
      ymin = .data$y - 0.36, ymax = .data$y + 0.36
    )

  common_segments <- tibble::tibble(
    x = 0,
    xend = 0,
    y = head(common$y + 0.34, -1),
    yend = tail(common$y - 0.34, -1)
  )

  branch_segments <- outcomes |>
    dplyr::transmute(
      x = final_common$x,
      y = final_common$y + 0.34,
      xend = .data$x,
      yend = .data$y - 0.36
    )

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = common_segments,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      linewidth = 0.45,
      lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = branch_segments,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      linewidth = 0.45,
      lineend = "round"
    ) +
    ggplot2::geom_rect(
      data = common_rects,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
      fill = "#F7F7F7",
      color = "#2F2F2F",
      linewidth = 0.35
    ) +
    ggplot2::geom_rect(
      data = outcome_rects,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
      fill = "#EAF3F0",
      color = "#2F2F2F",
      linewidth = 0.35
    ) +
    ggplot2::geom_text(
      data = common,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 3.2,
      lineheight = 0.95
    ) +
    ggplot2::geom_text(
      data = outcomes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 2.9,
      lineheight = 0.95
    ) +
    ggplot2::scale_y_reverse(expand = ggplot2::expansion(mult = 0.08)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::margin(12, 18, 12, 18))
}

save_analysis_flow <- function(plot,
                               path = "03_outputs/results/flow_diagram_mice.png",
                               width = 10,
                               height = 7,
                               dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
  path
}

#' Supplementary baseline table comparing complete-case and imputed datasets.
make_table_one_cc_vs_mice <- function(cc_analysis_long,
                                      mice_analysis_long,
                                      id = "pt",
                                      visit = ".visit_osteo",
                                      imp_col = ".imp") {
  mice_long <- .to_long(mice_analysis_long, imp_col)
  imp_values <- .imp_values(mice_long, imp_col)

  cc_baseline <- make_display_baseline(cc_analysis_long, id = id, visit = visit) |>
    dplyr::select(dplyr::any_of(c(id, .TABLE_VARS))) |>
    dplyr::mutate(dataset = "Complete case")

  cc_repeated <- purrr::map_dfr(imp_values, function(i) {
    cc_baseline |>
      dplyr::mutate("{imp_col}" := i)
  })

  mice_baseline <- make_display_baseline_imputed(
    mice_long,
    id = id,
    visit = visit,
    imp_col = imp_col
  ) |>
    dplyr::select(dplyr::any_of(c(id, imp_col, .TABLE_VARS))) |>
    dplyr::mutate(dataset = "Multiple imputation")

  combined <- dplyr::bind_rows(cc_repeated, mice_baseline) |>
    dplyr::mutate(dataset = factor(.data$dataset, levels = c("Complete case", "Multiple imputation")))

  .manual_table_one(
    combined,
    by = "dataset",
    id = id,
    imp_col = imp_col,
    include_vars = .table_vars_for_data(combined),
    caption = "**Supplement.** Baseline characteristics: complete-case versus multiple imputation",
    footnote_suffix = " Complete-case rows are repeated across imputation indices to align denominators for pooled descriptive comparison. No significance tests shown."
  )
}

#' Supplementary baseline table comparing included and excluded MICE participants.
make_table_one_included_vs_excluded_mice <- function(full_analysis_long,
                                                     included_analysis_long,
                                                     id = "pt",
                                                     visit = ".visit_osteo",
                                                     imp_col = ".imp") {
  full_long <- .to_long(full_analysis_long, imp_col)
  included_long <- .to_long(included_analysis_long, imp_col)

  included_keys <- included_long |>
    dplyr::distinct(.data[[imp_col]], .data[[id]]) |>
    dplyr::mutate(inclusion_status = "Included")

  baseline <- make_display_baseline_imputed(
    full_long,
    id = id,
    visit = visit,
    imp_col = imp_col
  ) |>
    dplyr::left_join(included_keys, by = c(imp_col, id)) |>
    dplyr::mutate(
      inclusion_status = dplyr::coalesce(.data$inclusion_status, "Excluded"),
      inclusion_status = factor(.data$inclusion_status, levels = c("Included", "Excluded"))
    ) |>
    dplyr::select(dplyr::any_of(c(id, imp_col, "inclusion_status", .TABLE_VARS)))

  .manual_table_one(
    baseline,
    by = "inclusion_status",
    id = id,
    imp_col = imp_col,
    include_vars = .table_vars_for_data(baseline),
    caption = "**Supplement.** Baseline characteristics: included versus excluded participants in the multiple imputed dataset",
    footnote_suffix = " Inclusion is based on the general MICE analytic sample. No significance tests shown."
  )
}
