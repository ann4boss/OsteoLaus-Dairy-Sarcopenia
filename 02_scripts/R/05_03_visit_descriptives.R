# =============================================================================
# R/05_03_visit_descriptives.R
# =============================================================================
# Visit structure and timing descriptive plots
#
# Functions:
#   .visit_colors()          — n evenly-spaced colours from VISIT_PALETTE
#   .theme_visit()           — shared Helvetica minimal ggplot theme
#   .extract_df()            — unwraps a mids object to its plain $data
#   .visits_to_long()        — normalises data to long format with an .imp column
#   analyze_visits_structure() — counts visits (1-4) per participant
#   plot_timing_violin()     — violin + boxplot of time_since_baseline by visit
#   plot_patient_coverage()  — bar chart of % patients seen per time point
#   save_visit_descriptives() — generates and saves all visit descriptive plots
# =============================================================================

# -----------------------------------------------------------------------------
# VISIT_PALETTE / .visit_colors()
# -----------------------------------------------------------------------------
VISIT_PALETTE <- c(
    "#E76254FF", "#EF8A47FF", "#F7AA58FF", "#FFD06FFF", "#FFE6B7FF",
    "#AADCE0FF", "#72BCD5FF", "#528FADFF", "#376795FF", "#1E466EFF"
)

# Pick n evenly-spaced colors from VISIT_PALETTE
.visit_colors <- function(visit_levels) {
    n   <- length(visit_levels)
    idx <- round(seq(1, length(VISIT_PALETTE), length.out = n))
    stats::setNames(VISIT_PALETTE[idx], visit_levels)
}

# -----------------------------------------------------------------------------
# .theme_visit()
# -----------------------------------------------------------------------------
# Shared Helvetica minimal theme
.theme_visit <- function() {
    ggplot2::theme_minimal(base_family = "Helvetica") +
        ggplot2::theme(
            plot.title       = ggplot2::element_text(hjust = 0.5, face = "bold", size = 13),
            axis.text        = ggplot2::element_text(size = 10),
            axis.title       = ggplot2::element_text(size = 11),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position  = "bottom",
            legend.title     = ggplot2::element_blank()
        )
}

# -----------------------------------------------------------------------------
# .extract_df()
# -----------------------------------------------------------------------------
# Extract a plain data.frame from a mids object or return as-is
.extract_df <- function(data) {
    if (inherits(data, "mids")) data$data else data
}

# -----------------------------------------------------------------------------
# .visits_to_long()
# -----------------------------------------------------------------------------
# Convert mids or plain data.frame to long format with .imp column
.visits_to_long <- function(data) {
    if (inherits(data, "mids")) {
        mice::complete(data, action = "long", include = FALSE)
    } else if (".imp" %in% names(data)) {
        data
    } else {
        dplyr::mutate(data, .imp = 0L)
    }
}


# -----------------------------------------------------------------------------
# analyze_visits_structure
# Count how many visits (1–4) each participant has.
# Works with mids objects, long-format imputed data, or plain data frames.
# -----------------------------------------------------------------------------
analyze_visits_structure <- function(data,
                                     pt_col    = "pt",
                                     visit_col = "time_point") {

    long <- .visits_to_long(data)
    has_imputations <- length(unique(long$.imp)) > 1L

    if (has_imputations) {
        long |>
            dplyr::group_by(.imp, !!dplyr::sym(pt_col)) |>
            dplyr::summarise(
                n_visits = dplyr::n_distinct(!!dplyr::sym(visit_col)),
                visits   = paste(sort(unique(!!dplyr::sym(visit_col))), collapse = ","),
                .groups  = "drop"
            ) |>
            dplyr::group_by(.imp) |>
            dplyr::summarise(
                n_participants_total = dplyr::n_distinct(!!dplyr::sym(pt_col)),
                `1_visit`  = sum(n_visits == 1L),
                `2_visits` = sum(n_visits == 2L),
                `3_visits` = sum(n_visits == 3L),
                `4_visits` = sum(n_visits == 4L),
                .groups = "drop"
            )
    } else {
        long |>
            dplyr::group_by(!!dplyr::sym(pt_col)) |>
            dplyr::summarise(
                n_visits = dplyr::n_distinct(!!dplyr::sym(visit_col)),
                .groups  = "drop"
            ) |>
            dplyr::summarise(
                n_participants_total = dplyr::n(),
                `1_visit`  = sum(n_visits == 1L),
                `2_visits` = sum(n_visits == 2L),
                `3_visits` = sum(n_visits == 3L),
                `4_visits` = sum(n_visits == 4L)
            )
    }
}


# -----------------------------------------------------------------------------
# plot_timing_violin
# Violin + boxplot of time_since_baseline by time_point.
# T4 is excluded (OsteoLaus-only visit; no CoLaus counterpart).
# -----------------------------------------------------------------------------
plot_timing_violin <- function(data,
                               timing_var     = "time_since_baseline",
                               visit_var      = "time_point",
                               exclude_visits = "T4",
                               fill_color     = "#376795FF",
                               y_label        = "Years since baseline") {

    df <- .extract_df(data)
    df <- dplyr::filter(df, !(.data[[visit_var]] %in% exclude_visits))

    visit_levels <- sort(unique(as.character(df[[visit_var]])))

    plot <- ggplot2::ggplot(
        df,
        ggplot2::aes(
            x = factor(.data[[visit_var]], levels = visit_levels),
            y = .data[[timing_var]]
        )
    ) +
        ggplot2::geom_violin(fill = fill_color, alpha = 0.7, colour = NA) +
        ggplot2::geom_boxplot(width = 0.12, fill = "white",
                              colour = "grey30", outlier.size = 0.8) +
        ggplot2::labs(title = NULL, x = "Time point", y = y_label) +
        .theme_visit() +
        ggplot2::theme(
            axis.line   = ggplot2::element_line(colour = "black", linewidth = 0.5),
            axis.ticks  = ggplot2::element_line(colour = "black", linewidth = 0.4)
        )

    summary <- df |>
        dplyr::group_by(visit = factor(.data[[visit_var]], levels = visit_levels)) |>
        dplyr::summarise(
            n      = dplyr::n(),
            median = median(.data[[timing_var]], na.rm = TRUE),
            q25    = quantile(.data[[timing_var]], 0.25, na.rm = TRUE),
            q75    = quantile(.data[[timing_var]], 0.75, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            median_diff_from_prev = median - dplyr::lag(median)
        )

    list(plot = plot, summary = summary)
}


# -----------------------------------------------------------------------------
# plot_patient_coverage
# Bar chart: % of total unique patients seen at each time point.
# Accepts a named list of mids/data.frames or a single one.
# -----------------------------------------------------------------------------
plot_patient_coverage <- function(datasets_list,
                                  dataset_names = NULL,
                                  time_col      = "time_point",
                                  pt_col        = "pt",
                                  title         = "Patient coverage across time points",
                                  xlab          = "Time point",
                                  ylab          = "Percentage of total patients (%)",
                                  show_values   = TRUE) {

    if (is.data.frame(datasets_list) || inherits(datasets_list, "mids")) {
        datasets_list <- list(datasets_list)
    }

    if (is.null(dataset_names)) {
        nms <- names(datasets_list)
        dataset_names <- if (!is.null(nms) && all(nzchar(nms))) nms
                         else if (length(datasets_list) == 1L) "Dataset"
                         else paste0("Dataset ", seq_along(datasets_list))
    }

    process_one <- function(data, name) {
        df         <- .extract_df(data)
        total_pts  <- dplyr::n_distinct(df[[pt_col]])
        pt_counts  <- df |>
            dplyr::group_by(!!dplyr::sym(time_col)) |>
            dplyr::summarise(n_unique = dplyr::n_distinct(!!dplyr::sym(pt_col)),
                             .groups  = "drop") |>
            dplyr::rename(time_point = !!dplyr::sym(time_col)) |>
            dplyr::mutate(
                percentage     = n_unique / total_pts * 100,
                dataset        = name,
                total_patients = total_pts
            )
        pt_counts
    }

    combined <- purrr::map2_dfr(datasets_list, dataset_names, process_one)
    combined$time_point <- factor(combined$time_point,
                                  levels = sort(unique(combined$time_point)))

    dataset_levels <- unique(combined$dataset)
    colors         <- .visit_colors(dataset_levels)

    p <- ggplot2::ggplot(
        combined,
        ggplot2::aes(x = time_point, y = percentage, fill = dataset)
    ) +
        ggplot2::geom_col(
            position = ggplot2::position_dodge(width = 0.9),
            alpha    = 0.85,
            width    = 0.7
        ) +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::scale_y_continuous(
            limits = c(0, 110),
            breaks = seq(0, 100, 20),
            labels = paste0(seq(0, 100, 20), "%")
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab) +
        .theme_visit()

    if (show_values) {
        p <- p + ggplot2::geom_text(
            ggplot2::aes(
                label = paste0(round(percentage, 1), "%\n(n=", n_unique, ")")
            ),
            position = ggplot2::position_dodge(width = 0.9),
            vjust    = -0.25,
            size     = 3,
            family   = "Helvetica"
        )
    }

    list(plot = p, data = combined)
}


# -----------------------------------------------------------------------------
# save_visit_descriptives
# Generate all visit descriptive plots and save as PNG.
# Returns a character vector of saved file paths (use format = "file" in targets).
# -----------------------------------------------------------------------------
save_visit_descriptives <- function(data,
                                    out_dir        = "03_outputs/descriptives/visits",
                                    label          = "data",
                                    timing_var     = "time_since_baseline",
                                    visit_var      = "time_point",
                                    pt_col         = "pt",
                                    exclude_violin = "T4",
                                    width          = 8,
                                    height         = 5,
                                    dpi            = 180) {

    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    saved <- character(0)

    # ── 1. Violin: timing distribution (T4 excluded) ──────────────────────────
    p_violin <- plot_timing_violin(
        data           = data,
        timing_var     = timing_var,
        visit_var      = visit_var,
        exclude_visits = exclude_violin
    )
    f_violin <- file.path(out_dir, paste0(label, "_timing_violin.png"))
    ggplot2::ggsave(f_violin, p_violin, width = width, height = height, dpi = dpi)
    saved <- c(saved, f_violin)

    # ── 2. Patient coverage bar chart ─────────────────────────────────────────
    coverage <- plot_patient_coverage(
        datasets_list = data,
        dataset_names = label,
        time_col      = visit_var,
        pt_col        = pt_col
    )
    f_coverage <- file.path(out_dir, paste0(label, "_patient_coverage.png"))
    ggplot2::ggsave(f_coverage, coverage$plot, width = width, height = height, dpi = dpi)
    saved <- c(saved, f_coverage)

    cli::cli_inform(c(
        "v" = "Visit descriptives saved ({length(saved)} file{?s})",
        "i" = "Directory: {.file {out_dir}}"
    ))

    saved
}
