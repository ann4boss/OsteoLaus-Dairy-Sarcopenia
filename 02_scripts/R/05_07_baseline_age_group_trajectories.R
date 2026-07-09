# =============================================================================
# R/05_07_baseline_age_group_trajectories.R
# For each outcome variable: scatter of outcome vs. observed Age, coloured by
# 5-year baseline-age group (age at T1, floored), with a linear trajectory
# (lm) fit per group. Accepts plain data frames (CC) and mids objects (MICE,
# uses the underlying incomplete data$data — consistent with extract_data()
# in R/05_04_variables_descriptives.R).
# =============================================================================

# Reuses .TRAJ_PALETTE / .traj_label / .theme_traj / .col_names, defined in
# R/05_05_age_trajectories.R and loaded into the same environment via
# tar_source().

.BAGE_BREAKS <- seq(50, 100, by = 5)

.bage_extract <- function(data) {
    if (inherits(data, "mids")) data$data else data
}

# Build "50-54", "55-59", ..., "95-99", "100+" labels from .BAGE_BREAKS
.bage_group_levels <- function(breaks = .BAGE_BREAKS) {
    lo <- breaks[-length(breaks)]
    hi <- breaks[-1] - 1
    c(paste0(lo, "-", hi), paste0(breaks[length(breaks)], "+"))
}

.bage_assign_group <- function(age, breaks = .BAGE_BREAKS) {
    levels_ <- .bage_group_levels(breaks)
    idx <- findInterval(age, breaks, rightmost.closed = FALSE)
    idx[idx < 1L] <- 1L
    idx[idx > length(levels_)] <- length(levels_)
    factor(levels_[idx], levels = levels_)
}

# Evenly spaced colours across .TRAJ_PALETTE for however many age groups are
# actually present (interpolates if more groups than palette colours).
.bage_palette <- function(n, palette = .TRAJ_PALETTE) {
    if (n <= length(palette)) {
        idx <- round(seq(1, length(palette), length.out = n))
        palette[idx]
    } else {
        grDevices::colorRampPalette(palette)(n)
    }
}

# ── Data prep ──────────────────────────────────────────────────────────────

.bage_prepare <- function(data, var, age_col = "Age",
                          pt_col = "pt", visit_col = "time_point",
                          baseline_visit = "T1", breaks = .BAGE_BREAKS) {
    df <- .bage_extract(data)

    needed <- c(var, age_col, pt_col, visit_col)
    if (!all(needed %in% names(df))) return(NULL)

    baseline_ages <- df |>
        dplyr::filter(.data[[visit_col]] == baseline_visit) |>
        dplyr::group_by(.data[[pt_col]]) |>
        dplyr::summarise(Baseline_Age = floor(dplyr::first(.data[[age_col]])),
                          .groups = "drop")
    names(baseline_ages)[1] <- pt_col

    df |>
        dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[age_col]])) |>
        dplyr::inner_join(baseline_ages, by = pt_col) |>
        dplyr::filter(!is.na(.data$Baseline_Age)) |>
        dplyr::mutate(Baseline_Age_Group = .bage_assign_group(.data$Baseline_Age, breaks))
}

# ── Plot ───────────────────────────────────────────────────────────────────

.plot_baseline_age_group <- function(plot_data, var, age_col = "Age") {
    label <- .traj_label(var)

    present <- levels(plot_data$Baseline_Age_Group)[
        levels(plot_data$Baseline_Age_Group) %in% unique(plot_data$Baseline_Age_Group)
    ]
    cols <- rlang::set_names(.bage_palette(length(present)), present)

    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[age_col]], y = .data[[var]])) +
        ggplot2::geom_point(ggplot2::aes(colour = Baseline_Age_Group), alpha = 0.3, size = 1.5) +
        ggplot2::geom_smooth(
            ggplot2::aes(colour = Baseline_Age_Group, group = Baseline_Age_Group),
            method = "lm", se = TRUE, linewidth = 1.2
        ) +
        ggplot2::scale_colour_manual(values = cols, name = "Baseline age group", drop = TRUE) +
        ggplot2::scale_fill_manual(values = cols, guide = "none") +
        ggplot2::labs(
            title = paste0(label, " vs. Age by Baseline Age Group (5-year bins)"),
            x     = "Age (years)",
            y     = label
        ) +
        .theme_traj() +
        ggplot2::theme(legend.position = "right")
}

# ── Main entry point ───────────────────────────────────────────────────────

#' Plot outcome vs. age, coloured by 5-year baseline-age group, with a linear
#' trajectory fit per group, for each outcome variable.
#'
#' @param analysis  List returned by run_exclusions(). Must have $data (named
#'   list per outcome, plain data frame or mids) and/or $data_shared.
#' @param outcomes  Outcome variable names to plot.
#' @param out_dir   Output directory for PNGs.
#' @param width,height,dpi  Plot dimensions.
#' @return Named list of ggplot objects (invisibly). Files saved to `out_dir`.
plot_baseline_age_group_trajectories <- function(
    analysis,
    outcomes = c("HGS_MAX", "ALM_HT2_harmonised", "gait_speed"),
    out_dir  = "03_outputs/descriptives/baseline_age_group_trajectories",
    width    = 9,
    height   = 6,
    dpi      = 180
) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    plots <- purrr::map(outcomes, function(var) {
        data <- analysis$data[[var]] %||% analysis$data_shared
        if (is.null(data)) {
            cli::cli_warn("Skipping {.val {var}}: no data found.")
            return(NULL)
        }

        age_col <- .detect_age_col(.bage_extract(data))
        if (is.na(age_col)) {
            cli::cli_warn("Skipping {.val {var}}: no Age column found.")
            return(NULL)
        }

        plot_data <- .bage_prepare(data, var, age_col = age_col)
        if (is.null(plot_data) || nrow(plot_data) == 0L) {
            cli::cli_warn("Skipping {.val {var}}: no plottable rows found.")
            return(NULL)
        }

        plt <- .plot_baseline_age_group(plot_data, var, age_col)
        ggplot2::ggsave(
            file.path(out_dir, paste0("baseline_age_group_", var, ".png")),
            plt, width = width, height = height, dpi = dpi
        )
        plt
    }) |> rlang::set_names(outcomes)

    plots <- purrr::compact(plots)

    cli::cli_inform(c(
        "v" = "Baseline age group trajectories: {length(plots)} outcome{?s}",
        "i" = "Output: {.file {out_dir}}"
    ))

    invisible(plots)
}
