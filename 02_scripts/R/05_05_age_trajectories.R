# =============================================================================
# R/05_05_age_trajectories.R
# Mean ± SD trajectories of outcome and exposure variables by whole age year.
# Accepts both plain data frames (CC route) and mids objects (MICE route).
# For mids: per-imputation summaries are pooled (Rubin's rules for the mean;
# average within-imputation SD shown as error bars / ribbons).
#
# Produces two plot families:
#   1. Overall trajectory (mean ± SD per age year, all participants)
#   2. Outcome trajectories stratified by dairy_quartile_baseline
# =============================================================================

.TRAJ_PALETTE <- c(
    "#E76254FF", "#EF8A47FF", "#F7AA58FF", "#FFD06FFF", "#FFE6B7FF",
    "#AADCE0FF", "#72BCD5FF", "#528FADFF", "#376795FF", "#1E466EFF"
)

.TRAJ_FONT <- "Helvetica"

.TRAJ_LABELS <- c(
    HGS_MAX                         = "Handgrip strength [kg]",
    ALM_HT2_harmonised              = "ALMI [kg/m^2]",
    gait_speed                      = "Gait speed [m/s]",
    dairy_total_gday_cumavg         = "Total Dairy Intake [g/day]",
    dairy_fermented_gday_cumavg     = "Fermented Dairy Intake [g/day]",
    dairy_non_fermented_gday_cumavg = "Non-fermented Dairy Intake [g/day]",
    dairy_highfat_gday_cumavg       = "High-fat Dairy Intake [g/day]",
    dairy_lowfat_gday_cumavg        = "Low-fat Dairy Intake [g/day]"
)

# Same blue for every variable's line and points.
.TRAJ_COLORS <- c(
    HGS_MAX                         = "#1E466EFF",
    ALM_HT2_harmonised              = "#1E466EFF",
    gait_speed                      = "#1E466EFF",
    dairy_total_gday_cumavg         = "#1E466EFF",
    dairy_fermented_gday_cumavg     = "#1E466EFF",
    dairy_non_fermented_gday_cumavg = "#1E466EFF",
    dairy_highfat_gday_cumavg       = "#1E466EFF",
    dairy_lowfat_gday_cumavg        = "#1E466EFF"
)

# Warm → cool gradient across quartiles (low → high dairy)
.QUARTILE_COLORS <- c(
    "Q1" = "#E76254FF",
    "Q2" = "#F7AA58FF",
    "Q3" = "#72BCD5FF",
    "Q4" = "#1E466EFF"
)

.traj_label <- function(var) {
    if (var %in% names(.TRAJ_LABELS)) .TRAJ_LABELS[[var]] else gsub("_", " ", var)
}

# Axis-only variant: superscripts the "2" in ALMI's units via plotmath.
.traj_axis_label <- function(var) {
    if (var == "ALM_HT2_harmonised") expression("ALMI [kg/m" ^ 2 * "]")
    else .traj_label(var)
}

.theme_traj <- function() {
    ggplot2::theme_minimal(base_family = .TRAJ_FONT) +
        ggplot2::theme(
            axis.title         = ggplot2::element_text(size = 12),
            axis.text          = ggplot2::element_text(size = 10),
            axis.line          = ggplot2::element_line(colour = "black", linewidth = 0.4),
            axis.ticks         = ggplot2::element_line(colour = "black", linewidth = 0.3),
            axis.ticks.length  = ggplot2::unit(3, "pt"),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(colour = "grey90", linewidth = 0.4),
            panel.grid.minor.y = ggplot2::element_blank(),
            legend.position    = "none"
        )
}

# Doubles the density of y breaks (adds the midpoint of each major interval as
# its own labeled break) so the extra horizontal grid line also gets a label.
.scale_y_dense <- function() {
    ggplot2::scale_y_continuous(
        breaks = function(limits) {
            major <- scales::extended_breaks()(limits)
            step  <- diff(major)[1] / 2
            sort(unique(c(major, major + step)))
        },
        minor_breaks = NULL
    )
}

# Return column names regardless of mids vs plain data frame
.col_names <- function(data) {
    if (inherits(data, "mids")) names(data$data) else names(data)
}

# Prefer Age; fall back to Age_lag; return NA if neither present
.detect_age_col <- function(data) {
    cols <- .col_names(data)
    if ("Age"     %in% cols) "Age"
    else if ("Age_lag" %in% cols) "Age_lag"
    else NA_character_
}


# ── Summarisation: overall trajectories ───────────────────────────────────────

.summarise_df_by_age_year <- function(data, var, age_col) {
    data |>
        dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[age_col]])) |>
        dplyr::mutate(Age_Year = floor(.data[[age_col]])) |>
        dplyr::group_by(Age_Year) |>
        dplyr::summarise(
            mean    = mean(.data[[var]], na.rm = TRUE),
            sd      = dplyr::coalesce(stats::sd(.data[[var]], na.rm = TRUE), 0),
            n       = dplyr::n(),
            min_age = min(.data[[age_col]], na.rm = TRUE),
            max_age = max(.data[[age_col]], na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::arrange(Age_Year)
}

# mids: pool per-imputation summaries
.summarise_mids_by_age_year <- function(mids_obj, var, age_col) {
    long <- mice::complete(mids_obj, action = "long", include = FALSE)

    per_imp <- long |>
        dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[age_col]])) |>
        dplyr::mutate(Age_Year = floor(.data[[age_col]])) |>
        dplyr::group_by(.imp, Age_Year) |>
        dplyr::summarise(
            mean_i  = mean(.data[[var]], na.rm = TRUE),
            sd_i    = dplyr::coalesce(stats::sd(.data[[var]], na.rm = TRUE), 0),
            n_i     = dplyr::n(),
            min_age = min(.data[[age_col]], na.rm = TRUE),
            max_age = max(.data[[age_col]], na.rm = TRUE),
            .groups = "drop"
        )

    per_imp |>
        dplyr::group_by(Age_Year) |>
        dplyr::summarise(
            mean    = mean(mean_i),
            sd      = mean(sd_i, na.rm = TRUE),
            n       = round(mean(n_i)),
            min_age = mean(min_age, na.rm = TRUE),
            max_age = mean(max_age, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::arrange(Age_Year)
}

.summarise_by_age_year <- function(data, var, age_col) {
    if (inherits(data, "mids")) {
        .summarise_mids_by_age_year(data, var, age_col)
    } else {
        .summarise_df_by_age_year(data, var, age_col)
    }
}


# ── Summarisation: quartile-stratified trajectories ───────────────────────────

.summarise_df_by_age_quartile <- function(data, var, age_col,
                                          quartile_col = "dairy_quartile_baseline") {
    data |>
        dplyr::filter(
            !is.na(.data[[var]]),
            !is.na(.data[[age_col]]),
            !is.na(.data[[quartile_col]])
        ) |>
        dplyr::mutate(Age_Year = floor(.data[[age_col]])) |>
        dplyr::group_by(Age_Year, quartile = .data[[quartile_col]]) |>
        dplyr::summarise(
            mean    = mean(.data[[var]], na.rm = TRUE),
            sd      = dplyr::coalesce(stats::sd(.data[[var]], na.rm = TRUE), 0),
            n       = dplyr::n(),
            .groups = "drop"
        ) |>
        dplyr::mutate(quartile = as.character(quartile)) |>
        dplyr::arrange(quartile, Age_Year)
}

.summarise_mids_by_age_quartile <- function(mids_obj, var, age_col,
                                            quartile_col = "dairy_quartile_baseline") {
    long <- mice::complete(mids_obj, action = "long", include = FALSE)

    per_imp <- long |>
        dplyr::filter(
            !is.na(.data[[var]]),
            !is.na(.data[[age_col]]),
            !is.na(.data[[quartile_col]])
        ) |>
        dplyr::mutate(Age_Year = floor(.data[[age_col]])) |>
        dplyr::group_by(.imp, Age_Year, quartile = .data[[quartile_col]]) |>
        dplyr::summarise(
            mean_i = mean(.data[[var]], na.rm = TRUE),
            sd_i   = dplyr::coalesce(stats::sd(.data[[var]], na.rm = TRUE), 0),
            n_i    = dplyr::n(),
            .groups = "drop"
        )

    per_imp |>
        dplyr::group_by(Age_Year, quartile) |>
        dplyr::summarise(
            mean    = mean(mean_i),
            sd      = mean(sd_i, na.rm = TRUE),
            n       = round(mean(n_i)),
            .groups = "drop"
        ) |>
        dplyr::mutate(quartile = as.character(quartile)) |>
        dplyr::arrange(quartile, Age_Year)
}

.summarise_by_age_quartile <- function(data, var, age_col,
                                       quartile_col = "dairy_quartile_baseline") {
    if (inherits(data, "mids")) {
        .summarise_mids_by_age_quartile(data, var, age_col, quartile_col)
    } else {
        .summarise_df_by_age_quartile(data, var, age_col, quartile_col)
    }
}


# ── Plots ──────────────────────────────────────────────────────────────────────

.plot_one_trajectory <- function(summ, var, color) {
    y_upper <- max(summ$mean + summ$sd, na.rm = TRUE)
    y_lower <- min(summ$mean - summ$sd, na.rm = TRUE)
    y_pad   <- (y_upper - y_lower) * 0.18

    ggplot2::ggplot(summ, ggplot2::aes(x = Age_Year, y = mean)) +
        ggplot2::geom_errorbar(
            ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
            width = 0.45, alpha = 0.35, colour = color, linewidth = 0.5
        ) +
        ggplot2::geom_line(colour = color, alpha = 0.75, linewidth = 0.9) +
        ggplot2::geom_point(colour = color, size = 2.8) +
        ggrepel::geom_text_repel(
            ggplot2::aes(
                y     = mean + sd + ifelse(n == 1, y_pad * 0.18, 0),
                label = paste0("n=", n)
            ),
            size = 2.7, colour = "grey40", family = .TRAJ_FONT,
            direction = "y", seed = 42,
            nudge_y = y_pad * 0.04, point.padding = 0.02, box.padding = 0.02,
            min.segment.length = 0
        ) +
        ggplot2::scale_x_continuous(
            breaks       = scales::breaks_width(5),
            minor_breaks = scales::breaks_width(1)
        ) +
        .scale_y_dense() +
        ggplot2::coord_cartesian(ylim = c(y_lower - y_pad * 0.3, y_upper + y_pad)) +
        ggplot2::labs(
            x = "Age [years]",
            y = .traj_axis_label(var)
        ) +
        .theme_traj()
}

.plot_trajectory_by_quartile <- function(summ, var) {
    # Keep only quartile levels that exist in data and appear in .QUARTILE_COLORS
    present_q <- intersect(names(.QUARTILE_COLORS), unique(summ$quartile))
    col_scale  <- .QUARTILE_COLORS[present_q]
    fill_scale <- col_scale

    ggplot2::ggplot(
        summ,
        ggplot2::aes(
            x     = Age_Year,
            y     = mean,
            colour = quartile,
            fill  = quartile,
            group = quartile
        )
    ) +
        ggplot2::geom_ribbon(
            ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
            alpha = 0.12, colour = NA
        ) +
        ggplot2::geom_line(alpha = 0.85, linewidth = 0.9) +
        ggplot2::geom_point(size = 2.4) +
        ggplot2::scale_colour_manual(
            values = col_scale,
            name   = "Dairy quartile"
        ) +
        ggplot2::scale_fill_manual(
            values = fill_scale,
            name   = "Dairy quartile"
        ) +
        ggplot2::scale_x_continuous(
            breaks       = scales::breaks_width(5),
            minor_breaks = scales::breaks_width(1)
        ) +
        .scale_y_dense() +
        ggplot2::labs(
            x = "Age [years]",
            y = .traj_axis_label(var)
        ) +
        .theme_traj() +
        ggplot2::theme(
            legend.position  = "right",
            legend.title     = ggplot2::element_text(size = 10, face = "bold"),
            legend.text      = ggplot2::element_text(size = 10)
        )
}


# ── Main entry point ───────────────────────────────────────────────────────────

#' Plot mean ± SD trajectories by age year for outcomes and dairy exposures,
#' plus outcome trajectories stratified by dairy_quartile_baseline.
#'
#' Works with both CC (plain data frames) and MICE (mids objects). For mids,
#' per-imputation summaries are pooled before plotting.
#'
#' @param analysis      List returned by run_exclusions(). Must have $data
#'   (named list per outcome) and $data_shared. Elements may be plain data
#'   frames (CC) or mids objects (MICE).
#' @param quartile_col  Column name for dairy quartile stratification.
#' @param out_dir       Output directory for PNGs and CSVs.
#' @param width,height,dpi  Plot dimensions.
#' @return Named list with $overall and $by_quartile, each a named list of
#'   ggplot objects (invisibly). Files saved to `out_dir`.
plot_age_trajectories <- function(analysis,
                                  quartile_col = "dairy_quartile_baseline",
                                  out_dir      = "03_outputs/descriptives/age_trajectories",
                                  width        = 9,
                                  height       = 5.5,
                                  dpi          = 180) {

    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    shared <- analysis$data_shared

    # ── 1. Overall trajectories (all variables) ────────────────────────────────

    overall_specs <- list(
        list(var = "HGS_MAX",                         data = analysis$data$HGS_MAX),
        list(var = "ALM_HT2_harmonised",              data = analysis$data$ALM_HT2_harmonised),
        list(var = "gait_speed",                      data = analysis$data$gait_speed),
        list(var = "dairy_total_gday_cumavg",         data = shared),
        list(var = "dairy_fermented_gday_cumavg",     data = shared),
        list(var = "dairy_non_fermented_gday_cumavg", data = shared),
        list(var = "dairy_highfat_gday_cumavg",       data = shared),
        list(var = "dairy_lowfat_gday_cumavg",        data = shared)
    )

    overall_plots <- purrr::map(overall_specs, function(spec) {
        var  <- spec$var
        data <- spec$data

        if (!var %in% .col_names(data)) {
            cli::cli_warn("Skipping {.val {var}}: column not found.")
            return(NULL)
        }
        age_col <- .detect_age_col(data)
        if (is.na(age_col)) {
            cli::cli_warn("Skipping {.val {var}}: no Age column found.")
            return(NULL)
        }

        summ <- .summarise_by_age_year(data, var, age_col)
        if (is.null(summ) || nrow(summ) == 0L) return(NULL)

        summ <- dplyr::mutate(summ, variable = var, label = .traj_label(var)) |>
            dplyr::relocate(variable, label)

        readr::write_csv(summ, file.path(out_dir, paste0("trajectory_", var, ".csv")))

        color <- if (var %in% names(.TRAJ_COLORS)) .TRAJ_COLORS[[var]] else .TRAJ_PALETTE[7]
        plt   <- .plot_one_trajectory(summ, var, color)
        ggplot2::ggsave(file.path(out_dir, paste0("trajectory_", var, ".png")),
                        plt, width = width, height = height, dpi = dpi)
        plt
    }) |> rlang::set_names(purrr::map_chr(overall_specs, "var"))

    overall_plots <- purrr::compact(overall_plots)

    # ── 2. Outcome trajectories by dairy quartile ──────────────────────────────

    outcome_specs <- list(
        list(var = "HGS_MAX",            data = analysis$data$HGS_MAX),
        list(var = "ALM_HT2_harmonised", data = analysis$data$ALM_HT2_harmonised),
        list(var = "gait_speed",         data = analysis$data$gait_speed)
    )

    quartile_plots <- purrr::map(outcome_specs, function(spec) {
        var  <- spec$var
        data <- spec$data

        cols <- .col_names(data)
        if (!var %in% cols) {
            cli::cli_warn("Skipping quartile plot for {.val {var}}: outcome column not found.")
            return(NULL)
        }
        if (!quartile_col %in% cols) {
            cli::cli_warn("Skipping quartile plot for {.val {var}}: {.val {quartile_col}} not found.")
            return(NULL)
        }
        age_col <- .detect_age_col(data)
        if (is.na(age_col)) {
            cli::cli_warn("Skipping quartile plot for {.val {var}}: no Age column found.")
            return(NULL)
        }

        summ <- .summarise_by_age_quartile(data, var, age_col, quartile_col)
        if (is.null(summ) || nrow(summ) == 0L) return(NULL)

        summ <- dplyr::mutate(summ, variable = var, label = .traj_label(var)) |>
            dplyr::relocate(variable, label)

        readr::write_csv(summ,
            file.path(out_dir, paste0("trajectory_by_quartile_", var, ".csv")))

        plt <- .plot_trajectory_by_quartile(summ, var)
        ggplot2::ggsave(
            file.path(out_dir, paste0("trajectory_by_quartile_", var, ".png")),
            plt, width = width + 1.5, height = height, dpi = dpi
        )
        plt
    }) |> rlang::set_names(purrr::map_chr(outcome_specs, "var"))

    quartile_plots <- purrr::compact(quartile_plots)

    cli::cli_inform(c(
        "v" = "Age trajectories: {length(overall_plots)} overall + {length(quartile_plots)} by-quartile",
        "i" = "Output: {.file {out_dir}}"
    ))

    invisible(list(overall = overall_plots, by_quartile = quartile_plots))
}
