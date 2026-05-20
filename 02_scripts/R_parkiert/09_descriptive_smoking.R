# =============================================================================
# R/09_descriptive_smoking.R
# =============================================================================
# Longitudinal smoking behaviour analysis.
# Input: analysis_long — output of freeze_dataset().
#
# Functions
# ---------
#   make_smoking_prevalence_table()   Cross-tabulation of smoking status
#                                     per wave (n and %)
#   make_smoking_transition_matrix()  Wave-to-wave transition matrices
#                                     (% moving between statuses)
#   make_smoking_trajectory_plot()    Alluvial plot of individual trajectories
#                                     across all waves
#   make_smoking_sankey_consecutive() Per-pair stacked bar showing change vs
#                                     no change between consecutive waves
#   make_smoking_change_summary()     Tibble: who changed, how many times,
#                                     in which direction
#   make_smoking_change_plot_detail() Faceted bar chart: direction of change
#                                     (quit / relapsed / never changed) per
#                                     participant trajectory group
#   make_smoking_stability_table()    Summary table of trajectory stability:
#                                     always same / changed once / changed 2+
#
# =============================================================================


# Ordered factor levels used throughout
.SMK_LEVELS  <- c("Never", "Former", "Current")
.SMK_COLOURS <- c(
    "Never"   = "#4D9BE6",
    "Former"  = "#F4A261",
    "Current" = "#E84855"
)
.CHANGE_COLOURS <- c(
    "Quit"              = "#2D6A4F",
    "Relapsed"          = "#E84855",
    "Status unknown"    = "#AAAAAA"
)


# =============================================================================
# PRIVATE HELPERS
# =============================================================================

#' Extract tidy longitudinal smoking data (one row per pt × wave).
#' Drops rows with missing smoking_status and ensures factor ordering.
.smk_long <- function(analysis_long) {
    analysis_long |>
        dplyr::filter(!is.na(smoking_status)) |>
        dplyr::mutate(
            smoking_status      = factor(smoking_status, levels = .SMK_LEVELS),
            osteo_wave = forcats::fct_reorder(osteo_wave, osteo_wave_num)
        ) |>
        dplyr::select(pt, osteo_wave, osteo_wave_num, smoking_status)
}

#' Build all consecutive-wave pairs for a given dataset.
#' Returns a tibble with pt, wave_from, wave_to, smk_from, smk_to, changed.
.smk_pairs <- function(smk) {
    waves <- smk |>
        dplyr::distinct(osteo_wave, osteo_wave_num) |>
        dplyr::arrange(osteo_wave_num) |>
        dplyr::pull(osteo_wave)
    
    if (length(waves) < 2L) {
        return(tibble::tibble(
            pt = integer(), wave_from = character(), wave_to = character(),
            smk_from = character(), smk_to = character(), changed = logical()
        ))
    }
    
    purrr::map_dfr(seq_len(length(waves) - 1L), function(i) {
        w1 <- waves[i]
        w2 <- waves[i + 1L]
        
        d1 <- smk |>
            dplyr::filter(osteo_wave == w1) |>
            dplyr::select(pt, smk_from = smoking_status)
        d2 <- smk |>
            dplyr::filter(osteo_wave == w2) |>
            dplyr::select(pt, smk_to = smoking_status)
        
        d1 |>
            dplyr::inner_join(d2, by = "pt") |>
            dplyr::mutate(
                wave_from = as.character(w1),
                wave_to   = as.character(w2),
                changed   = as.character(smk_from) != as.character(smk_to)
            )
    })
}


# =============================================================================
# 1. Prevalence table
# =============================================================================

#' Cross-tabulation of smoking status per wave.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return Wide tibble: rows = smoking status, columns = wave (n + %).
make_smoking_prevalence_table <- function(analysis_long) {
    
    smk <- .smk_long(analysis_long)
    
    wave_totals <- smk |>
        dplyr::count(osteo_wave, name = "n_wave")
    
    smk |>
        dplyr::count(osteo_wave, smoking_status) |>
        dplyr::left_join(wave_totals, by = "osteo_wave") |>
        dplyr::mutate(
            pct   = round(n / n_wave * 100, 1),
            label = paste0(n, " (", pct, "%)")
        ) |>
        dplyr::select(osteo_wave, smoking_status, label) |>
        tidyr::pivot_wider(
            names_from  = osteo_wave,
            values_from = label,
            values_fill = "0 (0.0%)"
        ) |>
        dplyr::rename(`Smoking status` = smoking_status)
}


# =============================================================================
# 2. Transition matrices
# =============================================================================

#' Wave-to-wave transition matrices (row % of participants moving between
#' statuses between each consecutive pair of waves).
#'
#' @param analysis_long Output of freeze_dataset().
#' @return Named list of tibbles, one per consecutive wave pair.
make_smoking_transition_matrix <- function(analysis_long) {
    
    smk   <- .smk_long(analysis_long)
    pairs <- .smk_pairs(smk)
    
    if (nrow(pairs) == 0L) return(list())
    
    wave_pair_labels <- pairs |>
        dplyr::distinct(wave_from, wave_to) |>
        dplyr::mutate(
            pair_label = paste0(wave_from, " \u2192 ", wave_to)
        )
    
    purrr::map(
        seq_len(nrow(wave_pair_labels)),
        function(i) {
            wf <- wave_pair_labels$wave_from[i]
            wt <- wave_pair_labels$wave_to[i]
            lb <- wave_pair_labels$pair_label[i]
            
            sub <- pairs |>
                dplyr::filter(wave_from == wf, wave_to == wt)
            
            mat <- sub |>
                dplyr::count(smk_from, smk_to) |>
                dplyr::group_by(smk_from) |>
                dplyr::mutate(
                    row_total = sum(n),
                    pct       = round(n / row_total * 100, 1),
                    cell      = paste0(n, " (", pct, "%)")
                ) |>
                dplyr::ungroup() |>
                dplyr::select(smk_from, smk_to, cell) |>
                tidyr::pivot_wider(
                    names_from  = smk_to,
                    values_from = cell,
                    values_fill = "0 (0.0%)"
                ) |>
                dplyr::rename(`Status at {wf}` := smk_from)
            
            list(pair = lb, matrix = mat)
        }
    ) |>
        purrr::set_names(wave_pair_labels$pair_label)
}


# =============================================================================
# 3. Alluvial trajectory plot
# =============================================================================

#' Alluvial plot of individual smoking trajectories across all waves.
#'
#' Each participant is one flow. Flows are coloured by their Baseline status.
#' Participants missing smoking_status at any wave are shown as a separate "Missing"
#' stratum so the full sample is visible.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A ggplot object.
make_smoking_trajectory_plot <- function(analysis_long) {
    
    # Include all participants; fill missing with explicit NA level
    traj <- analysis_long |>
        dplyr::mutate(
            smk_status = dplyr::if_else(
                is.na(smoking_status), "Missing",
                as.character(smoking_status)
            ),
            smk_status = factor(
                smk_status,
                levels = c(.SMK_LEVELS, "Missing")
            ),
            osteo_wave = forcats::fct_reorder(osteo_wave, osteo_wave_num)
        ) |>
        dplyr::select(pt, osteo_wave, osteo_wave_num, smk_status)
    
    colours_extended <- c(
        .SMK_COLOURS,
        "Missing" = "#DDDDDD"
    )
    
    ggplot2::ggplot(
        traj,
        ggplot2::aes(
            x        = osteo_wave,
            stratum  = smk_status,
            alluvium = pt,
            fill     = smk_status,
            label    = smk_status
        )
    ) +
        ggalluvial::geom_flow(
            stat      = "alluvium",
            aes.bind  = "flows",
            alpha     = 0.4,
            colour    = "white",
            linewidth = 0.1
        ) +
        ggalluvial::geom_stratum(
            width     = 0.45,
            colour    = "white",
            linewidth = 0.3
        ) +
        ggplot2::scale_fill_manual(
            values = colours_extended,
            name   = "Smoking status"
        ) +
        ggplot2::scale_x_discrete(expand = c(0.04, 0.04)) +
        ggplot2::labs(
            title    = "Individual smoking trajectories across OsteoLaus waves",
            subtitle = "Each band represents participants sharing the same transition path",
            x        = NULL,
            y        = "Number of participants",
            caption  = "Grey = missing status at that wave"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "right")
}


# =============================================================================
# 4. Consecutive-pair change plot
# =============================================================================

#' Stacked bar showing proportion changed / unchanged for each consecutive
#' wave pair, with absolute counts labelled inside bars.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A ggplot object.
make_smoking_sankey_consecutive <- function(analysis_long) {
    
    smk   <- .smk_long(analysis_long)
    pairs <- .smk_pairs(smk)
    
    if (nrow(pairs) == 0L) {
        return(
            ggplot2::ggplot() +
                ggplot2::annotate("text", x = 1, y = 1,
                                  label = "Insufficient wave data") +
                ggplot2::theme_void()
        )
    }
    
    plot_data <- pairs |>
        dplyr::mutate(
            wave_pair = paste0(wave_from, " \u2192 ", wave_to),
            wave_pair = factor(wave_pair, levels = unique(wave_pair)),
            status    = dplyr::if_else(changed, "Changed status", "No change")
        ) |>
        dplyr::count(wave_pair, status) |>
        dplyr::group_by(wave_pair) |>
        dplyr::mutate(
            total = sum(n),
            pct   = round(n / total * 100, 1)
        ) |>
        dplyr::ungroup()
    
    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = wave_pair, y = n, fill = status)
    ) +
        ggplot2::geom_col(
            position  = "stack",
            colour    = "white",
            linewidth = 0.3
        ) +
        ggplot2::geom_text(
            ggplot2::aes(
                label = paste0(n, "\n(", pct, "%)")
            ),
            position  = ggplot2::position_stack(vjust = 0.5),
            size      = 3,
            colour    = "white",
            fontface  = "bold",
            lineheight = 0.9
        ) +
        ggplot2::scale_fill_manual(
            values = c(
                "No change"      = "#4D9BE6",
                "Changed status" = "#E84855"
            ),
            name = NULL
        ) +
        ggplot2::labs(
            title    = "Smoking status stability between consecutive waves",
            subtitle = "Participants observed at both waves with non-missing status",
            x        = NULL,
            y        = "n participants"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            axis.text.x     = ggplot2::element_text(angle = 25, hjust = 1),
            legend.position = "right"
        )
}


# =============================================================================
# 5. Change summary tibble
# =============================================================================

#' Participant-level summary of smoking trajectory.
#'
#' For each participant who has >= 2 non-missing smoking observations,
#' returns: number of waves observed, number of status changes, direction
#' (ever quit, ever relapsed, always same), and a compact trajectory string
#' such as "Never-Never-Former-Former-Former".
#'
#' @param analysis_long Output of freeze_dataset().
#' @return Tibble with one row per participant who has >= 2 observations.
make_smoking_change_summary <- function(analysis_long) {
    
    smk <- .smk_long(analysis_long)
    
    smk |>
        dplyr::arrange(pt, osteo_wave_num) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            n_waves_observed = dplyr::n(),
            trajectory       = paste(as.character(smoking_status), collapse = "-"),
            n_changes        = sum(
                as.character(smoking_status) != dplyr::lag(as.character(smoking_status)),
                na.rm = TRUE
            ),
            ever_quit = any(
                dplyr::lag(as.character(smoking_status)) == "Current" &
                    as.character(smoking_status) %in% c("Former", "Never"),
                na.rm = TRUE
            ),
            ever_relapsed = any(
                dplyr::lag(as.character(smoking_status)) %in% c("Former", "Never") &
                    as.character(smoking_status) == "Current",
                na.rm = TRUE
            ),
            .groups = "drop"
        ) |>
        dplyr::filter(n_waves_observed >= 2L) |>
        dplyr::mutate(
            trajectory_group = dplyr::case_when(
                n_changes == 0                    ~ "Always same status",
                ever_quit   & !ever_relapsed      ~ "Quit only",
                ever_relapsed & !ever_quit        ~ "Relapsed only",
                ever_quit   &  ever_relapsed      ~ "Quit and relapsed",
                TRUE                              ~ "Changed (direction unclear)"
            )
        )
}


# =============================================================================
# 6. Direction-of-change plot
# =============================================================================

#' Bar chart showing trajectory group counts and proportions.
#'
#' Bars are split by the most common starting status (Never / Former /
#' Current) and faceted or stacked so the reader can see both the
#' absolute count and the proportion within each starting group.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A patchwork object (two panels).
make_smoking_change_plot_detail <- function(analysis_long) {
    
    change_summary <- make_smoking_change_summary(analysis_long)
    
    # Join starting status (Baseline)
    bsl_smk <- analysis_long |>
        dplyr::filter(osteo_wave == "Baseline", !is.na(smoking_status)) |>
        dplyr::select(pt, start_status = smoking_status) |>
        dplyr::mutate(
            start_status = factor(as.character(start_status),
                                  levels = .SMK_LEVELS)
        )
    
    plot_data <- change_summary |>
        dplyr::left_join(bsl_smk, by = "pt") |>
        dplyr::filter(!is.na(start_status)) |>
        dplyr::count(start_status, trajectory_group) |>
        dplyr::group_by(start_status) |>
        dplyr::mutate(
            pct = round(n / sum(n) * 100, 1)
        ) |>
        dplyr::ungroup()
    
    group_colours <- c(
        "Always same status"           = "#4D9BE6",
        "Quit only"                    = "#2D6A4F",
        "Relapsed only"                = "#E84855",
        "Quit and relapsed"            = "#F4A261",
        "Changed (direction unclear)"  = "#AAAAAA"
    )
    
    # Panel A — absolute counts
    p_count <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
            x    = start_status,
            y    = n,
            fill = trajectory_group
        )
    ) +
        ggplot2::geom_col(
            position  = "stack",
            colour    = "white",
            linewidth = 0.3
        ) +
        ggplot2::scale_fill_manual(values = group_colours, name = "Trajectory") +
        ggplot2::labs(
            title = "Smoking trajectory by baseline status",
            x     = "Baseline smoking status",
            y     = "n participants"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "right")
    
    # Panel B — proportion within each starting status
    p_pct <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
            x    = start_status,
            y    = pct,
            fill = trajectory_group
        )
    ) +
        ggplot2::geom_col(
            position  = "fill",
            colour    = "white",
            linewidth = 0.3
        ) +
        ggplot2::scale_y_continuous(
            labels = scales::label_percent(scale = 100)
        ) +
        ggplot2::scale_fill_manual(values = group_colours, name = "Trajectory") +
        ggplot2::labs(
            x = "Baseline smoking status",
            y = "% of participants"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "right")
    
    patchwork::wrap_plots(p_count, p_pct, ncol = 2) +
        patchwork::plot_layout(guides = "collect") &
        ggplot2::theme(legend.position = "bottom")
}


# =============================================================================
# 7. Stability summary table
# =============================================================================

#' Summary table of trajectory stability across all waves.
#'
#' Rows: trajectory group (always same / changed once / changed 2+).
#' Columns: n, %, breakdown by baseline status.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A gtsummary tbl_summary object.
make_smoking_stability_table <- function(analysis_long) {
    
    change_summary <- make_smoking_change_summary(analysis_long)
    
    bsl_smk <- analysis_long |>
        dplyr::filter(osteo_wave == "Baseline", !is.na(smoking_status)) |>
        dplyr::select(pt, baseline_smoking = smoking_status) |>
        dplyr::mutate(
            baseline_smoking = factor(
                as.character(baseline_smoking), levels = .SMK_LEVELS
            )
        )
    
    tbl_data <- change_summary |>
        dplyr::left_join(bsl_smk, by = "pt") |>
        dplyr::mutate(
            stability = dplyr::case_when(
                n_changes == 0 ~ "No change across waves",
                n_changes == 1 ~ "Changed once",
                n_changes >= 2 ~ "Changed \u22652 times"
            ),
            stability = factor(
                stability,
                levels = c(
                    "No change across waves",
                    "Changed once",
                    "Changed \u22652 times"
                )
            )
        ) |>
        dplyr::select(stability, trajectory_group, baseline_smoking,
                      n_waves_observed)
    
    tbl_data |>
        gtsummary::tbl_summary(
            by        = stability,
            label     = list(
                trajectory_group  ~ "Trajectory type",
                baseline_smoking  ~ "Baseline smoking status",
                n_waves_observed  ~ "Waves with smoking data (n)"
            ),
            statistic = list(
                gtsummary::all_continuous()  ~ "{median} ({p25}, {p75})",
                gtsummary::all_categorical() ~ "{n} ({p}%)"
            ),
            missing = "ifany"
        ) |>
        gtsummary::add_overall(last = FALSE) |>
        gtsummary::add_p(
            test = list(
                gtsummary::all_continuous()  ~ "kruskal.test",
                gtsummary::all_categorical() ~ "chisq.test"
            ),
            pvalue_fun = gtsummary::style_pvalue
        ) |>
        gtsummary::bold_labels() |>
        gtsummary::bold_p(t = 0.05) |>
        gtsummary::modify_caption(
            "**Table S.** Smoking trajectory stability across OsteoLaus waves"
        )
}


# =============================================================================
# 8. Heat-map of wave-pair transitions
# =============================================================================

#' Tile heat-map showing the proportion of participants transitioning between
#' each pair of smoking statuses, faceted by consecutive wave pair.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A ggplot object.
make_smoking_transition_heatmap <- function(analysis_long) {
    
    smk   <- .smk_long(analysis_long)
    pairs <- .smk_pairs(smk)
    
    if (nrow(pairs) == 0L) {
        return(
            ggplot2::ggplot() +
                ggplot2::annotate("text", x = 1, y = 1,
                                  label = "Insufficient wave data") +
                ggplot2::theme_void()
        )
    }
    
    tile_data <- pairs |>
        dplyr::mutate(
            wave_pair = paste0(wave_from, "\u2192", wave_to),
            wave_pair = factor(wave_pair, levels = unique(wave_pair))
        ) |>
        dplyr::count(wave_pair, smk_from, smk_to) |>
        dplyr::group_by(wave_pair, smk_from) |>
        dplyr::mutate(
            row_pct = round(n / sum(n) * 100, 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
            smk_from = factor(as.character(smk_from), levels = .SMK_LEVELS),
            smk_to   = factor(as.character(smk_to),   levels = .SMK_LEVELS)
        )
    
    ggplot2::ggplot(
        tile_data,
        ggplot2::aes(x = smk_to, y = smk_from, fill = row_pct)
    ) +
        ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
        ggplot2::geom_text(
            ggplot2::aes(
                label = paste0(n, "\n(", row_pct, "%)")
            ),
            size       = 2.8,
            lineheight = 0.9,
            colour     = "grey20"
        ) +
        ggplot2::facet_wrap(~ wave_pair, ncol = 2) +
        ggplot2::scale_fill_gradient(
            low  = "#F7FBFF",
            high = "#2171B5",
            name = "Row %\n(from status)"
        ) +
        ggplot2::scale_x_discrete(position = "top") +
        ggplot2::labs(
            title    = "Smoking status transition heat-map",
            subtitle = "Row % = proportion transitioning from row status to column status",
            x        = "Status at next wave",
            y        = "Status at current wave"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            panel.grid   = ggplot2::element_blank(),
            strip.text   = ggplot2::element_text(face = "bold"),
            axis.text.x  = ggplot2::element_text(face = "bold"),
            axis.text.y  = ggplot2::element_text(face = "bold"),
            legend.position = "right"
        )
}