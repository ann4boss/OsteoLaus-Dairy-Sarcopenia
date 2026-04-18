# =============================================================================
# R/09_descriptive.R
# =============================================================================
# Descriptive analysis functions.
# Input: analysis_long — output of freeze_dataset().
#
# Functions
# ---------
#   make_table_one()              Baseline characteristics (overall)
#   make_table_one_by_quartile()  Baseline characteristics stratified by
#                                 baseline dairy quartile, with p-values,
#                                 SEM, and 95% CI for continuous variables
#   make_missing_summary()        % missing per variable × wave (wide table;
#                                 column headers show wave + n)
#   make_wave_summary()           Completeness per wave
#   make_exposure_plots()         Dairy distribution histograms + boxplots
#   make_outcome_plots()          EWGSOP2 stage prevalence by wave
#   make_cumavg_trajectory_plot() Cumulative dairy trajectory with CI ribbon
#   make_dairy_quartile_flow()    Alluvial/sankey plot of quartile transitions
#                                 across waves
#   make_smoking_change_plot()    Bar chart of smoking status changes
#
# Note on gait speed
# ------------------
#   Gait speed is first measured at V4, not at OsteoLaus Baseline.
#   All table functions therefore source gait_speed from V4 (each
#   participant's earliest available V4 value) rather than Baseline.
#   The variable label reflects this.
#
# Loaded by tar_source() in _targets.R — no direct source() calls needed.
# =============================================================================

# Shared label list used by both table functions.
# gait_speed label notes that V4 is used as the first available time point.
.TABLE_LABELS <- list(
    baseline_osteo_age    ~ "Age at OsteoLaus Baseline (yr)",
    education_level       ~ "Education level (ISCED)",
    baseline_bmi          ~ "BMI at OsteoLaus Baseline (kg/m\u00b2)",
    BMI_category          ~ "BMI category",
    sbsmk                 ~ "Smoking status",
    alcohol_category      ~ "Alcohol intake",
    pa_levels             ~ "Physical activity level",
    diabetes_status       ~ "Diabetes status",
    HTN_status            ~ "Hypertension",
    hrt_status            ~ "HRT status",
    cdv_event             ~ "CVD event (any)",
    hypolip_drug_status   ~ "Lipid-lowering medication",
    corticoids_status     ~ "Systemic corticosteroids",
    calcium_status        ~ "Calcium supplement",
    vitD_status           ~ "Vitamin D supplement",
    bisphosphonate_status ~ "Bisphosphonate use",
    dairy_total_gday      ~ "Total dairy intake (g/day)",
    dairy_fermented_gday  ~ "Fermented dairy (g/day)",
    dairy_non_fermented_gday ~ "Non-fermented dairy (g/day)",
    dairy_lowfat_gday     ~ "Low-fat dairy (g/day)",
    dairy_highfat_gday    ~ "High-fat dairy (g/day)",
    ewgsop2_sarcopenia_stage ~ "EWGSOP2 sarcopenia stage",
    handgrip_max_all      ~ "Grip strength (kg)",
    ALM_HT2               ~ "ALMI (kg/m\u00b2)",
    gait_speed            ~ "Gait speed at V4 (m/s)"
)

.TABLE_VARS <- c(
    "baseline_osteo_age", "education_level", "baseline_bmi", "BMI_category",
    "sbsmk", "alcohol_category", "pa_levels",
    "diabetes_status", "HTN_status", "hrt_status", "cdv_event",
    "hypolip_drug_status", "corticoids_status", "calcium_status",
    "vitD_status", "bisphosphonate_status",
    "dairy_total_gday", "dairy_fermented_gday", "dairy_non_fermented_gday",
    "dairy_lowfat_gday", "dairy_highfat_gday",
    "ewgsop2_sarcopenia_stage", "handgrip_max_all", "ALM_HT2", "gait_speed"
)


# =============================================================================
# PRIVATE HELPER — display-baseline dataset
# =============================================================================

#' Build a single-row-per-participant "display Baseline" tibble.
#'
#' All variables come from the OsteoLaus Baseline wave **except** gait_speed,
#' which is sourced from V4 (the first wave at which it is measured).  This
#' prevents all-NA columns in Table 1 and avoids the gtsummary "not enough
#' groups" error when gait speed is included.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return One-row-per-participant tibble ready for tbl_summary().
.make_display_baseline <- function(analysis_long) {
    
    bsl <- analysis_long |>
        dplyr::filter(osteo_wave == "Baseline")
    
    # Earliest non-missing V4 gait speed per participant
    v4_gait <- analysis_long |>
        dplyr::filter(osteo_wave == "V4", !is.na(gait_speed)) |>
        dplyr::distinct(pt, .keep_all = FALSE) |>
        dplyr::left_join(
            dplyr::select(
                dplyr::filter(analysis_long, osteo_wave == "V4"),
                pt, gait_speed
            ),
            by = "pt"
        ) |>
        dplyr::select(pt, gait_speed)
    
    # Replace Baseline gait_speed (always NA) with V4 value
    bsl |>
        dplyr::select(-dplyr::any_of("gait_speed")) |>
        dplyr::left_join(v4_gait, by = "pt")
}


# =============================================================================
# Table 1 — Baseline characteristics (overall)
# =============================================================================

#' Baseline characteristics for all hard-included participants.
#'
#' gait_speed is sourced from V4 (first wave it is measured).
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A gtsummary tbl_summary object.
make_table_one <- function(analysis_long) {
    .make_display_baseline(analysis_long) |>
        dplyr::select(dplyr::any_of(.TABLE_VARS)) |>
        gtsummary::tbl_summary(
            label     = .TABLE_LABELS,
            statistic = list(
                gtsummary::all_continuous()  ~ "{mean} ({sd})",
                gtsummary::all_categorical() ~ "{n} ({p}%)"
            ),
            digits  = list(gtsummary::all_continuous() ~ 1),
            missing = "always",
            missing_text = "Missing"
        ) |>
        gtsummary::add_n() |>
        gtsummary::bold_labels()
}


# =============================================================================
# Table 1 stratified by baseline dairy intake quartile
# =============================================================================

#' Baseline characteristics by baseline dairy quartile (Q1–Q4).
#'
#' One column per quartile + Overall column. Continuous variables reported as
#' mean (SD); categorical as n (%). P-values from one-way ANOVA (continuous)
#' or chi-squared (categorical). Missing counts shown for every variable.
#'
#' gait_speed is sourced from V4 (first wave it is measured).
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A gtsummary tbl_summary object.
make_table_one_by_quartile <- function(analysis_long) {
    
    bsl <- .make_display_baseline(analysis_long) |>
        dplyr::select(dplyr::any_of(c(.TABLE_VARS, "baseline_dairy_quartile"))) 
    
    if (!"baseline_dairy_quartile" %in% names(bsl) ||
        all(is.na(bsl$baseline_dairy_quartile))) {
        cli::cli_warn(
            "make_table_one_by_quartile(): baseline_dairy_quartile absent or all NA — returning overall Table 1 instead."
        )
        return(make_table_one(analysis_long))
    }
    
    tbl <- bsl |>
        gtsummary::tbl_summary(
            by        = baseline_dairy_quartile,
            label     = .TABLE_LABELS,
            statistic = list(
                gtsummary::all_continuous()  ~ "{mean} ({sd}) {median} [{p25}, {p75}]",
                gtsummary::all_categorical() ~ "{n} ({p}%)"
            ),
            digits  = list(gtsummary::all_continuous() ~ 1),
            missing = "ifany",
            missing_text = "Missing"
        ) |>
        gtsummary::add_overall(last = FALSE) |>
        gtsummary::add_p(
            test = list(
                gtsummary::all_continuous()  ~ "oneway.test",
                gtsummary::all_categorical() ~ "chisq.test"
            ),
            pvalue_fun = gtsummary::style_pvalue
        ) |>
        gtsummary::add_n() |>
        gtsummary::bold_labels() |>
        gtsummary::bold_p(t = 0.05)
    
    
    
    # ---- FINAL FORMATTING ----
    tbl |>
        gtsummary::modify_caption(
            "**Table 1.** Baseline characteristics by dairy intake quartile"
        ) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~
                "Mean (SD) for continuous; n (%) for categorical. \
         Q1 = lowest dairy intake, Q4 = highest."
        )
}


# =============================================================================
# Missing data summary
# =============================================================================

#' Per-wave missing data summary.
#'
#' Rows = variables (listed once). Columns = OsteoLaus waves.
#' Column headers include wave label AND n at that wave.
#' Cells show % missing (rounded to 1 decimal place).
#'
#' @param analysis_long Output of freeze_dataset().
#' @return Wide tibble ready for gt::gt() or knitr::kable().
make_missing_summary <- function(analysis_long) {
    
    key_vars <- c(
        "baseline_osteo_age", "education_level", "BMI", "BMI_category",
        "sbsmk", "alcohol_category", "pa_levels",
        "diabetes_status", "HTN_status", "hrt_status", "cdv_event",
        "dairy_total_gday", "dairy_fermented_gday", "dairy_non_fermented_gday",
        "dairy_lowfat_gday", "dairy_highfat_gday", "dairy_cumavg",
        "ewgsop2_sarcopenia_stage",
        "handgrip_max_all", "ALM_HT2", "gait_speed", "fnih_sarcopenia"
    )
    
    present <- intersect(key_vars, names(analysis_long))
    
    # Per-wave n (used in column headers)
    wave_n <- analysis_long |>
        dplyr::group_by(osteo_wave, osteo_wave_num) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::arrange(osteo_wave_num)
    
    # % missing per variable per wave — long format
    miss_long <- analysis_long |>
        dplyr::group_by(osteo_wave, osteo_wave_num) |>
        dplyr::summarise(
            dplyr::across(
                dplyr::all_of(present),
                ~ round(mean(is.na(.x)) * 100, 1)
            ),
            .groups = "drop"
        ) |>
        dplyr::arrange(osteo_wave_num) |>
        tidyr::pivot_longer(
            cols      = dplyr::all_of(present),
            names_to  = "variable",
            values_to = "pct_missing"
        ) |>
        # Build column label: "Baseline\n(n=1 234)"
        dplyr::left_join(wave_n, by = c("osteo_wave", "osteo_wave_num")) |>
        dplyr::mutate(
            wave_label = glue::glue("{osteo_wave}\n(n\u00a0=\u00a0{scales::comma(n)})")
        )
    
    # Pivot to wide — one row per variable, one column per wave
    miss_long |>
        dplyr::select(variable, wave_label, pct_missing) |>
        tidyr::pivot_wider(
            names_from  = wave_label,
            values_from = pct_missing
        ) |>
        # Restore the original row ordering
        dplyr::mutate(
            variable = factor(variable, levels = present)
        ) |>
        dplyr::arrange(variable) |>
        dplyr::mutate(variable = as.character(variable))
}


# =============================================================================
# Wave summary table
# =============================================================================

#' Number of participants and outcome/FFQ completeness per OsteoLaus wave.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return Tibble with one row per wave, ordered by wave number.
make_wave_summary <- function(analysis_long) {
    
    analysis_long |>
        dplyr::group_by(osteo_wave, osteo_wave_num) |>
        dplyr::summarise(
            n_participants       = dplyr::n_distinct(pt),
            n_with_stage         = sum(!is.na(ewgsop2_sarcopenia_stage)),
            pct_outcome_complete = round(
                mean(!is.na(ewgsop2_sarcopenia_stage)) * 100, 1
            ),
            pct_grip_complete    = round(mean(!is.na(handgrip_max_all)) * 100, 1),
            pct_alm_complete     = round(mean(!is.na(ALM_HT2))          * 100, 1),
            pct_gait_complete    = round(mean(!is.na(gait_speed))        * 100, 1),
            pct_ffq_complete     = round(mean(!is.na(dairy_total_gday))  * 100, 1),
            median_dairy_gday    = round(median(dairy_total_gday, na.rm = TRUE), 1),
            .groups = "drop"
        ) |>
        dplyr::arrange(osteo_wave_num) |>
        dplyr::select(-osteo_wave_num)
}


# =============================================================================
# Exposure plots
# =============================================================================

#' Distribution plots for dairy exposure variables.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A patchwork plot object.
make_exposure_plots <- function(analysis_long) {
    
    dairy_cols   <- c(
        "dairy_total_gday", "dairy_fermented_gday", "dairy_non_fermented_gday",
        "dairy_lowfat_gday", "dairy_highfat_gday"
    )
    dairy_labels <- c(
        dairy_total_gday         = "Total dairy",
        dairy_fermented_gday     = "Fermented",
        dairy_non_fermented_gday = "Non-fermented",
        dairy_lowfat_gday        = "Low-fat",
        dairy_highfat_gday       = "High-fat"
    )
    
    ffq_data <- analysis_long |>
        dplyr::filter(!is.na(dairy_total_gday)) |>
        tidyr::pivot_longer(
            cols      = dplyr::any_of(dairy_cols),
            names_to  = "exposure",
            values_to = "grams_day"
        ) |>
        dplyr::mutate(
            exposure = factor(
                dplyr::recode(exposure, !!!dairy_labels),
                levels = dairy_labels
            )
        )
    
    p_hist <- ggplot2::ggplot(ffq_data, ggplot2::aes(x = grams_day)) +
        ggplot2::geom_histogram(
            bins = 40, fill = "#2E86AB", colour = "white", linewidth = 0.2
        ) +
        ggplot2::facet_wrap(~ exposure, scales = "free", ncol = 2) +
        ggplot2::labs(
            title   = "Distribution of dairy intake (g/day)",
            x       = "g/day", y = "Count",
            caption = "All OsteoLaus waves with FFQ data"
        ) +
        ggplot2::theme_minimal(base_size = 12)
    
    p_box <- ggplot2::ggplot(
        ffq_data, ggplot2::aes(x = exposure, y = grams_day, fill = exposure)
    ) +
        ggplot2::geom_boxplot(
            show.legend = FALSE, outlier.size = 0.5, alpha = 0.8
        ) +
        ggplot2::scale_fill_brewer(palette = "Set2") +
        ggplot2::labs(
            title = "Dairy intake by sub-category", x = NULL, y = "g/day"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
        )
    
    patchwork::wrap_plots(p_hist, p_box, ncol = 1)
}


# =============================================================================
# Outcome plots
# =============================================================================

#' EWGSOP2 sarcopenia stage prevalence by OsteoLaus wave.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A ggplot object.
make_outcome_plots <- function(analysis_long) {
    
    stage_data <- analysis_long |>
        dplyr::filter(!is.na(ewgsop2_sarcopenia_stage)) |>
        dplyr::count(osteo_wave, osteo_wave_num, ewgsop2_sarcopenia_stage) |>
        dplyr::group_by(osteo_wave) |>
        dplyr::mutate(pct = n / sum(n) * 100) |>
        dplyr::ungroup() |>
        dplyr::mutate(
            osteo_wave = forcats::fct_reorder(osteo_wave, osteo_wave_num)
        )
    
    ggplot2::ggplot(
        stage_data,
        ggplot2::aes(x = osteo_wave, y = pct, fill = ewgsop2_sarcopenia_stage)
    ) +
        ggplot2::geom_col(
            position = "stack", colour = "white", linewidth = 0.3
        ) +
        ggplot2::scale_fill_manual(
            values = c(
                "No sarcopenia" = "#2D6A4F",
                "Probable"      = "#F4A261",
                "Confirmed"     = "#E76F51",
                "Severe"        = "#E84855"
            ),
            name = "EWGSOP2 stage"
        ) +
        ggplot2::scale_y_continuous(
            labels = scales::label_percent(scale = 1)
        ) +
        ggplot2::labs(
            title   = "EWGSOP2 sarcopenia stage prevalence by wave",
            x       = NULL, y = "% of participants",
            caption = "OsteoLaus participants with non-missing stage"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "right")
}


# =============================================================================
# Cumulative dairy trajectory
# =============================================================================

#' Mean cumulative dairy intake (dairy_cumavg) by wave with 95% CI ribbon.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A ggplot object.
make_cumavg_trajectory_plot <- function(analysis_long) {
    
    traj <- analysis_long |>
        dplyr::filter(!is.na(dairy_cumavg)) |>
        dplyr::group_by(osteo_wave, osteo_wave_num) |>
        dplyr::summarise(
            mean_cumavg = mean(dairy_cumavg, na.rm = TRUE),
            se          = sd(dairy_cumavg,   na.rm = TRUE) /
                sqrt(sum(!is.na(dairy_cumavg))),
            n           = sum(!is.na(dairy_cumavg)),
            .groups     = "drop"
        ) |>
        dplyr::mutate(
            lo95 = mean_cumavg - 1.96 * se,
            hi95 = mean_cumavg + 1.96 * se,
            osteo_wave = forcats::fct_reorder(osteo_wave, osteo_wave_num)
        )
    
    ggplot2::ggplot(traj, ggplot2::aes(x = osteo_wave, y = mean_cumavg,
                                       group = 1)) +
        ggplot2::geom_ribbon(
            ggplot2::aes(ymin = lo95, ymax = hi95),
            fill = "#2E86AB", alpha = 0.2
        ) +
        ggplot2::geom_line(colour = "#2E86AB", linewidth = 0.8) +
        ggplot2::geom_point(colour = "#2E86AB", size = 2) +
        ggplot2::labs(
            title   = "Cumulative mean dairy intake by OsteoLaus wave",
            x       = NULL,
            y       = "Cumulative mean dairy intake (g/day)",
            caption = "Mean \u00b1 95% CI"
        ) +
        ggplot2::theme_minimal(base_size = 12)
}


# =============================================================================
# Dairy quartile flow — transition plot
# =============================================================================

#' Visualise how participants move between dairy intake quartiles across waves.
#'
#' Uses a Sankey / alluvial diagram (ggalluvial). Each participant is assigned
#' a time-varying dairy quartile computed from their instantaneous
#' dairy_total_gday value, cut at the Baseline population quartile boundaries.
#' Flows show how many participants remain in the same quartile or move up/down
#' between consecutive waves. Only participants with dairy data at both
#' consecutive waves are counted in each flow.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A ggplot object.
make_dairy_quartile_flow <- function(analysis_long) {
    
    # Use baseline quartile boundaries from participants column if available;
    # otherwise compute from all waves pooled.
    if ("baseline_dairy_quartile" %in% names(analysis_long)) {
        bsl_vals <- analysis_long |>
            dplyr::filter(osteo_wave == "Baseline",
                          !is.na(dairy_total_gday)) |>
            dplyr::pull(dairy_total_gday)
        q_breaks <- quantile(bsl_vals, probs = c(0, .25, .5, .75, 1),
                             na.rm = TRUE)
    } else {
        q_breaks <- quantile(analysis_long$dairy_total_gday,
                             probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
    }
    
    # Assign time-varying quartile using baseline boundaries
    flow_data <- analysis_long |>
        dplyr::filter(!is.na(dairy_total_gday)) |>
        dplyr::mutate(
            dairy_q = cut(
                dairy_total_gday,
                breaks         = q_breaks,
                labels         = c("Q1", "Q2", "Q3", "Q4"),
                include.lowest = TRUE
            ),
            osteo_wave = forcats::fct_reorder(osteo_wave, osteo_wave_num)
        ) |>
        dplyr::filter(!is.na(dairy_q)) |>
        dplyr::select(pt, osteo_wave, osteo_wave_num, dairy_q)
    
    ggplot2::ggplot(
        flow_data,
        ggplot2::aes(
            x        = osteo_wave,
            stratum  = dairy_q,
            alluvium = pt,
            fill     = dairy_q,
            label    = dairy_q
        )
    ) +
        ggalluvial::geom_flow(
            stat      = "alluvium",
            aes.bind  = "flows",   # replaces deprecated aes.bind = TRUE
            alpha     = 0.45,
            colour    = "white",
            linewidth = 0.15
        ) +
        ggalluvial::geom_stratum(
            width     = 0.4,
            colour    = "white",
            linewidth = 0.3
        ) +
        ggplot2::scale_fill_brewer(palette = "RdYlGn", direction = 1,
                                   name = "Dairy quartile") +
        ggplot2::scale_x_discrete(expand = c(0.05, 0.05)) +
        ggplot2::labs(
            title    = "Dairy intake quartile transitions across OsteoLaus waves",
            subtitle = glue::glue(
                "Quartile boundaries fixed at Baseline: ",
                "Q1 \u2264 {round(q_breaks[2],0)} g/day, ",
                "Q2 \u2264 {round(q_breaks[3],0)} g/day, ",
                "Q3 \u2264 {round(q_breaks[4],0)} g/day"
            ),
            x       = NULL,
            y       = "Number of participants",
            caption = "Only participants with dairy data at each wave included"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "right")
}


# =============================================================================
# Smoking status change plot
# =============================================================================

#' Visualise smoking status changes across OsteoLaus waves.
#'
#' Two complementary panels:
#'   Top: stacked bar showing the raw count of each status per wave
#'        (Never / Former / Current), with n labelled.
#'   Bottom: for participants present at >= 2 consecutive waves,
#'           bar chart showing how many changed smoking status between
#'           each pair of consecutive waves vs stayed the same.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A patchwork plot object (two panels, stacked vertically).
make_smoking_change_plot <- function(analysis_long) {
    
    smk_data <- analysis_long |>
        dplyr::filter(!is.na(sbsmk)) |>
        dplyr::mutate(
            osteo_wave = forcats::fct_reorder(osteo_wave, osteo_wave_num)
        )
    
    smk_colours <- c(
        "Never"   = "#4D9BE6",
        "Former"  = "#F4A261",
        "Current" = "#E84855"
    )
    
    # ── Panel A: Raw counts per wave ──────────────────────────────────────────
    p_counts <- smk_data |>
        dplyr::count(osteo_wave, sbsmk) |>
        ggplot2::ggplot(
            ggplot2::aes(x = osteo_wave, y = n, fill = sbsmk)
        ) +
        ggplot2::geom_col(position = "stack", colour = "white",
                          linewidth = 0.3) +
        ggplot2::scale_fill_manual(values = smk_colours,
                                   name = "Smoking status") +
        ggplot2::labs(
            title = "Smoking status distribution by wave",
            x = NULL, y = "n participants"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "right")
    
    # ── Panel B: Change between consecutive waves ─────────────────────────────
    waves_ordered <- analysis_long |>
        dplyr::distinct(osteo_wave, osteo_wave_num) |>
        dplyr::arrange(osteo_wave_num) |>
        dplyr::pull(osteo_wave)
    
    consecutive_pairs <- if (length(waves_ordered) >= 2L) {
        purrr::map_dfr(
            seq_len(length(waves_ordered) - 1L),
            function(i) {
                w1 <- waves_ordered[i]
                w2 <- waves_ordered[i + 1L]
                
                d1 <- dplyr::filter(smk_data, osteo_wave == w1) |>
                    dplyr::select(pt, smk_w1 = sbsmk)
                d2 <- dplyr::filter(smk_data, osteo_wave == w2) |>
                    dplyr::select(pt, smk_w2 = sbsmk)
                
                d1 |>
                    dplyr::inner_join(d2, by = "pt") |>
                    dplyr::mutate(
                        changed   = smk_w1 != smk_w2,
                        wave_pair = paste0(w1, " \u2192 ", w2)
                    ) |>
                    dplyr::count(wave_pair, changed)
            }
        )
    } else {
        tibble::tibble(
            wave_pair = character(), changed = logical(), n = integer()
        )
    }
    
    p_change <- if (nrow(consecutive_pairs) > 0L) {
        consecutive_pairs |>
            dplyr::mutate(
                status = dplyr::if_else(
                    changed, "Changed status", "No change"
                ),
                wave_pair = factor(wave_pair,
                                   levels = unique(wave_pair))
            ) |>
            ggplot2::ggplot(
                ggplot2::aes(x = wave_pair, y = n, fill = status)
            ) +
            ggplot2::geom_col(position = "stack", colour = "white",
                              linewidth = 0.3) +
            ggplot2::geom_text(
                ggplot2::aes(label = n),
                position = ggplot2::position_stack(vjust = 0.5),
                size = 3.2, colour = "white", fontface = "bold"
            ) +
            ggplot2::scale_fill_manual(
                values = c("No change" = "#4D9BE6",
                           "Changed status" = "#E84855"),
                name = NULL
            ) +
            ggplot2::labs(
                title    = "Smoking status change between consecutive waves",
                subtitle = "Participants present at both waves with non-missing status",
                x        = NULL,
                y        = "n participants"
            ) +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(
                axis.text.x     = ggplot2::element_text(angle = 25, hjust = 1),
                legend.position = "right"
            )
    } else {
        ggplot2::ggplot() +
            ggplot2::annotate("text", x = 1, y = 1,
                              label = "Insufficient wave data") +
            ggplot2::theme_void()
    }
    
    patchwork::wrap_plots(p_counts, p_change, ncol = 1, heights = c(1.2, 1))
}