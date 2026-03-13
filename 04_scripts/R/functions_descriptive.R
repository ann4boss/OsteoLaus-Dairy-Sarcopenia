# =============================================================================
# R/functions_descriptive.R
# =============================================================================
# All descriptive analysis functions.
# Input: analysis_long — output of freeze_dataset().
# Each function returns a self-contained object (gt table, ggplot, tibble).

# ── Table 1 — Baseline characteristics ---------------------------------------

#' Produce a gtsummary Table 1 for OsteoLaus baseline participants
#'
#' Includes all time-invariant and baseline time-varying covariates,
#' plus dairy exposures and the primary outcome.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A gtsummary tbl_summary object.
make_table_one <- function(analysis_long) {
    analysis_long |>
        dplyr::filter(in_osteolaus & .wave == "Baseline") |>
        dplyr::select(dplyr::any_of(c(
            # Baseline characteristics
            "age", "education_level", "BMI", "BMI_category",
            "alcohol_category", "sbsmk",
            "diabetes_status", "hrt_status", "cdv_event",
            # Dairy exposures (from FFQ; available from F1 in CoLaus/V2 in OsteoLaus)
            "dairy_total", "dairy_fermented", "dairy_non_fermented",
            # Primary outcome
            "ewgsop2_sarcopenia_stage"
        ))) |>
        gtsummary::tbl_summary(
            statistic = list(
                gtsummary::all_continuous()  ~ "{mean} ({sd})",
                gtsummary::all_categorical() ~ "{n} ({p}%)"
            ),
            digits  = list(gtsummary::all_continuous() ~ 1),
            missing = "ifany"
        ) |>
        gtsummary::add_n() |>
        gtsummary::bold_labels()
}

# ── Missing data summary -------------------------------------------------------

#' Produce a per-wave missing data summary for key variables
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A tibble with variables as rows and waves as columns,
#'         cells = % missing among OsteoLaus participants.
make_missing_summary <- function(analysis_long) {
    key_vars <- c(
        "age", "BMI", "education_level",
        "alcohol_category", "sbsmk", "diabetes_status",
        "cdv_event", "hrt_status",
        "dairy_total", "dairy_fermented", "dairy_non_fermented",
        "dairy_lowfat", "dairy_highfat",
        "ewgsop2_sarcopenia_stage",
        "handgrip", "HGS_MAX", "ALM", "ALM_HT2", "alm_ht2", "6MGS"
    )
    present <- intersect(key_vars, names(analysis_long))
    
    analysis_long |>
        dplyr::filter(in_osteolaus) |>
        dplyr::group_by(.wave) |>
        dplyr::summarise(
            dplyr::across(
                dplyr::all_of(present),
                ~ round(mean(is.na(.x)) * 100, 1)
            ),
            .groups = "drop"
        ) |>
        tidyr::pivot_longer(-.wave, names_to = "variable", values_to = "pct_missing") |>
        tidyr::pivot_wider(names_from = .wave, values_from = pct_missing)
}

# ── Cohort flow table ---------------------------------------------------------

#' Build a CONSORT-style participant flow tibble
#'
#' @param derived_list  Named list output of derive_variables() ($data, $log).
#' @param analysis_long Output of freeze_dataset().
#' @return A tibble with columns: step, n_participants.
make_cohort_flow <- function(derived_list, analysis_long) {
    # derived_list is a named list ($data, $log); unpack $data
    d <- derived_list$data
    
    tibble::tribble(
        ~step,                                                  ~n_participants,
        "CoLaus / OsteoLaus raw",
        dplyr::n_distinct(d$pt),
        
        "OsteoLaus sub-cohort (female participants)",
        dplyr::n_distinct(d$pt[d$.cohort == "OsteoLaus"]),
        
        "After excluding missing baseline sarcopenia outcome",
        dplyr::n_distinct(analysis_long$pt[analysis_long$in_osteolaus]),
        
        "With \u22651 follow-up outcome (eligible longitudinal analysis)",
        dplyr::n_distinct(
            analysis_long$pt[analysis_long$in_osteolaus &
                                 analysis_long$has_followup_outcome]
        ),
        
        "With FFQ data (dairy exposure analyses)",
        dplyr::n_distinct(
            analysis_long$pt[analysis_long$in_osteolaus & analysis_long$has_ffq]
        )
    )
}

# ── Wave summary table --------------------------------------------------------

#' Number of participants and outcome/FFQ completeness per wave
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A tibble with one row per wave, ordered by wave number.
make_wave_summary <- function(analysis_long) {
    analysis_long |>
        dplyr::filter(in_osteolaus) |>
        dplyr::group_by(.wave, .wave_num) |>
        dplyr::summarise(
            n_participants       = dplyr::n_distinct(pt),
            pct_outcome_complete = round(
                mean(!is.na(ewgsop2_sarcopenia_stage)) * 100, 1
            ),
            pct_ffq_complete     = round(
                mean(!is.na(dairy_total)) * 100, 1
            ),
            .groups = "drop"
        ) |>
        dplyr::arrange(.wave_num)
}

# ── Exposure plots ------------------------------------------------------------

#' Distribution plots for dairy exposure variables
#'
#' Returns a patchwork of a histogram grid (one panel per exposure sub-category)
#' and a boxplot comparing sub-categories side by side.
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A patchwork plot object.
make_exposure_plots <- function(analysis_long) {
    
    ffq_data <- analysis_long |>
        dplyr::filter(in_osteolaus & has_ffq & !is.na(dairy_total)) |>
        tidyr::pivot_longer(
            cols      = dplyr::any_of(c(
                "dairy_total", "dairy_fermented", "dairy_non_fermented",
                "dairy_lowfat", "dairy_highfat"
            )),
            names_to  = "exposure",
            values_to = "grams_day"
        ) |>
        dplyr::mutate(
            exposure = stringr::str_replace_all(exposure, "_", " ") |>
                stringr::str_to_title()
        )
    
    p_hist <- ggplot2::ggplot(ffq_data, ggplot2::aes(x = grams_day)) +
        ggplot2::geom_histogram(bins = 40, fill = "#2E86AB", colour = "white",
                                linewidth = 0.2) +
        ggplot2::facet_wrap(~ exposure, scales = "free", ncol = 2) +
        ggplot2::labs(
            title   = "Distribution of dairy intake (g/day)",
            x       = "g/day",
            y       = "Count",
            caption = "OsteoLaus participants with FFQ data"
        ) +
        ggplot2::theme_minimal(base_size = 12)
    
    p_box <- ggplot2::ggplot(
        ffq_data,
        ggplot2::aes(x = exposure, y = grams_day, fill = exposure)
    ) +
        ggplot2::geom_boxplot(show.legend = FALSE, outlier.size = 0.5, alpha = 0.8) +
        ggplot2::scale_fill_brewer(palette = "Set2") +
        ggplot2::labs(title = "Dairy intake by sub-category", x = NULL, y = "g/day") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
    
    patchwork::wrap_plots(p_hist, p_box, ncol = 1)
}

# ── Outcome plots -------------------------------------------------------------

#' Sarcopenia stage prevalence by wave (stacked bar chart)
#'
#' @param analysis_long Output of freeze_dataset().
#' @return A ggplot object.
make_outcome_plots <- function(analysis_long) {
    
    stage_data <- analysis_long |>
        dplyr::filter(in_osteolaus & !is.na(ewgsop2_sarcopenia_stage)) |>
        dplyr::count(.wave, .wave_num, ewgsop2_sarcopenia_stage) |>
        dplyr::group_by(.wave) |>
        dplyr::mutate(pct = n / sum(n) * 100) |>
        dplyr::ungroup() |>
        dplyr::mutate(.wave = forcats::fct_reorder(.wave, .wave_num))
    
    ggplot2::ggplot(
        stage_data,
        ggplot2::aes(x = .wave, y = pct, fill = ewgsop2_sarcopenia_stage)
    ) +
        ggplot2::geom_col(position = "stack", colour = "white", linewidth = 0.3) +
        ggplot2::scale_fill_manual(
            values = c(
                "0-None"      = "#2D6A4F",
                "1-Probable"  = "#F4A261",
                "2-Confirmed" = "#E76F51",
                "3-Severe"    = "#E84855"
            ),
            name = "EWGSOP2 stage"
        ) +
        ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        ggplot2::labs(
            title   = "EWGSOP2 sarcopenia stage prevalence by wave",
            x       = NULL,
            y       = "% of participants",
            caption = "OsteoLaus participants with complete outcome"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "right")
}