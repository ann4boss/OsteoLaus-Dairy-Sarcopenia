# =============================================================================
# R/completeness_by_visit.R
# =============================================================================
# Calculates, for each OsteoLaus visit (Baseline, V2, V3, V4, V5):
#
#   1. Per-variable completeness  — % of participants with a non-NA value
#   2. Row-wise completeness      — % of participants with NO missing value
#                                   across ALL included variables
#   3. A heatmap visualisation    — variable × visit grid coloured by % complete
#
# VARIABLES ASSESSED
# ------------------
#   Age, BMI, smoking_status, hrt_status, dairy_total_gday,
#   hypolip_drug_status, corticoids_status, vitD_status, calcium_status,
#   bisphosphonate_status, HTN_status, diabetes_status, sumtot1
#
# INPUT
# -----
#   df  — merged OsteoLaus–CoLaus tibble after derive_combined() and
#          merge_closest_exams(). Must contain .visit_osteo (or .visit if the
#          OsteoLaus visit column has a different name — adjust VISIT_COL below).
#
# OUTPUT
# ------
#   Named list:
#     $by_variable  — tibble: visit × variable × n_total × n_complete × pct_complete
#     $row_complete  — tibble: visit × n_total × n_all_complete × pct_all_complete
#     $plot          — ggplot2 heatmap (variable × visit, fill = pct_complete)
#
# USAGE
# -----
#   result <- completeness_by_visit(merged_derived_df)
#   print(result$row_complete)
#   print(result$plot)
#
# =============================================================================

# ── Variables to assess ────────────────────────────────────────────────────────
.COMPLETENESS_VARS <- c(
    "Age",
    "BMI",
    "smoking_status",
    "hrt_status",
    "dairy_total_gday",
    "hypolip_drug_status",
    "corticoids_status",
    "vitD_status",
    "calcium_status",
    "bisphosphonate_status",
    "HTN_status",
    "diabetes_status",
    "sumtot1",
    "alcohol_category",
    #"pa_levels",
    "HGS_MAX"
    #"ALM_HT2",
    #"gait_speed"
)

# ── OsteoLaus visit column name in the merged dataset ─────────────────────────
# Change to ".visit" if your merged data uses that name instead.
VISIT_COL <- ".visit_osteo"

# ── Ordered visit levels ───────────────────────────────────────────────────────
VISIT_LEVELS <- c("Baseline", "V2", "V3", "V4", "V5")


# =============================================================================
# Main function
# =============================================================================

#' Calculate analysis-variable completeness by OsteoLaus visit.
#'
#' @param df  Merged OsteoLaus–CoLaus tibble after derive_combined(). Must
#'   contain the column named by \code{visit_col} and the variables listed in
#'   \code{vars}.
#' @param vars      Character vector of variable names to assess.
#'   Defaults to \code{.COMPLETENESS_VARS}.
#' @param visit_col Character. Name of the OsteoLaus visit column.
#'   Defaults to \code{VISIT_COL}.
#' @param save_plot Logical. Write the heatmap to \code{out_path}? Default FALSE.
#' @param out_path  File path for the saved PNG (used when save_plot = TRUE).
#'
#' @return Named list: \code{by_variable}, \code{row_complete}, \code{plot}.
completeness_by_visit <- function(
        df,
        vars       = .COMPLETENESS_VARS,
        visit_col  = VISIT_COL,
        save_plot  = FALSE,
        out_path   = "completeness_heatmap.png"
) {
    
    cli::cli_h1("Analysis-variable completeness by OsteoLaus visit")
    
    # ── Guards ─────────────────────────────────────────────────────────────────
    if (!visit_col %in% names(df))
        cli::cli_abort(
            "Column {.val {visit_col}} not found. \\
             Set visit_col to the correct OsteoLaus visit column name."
        )
    
    present_vars <- intersect(vars, names(df))
    absent_vars  <- setdiff(vars, names(df))
    
    if (length(absent_vars) > 0)
        cli::cli_warn(c(
            "!" = "The following variables were not found and are excluded:",
            "*" = "{.val {absent_vars}}"
        ))
    
    if (length(present_vars) == 0)
        cli::cli_abort("No requested variables found in the data.")
    
    # ── Restrict to OsteoLaus visits only and coerce visit to ordered factor ───
    df_visits <- df |>
        dplyr::filter(.data[[visit_col]] %in% VISIT_LEVELS) |>
        dplyr::mutate(
            .visit_ordered = factor(
                as.character(.data[[visit_col]]),
                levels = VISIT_LEVELS,
                ordered = TRUE
            )
        )
    
    n_rows_total <- nrow(df_visits)
    if (n_rows_total == 0)
        cli::cli_abort("No rows remain after filtering to OsteoLaus visit levels.")
    
    # ── 1. Per-variable completeness ───────────────────────────────────────────
    by_variable <- df_visits |>
        dplyr::group_by(visit = .visit_ordered) |>
        dplyr::summarise(
            dplyr::across(
                dplyr::all_of(present_vars),
                list(
                    n_total    = ~ dplyr::n(),
                    n_complete = ~ sum(!is.na(.x))
                ),
                .names = "{.col}__{.fn}"
            ),
            .groups = "drop"
        ) |>
        tidyr::pivot_longer(
            cols      = -visit,
            names_to  = c("variable", ".value"),
            names_sep = "__"
        ) |>
        dplyr::mutate(
            pct_complete = round(100 * n_complete / n_total, 1),
            variable     = factor(variable, levels = rev(present_vars))  # rev for heatmap y-axis
        ) |>
        dplyr::arrange(visit, variable)
    
    # ── 2. Row-wise completeness (all vars non-NA simultaneously) ──────────────
    row_complete <- df_visits |>
        dplyr::group_by(visit = .visit_ordered) |>
        dplyr::summarise(
            n_total       = dplyr::n(),
            n_all_complete = sum(
                rowSums(is.na(dplyr::pick(dplyr::all_of(present_vars)))) == 0L
            ),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            pct_all_complete = round(100 * n_all_complete / n_total, 1)
        )
    
    # ── 3. Longitudinal completeness (ALL visits + ALL vars) ───────────────────
    if (!"pt" %in% names(df_visits))
        cli::cli_abort("Column {.val pt} (participant ID) not found.")
    
    longitudinal_complete <- df_visits |>
        dplyr::group_by(pt) |>
        dplyr::summarise(
            n_visits_present = dplyr::n_distinct(.visit_ordered),
            
            # check all variables across all rows (visits)
            all_complete = all(
                rowSums(is.na(dplyr::pick(dplyr::all_of(present_vars)))) == 0L
            ) &
                # ensure participant actually has ALL visits
                (n_visits_present == length(VISIT_LEVELS)),
            
            .groups = "drop"
        ) |>
        dplyr::summarise(
            n_participants      = dplyr::n(),
            n_complete_all_time = sum(all_complete),
            pct_complete_all_time = round(
                100 * n_complete_all_time / n_participants, 1
            )
        )
    
    # ── Console report ─────────────────────────────────────────────────────────
    cli::cli_h2("Row-wise completeness (all {length(present_vars)} variables non-NA)")
    cli::cli_inform(
        paste(capture.output(print(row_complete, n = Inf)), collapse = "\n")
    )
    
    cli::cli_h2("Per-variable completeness (% complete by visit)")
    wide_report <- by_variable |>
        dplyr::select(visit, variable, pct_complete) |>
        tidyr::pivot_wider(names_from = visit, values_from = pct_complete)
    cli::cli_inform(
        paste(capture.output(print(wide_report, n = Inf)), collapse = "\n")
    )
    
    cli::cli_h2("Longitudinal completeness (all visits + all variables)")
    cli::cli_inform(
        paste(capture.output(print(longitudinal_complete)), collapse = "\n")
    )
    
    # ── 3. Heatmap ─────────────────────────────────────────────────────────────
    # Colour scale: red (<70%) → amber (70–85%) → green (>85%)
    # Row-wise completeness is overlaid as a separate panel below the heatmap.
    
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        cli::cli_warn("ggplot2 not available — skipping plot.")
        p <- NULL
    } else {
        
        # Main heatmap data
        heat_df <- by_variable |>
            dplyr::mutate(
                label = paste0(pct_complete, "%")
            )
        
        # Row-complete data formatted as a single "variable" for bottom strip
        row_df <- row_complete |>
            dplyr::mutate(
                variable     = factor("ALL complete", levels = c("ALL complete")),
                pct_complete = pct_all_complete,
                n_complete   = n_all_complete,
                n_total      = n_total,
                label        = paste0(pct_all_complete, "%")
            ) |>
            dplyr::select(visit, variable, n_total, n_complete,
                          pct_complete, label)
        
        # Bind: put ALL-complete strip above the per-variable rows
        plot_df <- dplyr::bind_rows(
            row_df,
            heat_df |> dplyr::mutate(variable = as.character(variable)) |>
                dplyr::mutate(variable = factor(variable,
                                                levels = c("ALL complete",
                                                           levels(heat_df$variable))))
        )
        
        p <- ggplot2::ggplot(
            plot_df,
            ggplot2::aes(
                x    = visit,
                y    = variable,
                fill = pct_complete
            )
        ) +
            ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
            ggplot2::geom_text(
                ggplot2::aes(label = label),
                size   = 3.2,
                colour = "grey10",
                fontface = "bold"
            ) +
            # Colour scale
            ggplot2::scale_fill_gradientn(
                colours = c("#d73027", "#fc8d59", "#fee090",
                            "#91cf60", "#1a9850"),
                values  = scales::rescale(c(0, 60, 75, 90, 100)),
                limits  = c(0, 100),
                name    = "% complete"
            ) +
            # Separator line between ALL-complete strip and per-variable rows
            ggplot2::geom_hline(
                yintercept = length(present_vars) + 0.5,
                colour     = "grey30",
                linewidth  = 0.9,
                linetype   = "dashed"
            ) +
            ggplot2::scale_x_discrete(position = "top") +
            ggplot2::labs(
                title    = "Completeness of analysis variables by OsteoLaus visit",
                subtitle = paste0(
                    length(present_vars), " variables assessed  |  ",
                    "Top row = participants with ALL variables non-missing"
                ),
                x = NULL,
                y = NULL
            ) +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(
                plot.title       = ggplot2::element_text(face = "bold", size = 13),
                plot.subtitle    = ggplot2::element_text(colour = "grey40", size = 10),
                axis.text.x      = ggplot2::element_text(face = "bold", size = 11),
                axis.text.y      = ggplot2::element_text(size = 10),
                panel.grid       = ggplot2::element_blank(),
                legend.position  = "right",
                plot.margin      = ggplot2::margin(12, 12, 12, 12)
            )
        
        if (save_plot) {
            ggplot2::ggsave(
                out_path, plot = p,
                width = 9, height = 5.5, dpi = 150
            )
            cli::cli_inform(c("v" = "Heatmap saved to {.path {out_path}}"))
        }
    }
    
    cli::cli_inform(c("v" = "completeness_by_visit() complete."))
    
    invisible(list(
        by_variable            = by_variable,
        row_complete           = row_complete,
        longitudinal_complete  = longitudinal_complete,
        plot                   = p
    ))
}


# =============================================================================
# Convenience: completeness for MICE-imputed data (per .imp)
# =============================================================================

#' Check completeness across all .imp datasets and summarise mean ± SD.
#'
#' Useful for verifying that imputation actually resolved missingness
#' consistently across datasets. Calls completeness_by_visit() on each
#' imputed dataset and pools the row_complete results.
#'
#' @param mice_long Long-format tibble with .imp column (output of
#'   mice_merge_derive()). The .imp == 0 observed dataset is included for
#'   comparison.
#' @param vars     Character vector of variables. Defaults to .COMPLETENESS_VARS.
#' @param visit_col Character. OsteoLaus visit column. Default VISIT_COL.
#'
#' @return Tibble: visit × mean_pct_complete ± sd_pct_complete,
#'   plus the pre-imputation (.imp == 0) pct for comparison.
completeness_mice_summary <- function(
        mice_long,
        vars      = .COMPLETENESS_VARS,
        visit_col = VISIT_COL
) {
    
    cli::cli_h1("Completeness summary across MICE imputed datasets")
    
    imp_ids <- sort(unique(mice_long$.imp))
    
    results <- purrr::map_dfr(imp_ids, function(i) {
        df_i <- dplyr::filter(mice_long, .imp == i)
        res_i <- completeness_by_visit(df_i, vars = vars,
                                       visit_col = visit_col)
        res_i$row_complete |>
            dplyr::mutate(.imp = i)
    })
    
    # Separate observed (.imp == 0) from imputed
    observed  <- dplyr::filter(results, .imp == 0L)
    imputed   <- dplyr::filter(results, .imp >  0L)
    
    summary_tbl <- imputed |>
        dplyr::group_by(visit) |>
        dplyr::summarise(
            n_total             = mean(n_total),
            mean_pct_complete   = round(mean(pct_all_complete), 1),
            sd_pct_complete     = round(sd(pct_all_complete),   1),
            .groups = "drop"
        ) |>
        dplyr::left_join(
            observed |> dplyr::select(visit, pct_observed = pct_all_complete),
            by = "visit"
        )
    
    cli::cli_h2("Row-wise completeness: observed vs imputed (mean \u00b1 SD)")
    cli::cli_inform(
        paste(capture.output(print(summary_tbl, n = Inf)), collapse = "\n")
    )
    
    invisible(summary_tbl)
}