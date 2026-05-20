# =============================================================================
# R/tableOne.R
# =============================================================================
# Simple Table 1 script
#
# Supports both complete-case (cc) and multiply-imputed (mice) datasets.
# For imputed datasets, pass a long-format data frame that includes a `.imp`
# column (the imputation index, as produced by mice::complete(..., "long")).
#
# Saves output to:
#   06_outputs/TableOne/Date_YYYY-MM-DD/<cc|mice>/
# =============================================================================


.TABLE_LABELS <- list(
    Age ~ "Age",
    education_level ~ "Education level (ISCED)",
    mrtsts2 ~ "Marital status",
    BMI_category ~ "BMI category",
    smoking_status ~ "Smoking status",
    alcohol_category_conso ~ "Alcohol intake",
    alcohol_category_sumalco ~ "Alcohol intake",
    pa_levels_tertile_f1 ~ "Physical activity level",
    pa_levels_tertile_f2 ~ "Physical activity level",
    pa_levels_who_f1 ~ "Physical activity level",
    pa_levels_who_f2 ~ "Physical activity level",
    diabetes_status ~ "Diabetes status",
    htn_status ~ "Hypertension",
    hrt_status ~ "HRT status",
    cdv_event ~ "CVD event (any)",
    hypolip_drug_status ~ "Lipid-lowering medication",
    corticoids_status ~ "Systemic corticosteroids",
    calcium_status ~ "Calcium supplement",
    vitD_status ~ "Vitamin D supplement",
    bisphosphonate_status ~ "Bisphosphonate use",
    benzo_status ~ "Benzo use",
    sumtot1 ~ "Caloric intake (kcal/day)",
    dairy_total_gday ~ "Total dairy intake (g/day)",
    dairy_fermented_gday ~ "Fermented dairy (g/day)",
    dairy_non_fermented_gday ~ "Non-fermented dairy (g/day)",
    dairy_lowfat_gday ~ "Low-fat dairy (g/day)",
    dairy_highfat_gday ~ "High-fat dairy (g/day)",
    HGS_MAX ~ "Grip strength (kg)",
    ALM_HT2_Lunar ~ "ALMI Lunar (kg/m\u00b2)",        
    gait_speed ~ "Gait speed at V4 (m/s)",
    ewgsop2_sarcopenia_stage ~ "EWGSOP2 sarcopenia stage",
    fnih_sarcopenia ~ "FNIH Sarcopenia"
)

.TABLE_VARS <- c(
    "Age", "education_level", "mrtsts2", "BMI_category",
    "smoking_status", "alcohol_category_conso", "pa_levels_tertile_f1",
    "diabetes_status", "htn_status", "hrt_status",
    "hypolip_drug_status", "corticoids_status", "calcium_status",
    "vitD_status", "bisphosphonate_status", "sumtot1",
    "dairy_total_gday", "dairy_fermented_gday", "dairy_non_fermented_gday",
    "dairy_lowfat_gday", "dairy_highfat_gday",
    "ewgsop2_sarcopenia_stage", "HGS_MAX", "ALM_HT2_Lunar", "gait_speed"
)


# 
.table_vars_for_data <- function(data, extra_vars = character(0)) {
    intersect(c(.TABLE_VARS, extra_vars), names(data))
}

.normalize_table_types <- function(data) {
    data |>
        dplyr::mutate(
            dplyr::across(
                where(is.ordered),
                ~ factor(as.character(.x), levels = levels(.x), ordered = FALSE)
            )
        )
}

# ---------------------------------------------------------------------------
# Helper: detect whether the dataset is a mice long-format imputed dataset
# ---------------------------------------------------------------------------
is_imputed <- function(data) {
    ".imp" %in% names(data) && length(unique(data[[".imp"]])) > 1
}

# ---------------------------------------------------------------------------
# Pooling helpers for imputed data
# ---------------------------------------------------------------------------

# Pool continuous variables: pooled mean = mean of imputation means;
# pooled SD (descriptive) = mean of imputation SDs.
.pool_continuous <- function(x_list) {
    # x_list: named list of numeric vectors, one per imputation
    means <- vapply(x_list, mean, numeric(1), na.rm = TRUE)
    sds   <- vapply(x_list, sd,   numeric(1), na.rm = TRUE)
    list(mean = mean(means), sd = mean(sds))
}

# Pool categorical variables: pooled proportion = mean of per-imputation
# proportions. Pooled count = n * pooled proportion.
.pool_categorical <- function(x_list, n) {
    # x_list: named list of factor/character vectors, one per imputation
    all_levels <- unique(unlist(lapply(x_list, function(x) as.character(x[!is.na(x)]))))
    m <- length(x_list)
    props <- vapply(all_levels, function(k) {
        mean(vapply(x_list, function(x) mean(as.character(x) == k, na.rm = TRUE), numeric(1)))
    }, numeric(1))
    counts <- round(n * props)
    list(levels = all_levels, props = props, counts = counts)
}

# ---------------------------------------------------------------------------
# labels_for_data
# ---------------------------------------------------------------------------
labels_for_data <- function(data, vars = names(data)) {
    vars <- intersect(vars, names(data))
    keep <- vapply(
        .TABLE_LABELS,
        function(label_formula) all.vars(label_formula[[2]]) %in% vars,
        logical(1)
    )
    .TABLE_LABELS[keep]
}

.label_for_var <- function(var) {
    lbl <- labels_for_data(stats::setNames(data.frame(NA), var), var)
    if (length(lbl) == 0L) return(var)
    as.character(lbl[[1]][[3]])
}

.fmt_cont <- function(mean, sd, median, p25, p75) {
    sprintf(
        "%.1f (%.1f); %.1f [%.1f, %.1f]",
        mean, sd, median, p25, p75
    )
}

.fmt_cat <- function(n, p) {
    sprintf("%.0f (%.1f%%)", n, p)
}

.mean_or_na <- function(x) {
    if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

.imp_values <- function(data, imp_col) {
    vals <- sort(unique(data[[imp_col]]))
    vals[!is.na(vals)]
}

.make_manual_table_object <- function(table_body, caption, footnote) {
    stat_cols <- grep("^stat_", names(table_body), value = TRUE)
    display <- table_body |>
        dplyr::select(label, level, dplyr::all_of(stat_cols))
    
    stat_labels <- as.list(sub("^stat_", "", stat_cols))
    names(stat_labels) <- stat_cols
    
    gt_tbl <- display |>
        gt::gt() |>
        gt::tab_header(title = gt::md(caption)) |>
        gt::cols_label(.list = c(list(label = "Characteristic", level = ""), stat_labels)) |>
        gt::fmt_markdown(columns = dplyr::all_of("label")) |>
        gt::tab_source_note(gt::md(footnote))
    
    structure(
        list(table_body = table_body, display = display, gt = gt_tbl),
        class = "manual_table_one"
    )
}

.pool_continuous_manual <- function(data, var, imp_col, group_col = NULL, group_value = NULL) {
    imps <- .imp_values(data, imp_col)
    
    pooled <- purrr::map_dfr(imps, function(imp) {
        d <- data |> dplyr::filter(.data[[imp_col]] == imp)
        if (!is.null(group_col)) {
            d <- d |> dplyr::filter(as.character(.data[[group_col]]) == group_value)
        }
        x <- d[[var]]
        n_obs <- sum(!is.na(x))
        if (n_obs == 0L) {
            tibble::tibble(n = 0, mean = NA_real_, sd = NA_real_,
                           median = NA_real_, p25 = NA_real_, p75 = NA_real_)
        } else {
            tibble::tibble(
                n = n_obs,
                mean = mean(x, na.rm = TRUE),
                sd = stats::sd(x, na.rm = TRUE),
                median = stats::median(x, na.rm = TRUE),
                p25 = stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE),
                p75 = stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE)
            )
        }
    })
    
    list(
        n = .mean_or_na(pooled$n),
        stat = .fmt_cont(
            .mean_or_na(pooled$mean),
            .mean_or_na(pooled$sd),
            .mean_or_na(pooled$median),
            .mean_or_na(pooled$p25),
            .mean_or_na(pooled$p75)
        )
    )
}

.categorical_levels <- function(x) {
    vals <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))
    vals[!is.na(vals)]
}

.pool_categorical_manual <- function(data, var, imp_col, group_col = NULL, group_value = NULL) {
    imps <- .imp_values(data, imp_col)
    levels <- .categorical_levels(data[[var]])
    has_missing <- any(is.na(data[[var]]))
    if (has_missing) levels <- c(levels, "Missing")
    
    purrr::map_dfr(levels, function(level) {
        pooled <- purrr::map_dfr(imps, function(imp) {
            d <- data |> dplyr::filter(.data[[imp_col]] == imp)
            if (!is.null(group_col)) {
                d <- d |> dplyr::filter(as.character(.data[[group_col]]) == group_value)
            }
            x <- d[[var]]
            denom <- length(x)
            n <- if (identical(level, "Missing")) {
                sum(is.na(x))
            } else {
                sum(as.character(x) == level, na.rm = TRUE)
            }
            tibble::tibble(
                n = n,
                p = if (denom > 0) 100 * n / denom else NA_real_
            )
        })
        
        n <- .mean_or_na(pooled$n)
        p <- .mean_or_na(pooled$p)
        tibble::tibble(
            level = level,
            n = n,
            p = p,
            stat = .fmt_cat(n, p)
        )
    })
}

.manual_group_levels <- function(data, by) {
    x <- data[[by]]
    levels <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))
    intersect(levels, unique(as.character(x[!is.na(x)])))
}

.insert_manual_heading <- function(body, group_vars, heading_label) {
    anchor_idx <- which(body$variable %in% group_vars)[1]
    if (is.na(anchor_idx)) return(body)
    
    heading_row <- body[anchor_idx, , drop = FALSE]
    heading_row[] <- lapply(heading_row, function(col) NA)
    heading_row$variable <- paste0(".heading_", gsub("\\s+", "_", heading_label))
    heading_row$row_type <- "heading"
    heading_row$label <- paste0("**", heading_label, "**")
    heading_row$level <- ""
    
    dplyr::bind_rows(
        body[seq_len(anchor_idx - 1L), , drop = FALSE],
        heading_row,
        body[seq(anchor_idx, nrow(body)), , drop = FALSE]
    )
}

.manual_table_one <- function(data,
                              by = NULL,
                              id = "pt",
                              imp_col = ".imp",
                              include_vars = NULL,
                              caption = "**Table 1.** Participant characteristics",
                              footnote_suffix = "") {
    data <- .normalize_table_types(data)
    if (is.null(include_vars)) {
        include_vars <- .table_vars_for_data(data)
    }
    include_vars <- setdiff(intersect(include_vars, names(data)), c(id, imp_col, by))
    continuous_vars <- include_vars[vapply(data[include_vars], is.numeric, logical(1))]
    categorical_vars <- setdiff(include_vars, continuous_vars)
    
    groups <- if (is.null(by)) NULL else .manual_group_levels(data, by)
    stat_cols <- if (is.null(by)) "stat_0" else paste0("stat_", seq_along(groups))
    stat_names <- if (is.null(by)) "Overall" else groups
    
    make_row <- function(variable, row_type, level = "", stats) {
        row <- tibble::tibble(
            variable = variable,
            row_type = row_type,
            label = .label_for_var(variable),
            level = level
        )
        for (i in seq_along(stat_cols)) {
            row[[stat_cols[[i]]]] <- stats[[i]]
        }
        row
    }
    
    rows <- list()
    for (var in continuous_vars) {
        stats <- if (is.null(by)) {
            list(.pool_continuous_manual(data, var, imp_col)$stat)
        } else {
            purrr::map(groups, ~.pool_continuous_manual(data, var, imp_col, by, .x)$stat)
        }
        rows[[length(rows) + 1L]] <- make_row(var, "continuous", "", stats)
    }
    
    for (var in categorical_vars) {
        if (is.null(by)) {
            pooled <- .pool_categorical_manual(data, var, imp_col)
            for (i in seq_len(nrow(pooled))) {
                rows[[length(rows) + 1L]] <- make_row(var, "categorical", pooled$level[[i]], list(pooled$stat[[i]]))
            }
        } else {
            pooled_by_group <- purrr::map(groups, ~.pool_categorical_manual(data, var, imp_col, by, .x))
            levels <- unique(unlist(purrr::map(pooled_by_group, "level")))
            for (level in levels) {
                stats <- purrr::map(pooled_by_group, function(x) {
                    hit <- x |> dplyr::filter(.data$level == !!level)
                    if (nrow(hit) == 0L) "" else hit$stat[[1]]
                })
                rows[[length(rows) + 1L]] <- make_row(var, "categorical", level, stats)
            }
        }
    }
    
    body <- dplyr::bind_rows(rows)
    names(body)[match(stat_cols, names(body))] <- paste0("stat_", make.names(stat_names))
    body <- .insert_manual_heading(body, .MEDICATION_VARS, "Medication")
    body <- .insert_manual_heading(body, .DAIRY_VARS, "Dairy consumption")
    
    footnote <- paste0(
        "Continuous variables: pooled mean (pooled SD); pooled median [pooled 25th, 75th percentile], ",
        "where each statistic is averaged across imputations. ",
        "Categorical variables: average count across imputations (average percentage within imputation). ",
        "Missing is included in categorical denominators when present. ",
        "Gait speed from V4; ALMI from V3. ",
        "Severe sarcopenia could not be defined at baseline as gait speed was not measured at that visit.",
        footnote_suffix
    )
    
    .make_manual_table_object(body, caption, footnote)
}

# ---------------------------------------------------------------------------
# Build one display row per participant for the baseline table.
#
# * Most variables: OsteoLaus Baseline
# * Gait speed: pulled from V4 (first measured there)
# * ALMI (ALM_HT2_Lunar): pulled from V3
#
# ---------------------------------------------------------------------------
make_display_baseline <- function(analysis_long,
                                  id             = "pt",
                                  visit           = ".visit_osteo",
                                  baseline_visit  = "Baseline",
                                  gait_visit      = "V4",
                                  almi_visit      = "V3") {
    
    if (!all(c(id, visit) %in% names(analysis_long))) {
        stop("`analysis_long` must contain `", id, "` and `", visit, "`.", call. = FALSE)
    }
    
    baseline <- analysis_long |>
        dplyr::filter(.data[[visit]] == baseline_visit)
    
    # --- Gait speed from V4 ---
    if ("gait_speed" %in% names(analysis_long)) {
        v4_gait <- analysis_long |>
            dplyr::filter(.data[[visit]] == gait_visit, !is.na(.data$gait_speed)) |>
            dplyr::group_by(.data[[id]]) |>
            dplyr::slice(1) |>
            dplyr::ungroup() |>
            dplyr::select(dplyr::all_of(id), gait_speed)
        
        baseline <- baseline |>
            dplyr::select(-dplyr::any_of("gait_speed")) |>
            dplyr::left_join(v4_gait, by = id)
    }
    
    # --- ALMI from V3 ---
    if ("ALM_HT2_Lunar" %in% names(analysis_long)) {
        v3_almi <- analysis_long |>
            dplyr::filter(.data[[visit]] == almi_visit, !is.na(.data$ALM_HT2_Lunar)) |>
            dplyr::group_by(.data[[id]]) |>
            dplyr::slice(1) |>
            dplyr::ungroup() |>
            dplyr::select(dplyr::all_of(id), ALM_HT2_Lunar)
        
        baseline <- baseline |>
            dplyr::select(-dplyr::any_of("ALM_HT2_Lunar")) |>
            dplyr::left_join(v3_almi, by = id)
    }
    
    baseline
}

# ---------------------------------------------------------------------------
# make_display_baseline for imputed data:
# Keep one row per participant per imputation. Table summaries are pooled
# manually downstream.
# ---------------------------------------------------------------------------
make_display_baseline_imputed <- function(analysis_long,
                                          id             = "pt",
                                          visit           = ".visit_osteo",
                                          imp_col        = ".imp",
                                          baseline_visit  = "Baseline",
                                          gait_visit      = "V4",
                                          almi_visit      = "V3") {
    
    if (!all(c(id, visit, imp_col) %in% names(analysis_long))) {
        stop("`analysis_long` must contain `", id, "`, `", visit, "`, and `", imp_col, "`.", call. = FALSE)
    }
    
    join_by_imp <- c(id, imp_col)
    if (is.numeric(analysis_long[[imp_col]]) && any(analysis_long[[imp_col]] == 0, na.rm = TRUE)) {
        analysis_long <- analysis_long |>
            dplyr::filter(.data[[imp_col]] > 0)
    }
    
    baseline_pooled <- analysis_long |>
        dplyr::filter(.data[[visit]] == baseline_visit)
    
    # Gait speed from V4
    if ("gait_speed" %in% names(analysis_long)) {
        gait_pooled <- analysis_long |>
            dplyr::filter(.data[[visit]] == gait_visit, !is.na(.data$gait_speed)) |>
            dplyr::group_by(.data[[id]], .data[[imp_col]]) |>
            dplyr::slice(1) |>
            dplyr::ungroup() |>
            dplyr::select(dplyr::all_of(join_by_imp), gait_speed)
        baseline_pooled <- baseline_pooled |>
            dplyr::select(-dplyr::any_of("gait_speed")) |>
            dplyr::left_join(gait_pooled, by = join_by_imp)
    }
    
    # ALMI from V3
    if ("ALM_HT2_Lunar" %in% names(analysis_long)) {
        almi_pooled <- analysis_long |>
            dplyr::filter(.data[[visit]] == almi_visit, !is.na(.data$ALM_HT2_Lunar)) |>
            dplyr::group_by(.data[[id]], .data[[imp_col]]) |>
            dplyr::slice(1) |>
            dplyr::ungroup() |>
            dplyr::select(dplyr::all_of(join_by_imp), ALM_HT2_Lunar)
        
        
        baseline_pooled <- baseline_pooled |>
            dplyr::select(-dplyr::any_of("ALM_HT2_Lunar")) |>
            dplyr::left_join(almi_pooled, by = join_by_imp)
    }
    
    baseline_pooled
}

# ---------------------------------------------------------------------------
# Variables that fall under each group heading.
# ---------------------------------------------------------------------------
.MEDICATION_VARS <- c(
    "hypolip_drug_status", "corticoids_status",
    "calcium_status", "vitD_status", "bisphosphonate_status"
)

.DAIRY_VARS <- c(
    "dairy_total_gday", "dairy_fermented_gday", "dairy_non_fermented_gday",
    "dairy_lowfat_gday", "dairy_highfat_gday"
)

# Insert a bold spanning label row immediately above the first variable in
# `group_vars` that is present in the table body.
.add_group_heading <- function(tbl, group_vars, heading_label) {
    body <- tbl$table_body
    anchor_idx <- which(body$variable %in% group_vars)[1]
    if (is.na(anchor_idx)) return(tbl)
    
    heading_row <- body[anchor_idx, , drop = FALSE]
    heading_row[] <- lapply(heading_row, function(col) {
        if (is.character(col)) NA_character_
        else if (is.logical(col)) NA
        else NA_real_
    })
    heading_row$variable  <- paste0(".heading_", gsub("\\s+", "_", heading_label))
    heading_row$var_type  <- "character"
    heading_row$row_type  <- "label"
    heading_row$label     <- paste0("**", heading_label, "**")
    heading_row$var_label <- paste0("**", heading_label, "**")
    
    tbl$table_body <- dplyr::bind_rows(
        body[seq_len(anchor_idx - 1L), , drop = FALSE],
        heading_row,
        body[seq(anchor_idx, nrow(body)), , drop = FALSE]
    )
    tbl
}

# ---------------------------------------------------------------------------
# Core gtsummary table builder (shared by overall and by-exposure functions).
# `baseline` must already be the single-row-per-participant display frame.
# ---------------------------------------------------------------------------
.CONTINUOUS_STAT <- "{mean} ({sd}); {median} [{p25}, {p75}]"

.build_tbl_summary <- function(baseline,
                               by              = NULL,
                               caption         = "**Table 1.** Participant characteristics",
                               footnote_suffix = "") {
    
    tbl <- baseline |>
        .normalize_table_types() |>
        gtsummary::tbl_summary(
            by        = if (!is.null(by)) dplyr::all_of(by) else NULL,
            label     = labels_for_data(baseline, names(baseline)),
            statistic = list(
                gtsummary::all_continuous()  ~ .CONTINUOUS_STAT,
                gtsummary::all_categorical() ~ "{n} ({p}%)"
            ),
            digits       = list(gtsummary::all_continuous() ~ 1),
            missing      = "ifany",
            missing_text = "Missing"
        ) |>
        gtsummary::add_n() |>
        gtsummary::modify_caption(caption) |>
        gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~ paste0(
                "Continuous variables: mean (SD); median [25th, 75th percentile]. ",
                "Categorical variables: n (%). ",
                "Missing shown only when present. ",
                "Gait speed from V4; ALMI from V3. ",
                "Severe sarcopenia could not be defined at baseline as gait speed was not measured at that visit.",
                footnote_suffix
            )
        ) |>
        gtsummary::bold_labels()
    
    if (!is.null(by)) {
        tbl <- tbl |> gtsummary::add_overall(last = FALSE)
    }
    
    # Insert group headings
    tbl <- .add_group_heading(tbl, .MEDICATION_VARS, "Medication")
    tbl <- .add_group_heading(tbl, .DAIRY_VARS,      "Dairy consumption")
    
    tbl
}

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

make_table_one <- function(analysis_long,
                           id      = "pt",
                           visit    = ".visit_osteo",
                           imp_col = ".imp") {
    
    if (is_imputed(analysis_long)) {
        baseline  <- make_display_baseline_imputed(
            analysis_long, id = id, visit = visit, imp_col = imp_col
        )
        fn_suffix <- " Estimates pooled manually across imputations."
        return(.manual_table_one(
            baseline,
            id              = id,
            imp_col         = imp_col,
            include_vars    = .table_vars_for_data(baseline),
            caption         = "**Table 1.** Participant characteristics",
            footnote_suffix = fn_suffix
        ))
    } else {
        baseline  <- make_display_baseline(analysis_long, id = id, visit = visit)
        fn_suffix <- ""
    }
    
    baseline <- baseline |> dplyr::select(dplyr::all_of(.table_vars_for_data(baseline)))
    
    
    .build_tbl_summary(
        baseline,
        caption         = "**Table 1.** Participant characteristics",
        footnote_suffix = fn_suffix
    )
}

make_table_one_by_exposure <- function(analysis_long,
                                       by      = "baseline_dairy_quartile",
                                       id      = "pt",
                                       visit    = ".visit_osteo",
                                       imp_col = ".imp") {
    
    if (is_imputed(analysis_long)) {
        baseline  <- make_display_baseline_imputed(
            analysis_long, id = id, visit = visit, imp_col = imp_col
        )
        fn_suffix <- " No significance tests are shown. Estimates pooled manually across imputations."
        
        baseline <- baseline |>
            dplyr::select(dplyr::any_of(c(.TABLE_VARS, by, id, imp_col))) |>
            .normalize_table_types()
        
        if (!by %in% names(baseline) || all(is.na(baseline[[by]]))) {
            warning("Exposure grouping variable `", by, "` is absent or all missing.")
            return(NULL)
        }
        
        return(.manual_table_one(
            baseline,
            by              = by,
            id              = id,
            imp_col         = imp_col,
            include_vars    = setdiff(.table_vars_for_data(baseline), by),
            caption         = "**Table 1.** Participant characteristics by exposure group",
            footnote_suffix = fn_suffix
        ))
    } else {
        baseline  <- make_display_baseline(analysis_long, id = id, visit = visit)
        fn_suffix <- ""
    }
    
    baseline <- baseline |>
        dplyr::select(dplyr::any_of(c(.TABLE_VARS, by))) |>
        .normalize_table_types()
    
    if (!by %in% names(baseline) || all(is.na(baseline[[by]]))) {
        warning("Exposure grouping variable `", by, "` is absent or all missing.")
        return(NULL)
    }
    
    .build_tbl_summary(
        baseline,
        by              = by,
        caption         = "**Table 1.** Participant characteristics by exposure group",
        footnote_suffix = paste0(" No significance tests are shown.", fn_suffix)
    )
}

# ---------------------------------------------------------------------------
# Saving helpers
# ---------------------------------------------------------------------------

save_gtsummary_table <- function(tbl, path_without_extension) {
    html_path <- paste0(path_without_extension, ".html")
    csv_path  <- paste0(path_without_extension, ".csv")
    
    if (inherits(tbl, "manual_table_one")) {
        gt::gtsave(tbl$gt, html_path)
        utils::write.csv(tbl$table_body, csv_path, row.names = FALSE)
    } else {
        gt::gtsave(gtsummary::as_gt(tbl), html_path)
        utils::write.csv(gtsummary::as_tibble(tbl), csv_path, row.names = FALSE)
    }
    
    c(html = html_path, csv = csv_path)
}

save_table_one_outputs <- function(analysis_long,
                                   output_root = "06_outputs/TableOne",
                                   date        = Sys.Date(),
                                   time        = Sys.time(),
                                   by          = "baseline_dairy_quartile",
                                   id          = "pt",
                                   visit        = ".visit_osteo",
                                   imp_col     = ".imp") {
    
    # Sub-folder: cc (complete case) or mice (imputed)
    method_label <- if (is_imputed(analysis_long)) "mice" else "cc"
    output_dir   <- file.path(output_root, paste(format(date, "%Y-%m-%d"), format(time, "%H:%M")), method_label)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    overall     <- make_table_one(analysis_long, id = id, visit = visit, imp_col = imp_col)
    by_exposure <- make_table_one_by_exposure(analysis_long, by = by,
                                              id = id, visit = visit, imp_col = imp_col)
    
    saved <- list(
        overall = save_gtsummary_table(
            overall,
            file.path(output_dir, "Table1_overall")
        )
    )
    
    if (!is.null(by_exposure)) {
        saved$by_exposure <- save_gtsummary_table(
            by_exposure,
            file.path(output_dir, paste0("Table1_by_", by))
        )
    }
    
    message("Tables saved to: ", output_dir)
    
    invisible(list(
        output_dir = output_dir,
        method     = method_label,
        tables     = list(overall = overall, by_exposure = by_exposure),
        files      = saved
    ))
}
