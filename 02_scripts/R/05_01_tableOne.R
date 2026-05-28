# =============================================================================
# R/tableOne.R
# =============================================================================
# Table 1 builder supporting complete-case (cc) and multiply-imputed (mice)
# datasets.
#
# Accepts either:
#   - A plain data.frame (complete-case or already-filtered baseline)
#   - A `mids` object from mice (preferred – avoids manual complete() calls)
#   - A long-format imputed data.frame with a `.imp` column
#
# Pooling (imputed data):
#   Continuous  – mean pooled via simple average (correct for descriptive
#                 Table 1); pooled SD = mean of within-imputation SDs
#                 (per van Buuren 2018 §9.5 "NOTE").
#                 `pool.scalar()` is used to also compute a pooled SE for
#                 reference / use in downstream tests if needed.
#   Categorical – proportions averaged across imputations (correct for
#                 descriptive purposes; see van Buuren 2018 §9.5).
#
# Saves output to (optional, call save_table_one_outputs()):
#   06_outputs/TableOne/Date_YYYY-MM-DD/<cc|mice>/
# =============================================================================

# ---------------------------------------------------------------------------
# Variable metadata
# ---------------------------------------------------------------------------
.TABLE_LABELS <- list(
    Age                      ~ "Age",
    education_level          ~ "Education level (ISCED)",
    mrtsts2                  ~ "Marital status",
    BMI_category             ~ "BMI category",
    smoking_status           ~ "Smoking status",
    alcohol_category_conso   ~ "Alcohol intake",
    alcohol_category_sumalco ~ "Alcohol intake",
    pa_levels_tertile_f1     ~ "Physical activity level",
    pa_levels_tertile_f2     ~ "Physical activity level",
    pa_levels_who_f1         ~ "Physical activity level",
    pa_levels_who_f2         ~ "Physical activity level",
    diabetes_status          ~ "Diabetes status",
    htn_status               ~ "Hypertension",
    hrt_status               ~ "HRT status",
    cdv_event                ~ "CVD event (any)",
    hypolip_drug_status      ~ "Lipid-lowering medication",
    corticoids_status        ~ "Systemic corticosteroids",
    calcium_status           ~ "Calcium supplement",
    vitD_status              ~ "Vitamin D supplement",
    bisphosphonate_status    ~ "Bisphosphonate use",
    benzo_status             ~ "Benzo use",
    sumtot1                  ~ "Caloric intake (kcal/day)",
    dairy_total_gday         ~ "Total dairy intake (g/day)",
    dairy_fermented_gday     ~ "Fermented dairy (g/day)",
    dairy_non_fermented_gday ~ "Non-fermented dairy (g/day)",
    dairy_lowfat_gday        ~ "Low-fat dairy (g/day)",
    dairy_highfat_gday       ~ "High-fat dairy (g/day)",
    HGS_MAX                  ~ "Grip strength (kg)",
    ALM_HT2_harmonised       ~ "ALMI (kg/m\u00b2)",
    gait_speed               ~ "Gait speed at V4 (m/s)",
    ewgsop2_sarcopenia_stage ~ "EWGSOP2 sarcopenia stage",
    fnih_sarcopenia          ~ "FNIH Sarcopenia"
)

.TABLE_VARS <- c(
    "Age", "education_level", "mrtsts2", "BMI_category",
    "smoking_status", "alcohol_category_conso", "pa_levels_tertile_f1",
    "diabetes_status", "htn_status", "hrt_status",
    "hypolip_drug_status", "corticoids_status", "calcium_status",
    "vitD_status", "bisphosphonate_status", "sumtot1",
    "dairy_total_gday", "dairy_fermented_gday", "dairy_non_fermented_gday",
    "dairy_lowfat_gday", "dairy_highfat_gday",
    "ewgsop2_sarcopenia_stage", "HGS_MAX", "ALM_HT2_harmonised", "gait_speed"
)

.MEDICATION_VARS <- c(
    "hypolip_drug_status", "corticoids_status",
    "calcium_status", "vitD_status", "bisphosphonate_status"
)

.DAIRY_VARS <- c(
    "dairy_total_gday", "dairy_fermented_gday", "dairy_non_fermented_gday",
    "dairy_lowfat_gday", "dairy_highfat_gday"
)

# ---------------------------------------------------------------------------
# Small utilities
# ---------------------------------------------------------------------------

#' Filter TABLE_VARS to columns that actually exist in `data`.
.table_vars_for_data <- function(data, extra_vars = character(0)) {
    intersect(c(.TABLE_VARS, extra_vars), names(data))
}

#' Drop ordered-factor status so gtsummary treats levels as unordered.
.normalize_table_types <- function(data) {
    data |>
        dplyr::mutate(
            dplyr::across(
                where(is.ordered),
                ~ factor(as.character(.x), levels = levels(.x), ordered = FALSE)
            )
        )
}

#' Resolve the human-readable label for a single variable name.
.label_for_var <- function(var) {
    hit <- Filter(
        function(f) identical(all.vars(f[[2]]), var),
        .TABLE_LABELS
    )
    if (length(hit) == 0L) return(var)
    as.character(hit[[1]][[3]])
}

#' Return only the label formulas whose LHS variable is present in `vars`.
labels_for_data <- function(data, vars = names(data)) {
    vars <- intersect(vars, names(data))
    keep <- vapply(
        .TABLE_LABELS,
        function(f) all.vars(f[[2]]) %in% vars,
        logical(1)
    )
    .TABLE_LABELS[keep]
}

# ---------------------------------------------------------------------------
# Input normalisation: accept mids objects or long-format data frames
# ---------------------------------------------------------------------------

#' TRUE when `data` is a mids object (mice package).
is_mids <- function(data) inherits(data, "mids")

#' TRUE when `data` is a long-format imputed data frame (has `.imp` column
#' with more than one imputation index).
is_imputed_long <- function(data) {
    is.data.frame(data) &&
        ".imp" %in% names(data) &&
        length(unique(data[[".imp"]])) > 1
}

#' TRUE for any imputed input (mids or long).
is_imputed <- function(data) is_mids(data) || is_imputed_long(data)

#' Coerce a mids object to a long-format data frame (imputations only,
#' `.imp > 0`).  Returns unchanged if already a data frame.
.to_long <- function(data, imp_col = ".imp") {
    if (is_mids(data)) {
        long <- mice::complete(data, action = "long", include = FALSE)
        # mice names the imputation column `.imp` by default – rename if needed
        if (imp_col != ".imp" && ".imp" %in% names(long)) {
            names(long)[names(long) == ".imp"] <- imp_col
        }
        return(long)
    }
    # Already a long data frame: strip original (imp == 0) if present
    if (is_imputed_long(data)) {
        return(data[data[[imp_col]] > 0, , drop = FALSE])
    }
    data
}

# ---------------------------------------------------------------------------
# Rubin's rules pooling helpers (van Buuren 2018, §9.5)
# ---------------------------------------------------------------------------

#' Sorted unique imputation indices from a long data frame.
.imp_values <- function(data, imp_col) {
    vals <- sort(unique(data[[imp_col]]))
    vals[!is.na(vals) & vals > 0]
}

#' Compute Rubin's-rules pooled mean and SE for a continuous variable,
#' plus the descriptive SD (mean of within-imputation SDs, per van Buuren).
#'
#' Uses mice::pool.scalar() internally, consistent with §9.5 example code.
#'
#' @param x_by_imp Named list: one numeric vector per imputation.
#' @param n        Total number of observations (complete-data n).
#' @return list(mean, sd, se, median, p25, p75)
.pool_continuous_rubin <- function(x_by_imp, n) {
    m     <- length(x_by_imp)
    means   <- vapply(x_by_imp, mean,   numeric(1), na.rm = TRUE)
    vars    <- vapply(x_by_imp, stats::var, numeric(1), na.rm = TRUE)
    sds     <- vapply(x_by_imp, stats::sd, numeric(1), na.rm = TRUE)
    medians <- vapply(x_by_imp, stats::median, numeric(1), na.rm = TRUE)
    p25s    <- vapply(x_by_imp, function(x) stats::quantile(x, .25, na.rm = TRUE, names = FALSE), numeric(1))
    p75s    <- vapply(x_by_imp, function(x) stats::quantile(x, .75, na.rm = TRUE, names = FALSE), numeric(1))
    
    # Rubin's rules via pool.scalar (Q = per-imputation statistic,
    # U = within-imputation variance of that statistic = var/n)
    pooled <- mice::pool.scalar(Q = means, U = vars / n, n = n, k = 1)
    
    list(
        mean   = pooled$qbar,                 # pooled mean
        sd     = mean(sds),                   # descriptive SD (mean of SDs)
        se     = sqrt(pooled$t),              # pooled SE (sqrt total variance)
        median = mean(medians),               # pooled median (average)
        p25    = mean(p25s),
        p75    = mean(p75s)
    )
}

#' Pool a categorical variable across imputations.
#' Returns a tibble with columns: level, n (avg count), p (avg %, 0-100).
#' Averaging proportions is the correct approach for descriptive stats
#' (van Buuren 2018 §9.5).
.pool_categorical_rubin <- function(x_by_imp, denom) {
    all_levels <- sort(unique(unlist(lapply(x_by_imp, function(x) {
        as.character(x[!is.na(x)])
    }))))
    has_missing <- any(vapply(x_by_imp, function(x) any(is.na(x)), logical(1)))
    if (has_missing) all_levels <- c(all_levels, "Missing")
    
    m <- length(x_by_imp)
    
    purrr::map_dfr(all_levels, function(lvl) {
        per_imp_p <- vapply(x_by_imp, function(x) {
            n_total <- length(x)
            n_lvl   <- if (identical(lvl, "Missing")) sum(is.na(x)) else sum(as.character(x) == lvl, na.rm = TRUE)
            if (n_total == 0L) NA_real_ else 100 * n_lvl / n_total
        }, numeric(1))
        
        avg_p <- mean(per_imp_p, na.rm = TRUE)
        tibble::tibble(
            level = lvl,
            n     = round(denom * avg_p / 100),
            p     = avg_p
        )
    })
}

# ---------------------------------------------------------------------------
# Convenience wrappers that work on a subset of the long data frame
# ---------------------------------------------------------------------------

.pool_continuous_for_group <- function(data, var, imp_col,
                                       group_col = NULL, group_value = NULL) {
    imps <- .imp_values(data, imp_col)
    x_by_imp <- lapply(imps, function(i) {
        d <- data[data[[imp_col]] == i, , drop = FALSE]
        if (!is.null(group_col))
            d <- d[as.character(d[[group_col]]) == group_value, , drop = FALSE]
        d[[var]]
    })
    # Use the average n across imputations as the denominator for pool.scalar
    n_avg <- round(mean(vapply(x_by_imp, function(x) sum(!is.na(x)), integer(1))))
    if (n_avg == 0L) return(list(n = 0L, stat = "—"))
    
    r <- .pool_continuous_rubin(x_by_imp, n_avg)
    list(
        n    = n_avg,
        stat = sprintf(
            "%.1f (%.1f); %.1f [%.1f, %.1f]",
            r$mean, r$sd, r$median, r$p25, r$p75
        )
    )
}

.pool_categorical_for_group <- function(data, var, imp_col,
                                        group_col = NULL, group_value = NULL) {
    imps <- .imp_values(data, imp_col)
    x_by_imp <- lapply(imps, function(i) {
        d <- data[data[[imp_col]] == i, , drop = FALSE]
        if (!is.null(group_col))
            d <- d[as.character(d[[group_col]]) == group_value, , drop = FALSE]
        d[[var]]
    })
    denom <- round(mean(vapply(x_by_imp, length, integer(1))))
    pooled <- .pool_categorical_rubin(x_by_imp, denom)
    pooled$stat <- sprintf("%.0f (%.1f%%)", pooled$n, pooled$p)
    pooled
}

# ---------------------------------------------------------------------------
# Baseline display frame builders
# ---------------------------------------------------------------------------

#' One row per participant for the baseline table (complete-case path).
#' Gait speed pulled from V4; ALMI from V3.
make_display_baseline <- function(analysis_long,
                                  id             = "pt",
                                  visit          = ".visit_osteo",
                                  baseline_visit = "Baseline",
                                  gait_visit     = "V4",
                                  almi_visit     = "V3") {
    if (!all(c(id, visit) %in% names(analysis_long)))
        stop("`analysis_long` must contain `", id, "` and `", visit, "`.", call. = FALSE)
    
    baseline <- analysis_long |> dplyr::filter(.data[[visit]] == baseline_visit)
    
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

#' One row per participant per imputation (imputed path).
make_display_baseline_imputed <- function(analysis_long,
                                          id             = "pt",
                                          visit          = ".visit_osteo",
                                          imp_col        = ".imp",
                                          baseline_visit = "Baseline",
                                          gait_visit     = "V4",
                                          almi_visit     = "V3") {
    if (!all(c(id, visit, imp_col) %in% names(analysis_long)))
        stop("`analysis_long` must contain `", id, "`, `", visit, "`, `", imp_col, "`.", call. = FALSE)
    
    join_by_imp <- c(id, imp_col)
    analysis_long <- analysis_long[analysis_long[[imp_col]] > 0, , drop = FALSE]
    
    baseline <- analysis_long |> dplyr::filter(.data[[visit]] == baseline_visit)
    
    if ("gait_speed" %in% names(analysis_long)) {
        gait_pool <- analysis_long |>
            dplyr::filter(.data[[visit]] == gait_visit, !is.na(.data$gait_speed)) |>
            dplyr::group_by(.data[[id]], .data[[imp_col]]) |>
            dplyr::slice(1) |>
            dplyr::ungroup() |>
            dplyr::select(dplyr::all_of(join_by_imp), gait_speed)
        baseline <- baseline |>
            dplyr::select(-dplyr::any_of("gait_speed")) |>
            dplyr::left_join(gait_pool, by = join_by_imp)
    }
    
    if ("ALM_HT2_Lunar" %in% names(analysis_long)) {
        almi_pool <- analysis_long |>
            dplyr::filter(.data[[visit]] == almi_visit, !is.na(.data$ALM_HT2_Lunar)) |>
            dplyr::group_by(.data[[id]], .data[[imp_col]]) |>
            dplyr::slice(1) |>
            dplyr::ungroup() |>
            dplyr::select(dplyr::all_of(join_by_imp), ALM_HT2_Lunar)
        baseline <- baseline |>
            dplyr::select(-dplyr::any_of("ALM_HT2_Lunar")) |>
            dplyr::left_join(almi_pool, by = join_by_imp)
    }
    baseline
}

# ---------------------------------------------------------------------------
# Manual table builder (imputed path)
# ---------------------------------------------------------------------------

.categorical_levels <- function(x) {
    vals <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))
    vals[!is.na(vals)]
}

.manual_group_levels <- function(data, by) {
    x <- data[[by]]
    lvls <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))
    intersect(lvls, unique(as.character(x[!is.na(x)])))
}

.insert_manual_heading <- function(body, group_vars, heading_label) {
    anchor_idx <- which(body$variable %in% group_vars)[1]
    if (is.na(anchor_idx)) return(body)
    heading_row         <- body[anchor_idx, , drop = FALSE]
    heading_row[]       <- lapply(heading_row, function(col) NA)
    heading_row$variable <- paste0(".heading_", gsub("\\s+", "_", heading_label))
    heading_row$row_type <- "heading"
    heading_row$label    <- paste0("**", heading_label, "**")
    heading_row$level    <- ""
    dplyr::bind_rows(
        body[seq_len(anchor_idx - 1L), , drop = FALSE],
        heading_row,
        body[seq(anchor_idx, nrow(body)), , drop = FALSE]
    )
}

# In R/tableOne.R — .make_manual_table_object()

.make_manual_table_object <- function(table_body, caption, footnote) {
    stat_cols <- grep("^stat_", names(table_body), value = TRUE)
    
    display <- table_body |>
        dplyr::select(label, level, row_type, dplyr::all_of(stat_cols))  # <-- add row_type
    
    stat_labels <- as.list(sub("^stat_", "", stat_cols))
    names(stat_labels) <- stat_cols
    
    gt_tbl <- display |>
        gt::gt() |>
        gt::tab_header(title = gt::md(caption)) |>
        gt::cols_label(
            .list = c(list(label = "Characteristic", level = ""), stat_labels)
            # row_type intentionally not labelled — it will be hidden below
        ) |>
        gt::fmt_markdown(columns = dplyr::all_of("label")) |>
        gt::tab_source_note(gt::md(footnote)) |>
        gt::tab_style(
            style     = gt::cell_text(weight = "bold"),
            locations = gt::cells_body(
                columns = label,
                rows    = row_type %in% c("heading", "label")  # now found
            )
        ) |>
        gt::cols_hide("row_type") |>                           # now found
        gt::opt_stylize(style = 1, color = "gray") |>
        gt::opt_table_font(font = gt::google_font("IBM Plex Sans"))
    
    structure(
        list(table_body = table_body, display = display, gt = gt_tbl),
        class = "manual_table_one"
    )
}

#' Build a pooled Table 1 from an imputed long data frame.
.manual_table_one <- function(data,
                              by              = NULL,
                              id              = "pt",
                              imp_col         = ".imp",
                              include_vars    = NULL,
                              caption         = "**Table 1.** Participant characteristics",
                              footnote_suffix = "") {
    data <- .normalize_table_types(data)
    if (is.null(include_vars)) include_vars <- .table_vars_for_data(data)
    include_vars    <- setdiff(intersect(include_vars, names(data)), c(id, imp_col, by))
    continuous_vars <- include_vars[vapply(data[include_vars], is.numeric, logical(1))]
    categ_vars      <- setdiff(include_vars, continuous_vars)
    
    groups    <- if (is.null(by)) NULL else .manual_group_levels(data, by)
    stat_cols <- if (is.null(by)) "stat_0" else paste0("stat_", seq_along(groups))
    stat_names <- if (is.null(by)) "Overall" else groups
    
    make_row <- function(variable, row_type, level = "", stats) {
        row <- tibble::tibble(
            variable = variable,
            row_type = row_type,
            label    = .label_for_var(variable),
            level    = level
        )
        for (i in seq_along(stat_cols)) row[[stat_cols[[i]]]] <- stats[[i]]
        row
    }
    
    rows <- list()
    
    for (var in continuous_vars) {
        stats <- if (is.null(by)) {
            list(.pool_continuous_for_group(data, var, imp_col)$stat)
        } else {
            purrr::map(groups, ~ .pool_continuous_for_group(data, var, imp_col, by, .x)$stat)
        }
        rows[[length(rows) + 1L]] <- make_row(var, "continuous", "", stats)
    }
    
    for (var in categ_vars) {
        if (is.null(by)) {
            pooled <- .pool_categorical_for_group(data, var, imp_col)
            for (i in seq_len(nrow(pooled)))
                rows[[length(rows) + 1L]] <- make_row(var, "categorical", pooled$level[[i]], list(pooled$stat[[i]]))
        } else {
            pooled_list <- purrr::map(groups, ~ .pool_categorical_for_group(data, var, imp_col, by, .x))
            levels_all  <- unique(unlist(purrr::map(pooled_list, "level")))
            for (lvl in levels_all) {
                stats <- purrr::map(pooled_list, function(x) {
                    hit <- dplyr::filter(x, .data$level == lvl)
                    if (nrow(hit) == 0L) "" else hit$stat[[1]]
                })
                rows[[length(rows) + 1L]] <- make_row(var, "categorical", lvl, stats)
            }
        }
    }
    
    body <- dplyr::bind_rows(rows)
    names(body)[match(stat_cols, names(body))] <- paste0("stat_", make.names(stat_names))
    body <- .insert_manual_heading(body, .MEDICATION_VARS, "Medication")
    body <- .insert_manual_heading(body, .DAIRY_VARS,      "Dairy consumption")
    
    footnote <- paste0(
        "Continuous: pooled mean (mean of within-imputation SDs); ",
        "pooled median [pooled Q1, Q3]. ",
        "Proportions averaged across imputations (van Buuren 2018, \u00a79.5). ",
        "Missing included in categorical denominators. ",
        "Gait speed from V4; ALMI from V3. ",
        "Severe sarcopenia could not be defined at baseline (gait speed not measured then).",
        footnote_suffix
    )
    
    .make_manual_table_object(body, caption, footnote)
}

# ---------------------------------------------------------------------------
# gtsummary path (complete-case)
# ---------------------------------------------------------------------------

.CONTINUOUS_STAT <- "{mean} ({sd}); {median} [{p25}, {p75}]"

.add_group_heading <- function(tbl, group_vars, heading_label) {
    body       <- tbl$table_body
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
                "Continuous: mean (SD); median [Q1, Q3]. ",
                "Categorical: n (%). ",
                "Missing shown only when present. ",
                "Gait speed from V4; ALMI from V3. ",
                "Severe sarcopenia not defined at baseline.",
                footnote_suffix
            )
        ) |>
        gtsummary::bold_labels()
    
    if (!is.null(by)) tbl <- tbl |> gtsummary::add_overall(last = FALSE)
    
    tbl <- .add_group_heading(tbl, .MEDICATION_VARS, "Medication")
    tbl <- .add_group_heading(tbl, .DAIRY_VARS,      "Dairy consumption")
    tbl
}

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Build overall Table 1.
#'
#' @param analysis_long A `mids` object, or a long data frame with a `.imp`
#'   column, or a plain complete-case data frame.
#' @param id       Participant ID column name.
#' @param visit    Visit column name (used to extract baseline row).
#' @param imp_col  Imputation index column (ignored for complete-case data).
#' @return A `manual_table_one` (imputed) or `gtsummary` object (cc).
make_table_one <- function(analysis_long,
                           id      = "pt",
                           visit   = ".visit_osteo",
                           imp_col = ".imp") {
    if (is_imputed(analysis_long)) {
        long      <- .to_long(analysis_long, imp_col)
        baseline  <- make_display_baseline_imputed(long, id = id, visit = visit, imp_col = imp_col)
        return(.manual_table_one(
            baseline,
            id              = id,
            imp_col         = imp_col,
            include_vars    = .table_vars_for_data(baseline),
            caption         = "**Table 1.** Participant characteristics",
            footnote_suffix = " Estimates pooled across imputations."
        ))
    }
    
    baseline <- make_display_baseline(analysis_long, id = id, visit = visit) |>
        dplyr::select(dplyr::all_of(.table_vars_for_data(analysis_long)))
    
    .build_tbl_summary(
        baseline,
        caption = "**Table 1.** Participant characteristics"
    )
}

#' Build Table 1 stratified by an exposure variable.
#'
#' @param by Grouping / exposure variable name.
#' @inheritParams make_table_one
make_table_one_by_exposure <- function(analysis_long,
                                       by      = "baseline_dairy_quartile",
                                       id      = "pt",
                                       visit   = ".visit_osteo",
                                       imp_col = ".imp") {
    if (is_imputed(analysis_long)) {
        long      <- .to_long(analysis_long, imp_col)
        baseline  <- make_display_baseline_imputed(long, id = id, visit = visit, imp_col = imp_col) |>
            dplyr::select(dplyr::any_of(c(.TABLE_VARS, by, id, imp_col))) |>
            .normalize_table_types()
        
        if (!by %in% names(baseline) || all(is.na(baseline[[by]]))) {
            warning("Grouping variable `", by, "` absent or all-missing.")
            return(NULL)
        }
        
        return(.manual_table_one(
            baseline,
            by              = by,
            id              = id,
            imp_col         = imp_col,
            include_vars    = setdiff(.table_vars_for_data(baseline), by),
            caption         = "**Table 1.** Participant characteristics by exposure group",
            footnote_suffix = " No significance tests shown. Estimates pooled across imputations."
        ))
    }
    
    baseline <- make_display_baseline(analysis_long, id = id, visit = visit) |>
        dplyr::select(dplyr::any_of(c(.TABLE_VARS, by))) |>
        .normalize_table_types()
    
    if (!by %in% names(baseline) || all(is.na(baseline[[by]]))) {
        warning("Grouping variable `", by, "` absent or all-missing.")
        return(NULL)
    }
    
    .build_tbl_summary(
        baseline,
        by              = by,
        caption         = "**Table 1.** Participant characteristics by exposure group",
        footnote_suffix = " No significance tests shown."
    )
}

# ---------------------------------------------------------------------------
# Rendering helpers
# ---------------------------------------------------------------------------

#' Convert a table object to a gt table (for inline Quarto rendering).
#' Works for both manual_table_one and gtsummary objects.
as_gt_table <- function(tbl) {
    if (inherits(tbl, "manual_table_one")) return(tbl$gt)
    gtsummary::as_gt(tbl)
}

# ---------------------------------------------------------------------------
# File saving (optional; not needed for Quarto inline rendering)
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
                                   output_root = "03_outputs/TableOne",
                                   date        = Sys.Date(),
                                   time        = Sys.time(),
                                   by          = "dairy_quartile_baseline",
                                   id          = "pt",
                                   visit       = ".visit_osteo",
                                   imp_col     = ".imp") {
    method_label <- if (is_imputed(analysis_long)) "mice" else "cc"
    output_dir   <- file.path(
        output_root,
        paste(format(date, "%Y-%m-%d"), format(time, "%H-%M")),
        method_label
    )
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    overall     <- make_table_one(analysis_long, id = id, visit = visit, imp_col = imp_col)
    by_exposure <- make_table_one_by_exposure(analysis_long, by = by,
                                              id = id, visit = visit, imp_col = imp_col)
    
    saved <- list(
        overall = save_gtsummary_table(overall, file.path(output_dir, "Table1_overall"))
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

# R/tableOne.R

#' Variant of save_table_one_outputs() for use in targets pipelines.
#'
#' Accepts pre-built table objects (already computed as separate targets)
#' rather than rebuilding them, and returns a plain character vector of
#' file paths suitable for format = "file" targets.
#'
#' The date subfolder is fixed at the date the target first runs and then
#' cached — timestamps are dropped to avoid a new folder on every tar_make().
save_table_one_outputs_targets <- function(overall,
                                           by_exposure,
                                           output_root = "03_outputs/TableOne",
                                           by          = "baseline_dairy_quartile",
                                           method      = c("cc", "mice")) {
    method     <- match.arg(method)
    output_dir <- file.path(output_root, method)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    paths <- save_gtsummary_table(overall, file.path(output_dir, "Table1_overall"))
    
    if (!is.null(by_exposure)) {
        paths <- c(paths, save_gtsummary_table(
            by_exposure,
            file.path(output_dir, paste0("Table1_by_", by))
        ))
    }
    
    message("Tables saved to: ", output_dir)
    unname(paths)   # format = "file" requires a plain character vector
}