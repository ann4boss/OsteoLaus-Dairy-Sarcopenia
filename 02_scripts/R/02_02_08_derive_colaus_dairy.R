# =============================================================================
# R/derive_colaus_dairy.R
# =============================================================================
# Derives dairy intake by summing FFQ amount columns.
#
# Each FFQ item contributes to one or more sub-categories as specified in the
# data dictionary (see below).
#
# Sub-category definitions
# ─────────────────────────
#   dairy_total_gday         all dairy items
#   dairy_fermented_gday     fermented dairy (yoghurt, cheese)
#   dairy_non_fermented_gday non-fermented dairy (milk products)
#   dairy_lowfat_gday        low-fat dairy items
#   dairy_highfat_gday       high-fat dairy items
#
# Data dictionary
# ─────────────────────────
#   FFQ1amount  -> total, fermented, high-fat        plain yogurt
#   FFQ2amount  -> total, fermented, low-fat         low-fat yogurt
#   FFQ3amount  -> total, fermented, high-fat        fruit yogurt
#   FFQ4amount  -> total, fermented, low-fat         cottage cheese 0%
#   FFQ5amount  -> total, fermented, high-fat        cottage cheese/ricotta
#   FFQ6amount  -> total, fermented, high-fat        feta/mozzarella
#   FFQ7amount  -> total, fermented, high-fat        gruyere/tomme/camembert
#   FFQ8amount  -> total, fermented, high-fat        cheese fondue
#   FFQ52amount -> total, non-fermented, high-fat    butter
#   FFQ53amount -> total, non-fermented, high-fat    Cream 35% (g/day)
#   FFQ63amount -> total, non-fermented, high-fat    cream tart/cake
#   FFQ68amount -> total, non-fermented, high-fat    ice cream/sorbet (assumed dairy)
#   FFQ71amount -> total, non-fermented, high-fat    butter for cooking
#   FFQ82amount -> total, non-fermented, low-fat     milk in coffee 0%
#   FFQ83amount -> total, non-fermented, high-fat    milk in coffee non-0%
#   FFQ84amount -> total, non-fermented, high-fat    coffee creamer
#   FFQ85amount -> total, non-fermented, lowf-at     milk drink 0%
#   FFQ86amount -> total, non-fermented, high-fat    milk drink non-0%
#
# Assumption 1: some products are recorded as mL/day and others as g/day. We assume
#               that the density of milk product is roughly 1 and we can therefore
#               directly sum up all products regardless of unit.
# Assumption 2: ice cream/sorbet is assumed a dairy product
# =============================================================================

#' Derive dairy sub-category intakes (g/day) for a CoLaus long tibble.
#' 
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with dairy_total_gday, dairy_fermented_gday,
#'   dairy_non_fermented_gday, dairy_lowfat_gday, dairy_highfat_gday
#'   (all numeric, g/day) added.
derive_dairy <- function(df) {
    
    # Item membership per sub-category (base column names, no visit prefix)
    .DAIRY_TOTAL        <- paste0("FFQ", c(1:8, 52, 53, 63, 68, 71, 82:86), "amount")
    .DAIRY_FERMENT      <- paste0("FFQ", c(1:8), "amount")
    .DAIRY_NON_FERM     <- paste0("FFQ", c(52, 53, 63, 68, 71, 82:86), "amount")
    .DAIRY_LOWFAT       <- paste0("FFQ", c(2, 4, 82, 85), "amount")
    .DAIRY_HIGHFAT      <- paste0("FFQ", c(1, 3, 5, 6, 7, 8, 52, 53, 63, 68, 71, 83, 84, 86), "amount")
    
    
    # ── Column Check ----------------------------------------------
    required_cols <- .DAIRY_TOTAL
    actual_cols <- names(df)
    missing_cols <- setdiff(required_cols, actual_cols)
    
    if (length(missing_cols) > 0) {
        cli::cli_warn(c(
            "x" = "derive_dairy: Missing required FFQ amount columns.",
            "i" = "Missing: {.val {missing_cols}}",
            "!" = "Dairy categories cannot be calculated without these sources."
        ))
        return(df)
    }
    
    # ── Helper ---------------------------------------------------------------
    # Check first if any row of the specified columns are present. If none are present, return NA.
    # Then check per row if any of the specified column is NA. If yes, return NA
    # Otherwise, when all columns for a row are non-NA sum the specified columns.
    
    sum_block <- function(data, cols) {
        
        # subset relevant columns
        x <- data[, cols, drop = FALSE]
        
        # 1. dataset-level check: if all values across all selected columns are NA
        if (all(is.na(x))) {
            return(rep(NA_real_, nrow(data)))
        }
        
        # 2. row-level logic
        sums <- apply(x, 1, function(row) {
            
            # if any NA in the row -> NA
            if (any(is.na(row))) {
                return(NA_real_)
            }
            
            # otherwise sum
            sum(row)
        })
        
        return(sums)
    }

    
    # ── Derivation -----------------------------------------------------------
    df <- df |>
        dplyr::mutate(
            dairy_total_gday         = sum_block(df, .DAIRY_TOTAL),
            dairy_fermented_gday     = sum_block(df, .DAIRY_FERMENT),
            dairy_non_fermented_gday = sum_block(df, .DAIRY_NON_FERM),
            dairy_lowfat_gday        = sum_block(df, .DAIRY_LOWFAT),
            dairy_highfat_gday       = sum_block(df, .DAIRY_HIGHFAT)
        ) |>
        dplyr::as_tibble()
    
    # ── Summary -------------------------------------------------------------------
    stats <- df |>
        dplyr::summarise(
            n_exceed = sum(
                (dairy_fermented_gday + dairy_non_fermented_gday) >
                    (dairy_total_gday + 0.01),
                na.rm = TRUE
            ),
            valid_total      = sum(!is.na(dairy_total_gday)),
            valid_fermented  = sum(!is.na(dairy_fermented_gday)),
            valid_non_ferm   = sum(!is.na(dairy_non_fermented_gday)),
            valid_lowfat     = sum(!is.na(dairy_lowfat_gday)),
            valid_highfat    = sum(!is.na(dairy_highfat_gday))
        )
    

    if (stats$n_exceed > 0) {
        cli::cli_warn(
            "derive_dairy: {stats$n_exceed} row(s) where fermented + non_fermented exceeds total."
        )
    }
    
    cli::cli_h2("Dairy sub-category derivation")
    cli::cli_inform(c(
        "v" = "derive_dairy: derived variables added.",
        " " = "total: {stats$valid_total}",
        " " = "fermented: {stats$valid_fermented}",
        " " = "non-fermented: {stats$valid_non_ferm}",
        " " = "low-fat: {stats$valid_lowfat}",
        " " = "high-fat: {stats$valid_highfat}"
    ))
    
    return(df)
}



# =============================================================================
# Cumulative average dairy intake
# =============================================================================
#
# Cumulative average derivation
# ─────────────────────────────
# For each participant and each dairy sub-category, two additional variables
# are derived:
#
#   <var>_cumavg      Running mean of all non-missing values from OsteoLaus
#                     baseline (the lowest visit number) up to and including
#                     the current visit.
#
#
# =============================================================================

#' Derive cumulative average dairy intakes for a CoLaus long tibble.
#'
#' @param df  CoLaus long tibble (one row per participant × visit) that already
#'   contains the five dairy sub-category columns produced by `derive_dairy()`.
#' @param id_col    Name of the participant identifier column (default `"id"`).
#' @param visit_col Name of the numeric visit identifier column (default
#'   `"visit"`). Rows are sorted ascending on this column within each
#'   participant; no explicit visit order needs to be supplied.
#'
#' @return `df` with ten new columns added (cumavg + cumavg_lag1 for each of
#'   the five dairy sub-categories). Row order is restored to match the input.

# Cumulative (running) mean that skips NA and returns NA until the first
# non-missing observation is seen. Shared by derive_dairy_cumavg() and the
# per-item / non-dairy protein cumavg helpers in
# R/02_02_19_derive_colaus_dairy_protein.R.
cumulative_mean_na <- function(x) {

    out         <- numeric(length(x))
    running_sum <- 0
    running_n   <- 0

    for (i in seq_along(x)) {

        if (!is.na(x[i])) {
            running_sum <- running_sum + x[i]
            running_n   <- running_n + 1
        }

        out[i] <- if (running_n == 0) NA_real_ else running_sum / running_n
    }

    out
}

# Ensure a visit column is ordered (factor or numeric) before a grouped
# cumulative-average pass. Returns the checked/coerced df, or NULL if the
# column can't be ordered (caller should warn and bail out).
.check_visit_order <- function(df, visit_col) {

    if (is.factor(df[[visit_col]])) {

        if (!is.ordered(df[[visit_col]])) {

            cli::cli_inform(c(
                "!" = paste0(
                    visit_col,
                    " is an unordered factor. Using current factor level order."
                )
            ))

            df[[visit_col]] <- factor(
                df[[visit_col]],
                levels  = levels(df[[visit_col]]),
                ordered = TRUE
            )
        }

    } else if (!is.numeric(df[[visit_col]])) {

        cli::cli_warn(c(
            "x" = paste0(visit_col, " must be numeric or factor.")
        ))

        return(NULL)
    }

    df
}

derive_dairy_cumavg <- function(df,
                                id_col    = "pt",
                                visit_col = ".visit") {

    # ── Dairy variables ------------------------------------------------------
    .DAIRY_VARS <- c(
        "dairy_total_gday",
        "dairy_fermented_gday",
        "dairy_non_fermented_gday",
        "dairy_lowfat_gday",
        "dairy_highfat_gday"
    )

    # ── Checks ---------------------------------------------------------------
    missing_cols <- setdiff(c(id_col, visit_col, .DAIRY_VARS), names(df))

    if (length(missing_cols) > 0) {
        cli::cli_warn(c(
            "x" = "derive_dairy_cumavg: Missing required columns.",
            "i" = "Missing: {.val {missing_cols}}"
        ))
        return(df)
    }

    checked_df <- .check_visit_order(df, visit_col)
    if (is.null(checked_df)) return(df)
    df <- checked_df

    # ── Derivation -----------------------------------------------------------
    df <- df |>
        dplyr::mutate(.row_order = dplyr::row_number()) |>
        dplyr::arrange(
            .data[[id_col]],
            .data[[visit_col]]
        ) |>
        dplyr::group_by(.data[[id_col]]) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(.DAIRY_VARS),
                cumulative_mean_na,
                .names = "{.col}_cumavg"
            )
        ) |>
        dplyr::ungroup() |>
        dplyr::arrange(.row_order) |>
        dplyr::select(-.row_order)
    
    # ── Summary --------------------------------------------------------------
    new_cols <- paste0(.DAIRY_VARS, "_cumavg")
    
    valid_counts <- vapply(
        new_cols,
        function(x) sum(!is.na(df[[x]])),
        integer(1)
    )
    
    cli::cli_h2("Cumulative average dairy derivation")
    
    cli::cli_inform(c(
        "v" = "derive_dairy_cumavg: cumulative averages added.",
        "i" = paste0(
            "Visit order: ",
            paste(levels(df[[visit_col]]), collapse = " < ")
        )
    ))
    
    cli::cli_dl(setNames(as.list(valid_counts), new_cols))
    
    return(df)
}