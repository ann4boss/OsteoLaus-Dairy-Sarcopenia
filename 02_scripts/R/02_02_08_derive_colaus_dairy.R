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
#   <var>_cumavg_lag1 Same running mean but lagged by one wave (i.e. the
#                     cumulative average computed up to the *preceding* visit).
#                     Intended for sensitivity analyses. NA at the first visit.
#
# Behaviour with missing values
# ─────────────────────────────
# • A missing value at a given visit does NOT break the cumulative average;
#   it is simply skipped (na.rm = TRUE within the running mean).
# • However, if *no* non-missing observation exists up to and including the
#   current visit, the cumulative average is NA (not 0).
# • The lag-1 variable is NA at the first visit (no prior wave exists) and
#   carries NA forward whenever the underlying cumulative average is NA.
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
derive_dairy_cumavg <- function(df,
                                id_col    = "pt",
                                visit_col = ".visit") {
    
    # ── Checks ---------------------------------------------------------------
    .DAIRY_VARS <- c(
        "dairy_total_gday",
        "dairy_fermented_gday",
        "dairy_non_fermented_gday",
        "dairy_lowfat_gday",
        "dairy_highfat_gday"
    )
    
    missing_dairy <- setdiff(.DAIRY_VARS, names(df))
    if (length(missing_dairy) > 0) {
        cli::cli_inform(c(
            "x" = "derive_dairy_cumavg: Missing dairy sub-category columns.",
            "i" = "Run {.fn derive_dairy} before calling this function.",
            "i" = "Missing: {.val {missing_dairy}}"
        ))
    }
    
    for (col in c(id_col, visit_col)) {
        if (!col %in% names(df)) {
            cli::cli_inform(
                "derive_dairy_cumavg: Column {.val {col}} not found in data."
            )
        }
    }
    
    # ── Safe visit handling --------------------------------------------------
    if (is.factor(df[[visit_col]])) {
        if (!is.ordered(df[[visit_col]])) {
            cli::cli_inform(c(
                "!" = "visit column is an unordered factor; treating level order as visit order."
            ))
            df[[visit_col]] <- factor(df[[visit_col]], ordered = TRUE)
        }
    } else if (!is.numeric(df[[visit_col]])) {
        cli::cli_inform(c(
            "x" = "visit column must be numeric or factor."
        ))
        return(df)
    }
    
    # ── Helper: cumulative mean (computed once per vector) -------------------
    cumulative_mean_na <- function(x) {
        n <- length(x)
        out <- numeric(n)
        
        run_sum <- 0
        run_n   <- 0
        
        for (i in seq_len(n)) {
            if (!is.na(x[i])) {
                run_sum <- run_sum + x[i]
                run_n   <- run_n + 1
            }
            out[i] <- if (run_n == 0) NA_real_ else run_sum / run_n
        }
        out
    }
    
    # ── Ordering -------------------------------------------------------------
    df <- df |>
        dplyr::mutate(.row_order = dplyr::row_number()) |>
        dplyr::arrange(.data[[id_col]], .data[[visit_col]])
    
    # ── Core computation (NO recomputation of cumulative means) --------------
    df <- dplyr::group_by(df, .data[[id_col]])
    
    for (v in .DAIRY_VARS) {
        
        cum <- cumulative_mean_na(df[[v]])
        
        df[[paste0(v, "_cumavg")]] <- cum
        df[[paste0(v, "_cumavg_lag1")]] <- dplyr::lag(cum, 1L, default = NA_real_)
    }
    
    df <- df |>
        dplyr::ungroup() |>
        dplyr::arrange(.row_order) |>
        dplyr::select(-.row_order)
    
    # ── Summary --------------------------------------------------------------
    new_cols <- unlist(lapply(.DAIRY_VARS, function(v) {
        c(paste0(v, "_cumavg"),
          paste0(v, "_cumavg_lag1"))
    }))
    
    valid_counts <- vapply(new_cols, function(col) {
        sum(!is.na(df[[col]]))
    }, integer(1))
    
    visit_order_msg <- if (is.factor(df[[visit_col]])) {
        paste("factor level order:", paste(levels(df[[visit_col]]), collapse = ", "))
    } else {
        paste("ascending numeric", visit_col)
    }
    
    cli::cli_h2("Cumulative average dairy derivation")
    cli::cli_inform(c(
        "v" = "derive_dairy_cumavg: 10 new columns added.",
        "i" = paste0("Visit ordering: ", visit_order_msg),
        "i" = "Non-missing counts per new column:"
    ))
    cli::cli_dl(setNames(as.list(valid_counts), new_cols))
    
    return(df)
}