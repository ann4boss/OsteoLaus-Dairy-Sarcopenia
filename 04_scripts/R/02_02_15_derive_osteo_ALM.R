# =============================================================================
# R/derive_osteolaus_alm.R
# =============================================================================
# Derives ALM-based body composition indices from DXA measurements.
#
#   ALM       Appendicular Lean Mass         sum limb lean mass        [g]
#   ALM_HT2   Appendicular Lean Mass index   ALM / (Height / 100)^2    [kg/m2]
#   ALM_BMI   ALM relative to BMI            ALM / BMI                 [kg/(kg/m2)]
#   ALM_WT    ALM relative to body weight    ALM / Weight              [unitless]
#
# Pre-existing columns with the same names are renamed to *_source before
# derived values are written, preserving originals for audit.
# =============================================================================

#' Derive ALM-based body composition indices for an OsteoLaus long tibble.
#'
#' @param df OsteoLaus long tibble after harmonisation and stacking.
#' @return df with ALM, ALM_HT2, ALM_BMI, and ALM_WT derived columns added.
derive_alm_indices <- function(df) {
    
    required_cols <- c(
        "LARM_LEAN_MASS", "RARM_LEAN_MASS",
        "LLEG_LEAN_MASS", "RLEG_LEAN_MASS",
        "Height", "BMI", "Weight"
    )
    
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0L) {
        cli::cli_warn(c(
            "derive_alm_indices(): missing required columns: {.val {missing_cols}}.",
            "i" = "ALM indices were not derived."
        ))
        return(df)
    }
    
    derived_cols <- c("ALM", "ALM_HT2", "ALM_BMI", "ALM_WT")
    source_cols <- paste0(derived_cols, "_source")
    
    existing_derived <- intersect(derived_cols, names(df))
    if (length(existing_derived) > 0L) {
        rename_from <- existing_derived
        rename_to <- paste0(existing_derived, "_source")
        
        source_collision <- intersect(rename_to, names(df))
        if (length(source_collision) > 0L) {
            cli::cli_abort(c(
                "derive_alm_indices() cannot preserve existing derived columns.",
                "x" = "Source backup columns already exist: {.val {source_collision}}",
                "i" = "Rename or remove those columns before deriving ALM indices."
            ))
        }
        
        df <- dplyr::rename(
            df,
            !!!stats::setNames(rename_from, rename_to)
        )
    }
    
    df <- df |>
        dplyr::mutate(
            ALM = dplyr::if_else(
                stats::complete.cases(dplyr::pick(dplyr::all_of(required_cols[1:4]))),
                round(LARM_LEAN_MASS + RARM_LEAN_MASS + LLEG_LEAN_MASS + RLEG_LEAN_MASS,
                    4
                ),
                NA_real_
            ),
            ALM_HT2 = dplyr::if_else(
                !is.na(ALM) & !is.na(Height) & Height > 0,
                round((ALM / 1000) / (Height / 100)^2, 4),
                NA_real_
            ),
            ALM_BMI = dplyr::if_else(
                !is.na(ALM) & !is.na(BMI) & BMI > 0,
                round((ALM / 1000) / BMI, 4),
                NA_real_
            ),
            ALM_WT = dplyr::if_else(
                !is.na(ALM) & !is.na(Weight) & Weight > 0,
                round((ALM / 1000) / Weight, 4),
                NA_real_
            )
        )
    
    cli::cli_h2("Derive ALM Indices")
    
    n_rows <- nrow(df)
    
    pct <- function(n, d) {
        if (d == 0L) NA_real_ else round(n / d * 100, 1)
    }
    
    fmt_num <- function(x, digits = 3L) {
        if (length(x) == 0L || all(is.na(x))) {
            "NA"
        } else {
            as.character(round(x, digits))
        }
    }
    
    summarise_index <- function(derived_col, source_col) {
        values <- df[[derived_col]]
        observed <- values[!is.na(values)]
        n_derived <- length(observed)
        n_missing <- n_rows - n_derived
        
        cli::cli_inform(c(
            "v" = "{.col {derived_col}} derived.",
            "i" = "Rows: {n_rows} | derived: {n_derived} | missing: {n_missing}",
            "i" = "Range: {fmt_num(min(observed), 3)} to {fmt_num(max(observed), 3)}",
            "i" = "Mean +/- SD: {fmt_num(mean(observed), 3)} +/- {fmt_num(stats::sd(observed), 3)}"
        ))
        
        if (!source_col %in% names(df)) {
            return(invisible(NULL))
        }
        
        both <- !is.na(df[[derived_col]]) & !is.na(df[[source_col]])
        n_both <- sum(both)
        
        if (n_both == 0L) {
            cli::cli_inform("i" = "Agreement with {.col {source_col}}: no overlapping non-missing values.")
            return(invisible(NULL))
        }
        
        diff <- abs(df[[derived_col]][both] - df[[source_col]][both])
        
        n_exact <- sum(diff < 0.001)
        n_close <- sum(diff >= 0.001 & diff < 0.1)
        n_differ <- sum(diff >= 0.1)
        
        cli::cli_inform(c(
            "i" = "Agreement with {.col {source_col}} (n = {n_both}):",
            "*" = "Difference < 0.001: {n_exact} ({pct(n_exact, n_both)}%)",
            "*" = "Difference 0.001 to 0.1: {n_close} ({pct(n_close, n_both)}%)",
            "*" = "Difference >= 0.1: {n_differ} ({pct(n_differ, n_both)}%)",
            "i" = "Median absolute difference: {fmt_num(stats::median(diff), 4)}",
            "i" = "Max absolute difference: {fmt_num(max(diff), 4)}"
        ))
        
        if (n_differ > 0L) {
            cli::cli_warn(
                "{n_differ} row(s) differ by >= 0.1 between derived {.col {derived_col}} and {.col {source_col}}."
            )
        }
        
        invisible(NULL)
    }
    
    purrr::walk2(derived_cols, source_cols, summarise_index)
    
    return(df)
}
