# =============================================================================
# R/derive_colaus_dairy_protein.R
# =============================================================================
# Per-FFQ-item cumulative averages and protein content for the 17 dairy FFQ
# items, plus cumulative average protein intake from non-dairy sources.
#
# Data dictionary — protein content (g protein / 100 g or 100 mL)
# ─────────────────────────────────────────────────────────────────
#   FFQ1amount  3.8   plain yoghurt
#   FFQ2amount  4.1   low-fat yoghurt
#   FFQ3amount  3.8   fruit yoghurt
#   FFQ4amount  8.0   cottage cheese 0%
#   FFQ5amount  8.0   cottage cheese/ricotta
#   FFQ6amount  15.0  feta/mozzarella
#   FFQ7amount  25.0  gruyere/tomme/camembert
#   FFQ8amount  3.8   cheese fondue
#   FFQ52amount 0.3   butter
#   FFQ53amount 2.2   cream 35% (g/day)
#   FFQ63amount 6.0   cream tart/cake
#   FFQ68amount 2.5   ice cream/sorbet
#   FFQ71amount 0.3   butter for cooking
#   FFQ82amount 3.4   milk in coffee 0%
#   FFQ83amount 3.4   milk in coffee non-0%
#   FFQ84amount 2.2   coffee creamer
#   FFQ85amount 3.4   milk drink 0%
#   FFQ86amount 3.4   milk drink non-0%
#
# Pipeline
# ─────────
#   derive_dairy_item_cumavg()      FFQ<n>amount        -> FFQ<n>amount_cumavg
#   derive_dairy_protein()          FFQ<n>amount_cumavg  -> Prot_content_FFQ<n>amount_cumavg
#                                    sum(Prot_content_*)  -> prot_content_dairy_cumavg
#   derive_nondairy_protein_cumavg() sumprot1             -> sumprot1_cumavg
#                                    sumprot1_cumavg - prot_content_dairy_cumavg
#                                                         -> prot_content_nondairy_cumavg
#
# Relies on `cumulative_mean_na()` and `.check_visit_order()`, both defined in
# R/02_02_08_derive_colaus_dairy.R.
# =============================================================================

.DAIRY_PROTEIN_CONTENT <- c(
    FFQ1amount  = 3.8,
    FFQ2amount  = 4.1,
    FFQ3amount  = 3.8,
    FFQ4amount  = 8.0,
    FFQ5amount  = 8.0,
    FFQ6amount  = 15.0,
    FFQ7amount  = 25.0,
    FFQ8amount  = 3.8,
    FFQ52amount = 0.3,
    FFQ53amount = 2.2,
    FFQ63amount = 6.0,
    FFQ68amount = 2.5,
    FFQ71amount = 0.3,
    FFQ82amount = 3.4,
    FFQ83amount = 3.4,
    FFQ84amount = 2.2,
    FFQ85amount = 3.4,
    FFQ86amount = 3.4
)

.DAIRY_ITEM_VARS <- names(.DAIRY_PROTEIN_CONTENT)


# ── Shared grouped cumulative-average helper ─────────────────────────────────
# Adds `<var>_cumavg` for each of `vars`, computed per `id_col`, ordered by
# `visit_col`. Row order is restored to match the input.
.add_grouped_cumavg <- function(df, vars, id_col, visit_col) {

    missing_cols <- setdiff(c(id_col, visit_col, vars), names(df))
    if (length(missing_cols) > 0) {
        cli::cli_warn(c(
            "x" = "Missing required columns for cumulative average.",
            "i" = "Missing: {.val {missing_cols}}"
        ))
        return(df)
    }

    checked_df <- .check_visit_order(df, visit_col)
    if (is.null(checked_df)) return(df)
    df <- checked_df

    df |>
        dplyr::mutate(.row_order = dplyr::row_number()) |>
        dplyr::arrange(.data[[id_col]], .data[[visit_col]]) |>
        dplyr::group_by(.data[[id_col]]) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(vars),
                cumulative_mean_na,
                .names = "{.col}_cumavg"
            )
        ) |>
        dplyr::ungroup() |>
        dplyr::arrange(.row_order) |>
        dplyr::select(-.row_order)
}


#' Derive per-item cumulative average intake for the 17 dairy FFQ items.
#'
#' @param df CoLaus long tibble containing the raw `FFQ<n>amount` columns.
#' @return `df` with `FFQ<n>amount_cumavg` added for each of the 17 items.
derive_dairy_item_cumavg <- function(df,
                                     id_col    = "pt",
                                     visit_col = ".visit") {

    out <- .add_grouped_cumavg(df, .DAIRY_ITEM_VARS, id_col, visit_col)

    new_cols     <- paste0(.DAIRY_ITEM_VARS, "_cumavg")
    valid_counts <- vapply(new_cols, function(x) sum(!is.na(out[[x]])), integer(1))

    cli::cli_h2("Per-item dairy FFQ cumulative average derivation")
    cli::cli_dl(setNames(as.list(valid_counts), new_cols))

    out
}


#' Derive dairy protein content from per-item cumulative averages.
#'
#' `Prot_content_FFQ<n>amount_cumavg = FFQ<n>amount_cumavg * content / 100`
#' `prot_content_dairy_cumavg = sum(Prot_content_FFQ<n>amount_cumavg)`
#'
#' @param df CoLaus long tibble containing `FFQ<n>amount_cumavg` for the 17
#'   dairy items (output of `derive_dairy_item_cumavg()`).
#' @return `df` with 17 `Prot_content_FFQ<n>amount_cumavg` columns and
#'   `prot_content_dairy_cumavg` added.
derive_dairy_protein <- function(df) {

    cumavg_cols <- paste0(.DAIRY_ITEM_VARS, "_cumavg")
    missing_cols <- setdiff(cumavg_cols, names(df))

    if (length(missing_cols) > 0) {
        cli::cli_warn(c(
            "x" = "derive_dairy_protein: Missing required cumavg columns.",
            "i" = "Missing: {.val {missing_cols}}",
            "!" = "Run derive_dairy_item_cumavg() first."
        ))
        return(df)
    }

    prot_cols <- paste0("Prot_content_", .DAIRY_ITEM_VARS, "_cumavg")

    df <- df |>
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(cumavg_cols),
                ~ .x * unname(.DAIRY_PROTEIN_CONTENT[sub("_cumavg$", "", dplyr::cur_column())]) / 100,
                .names = "Prot_content_{.col}"
            )
        )

    df$prot_content_dairy_cumavg <- rowSums(df[prot_cols])

    cli::cli_h2("Dairy protein content derivation")
    cli::cli_inform(c(
        "v" = "derive_dairy_protein: protein content columns added.",
        "i" = "prot_content_dairy_cumavg valid: {sum(!is.na(df$prot_content_dairy_cumavg))}"
    ))

    df
}


#' Derive cumulative average protein intake from non-dairy sources.
#'
#' `sumprot1_cumavg = cumulative_mean_na(sumprot1)`
#' `prot_content_nondairy_cumavg = sumprot1_cumavg - prot_content_dairy_cumavg`
#'
#' @param df CoLaus long tibble containing `sumprot1` (raw total dietary
#'   protein, g/day) and `prot_content_dairy_cumavg` (output of
#'   `derive_dairy_protein()`).
#' @return `df` with `sumprot1_cumavg` and `prot_content_nondairy_cumavg` added.
derive_nondairy_protein_cumavg <- function(df,
                                           id_col    = "pt",
                                           visit_col = ".visit") {

    if (!"prot_content_dairy_cumavg" %in% names(df)) {
        cli::cli_warn(c(
            "x" = "derive_nondairy_protein_cumavg: Missing prot_content_dairy_cumavg.",
            "!" = "Run derive_dairy_protein() first."
        ))
        return(df)
    }

    df <- .add_grouped_cumavg(df, "sumprot1", id_col, visit_col)

    if (!"sumprot1_cumavg" %in% names(df)) return(df)

    df$prot_content_nondairy_cumavg <- df$sumprot1_cumavg - df$prot_content_dairy_cumavg

    cli::cli_h2("Non-dairy protein cumulative average derivation")
    cli::cli_inform(c(
        "v" = "derive_nondairy_protein_cumavg: columns added.",
        "i" = "sumprot1_cumavg valid: {sum(!is.na(df$sumprot1_cumavg))}",
        "i" = "prot_content_nondairy_cumavg valid: {sum(!is.na(df$prot_content_nondairy_cumavg))}"
    ))

    df
}
