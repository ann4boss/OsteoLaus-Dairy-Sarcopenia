# =============================================================================
# R/derive_colaus_education.R
# =============================================================================
# Re-codes edtyp4 (4-level CoLaus education) to the 3-level ISCED grouping.
#
# Source → ISCED mapping:
#   edtyp4 = "University"    -> 3 = High    (ISCED 5-8)
#   edtyp4 = "High school"   -> 2 = Medium  (ISCED 3-4)
#   edtyp4 = "Apprenticeship"-> 2 = Medium  (ISCED 3-4)
#   edtyp4 = "Mandatory"     -> 1 = Low     (ISCED 0-2)
#
# education_level is fixed at Baseline and carried forward to all visits.
# =============================================================================

#' Derive education_level (ISCED 3-group) for a CoLaus long tibble.
#'
#' Derives education_level from edtyp4 at Baseline, then propagates the
#' value to all subsequent visits for each participant.
#'
#' @param df CoLaus long tibble (data.frame, tibble, or lazy_dt).
#' @return A tibble with education_level (ordered factor) added.
derive_education <- function(df) {

    # ── Ensure source columns are present ------------------------------------
    required_cols <- c("pt", "edtyp4")
    actual_cols <- names(df)
    missing_cols <- setdiff(required_cols, actual_cols)
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_education: missing required columns: {.val {missing_cols}}.
        education levels will not be derived."
        )
        return(df)
    }

    # ── Ensure plain tibble for grouped operations ---------------------------
    if (inherits(df, "dtplyr_step")) df <- dplyr::as_tibble(df)

    # ── Step 1: Derive education_level from edtyp4 at every row -------------
    # At non-Baseline visits edtyp4 will typically be NA; the carry-forward
    # in Step 2 fills those gaps.
    df <- df |>
        dplyr::mutate(
            education_level = factor(
                dplyr::case_when(
                    edtyp4 == "University"                 ~ "High (ISCED 5-8)",
                    edtyp4 %in% c("High school",
                                  "Apprenticeship")        ~ "Medium (ISCED 3-4)",
                    edtyp4 == "Mandatory"                  ~ "Low (ISCED 0-2)",
                    TRUE                                   ~ NA_character_
                ),
                levels  = c("Low (ISCED 0-2)", "Medium (ISCED 3-4)", "High (ISCED 5-8)"),
                ordered = FALSE
            )
        )

    # ── Step 2: Carry Baseline value forward to all visits per participant ---
    # Sort by pt and .visit to ensure Baseline comes first, then propagate
    # the first non-NA education_level downward (and upward as fallback).
    df <- df |>
        dplyr::arrange(pt, .visit) |>
        dplyr::group_by(pt) |>
        dplyr::mutate(
            education_level = dplyr::coalesce(
                # Forward-fill: take the first non-NA value across all visits
                education_level[!is.na(education_level)][1],
                education_level
            )
        ) |>
        dplyr::ungroup()

    # ── Diagnostics ----------------------------------------------------------
    n_participants   <- dplyr::n_distinct(df$pt)
    n_pts_with_edu   <- dplyr::n_distinct(df$pt[!is.na(df$education_level)])
    n_pts_without    <- n_participants - n_pts_with_edu
    n_rows_filled    <- sum(!is.na(df$education_level))
    pct_pts          <- sprintf("%.1f%%", (n_pts_with_edu / n_participants) * 100)

    cli::cli_h2("Derive Education Level")
    cli::cli_inform(c(
        "v" = "derive_education: education_level derived and propagated.",
        "i" = "Participants with education_level: {n_pts_with_edu} / {n_participants} ({pct_pts}).",
        "i" = "Rows with education_level filled (all visits): {n_rows_filled}.",
        if (n_pts_without > 0)
            c("!" = "{n_pts_without} participant(s) have no education_level at any visit (edtyp4 missing everywhere).")
        else
            c("v" = "All participants have education_level at all visits.")
    ))

    return(df)
}