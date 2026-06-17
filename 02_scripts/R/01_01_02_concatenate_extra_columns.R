# =============================================================================
# R/concatenate_extra_columns.R
# =============================================================================
# Helpers that join supplementary columns onto a base visit data frame.
#
# Functions:
#   add_ffq_columns()    — joins FFQ frequency/amount columns for a time point
#   add_extra_cols()     — joins additional columns
#   add_death_date()     — joins death date from the Deaths file
#   add_birth_date()     — joins birth date from the Baseline add-food file
#
# All joins are left joins keyed on "pt" so that participants absent from the
# supplementary file are retained with NA for the new columns.
# =============================================================================

# Cardiovascular event flags and questionnaire date present in the add-food
# files. These carry the visit-time point prefix in the raw CSV (e.g. "F1cmp").
.EXTRA_BASE_COLS <- c(
    "cmp", "hdv", "chf", "artm", "cad", "angn", "miac",
    "strk", "vslg", "ccth", "cabg", "datquest"
)

# FFQ item indices to exclude (items not used in the analysis).
.FFQ_EXCLUDE_NUMS <- c(1:8, 52, 53, 63, 68, 71, 82:86)


# -----------------------------------------------------------------------------
# add_ffq_columns()
# -----------------------------------------------------------------------------
#' Join FFQ frequency and amount columns for a given timepoint.
#'
#' Selects all columns matching `<timepoint>(freq)?FFQ<n>` from \code{add_df},
#' drops excluded item indices, then left-joins onto \code{base_df} by "pt".
#'
#' @param base_df   Data frame for one CoLaus visit (output of import_visit()).
#' @param add_df    Supplementary add-food data frame for the same visit.
#' @param timepoint Visit prefix used in raw column names, e.g. \code{""} for
#'   Baseline, \code{"F1"} for follow-up 1.
#'
#' @return \code{base_df} with FFQ columns appended; row count unchanged.
add_ffq_columns <- function(base_df, add_df, timepoint) {
    
    # Match both <tp>FFQ<n> and <tp>freqFFQ<n>
    ffq_cols_tp <- grep(
        paste0("^", timepoint, "(freq)?FFQ"),
        names(add_df),
        value = TRUE
    )
    
    # Drop excluded item indices
    ffq_cols_tp <- ffq_cols_tp[
        !as.integer(gsub(".*FFQ([0-9]+).*", "\\1", ffq_cols_tp)) %in%
            .FFQ_EXCLUDE_NUMS
    ]
    
    cols_to_add <- c("pt", intersect(ffq_cols_tp, names(add_df)))
    
    add_subset <- dplyr::select(add_df, dplyr::all_of(cols_to_add))
    
    dplyr::left_join(base_df, add_subset, by = "pt")
}


# -----------------------------------------------------------------------------
# add_extra_cols()
# -----------------------------------------------------------------------------
#' Join cardiovascular event flags and questionnaire date for a given timepoint.
#'
#' Selects columns in \code{.EXTRA_BASE_COLS} (prefixed by \code{timepoint})
#' from \code{add_df}, parses the questionnaire date, then left-joins onto
#' \code{base_df} by "pt".
#'
#' @param base_df   Data frame for one CoLaus visit.
#' @param add_df    Supplementary add-food data frame for the same visit.
#' @param timepoint Visit prefix, e.g. \code{""}, \code{"F1"}, \code{"F2"}.
#'
#' @return \code{base_df} with extra event-flag and date columns appended;
#'   row count unchanged.
add_extra_cols <- function(base_df, add_df, timepoint) {
    
    extra_cols_tp   <- paste0(timepoint, .EXTRA_BASE_COLS)
    cols_to_add     <- c("pt", intersect(extra_cols_tp, names(add_df)))
    
    add_subset <- dplyr::select(add_df, dplyr::all_of(cols_to_add))
    
    # Parse datquest to Date if present
    datquest_col <- paste0(timepoint, "datquest")
    if (datquest_col %in% names(add_subset)) {
        add_subset <- dplyr::mutate(
            add_subset,
            dplyr::across(dplyr::all_of(datquest_col), parse_exam_date)
        )
    }
    
    dplyr::left_join(base_df, add_subset, by = "pt")
}


# -----------------------------------------------------------------------------
# add_death_date()
# -----------------------------------------------------------------------------
#' Join death date onto a baseline data frame.
#'
#' @param baseline_df  Baseline visit data frame keyed on "pt".
#' @param death_df     Deaths file imported via import_visit(); must contain
#'   columns "pt" and "datdeath".
#'
#' @return \code{baseline_df} with a \code{datdeath} Date column appended;
#'   row count unchanged.
add_death_date <- function(baseline_df, death_df) {
    
    death_subset <- death_df |>
        dplyr::select(dplyr::all_of(c("pt", "datdeath"))) |>
        dplyr::mutate(datdeath = parse_exam_date(datdeath)) |>
        dplyr::distinct(pt, .keep_all = TRUE)
    
    dplyr::left_join(baseline_df, death_subset, by = "pt")
}


# -----------------------------------------------------------------------------
# add_birth_date()
# -----------------------------------------------------------------------------
#' Join birth date onto a baseline data frame.
#'
#' @param baseline_df  Baseline visit data frame keyed on "pt".
#' @param birth_df     Add-food baseline file imported via import_visit(); must
#'   contain columns "pt" and "datbirth".
#'
#' @return \code{baseline_df} with a \code{datbirth} Date column appended;
#'   row count unchanged.
add_birth_date <- function(baseline_df, birth_df) {
    
    birth_subset <- birth_df |>
        dplyr::select(dplyr::all_of(c("pt", "datbirth"))) |>
        dplyr::mutate(datbirth = parse_exam_date(datbirth)) |>
        dplyr::distinct(pt, .keep_all = TRUE)
    
    dplyr::left_join(baseline_df, birth_subset, by = "pt")
}