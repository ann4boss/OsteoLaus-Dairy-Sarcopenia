# =============================================================================
# R/00_utils_harmonise.R
# =============================================================================
# Low-level pure helpers for type coercion and recoding.
# Shared by harmonise_colaus.R and harmonise_osteo.R.
#
# =============================================================================

# -----------------------------------------------------------------------------
# Type coercion
# -----------------------------------------------------------------------------

#' Coerce a character vector to numeric, warning on value loss.
#'
#' @param x   Character vector.
#' @param col Column name used in the warning message.
#' @return Numeric vector; values that could not be coerced become NA.
safe_numeric <- function(x, col) {
    out  <- suppressWarnings(as.numeric(x))
    lost <- sum(!is.na(x) & is.na(out))
    if (lost > 0)
        cli::cli_warn("safe_numeric: {lost} value(s) lost coercing {.col {col}}.")
    out
}

#' Parse DDMonYYYY dates (e.g. "21mar2025") to ISO Date.
#'
#' str_to_title() normalises case before as.Date(), which is locale-sensitive
#' with %b and typically requires a capitalised first letter.
#'
#' @param x Character vector of dates in DDMonYYYY format.
#' @return Date vector.
parse_exam_date <- function(x)
    as.Date(stringr::str_to_title(x), format = "%d%b%Y")

# -----------------------------------------------------------------------------
# Sentinel recoding
# -----------------------------------------------------------------------------

#' Recode sentinel codes to NA, leaving other values unchanged.
#'
#' By default treats "8" and "9" as sentinels. These can mean
#' "Does not know", "Not applicable", or "No Data" depending on the variable.
#'
#' @param x     Character vector.
#' @param codes Character vector of sentinel codes to replace with NA.
#' @return Character vector with sentinel values replaced by NA.
sentinel_to_na <- function(x, codes = c("8", "9"))
    dplyr::if_else(x %in% codes, NA_character_, x)

#' Apply SENTINEL_NUMERIC to all matching columns in a data frame.
#'
#' Iterates over SENTINEL_NUMERIC (defined in 00_constants.R). For each entry
#' whose column is present in df (after prefix-stripping), replaces the
#' sentinel integer value(s) with NA_real_. This is called at the end of each
#' harmonise_*() function so it operates on base column names.
#'
#' @param df Data frame with base column names (prefix already stripped).
#' @return df with sentinel numeric values replaced by NA.
apply_sentinel_numeric <- function(df) {
    for (col in names(SENTINEL_NUMERIC)) {
        if (!col %in% names(df)) next
        sentinels <- as.numeric(SENTINEL_NUMERIC[[col]])
        is_sentinel <- !is.na(df[[col]]) & df[[col]] %in% sentinels
        n <- sum(is_sentinel)
        if (n > 0) {
            cli::cli_inform(
                "apply_sentinel_numeric: {n} value(s) in {.col {col}} \
         recoded to NA (sentinel: {.val {sentinels}})."
            )
            df[[col]][is_sentinel] <- NA_real_
        }
    }
    df
}

# -----------------------------------------------------------------------------
# Factor constructors
# -----------------------------------------------------------------------------

#' Build a Yes/No factor, treating sentinel codes as NA.
#'
#' @param x        Character vector with values "0" and "1".
#' @param sentinel Character vector of sentinel codes (passed to sentinel_to_na).
#' @return Factor with levels No / Yes.
yn_factor <- function(x, sentinel = c("8", "9"))
    factor(sentinel_to_na(x, sentinel), levels = c("0", "1"), labels = c("No", "Yes"))

#' Harmonise DIAB2, which has different level structures across CoLaus waves.
#'
#' F1         : 0 = No,     1 = Yes    (binary — no IFG level)
#' All others : 0 = Normal, 1 = IFG,   2 = Diabetes
#'
#' Per the data dictionary, F1 uses a binary coding without a distinct IFG
#' level. Participants coded 1 at F1 are assigned "Diabetes" directly.
#'
#' @param x    Character vector.
#' @param wave Pipeline wave label, e.g. "F1", "F2".
#' @return Ordered factor with levels Normal / IFG / Diabetes.
harmonise_diab2 <- function(x, wave) {
    if (wave == "F1") {
        out <- dplyr::case_when(
            x == "0" ~ "Normal",
            x == "1" ~ "Diabetes",
            TRUE     ~ NA_character_
        )
    } else {
        out <- dplyr::case_when(
            sentinel_to_na(x, "9") == "0" ~ "Normal",
            sentinel_to_na(x, "9") == "1" ~ "IFG",
            sentinel_to_na(x, "9") == "2" ~ "Diabetes",
            TRUE                           ~ NA_character_
        )
    }
    factor(out, levels = c("Normal", "IFG", "Diabetes"))
}


# -----------------------------------------------------------------------------
# Validation helper
# -----------------------------------------------------------------------------

#' Validate that a harmonise_*() function did not drop or unexpectedly add columns.
#'
#' Columns in `cols_added` are expected to be present after but not before
#' harmonisation (e.g. exam_date_iso derived from datexam).
#' Columns in `cols_renamed` are expected to disappear (old name) and be
#' replaced by their new name (e.g.gait_speed), which must be listed in `cols_added`.
#' Any other addition or removal stops the pipeline with a clear error.
#'
#' @param cols_before  Character vector of column names before harmonisation.
#' @param cols_after   Character vector of column names after harmonisation.
#' @param wave         Wave label used in error messages.
#' @param cols_added   Character vector of column names legitimately added.
#'   Default: "exam_date_iso".
#' @param cols_renamed Named character vector of columns that are renamed:
#'   names = old column names (removed), values = new column names (must be
#'   in cols_added). Default: empty (no renames expected).
#' @return Invisible NULL on success; stops with an error on mismatch.
validate_harmonise <- function(cols_before,
                               cols_after,
                               wave,
                               cols_added   = "exam_date_iso",
                               cols_renamed = character(0)) {
    
    # Build the expected set: start with before, remove old rename names,
    # then add legitimately new column names.
    old_names <- names(cols_renamed) 
    new_names <- unname(cols_renamed)
    
    expected_after <- c(
        setdiff(cols_before, old_names),  # remove old names
        new_names,                        # add renamed columns
        cols_added                       # add legitimate new columns
    )
    
    missing_cols <- setdiff(expected_after, cols_after)
    extra_cols   <- setdiff(cols_after, expected_after)
    
    if (length(missing_cols) > 0 || length(extra_cols) > 0) {
        if (length(missing_cols) > 0)
            cli::cli_alert_danger(
                "[{wave}] Columns missing after harmonisation: {paste(missing_cols, collapse = ', ')}"
            )
        if (length(extra_cols) > 0)
            cli::cli_alert_danger(
                "[{wave}] Unexpected columns added during harmonisation: {paste(extra_cols, collapse = ', ')}"
            )
        cli::cli_abort(
            "[{wave}] Column mismatch in harmonise(). See messages above."
        )
    }
    
    cli::cli_inform(
        c("v" = "[{wave}] harmonise(): column check passed ({length(cols_after)} columns).")
    )
    invisible(NULL)
}