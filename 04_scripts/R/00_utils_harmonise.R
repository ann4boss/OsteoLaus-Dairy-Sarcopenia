# =============================================================================
# R/utils_harmonise.R
# =============================================================================
# Low-level pure helpers for type coercion and recoding.
# Shared by harmonise_colaus.R and harmonise_osteo.R.
# =============================================================================

# -----------------------------------------------------------------------------
# Strip wave prefixes helper
# -----------------------------------------------------------------------------
#' Strip a prefix from column names
#'
#' Removes a literal prefix from every column in a data frame whose name
#' starts with that prefix. Columns not starting with the prefix are left
#' unchanged. If the prefix is empty or NULL, the data frame is returned
#' unchanged.
#'
#' This is a general utility that can be used for wave-based prefixes
#' (e.g. from COHORT_META) as well as arbitrary prefixes such as "H_".
#' No explicit list of variable names is required, the prefix is removed
#' wherever it occurs at the start of a column name.
#'
#' Examples:
#'   strip_prefix(df, "F1")      renames "F1age"   -> "age"
#'   strip_prefix(df, "Bsl_")    renames "Bsl_Age" -> "Age"
#'   strip_prefix(df, "H_")      renames "H_ALM"   -> "ALM"
#'   strip_prefix(df, "")        no-op
#'
#' @param df     A data frame.
#' @param prefix A character string prefix to remove.
#' @return The data frame with updated column names.
#'
#' @details If removing the prefix creates duplicate column names, a warning is issued.
strip_prefix <- function(df, prefix = NULL, ...) {
    
    # Guard against NULL or empty prefix, which would cause all names to be stripped.
    if (is.null(prefix) || prefix == "") return(df)
    
    cols <- names(df)
    new  <- ifelse(startsWith(cols, prefix),
                   substring(cols, nchar(prefix) + 1L),
                   cols)
    
    # warn on duplicates after stripping
    dupes <- new[duplicated(new) & startsWith(cols, prefix)]
    if (length(dupes) > 0) {
        cli::cli_warn(c("*" = "Duplicate column names after stripping: {.val {dupes}}"))
    }
    
    df <- df |> 
        dplyr::rename_with(~ ifelse(startsWith(.x, prefix), 
                                    substring(.x, nchar(prefix) + 1L), 
                                    .x))
    
    return(df)
}


# -----------------------------------------------------------------------------
# Type coercion for exam dates
# -----------------------------------------------------------------------------
#' Parse DDMonYYYY dates (e.g. "21mar2025") to ISO Date.
#'
#' str_to_title() normalises case before as.Date(), which is locale-sensitive
#' with %b and typically requires a capitalised first letter.
#'
#' @param x Character vector of dates in DDMonYYYY format.
#' @return Date vector.
parse_exam_date <- function(x) {
    as.Date(x, format = "%d%b%Y")
}


# -----------------------------------------------------------------------------
# Type coercion numeric
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
        cli::cli_warn(c("!" = "safe_numeric: {lost} value(s) lost coercing {.col {col}}."))
    return(out)
}


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
sentinel_to_na <- function(x, codes = c("8", "9")) {
    replace(x, x %in% codes, NA)
}

#' Build a Yes/No factor, treating sentinel codes as NA.
#'
#' @param x        Character vector with values "0" and "1".
#' @param sentinel Character vector of sentinel codes (passed to sentinel_to_na).
#' @return Factor with levels No / Yes.
yn_factor <- function(x, sentinel = c("8", "9"))
    factor(sentinel_to_na(x, sentinel), levels = c("0", "1"), labels = c("No", "Yes"))


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
    
    # Identify which columns from our constant list are actually in the current df
    cols_to_fix <- intersect(names(df), names(SENTINEL_NUMERIC))
    
    if (length(cols_to_fix) == 0) return(df)
    
    df |>
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(cols_to_fix),
                ~ {
                    # Capture current column name to get the specific sentinel code
                    col_name <- dplyr::cur_column()
                    codes <- as.numeric(SENTINEL_NUMERIC[[col_name]])
                    
                    
                    replace(.x, .x %in% codes, NA)
                }
            )
        )
}