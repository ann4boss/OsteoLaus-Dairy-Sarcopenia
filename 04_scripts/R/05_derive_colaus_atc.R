# =============================================================================
# R/05_derive_colaus_atc.R
# =============================================================================
# Derives binary medication status flags from raw ATC code columns.
#
# Source columns: ATC1 ... ATC21 (character, one ATC code per column).
# Each row may have up to 21 reported medications. A flag is set to "Yes"
# if ANY of the 21 ATC columns contains a code starting with the relevant
# prefix. Over-the-counter drugs (ATC_OTC1 ... ATC_OTC17) are also searched.
#
# ATC prefix -> derived variable mapping is defined in ATC_PREFIXES
# (00_constants.R):
#   C10   -> hypolip_drug_status   (lipid-lowering drugs)
#   H02   -> corticoids_status     (systemic corticosteroids)
#   A11   -> vitD_status           (vitamin D supplements)
#   A12A  -> calcium_status        (calcium supplements)
#   N05B  -> benzo_status          (benzodiazepines)
#   M05BA -> bisphosphonate_status (bisphosphonates)
#
# Validation:
#   hypolip_drug_status is compared against hypolip (self-reported).
#   Mismatches are warnings for review; some discordance is expected.
#
# =============================================================================


# ATC column names present in both CoLaus raw and harmonised data
.ATC_COLS     <- paste0("ATC", 1:21)
.ATC_OTC_COLS <- paste0("ATC_OTC", 1:17)
.ALL_ATC_COLS <- c(.ATC_COLS, .ATC_OTC_COLS)

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

#' Test whether any ATC column in a row starts with a given prefix.
#'
#' @param df      Data frame with ATC columns.
#' @param cols    Character vector of ATC column names present in df.
#' @param prefix  ATC prefix string, e.g. "C10".
#' @return Logical vector, one value per row. NA when no ATC codes are present
#'   for that row (all ATC columns empty or NA).
.any_atc_starts_with <- function(df, cols, prefix) {
    if (length(cols) == 0) return(rep(NA, nrow(df)))
    mat <- as.matrix(dplyr::select(df, dplyr::all_of(cols)))
    apply(mat, 1, function(row) {
        vals <- row[!is.na(row) & nchar(row) > 0]
        if (length(vals) == 0) return(NA)
        any(startsWith(vals, prefix))
    })
}

#' Convert a logical vector to a Yes/No factor.
.to_yn_factor <- function(x)
    factor(
        dplyr::case_when(
            is.na(x) ~ NA_character_,
            x        ~ "Yes",
            TRUE     ~ "No"
        ),
        levels = c("No", "Yes")
    )

# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------

#' Derive ATC-based medication status flags for a CoLaus long tibble.
#'
#' ATC prefix -> variable name mapping is read from ATC_PREFIXES in
#' 00_constants.R. Adding a new drug class only requires updating that constant.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with hypolip_drug_status, corticoids_status, vitD_status,
#'   calcium_status, benzo_status, bisphosphonate_status (all Yes/No factors)
#'   added or replaced.
derive_atc <- function(df) {
    
    atc_cols_present <- intersect(.ALL_ATC_COLS, names(df))
    
    if (length(atc_cols_present) == 0) {
        cli::cli_warn(
            "derive_atc: no ATC code columns found in data. \
       Medication status flags will not be derived."
        )
        return(df)
    }
    
    # ── Derive prefix-based flags from ATC_PREFIXES constant -------------------
    for (varname in names(ATC_PREFIXES)) {
        prefix <- ATC_PREFIXES[[varname]]
        df[[varname]] <- .to_yn_factor(
            .any_atc_starts_with(df, atc_cols_present, prefix)
        )
    }
    
    # ── Validation: hypolip_drug_status vs hypolip (self-reported) -------------
    .validate_hypolip(df)
    
    return(df)
}

# -----------------------------------------------------------------------------
# Validation helper (internal)
# -----------------------------------------------------------------------------

.validate_hypolip <- function(df) {
    
    if ("hypolip" %in% names(df)) {
        n_atc_yes_self_no <- sum(
            !is.na(df$hypolip_drug_status) & !is.na(df$hypolip) &
                df$hypolip_drug_status == "Yes" & df$hypolip == "No",
            na.rm = TRUE
        )
        n_atc_no_self_yes <- sum(
            !is.na(df$hypolip_drug_status) & !is.na(df$hypolip) &
                df$hypolip_drug_status == "No" & df$hypolip == "Yes",
            na.rm = TRUE
        )
        if (n_atc_yes_self_no > 0)
            cli::cli_warn(
                "derive_atc: {n_atc_yes_self_no} row(s) with ATC C10 present but \
         self-reported {.col hypolip} = No."
            )
        if (n_atc_no_self_yes > 0)
            cli::cli_warn(
                "derive_atc: {n_atc_no_self_yes} row(s) with no ATC C10 but \
         self-reported {.col hypolip} = Yes."
            )
    }
}