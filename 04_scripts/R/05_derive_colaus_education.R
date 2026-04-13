# =============================================================================
# R/derive_colaus_education.R
# =============================================================================
# Re-codes edtyp4 (4-level CoLaus education) to the 3-level ISCED grouping
# used as a fixed covariate in the analysis.
#
# Source → ISCED mapping:
#   edtyp4 = "University"    -> 3 = High    (ISCED 5-8)
#   edtyp4 = "High school"   -> 2 = Medium  (ISCED 3-4)
#   edtyp4 = "Apprenticeship"-> 2 = Medium  (ISCED 3-4)
#   edtyp4 = "Mandatory"     -> 1 = Low     (ISCED 0-2)
#
# education_level is fixed at Baseline and carried forward.

#' Derive education_level (ISCED 3-group) for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble (data.frame, tibble, or lazy_dt).
#' @return A lazy_dt or tibble with education_level added and edtyp4 removed.
derive_education <- function(df) {
    
    # Ensure the source column is present before attempting to derive; if not, warn and return unchanged
    if (!"edtyp4" %in% names(df)) {
        cli::cli_warn(
            "derive_education: source column {.col edtyp4} not found. \
       education_level will not be derived."
        )
        return(df)
    }
    
    # Ensure we're working with a lazy_dt for efficient mutation; if not, convert it
    if (!inherits(df, "dtplyr_step")) {
        df <- dtplyr::lazy_dt(df)
    }
    
    df <- df %>%
        dplyr::mutate(
            education_level = dplyr::case_when(
                # fcase/case_when handles NA automatically, but being explicit is safer
                is.na(edtyp4)                            ~ NA_integer_,
                edtyp4 == "University"                   ~ 3L,
                edtyp4 %in% c("High school", 
                              "Apprenticeship")           ~ 2L,
                edtyp4 == "Mandatory"                    ~ 1L,
                TRUE                                     ~ NA_integer_
            ) %>%
                factor(
                    levels  = 1:3,
                    labels  = c("Low (ISCED 0-2)", "Medium (ISCED 3-4)", "High (ISCED 5-8)"),
                    ordered = TRUE
                )
        ) %>%
        # Remove the original source column as requested
        dplyr::select(-edtyp4)
    
    # collect as tibble
    df <- df %>% dplyr::as_tibble()
    
    # give feedback on the derived variable
    n_derived <- sum(!is.na(df$education_level))
    n_unique_pt <- length(unique(df$pt))
    percent_derived <- sprintf("%.1f%%", (n_derived / n_unique_pt) * 100)
    
    cli::cli_h2("Derive Education Level")
    cli::cli_inform(c("i" = "derive_education: education_level derived for {n_derived} participant(s) ({percent_derived} of {n_unique_pt} unique participants)."
                      ))
        
    return(df)
}