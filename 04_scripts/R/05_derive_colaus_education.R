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
#
# Depends on: nothing

#' Derive education_level (ISCED 3-group) for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with education_level (ordered factor) added.
derive_education <- function(df) {
    
    df <- dplyr::mutate(df,
                        education_level = dplyr::case_when(
                            is.na(edtyp4)                      ~ NA_integer_,
                            edtyp4 == "University"             ~ 3L,
                            edtyp4 %in% c("High school",
                                          "Apprenticeship")    ~ 2L,
                            edtyp4 == "Mandatory"              ~ 1L,
                            TRUE                               ~ NA_integer_
                        ) |>
                            factor(
                                levels  = 1:3,
                                labels  = c("Low (ISCED 0-2)", "Medium (ISCED 3-4)", "High (ISCED 5-8)"),
                                ordered = TRUE
                            )
    )

    
    return(df)
}