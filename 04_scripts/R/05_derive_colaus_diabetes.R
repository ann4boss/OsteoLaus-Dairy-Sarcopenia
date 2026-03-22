# TODO: needs to be simpler!!
#=============================================================================
# R/derive_colaus_diabetes.R
# =============================================================================
# Derives diabetes_status from multiple source variables.
#
# Source variables and their roles:
#   dbtld     — self-reported diagnosis
#   dbdrg     — self-reported diabetes drug history
#   orldrg    — oral diabetes drug
#   insn      — on insulin
#   antiDIAB  — any antidiabetic drug
#   DIAB      — diabetes diagnosis flag
#   DIAB_Hb   — HbA1c-based diabetes flag
#
# Derivation hierarchy (first matching condition wins):
#   3 = Diab + Insulin  : insn == "Yes"
#   2 = Diab oral/lab   : dbdrg == "Yes" OR orldrg == "Yes" OR antiDIAB == "Yes"
#                         OR DIAB == "Yes" OR DIAB_Hb == "Yes" OR dbtld == "Yes"
#   1 = No diabetes     : all above are No / NA
#
# Validation: DIAB2 (0=Normal, 1=IFG, 2=Diabetes at most waves) is used to
# cross-check. A warning is raised for rows where DIAB2 == "Diabetes" but
# diabetes_status == 1, or where DIAB2 == "Normal" but diabetes_status > 1.
#
# Note: IGT/Prediabetes (level 4) is not directly derivable from available
# variables — DIAB2 == "IFG" is the only indicator but is labelled as
# Validation in the data dictionary. This level is therefore not assigned here.
# TODO: confirm whether IFG from DIAB2 should populate level 4.
#
# Depends on: nothing

#' Derive diabetes_status for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with diabetes_status (factor) added.
derive_diabetes <- function(df) {
    
    # Helper: safely test a Yes/No factor column, returns FALSE when NA
    .is_yes <- function(x) !is.na(x) & x == "Yes"
    
    df <- dplyr::mutate(df,
                        diabetes_status = dplyr::case_when(
                            .is_yes(insn)                                                    ~ 3L,
                            .is_yes(dbdrg) | .is_yes(orldrg) | .is_yes(antiDIAB) |
                                .is_yes(DIAB) | .is_yes(DIAB_Hb) | .is_yes(dbtld)            ~ 2L,
                            # Only assign No-diabetes when at least one source variable is present
                            !is.na(DIAB) | !is.na(antiDIAB) | !is.na(dbtld)                ~ 1L,
                            TRUE                                                             ~ NA_integer_
                        ) |>
                            factor(
                                levels = 1:3,
                                labels = c("No diabetes", "Diabetes (oral/lab)", "Diabetes (insulin)")
                            )
    )
    
    # ── Validation against DIAB2 ───────────────────────────────────────────────
    if ("DIAB2" %in% names(df)) {
        n_false_neg <- sum(
            !is.na(df$diabetes_status) & !is.na(df$DIAB2) &
                as.character(df$DIAB2) == "Diabetes" & df$diabetes_status == "No diabetes",
            na.rm = TRUE
        )
        n_false_pos <- sum(
            !is.na(df$diabetes_status) & !is.na(df$DIAB2) &
                as.character(df$DIAB2) == "Normal" &
                df$diabetes_status %in% c("Diabetes (oral/lab)", "Diabetes (insulin)"),
            na.rm = TRUE
        )
        if (n_false_neg > 0)
            cli::cli_warn(
                "derive_diabetes: {n_false_neg} row(s) where DIAB2 = Diabetes but \\
         diabetes_status = No diabetes. Check source variables."
            )
        if (n_false_pos > 0)
            cli::cli_warn(
                "derive_diabetes: {n_false_pos} row(s) where DIAB2 = Normal but \\
         diabetes_status indicates diabetes. Check source variables."
            )
    }
    
    return(df)
}