# TODO check description is still up to date
# =============================================================================
# R/derive_colaus.R
# =============================================================================
# Applies all CoLaus-specific derivations in sequence.
# This is the only file _targets.R needs to call for CoLaus derivation.
#
# Derivation order:
#   1. education   — from edtyp4
#   2. alcohol     — from conso_hebdo / sumalco
#   3. diabetes    — from DIAB, dbtld, dbdrg, orldrg, insn, antiDIAB, DIAB_Hb
#   4. cvd         — from 13 component flags
#   5. hrt         — from esthrp, esthrpage (Baseline) and bthc (F1+)
#   6. pa          — from PAFQ_MPA, PAFQ_VPA
#   7. dairy       — from FFQ amount columns; outputs *_gday columns
#   8. atc         — from ATC1:21 / ATC_OTC1:17 raw codes
#   9. htn         — from antiHTA, crbpmed, HTA (Yes/No factors)
#   10.smoking      — from sbsmk; requires trajectory correction
# =============================================================================

#' Apply all CoLaus-specific derivations to the stacked CoLaus tibble.
#'
#' @param df Output of stack_waves() for CoLaus.
#' @return df with all derived variables appended.
derive_colaus <- function(df) {
    
    cli::cli_h1("Derive Variables for CoLaus")
    
    # ── Guard: Single Cohort ------------------------------------------------
    cohorts <- df |> dplyr::distinct(.cohort) |> dplyr::pull(.cohort)
    if (!"CoLaus" %in% cohorts) {
        cli::cli_abort("derive_colaus() received non-CoLaus data: {.val {cohorts}}")
    }
    
    # ── Execute Derivation Chain ----------------------------------------------
    df_derived <- df |>
        derive_education()  |>
        derive_smoking()    |>
        derive_alcohol()    |>
        derive_diabetes()   |>
        derive_cvd()        |>
        derive_hrt()        |>
        derive_pa()         |>
        derive_dairy()      |>
        derive_atc()        |>
        derive_htn()
        
    
    # ── Finalize ------------------------------------------------
    return(dplyr::as_tibble(df_derived))
}