# =============================================================================
# R/05_derive_colaus.R
# =============================================================================
# Applies all CoLaus-specific derivations in sequence.
# This is the only file _targets.R needs to call for CoLaus derivation.
#
# Derivation order (dependencies noted — earlier steps feed later ones):
#   1. education   — fixed covariate; from edtyp4; no dependencies
#   2. alcohol     — from conso_hebdo / sumalco
#   3. diabetes    — from DIAB, dbtld, dbdrg, orldrg, insn, antiDIAB, DIAB_Hb
#   4. cvd         — from 13 component flags
#   5. hrt         — from esthrp, esthrpage (Baseline) and bthc (F1+)
#   6. pa          — from PAFQ_MPA, PAFQ_VPA
#   7. dairy       — from FFQ amount columns; outputs *_gday columns
#   8. atc         — from ATC1:21 / ATC_OTC1:17 raw codes
#   9. htn         — from antiHTA, crbpmed, HTA (Yes/No factors)
#
# Depends on: R/05_derive_colaus_education.R
#             R/05_derive_colaus_alcohol.R
#             R/05_derive_colaus_diabetes.R
#             R/05_derive_colaus_cvd.R
#             R/05_derive_colaus_hrt.R
#             R/05_derive_colaus_pa.R
#             R/05_derive_colaus_dairy.R
#             R/05_derive_colaus_atc.R
#             R/05_derive_colaus_htn.R
# =============================================================================

source("04_scripts/R/05_derive_colaus_education.R")
source("04_scripts/R/05_derive_colaus_alcohol.R")
source("04_scripts/R/05_derive_colaus_diabetes.R")
source("04_scripts/R/05_derive_colaus_cvd.R")
source("04_scripts/R/05_derive_colaus_hrt.R")
source("04_scripts/R/05_derive_colaus_pa.R")
source("04_scripts/R/05_derive_colaus_dairy.R")
source("04_scripts/R/05_derive_colaus_atc.R")
source("04_scripts/R/05_derive_colaus_htn.R")

#' Apply all CoLaus-specific derivations to the stacked CoLaus tibble.
#'
#' @param df Output of stack_waves() for CoLaus.
#' @return df with all derived variables appended.
derive_colaus <- function(df) {
    stopifnot(unique(df$.cohort) == "CoLaus")
    
    df |>
        derive_education() |>
        derive_alcohol()   |>
        derive_diabetes()  |>
        derive_cvd()       |>
        derive_hrt()       |>
        derive_pa()        |>
        derive_dairy()     |>
        derive_atc()       |>
        derive_htn()
}