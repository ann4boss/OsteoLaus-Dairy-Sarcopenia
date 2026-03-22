# =============================================================================
# 03_R/00_constants.R
# =============================================================================
# Static lookup data shared across the entire pipeline.
#
# Contents:
#   DATE_REGEX          Regex for the DDMonYYYY format used in both cohorts
#   COHORT_META         Wave labels, CSV prefixes, wave numbers per cohort
#   SENTINEL_NUMERIC    Per-variable numeric sentinel codes that mean "missing"
#                       (applied in harmonise layer via apply_sentinel_numeric())
#   ATC_PREFIXES        ATC prefix -> derived variable name mapping
#   RANGE_LIMITS        Physiologically plausible ranges for outcome/exposure vars
#   EWGSOP2             EWGSOP2 (2019) sarcopenia cut-offs (women only)
#   FNIH                FNIH sarcopenia cut-offs (women only)
# =============================================================================

# Date format in both cohorts: DDMonYYYY, e.g. "21mar2025".
# Regex accepts mixed case because SAS exports vary.
DATE_REGEX <- "^[0-9]{2}[A-Za-z]{3}[0-9]{4}$"

# -----------------------------------------------------------------------------
# Cohort metadata
# Wave-prefixed column naming:
#
#   CoLaus
#     .wave      CSV prefix   Example
#     Baseline   (none)       datexam, age
#     F1         F1           F1datexam, F1age
#     F2         F2           F2datexam, F2age
#     F3         F3           F3datexam, F3age
#
#   OsteoLaus
#     .wave      CSV prefix   Example
#     Baseline   Bsl_         Bsl_SCAN_date, Bsl_Age
#     V2         V2_          V2_SCAN_date,  V2_Age
#     V3         V3_          V3_SCAN_date,  V3_Age
#     V4         V4_          V4_SCAN_date,  V4_Age
#     V5         V5_          V5_SCAN_date,  V5_Age
# -----------------------------------------------------------------------------

COHORT_META <- list(
    CoLaus = list(
        date_col_base = "datexam",
        wave_prefix = c(
            Baseline = "",
            F1       = "F1",
            F2       = "F2",
            F3       = "F3"
        ),
        wave_num = c(
            Baseline = 0L,
            F1       = 1L,
            F2       = 2L,
            F3       = 3L
        )
    ),
    OsteoLaus = list(
        date_col_base = "SCAN_date",
        wave_prefix = c(
            Baseline = "Bsl_",
            V2       = "V2_",
            V3       = "V3_",
            V4       = "V4_",
            V5       = "V5_"
        ),
        wave_num = c(
            Baseline = 0L,
            V2       = 1L,
            V3       = 2L,
            V4       = 3L,
            V5       = 4L
        )
    )
)

# -----------------------------------------------------------------------------
# Numeric sentinel codes
# Variables where a specific numeric value encodes "does not know" or
# "not applicable". These are recoded to NA in the harmonise layer by
# apply_sentinel_numeric() (defined in 00_utils_harmonise.R), which iterates
# over this list and applies it to each matching column after prefix-stripping.
# Key: base column name (post-prefix-stripping). Value: vector of sentinel codes.
# -----------------------------------------------------------------------------

SENTINEL_NUMERIC <- list(
    esthrpage = 99L    # 99 = "does not know" age at HRT start
)

# -----------------------------------------------------------------------------
# ATC prefix -> derived variable name mapping
# Centralised here so adding a new drug class only requires one edit.
# Used by derive_colaus_atc.R via the ATC_PREFIXES constant.
#
# Prefixes:
#   C10   lipid-lowering drugs
#   H02   systemic corticosteroids
#   A11   vitamin D supplements
#   A12A  calcium supplements (incl. combination A12AX products)
#   N05B  benzodiazepines (anxiolytics)
#   M05BA bisphosphonates (osteoporosis treatment)
# -----------------------------------------------------------------------------

ATC_PREFIXES <- list(
    hypolip_drug_status   = "C10",
    corticoids_status     = "H02",
    vitD_status           = "A11",
    calcium_status        = "A12A",
    benzo_status          = "N05B",
    bisphosphonate_status = "M05BA"
)

# -----------------------------------------------------------------------------
# Physiological range limits
# Used in build_visits() and build_participants() to flag implausible values.
# Values outside [lo, hi] are set to NA with a companion *_oob column = TRUE.
#
# energy_kcal: 500-4200 kcal/day is an exclusion criterion per variable
#   definitions. Rows outside this range are flagged and excluded downstream.
# -----------------------------------------------------------------------------

RANGE_LIMITS <- list(
    HGS_peak          = c(lo =  1,    hi =  90),    # kg  handgrip strength
    ALM_HT2          = c(lo =  2,    hi =  12),    # kg/m2 appendicular lean mass index
    ALM_BMI          = c(lo =  0.2,  hi =   1.2),  # ALM / BMI ratio
    gait_speed       = c(lo =  0.1,  hi =   3.0),  # m/s 6-metre gait speed
    BMI              = c(lo = 10,    hi =  70),     # kg/m2
    Age              = c(lo = 49,    hi = 100),     # years
    Height           = c(lo = 120,   hi = 200),     # cm
    Weight           = c(lo = 30,    hi = 200),     # kg
    dairy_total_gday = c(lo =  0,    hi = 1500),    # g/day total dairy
    energy_kcal      = c(lo = 500,   hi = 4200)     # kcal/day (FFQ completeness check)
)

# -----------------------------------------------------------------------------
# EWGSOP2 (2019) sarcopenia thresholds — women only
# OsteoLaus is an all-female cohort; only female cut-offs are needed.
#
# Staging logic (applied in 06_derived_combined.R):
#   Probable sarcopenia : HGS_peak < hgs_kg
#   Confirmed sarcopenia: probable + ALM_HT2 < almi_kgm2
#   Severe sarcopenia   : confirmed + gait_speed <= gait_ms
#
# Reference: Cruz-Jentoft et al. Age Ageing. 2019;48(1):16-31.
# -----------------------------------------------------------------------------

EWGSOP2 <- list(
    hgs_kg    = 16.0,   # kg    low grip strength (women)
    almi_kgm2 =  5.5,   # kg/m2 low ALMI (women)
    gait_ms   =  0.8    # m/s   low gait speed (<=)
)

# -----------------------------------------------------------------------------
# FNIH sarcopenia thresholds — women only
# Sarcopenia: grip strength < hgs_kg AND ALM/BMI < alm_bmi
#
# Reference: Studenski et al. J Gerontol A Biol Sci Med Sci. 2014;69(5):547-558.
# -----------------------------------------------------------------------------

FNIH <- list(
    hgs_kg  = 16.0,    # kg          low grip strength (women)
    alm_bmi =  0.512   # kg/(kg/m2)  low ALM/BMI (women)
)