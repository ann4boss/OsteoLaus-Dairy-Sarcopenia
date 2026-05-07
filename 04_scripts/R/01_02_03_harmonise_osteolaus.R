# =============================================================================
# R/harmonise_osteolaus.R
# =============================================================================
# Type coercion, date parsing, and factor coding for a single OsteoLaus visit.
#
# Column names enter prefixed (e.g. "Bsl_Age") and leave in base format ("Age")
# after strip_visit_prefix() runs at the top of harmonise_osteo().
# OsteoLaus V5 DXA columns carry an additional H_ prefix that is stripped
# separately after the visit prefix is removed.
# =============================================================================


# -----------------------------------------------------------------------------
# Column lists
# -----------------------------------------------------------------------------

.OSTEO_NUMERIC_COLS <- c(
    "Age", "Height", "Weight", "BMI",
    "ALM", "ALM_HT2", "ALM_WT", "ALM_BMI",
    "6MGS",
    "HGS_R1", "HGS_R2", "HGS_R3", "HGS_L1", "HGS_L2", "HGS_L3", "HGS_MAX",
    "HEAD_LEAN_MASS",
    "LARM_LEAN_MASS",  "RARM_LEAN_MASS",  "ARMS_LEAN_MASS",
    "LLEG_LEAN_MASS",  "RLEG_LEAN_MASS",  "LEGS_LEAN_MASS",
    "TRUNK_LEAN_MASS", "LTRUNK_LEAN_MASS", "RTRUNK_LEAN_MASS",
    "SUBTOT_LEAN_MASS", "WBTOT_LEAN_MASS",
    "LTOTAL_LEAN_MASS", "RTOTAL_LEAN_MASS",
    "ANDROID_LEAN_MASS", "GYNOID_LEAN_MASS", "AND_plus_GYN_LEAN_MASS",
    "TUG_TIME",
    "SARCF_TOTAL"
)

.OSTEO_TUG_FLAGS <- c("TUG_GETUP", "TUG_GO", "TUG_TURN", "TUG_GOBACKSIT")

.OSTEO_SARCF_ITEMS <- c("SARCF_STRENGHT", "SARCF_WALK", "SARCF_CHAIR", "SARCF_STAIRS")

.DROP_COLS <- c("SCAN_date", "id_pat", "PATIENT_KEY")


# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------

#' Harmonise a single OsteoLaus visit.
#'
#' Strips the visit prefix and H_ DXA prefix at V5, parses dates, coerces
#' types, and codes factors. No columns are dropped.
#'
#' @param df Output of import_visit() for an OsteoLaus visit.
#' @return Tibble with correctly typed columns.
harmonise_osteo <- function(df) {
    
    # Basic validation
    cohorts <- df |> dplyr::distinct(.cohort) |> dplyr::pull(.cohort)
    visits   <- df |> dplyr::distinct(.visit) |> dplyr::pull(.visit)
    
    stopifnot(cohorts == "OsteoLaus")
    cohort <- cohorts[1]
    visit   <- visits[1]
    
    # ── Strip visit prefix -----------------------------------------
    # e.g. "Bsl_Age" -> "Age", "V2_SCAN_date" -> "SCAN_date".
    prefix <- COHORT_META[[cohort]][["visit_prefix"]][[visit]]
    df <- strip_prefix(df, prefix)
    
    # V5: strip additional H_ prefix from DXA columns
    # e.g. "H_ALM" -> "ALM".
    if (visit == "5") {
        df <- strip_prefix(df, "H_")
    }
    
    # ── Start dtplyr pipeline -----------------------------------------
    df <- df |>
        
        # ── DXA Method & IDs -----------------------------------------
        dplyr::mutate(
            DXA_method = factor(
                dplyr::if_else(visit %in% c("3", "4", "5"), "Hologic", "Lunar")
            ),
            pt            = as.integer(pt),
            exam_date_iso = parse_exam_date(SCAN_date)
        ) |>
        
        # ── Continuous variables -----------------------------------------
        dplyr::mutate(
            dplyr::across(
                dplyr::any_of(.OSTEO_NUMERIC_COLS),
                ~ safe_numeric(.x, dplyr::cur_column())
            )
        ) |>
        
        # ── Categorical factors -----------------------------------------
        dplyr::mutate(
            # visit
            dplyr::across(dplyr::any_of(".visit"),
                          ~ factor(.x, levels = c("1", "2", "3", "4", "5"), labels = c("Baseline", "V2", "V3", "V4", "V5"))),
            
            # Ethnicity
            # TODO: Level 2 is mapped to "Unknown" pending data dictionary clarification.
            dplyr::across(dplyr::any_of("Ethnicity"),
                          ~ factor(.x, levels = c("1", "2", "3"), 
                                   labels = c("White", "Unknown", "Other"))),
            
            # Binary TUG step flags (V4/V5)
            dplyr::across(dplyr::any_of(.OSTEO_TUG_FLAGS),
                          ~ yn_factor(.x, sentinel = character(0))),
            
            # TUG_SCORE: ordinal
            dplyr::across(dplyr::any_of("TUG_SCORE"),
                          ~ factor(.x, levels = as.character(0:4), ordered = TRUE)),
            
            # SARC-F items (V5)
            dplyr::across(dplyr::any_of(.OSTEO_SARCF_ITEMS),
                          ~ factor(.x, levels = c("0", "1", "2"),
                                   labels = c("None", "Some", "A lot or unable"))),
            
            # SARC-F Falls
            dplyr::across(dplyr::any_of("SARCF_FALL"),
                          ~ factor(.x, levels = c("0", "1", "2"),
                                   labels = c("None", "1-3 falls", "4+ falls")))
        ) |>
        
        # drop columns not needed for analysis (not requested to be retained in the harmonised dataset)
        dplyr::select(-dplyr::any_of(.DROP_COLS)) |>
        
        # ── Final Rename -----------------------------------------
        # `6MGS` starts with a digit and is not a valid R name. Rename to
        # gait_speed here so all downstream code.
        dplyr::rename(gait_speed = any_of("6MGS"))
    
    
    
    return(df)
}