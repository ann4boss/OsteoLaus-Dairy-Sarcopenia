# =============================================================================
# R/02_harmonise_osteolaus.R
# =============================================================================
# Type coercion, date parsing, and factor coding for a single OsteoLaus wave.
# No columns are dropped here.
#
# Column names enter prefixed (e.g. "Bsl_Age") and leave in base format ("Age")
# after strip_wave_prefix() runs at the top of harmonise_osteo().
# OsteoLaus V5 DXA columns carry an additional H_ prefix that is stripped
# separately after the wave prefix is removed.
#
# Depends on: R/00_utils_harmonise.R (safe_numeric, parse_exam_date, yn_factor)
#             R/00_utils_strip_prefix.R (strip_wave_prefix, strip_prefix_literal)
# =============================================================================

source("04_scripts/R/00_utils_harmonise.R")
source("04_scripts/R/00_utils_strip_prefix.R")

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

# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------

#' Harmonise a single OsteoLaus wave.
#'
#' Strips the wave prefix and H_ DXA prefix at V5, parses dates, coerces
#' types, and codes factors. No columns are dropped. BMI_category is NOT
#' coded here — it is derived in derive_osteo_bmi.R from the numeric BMI
#' column using WHO cut-offs.
#'
#' @param df Output of import_wave() for an OsteoLaus wave.
#' @return Tibble with correctly typed columns.
harmonise_osteo <- function(df) {
    
    stopifnot(unique(df$.cohort) == "OsteoLaus")
    wave <- unique(df$.wave)
    
    # ── Strip wave prefix -> base names -----------------------------------------
    # e.g. "Bsl_Age" -> "Age", "V2_SCAN_date" -> "SCAN_date".
    df <- strip_wave_prefix(df, wave, "OsteoLaus")
    
    # ── V5: strip additional H_ prefix from DXA columns ------------------------
    # After wave-prefix stripping, V5 DXA columns still carry "H_",
    # e.g. "H_ALM" -> "ALM".
    if (wave == "V5")
        df <- strip_prefix_literal(df, "H_")
    
    # Snapshot base-name columns after all prefix stripping but before any
    # type coercions or new column additions. The 6MGS -> gait_speed rename
    # is declared explicitly in the validate_harmonise() call at the end.
    cols_before <- names(df)
    
    # ── Date & primary key -------------------------------------------------------
    df <- dplyr::mutate(df,
                        pt            = as.integer(pt),
                        exam_date_iso = parse_exam_date(SCAN_date)
    )
    
    # ── Continuous variables -----------------------------------------------------
    df <- dplyr::mutate(df,
                        dplyr::across(
                            dplyr::any_of(.OSTEO_NUMERIC_COLS),
                            ~ safe_numeric(.x, dplyr::cur_column())
                        )
    )
    
    # ── Ethnicity: 1 = White, 2 = Other (unknown), 3 = Other -------------------
    # Per variable definitions: 1 = White, 2 = not documented, 3 = Other.
    # TODO: Level 2 is mapped to "Unknown" pending data dictionary clarification.
    if ("Ethnicity" %in% names(df))
        df <- dplyr::mutate(df,
                            Ethnicity = factor(
                                Ethnicity,
                                levels = c("1", "2", "3"),
                                labels = c("White", "Unknown", "Other")
                            )
        )
    
    # ── Binary TUG step flags (V4/V5 only) --------------------------------------
    # No sentinel codes for these flags — pass empty sentinel vector.
    df <- dplyr::mutate(df,
                        dplyr::across(
                            dplyr::any_of(.OSTEO_TUG_FLAGS),
                            ~ yn_factor(.x, sentinel = character(0))
                        )
    )
    
    # ── TUG_SCORE: ordinal 0-4 (V4/V5 only) ------------------------------------
    if ("TUG_SCORE" %in% names(df))
        df <- dplyr::mutate(df,
                            TUG_SCORE = factor(TUG_SCORE, levels = as.character(0:4), ordered = TRUE)
        )
    
    # ── SARC-F item factors (V5 only) -------------------------------------------
    df <- dplyr::mutate(df,
                        dplyr::across(
                            dplyr::any_of(.OSTEO_SARCF_ITEMS),
                            ~ factor(.x, levels = c("0", "1", "2"),
                                     labels = c("None", "Some", "A lot or unable"))
                        ),
                        dplyr::across(
                            dplyr::any_of("SARCF_FALL"),
                            ~ factor(.x, levels = c("0", "1", "2"),
                                     labels = c("None", "1-3 falls", "4+ falls"))
                        )
    )
    
    # ── Rename syntactically invalid column name ----------------------------
    # `6MGS` starts with a digit and is not a valid R name. Rename to
    # gait_speed here so all downstream code (derive_combined, build_visits,
    # etc.) can reference it without backticks or .data[["6MGS"]] workarounds.
    df <- dplyr::rename(df, gait_speed = dplyr::any_of("6MGS"))
    
    
    # -- Validate: no columns dropped, only exam_date_iso added -----------------
    # exam_date_iso is added; 6MGS is renamed to gait_speed (present only when
    # the column existed in the source data
    cols_renamed <- character(0)
    
    if ("6MGS" %in% cols_before) {
        cols_renamed["6MGS"] <- "gait_speed"
    }
    
    validate_harmonise(
        cols_before  = cols_before,
        cols_after   = names(df),
        wave         = wave,
        cols_added   = "exam_date_iso",
        cols_renamed = cols_renamed
    )
    
    
    
    return(df)
}