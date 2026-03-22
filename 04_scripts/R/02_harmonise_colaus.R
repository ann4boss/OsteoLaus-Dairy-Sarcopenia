# =============================================================================
# R/02_harmonise_colaus.R
# =============================================================================
# Type coercion, date parsing, and factor coding for a single CoLaus wave.
# No columns are dropped here -> validated by validate_harmonise_colaus().
#
# Column names enter prefixed (e.g. "F1age") and leave in base format ("age")
# after strip_wave_prefix() runs at the top of harmonise_colaus().
#
# =============================================================================

# -----------------------------------------------------------------------------
# Column lists
# -----------------------------------------------------------------------------
.COLAUS_NUMERIC_COLS <- c(
  "age", "ht", "wt", "BMI", "WHR", "bmpsc",
  "conso_hebdo", "sumalco",
  "esthrpage", "subsp", "subyr", "povdage",
  "handgrip",
  # all columns starting with "agediag"
  paste0("agediag", c("_dbts","cmp","hdc", "hdv", "chf", "artm",
                       "cad", "angn", "miac", "strk", "vslg", "ccth", "cabg", "pcin")),
  # Dietary totals (incl. and excl. alcohol)
  "sumtot1",  "sumtot3",
  "sumprot1", "sumprot3", "sumpveg1", "sumpveg3", "sumpani1", "sumpani3",
  "sumgluc1", "sumgluc3", "sumlipi1", "sumlipi3",
  "sumvitd1", "sumvitd3", "sumcalc3",
  "pct_prot1", "pct_prot3", "pct_pveg1", "pct_pveg3",
  "pct_pani1", "pct_pani3", "pct_gluc1", "pct_gluc3",
  "pct_lipi1", "pct_lipi3", "pct_alco1",
  # Dairy compliance score and FFQ amounts
  "Dairy", "Diet_compl", "mnwlk",
  paste0("FFQ",     c(1:8, 52, 53, 63, 68, 71, 82:86), "amount"),
  paste0("freqFFQ", c(1:8, 52, 53, 63, 68, 71, 82:86)),
  # Physical activity
  "PAFQ_SE",  "PAFQ_SE_pct",
  "PAFQ_LPA", "PAFQ_LPA_pct",
  "PAFQ_MPA", "PAFQ_MPA_pct",
  "PAFQ_VPA", "PAFQ_VPA_pct"
)

# Yes/No binary variables with default sentinels.
.COLAUS_YN_COLS <- c(
  "alcuse", "antiDIAB", "DIAB", "DIAB_Hb",
  "dbdrg", "orldrg", "insn",
  "antiHTA", "HTA", "hypolip",
  "esthrp", "bthc",
  "metab_synd",
  "cvdbase_adj", "dbtld", "hctld",
  "miac", "strk", "chf", "cad", "angn", "cmp",
  "hdc",  "hdv",  "artm", "vslg", "ccth", "cabg", "pcin"
)


# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------

#' Harmonise a single CoLaus wave.
#'
#' Strips the wave prefix, parses dates, coerces types, codes factors, and
#' applies numeric sentinel recoding. No columns are dropped.
#'
#' @param df Output of import_wave() for a CoLaus wave.
#' @return Tibble with correctly typed columns.
harmonise_colaus <- function(df) {
  
  stopifnot(unique(df$.cohort) == "CoLaus")
  wave <- unique(df$.wave)
  
  # ── Strip wave prefix -> base names -----------------------------------------
  # e.g. "F1age" -> "age". No-op at Baseline (empty prefix).
  df <- strip_wave_prefix(df, wave, "CoLaus")
  
  # Snapshot base-name columns immediately after prefix stripping.
  # The validator compares these against names(df) at the end of the function.
  cols_before <- names(df)
  
  # ── Date & primary key -------------------------------------------------------
  df <- dplyr::mutate(df,
                      pt            = as.integer(pt),
                      exam_date_iso = parse_exam_date(datexam)
  )
  
  # ── Continuous variables -----------------------------------------------------
  df <- dplyr::mutate(df,
                      dplyr::across(
                        dplyr::any_of(.COLAUS_NUMERIC_COLS),
                        ~ safe_numeric(.x, dplyr::cur_column())
                      )
  )
  
  # ── Apply SENTINEL_NUMERIC ---------------------------------------------------
  # esthrpage: 99 -> NA (centralised in constants, applied here after numeric
  # coercion so the comparison works correctly on numeric values).
  df <- apply_sentinel_numeric(df)
  
  # ── Simple Yes/No binaries (sentinel 8 and 9) --------------------------------
  df <- dplyr::mutate(df,
                      dplyr::across(dplyr::any_of(.COLAUS_YN_COLS), ~ yn_factor(.x))
  )
  
  
  # ── Dairy_OK binary ----------------------------------------------------------
  # 0 = < 3 servings/day, 1 = >= 3 servings/day (Swiss dietary guidelines).
  if ("Dairy_OK" %in% names(df))
    df <- dplyr::mutate(df,
                        Dairy_OK = factor(Dairy_OK, levels = c("0", "1"), labels = c("No", "Yes"))
    )
  
  # ── Multi-level factors ------------------------------------------------------
  df <- dplyr::mutate(df,
                      dplyr::across(
                        dplyr::any_of("sbsmk"),
                        ~ factor(sentinel_to_na(.x, "9"),
                                 levels = c("0", "1", "2"), labels = c("Never", "Former", "Current"))
                      ),
                      
                      dplyr::across(
                        dplyr::any_of("alcool4"),
                        ~ factor(.x, levels = c("0", "1"),
                                 labels = c("Non-drinker", "Drinker"))
                      ),
                      dplyr::across(
                        dplyr::any_of("edtyp4"),
                        # Levels ordered from highest to lowest education to match
                        # the derived education_level (ISCED) ordering in derive_education().
                        ~ factor(.x, levels = c("1", "2", "3", "4"),
                                 labels = c("University", "High school", "Apprenticeship", "Mandatory"))
                      ),
                      dplyr::across(
                        dplyr::any_of("phyact"),
                        ~ factor(sentinel_to_na(.x, "9"),
                                 levels = c("0", "1", "2"), labels = c("Never", "Once/week", "Twice/week"))
                      ),
                      dplyr::across(
                        dplyr::any_of("lateralite"),
                        ~ factor(.x, levels = c("1", "2", "3"), labels = c("Right", "Left", "Ambidextrous"))
                      ),
                      dplyr::across(
                        dplyr::any_of("sex"),
                        ~ factor(.x, levels = c("0", "1"), labels = c("Female", "Male"))
                      ),
                      dplyr::across(
                        dplyr::any_of("ethori_self"),
                        ~ factor(.x, levels = c("A", "B", "W", "O", "X", "K"),
                                 labels = c("Asian", "Black/African", "White", "Other", "Unknown", "Does not know"))
                      ),
                      dplyr::across(
                        dplyr::any_of("mrtsts2"),
                        ~ factor(.x, levels = c("0", "1"), labels = c("Living alone", "Living in couple"))
                      ),
                      dplyr::across(
                        dplyr::any_of("crbpmed"),
                        # Sentinel: 8 = Not relevant, 9 = Does not know -> both to NA.
                        ~ factor(sentinel_to_na(.x, c("8", "9")),
                                 levels = c("0", "1"), labels = c("No", "Yes"))
                      ),
                      # FFQ frequency codes 1-7 (any of the 86 items actually present in data)
                      dplyr::across(
                        dplyr::any_of(paste0("FFQ", 1:86)),
                        ~ factor(.x, levels = as.character(1:7),
                                 labels = c("Never", "1/month", "2-3/month", "1-2/week",
                                            "3-4/week", "1/day", "2+/day"))
                      ),
                      # FFQ portion size codes 1-3
                      dplyr::across(
                        dplyr::any_of(paste0("FFQp", 1:86)),
                        ~ factor(.x, levels = c("1", "2", "3"), labels = c("Less", "Equal", "More"))
                      )
  )
  
  # ── handgrip_com: wave-specific coding --------------------------------------
  # Baseline: 0 = No problem, 1 = Yes (problem noted).
  # F2/F3:    0 = No problem, 1 = Pain/arthrosis, 2 = No time/home/rejected.
  # Harmonised to a common 3-level factor; Baseline "1" maps to "Pain/arthrosis"
  # as the closest available category.
  if ("handgrip_com" %in% names(df))
    df <- dplyr::mutate(df,
                        handgrip_com = factor(
                          handgrip_com,
                          levels = c("0", "1", "2"),
                          labels = c("No problem", "Pain/arthrosis", "No time/home/rejected")
                        )
    )
  
  # ── DIAB2: wave-specific level structure ------------------------------------
  if ("DIAB2" %in% names(df))
    df <- dplyr::mutate(df, DIAB2 = harmonise_diab2(DIAB2, wave))
  
  # -- Validate: no columns dropped, only exam_date_iso added -----------------
  validate_harmonise(cols_before, names(df), wave)
  
  return(df)
}
