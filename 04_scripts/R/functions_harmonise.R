# =============================================================================
# R/functions_harmonise.R
# =============================================================================
# Type conversion, date parsing, and sentinel recoding.
# No columns are dropped here.
#
# Both cohorts prefix column names per wave (see WAVE_PREFIX in
# functions_import.R). harmonise_colaus() and harmonise_osteo() each begin
# with a bulk rename step that strips the wave prefix so all downstream
# code works against consistent base names regardless of wave.
#
# OsteoLaus V5 DXA columns carry an additional H_ prefix that is stripped
# after the wave-prefix rename.

# ── Low-level helpers ---------------------------------------------------------

# Coerce a character column to numeric, warning on value loss
safe_numeric <- function(x, col) {
  out  <- suppressWarnings(as.numeric(x))
  lost <- sum(!is.na(x) & is.na(out))
  if (lost > 0)
    warning(glue::glue("safe_numeric: {lost} value(s) lost coercing '{col}'"))
  out
}

# Recode sentinel codes to NA, keeping other values unchanged
# By default treats "8" and "9" as sentinels for simplification. 
# "8" and "9" can mean the following depending on the variable: "Does not know", "Not applicable", "No Data".
sentinel_to_na <- function(x, codes = c("8", "9"))
  dplyr::if_else(x %in% codes, NA_character_, x)

# Build a Yes/No factor, treating sentinel codes as NA
yn_factor <- function(x, sentinel = c("8", "9"))
  factor(sentinel_to_na(x, sentinel), levels = c("0", "1"), labels = c("No", "Yes"))

# Parse DDMonYYYY dates (e.g. "21mar2025") to ISO Date.
# str_to_title() normalises case before as.Date(), which is locale-sensitive
# with %b and typically requires a capital first letter.
parse_exam_date <- function(x)
  as.Date(stringr::str_to_title(x), format = "%d%b%Y")

# Harmonise DIAB2, which has different levels at F1 vs all other CoLaus waves.
# F1         : 0=No, 1=Yes (binary — no IFG level)
# All others : 0=Normal, 1=IFG, 2=Diabetes
#TODO: is this correct?
harmonise_diab2 <- function(x, wave) {
  if (wave == "F1") {
    out <- dplyr::case_when(x == "0" ~ "Normal", x == "1" ~ "Diabetes", TRUE ~ NA_character_)
  } else {
    out <- dplyr::case_when(
      sentinel_to_na(x, "9") == "0" ~ "Normal",
      sentinel_to_na(x, "9") == "1" ~ "IFG",
      sentinel_to_na(x, "9") == "2" ~ "Diabetes",
      TRUE                           ~ NA_character_
    )
  }
  factor(out, levels = c("Normal", "IFG", "Diabetes"))
}

#' Strip the wave prefix from every column in df that starts with it.
#'
#' Looks up the prefix for the given cohort/wave from WAVE_PREFIX. If the
#' prefix is empty (CoLaus Baseline) the data frame is returned unchanged.
#' Any column whose name begins with the prefix is renamed to the suffix that
#' remains after removing it — no explicit list of base names required.
#'
#' @param df     Data frame straight from import_wave().
#' @param wave   Pipeline wave label, e.g. "F1", "V2", "Baseline".
#' @param cohort "CoLaus" or "OsteoLaus".
#' @return df with wave-prefix stripped from all affected column names.
strip_wave_prefix <- function(df, wave, cohort) {
  prefix <- WAVE_PREFIX[[cohort]][[wave]]
  if (nchar(prefix) == 0) return(df)
  
  cols        <- names(df)
  has_prefix  <- startsWith(cols, prefix)
  new_names   <- ifelse(has_prefix, substring(cols, nchar(prefix) + 1L), cols)
  # stats::setNames() is used to rename all columns in one step 
  stats::setNames(df, new_names)
}

#' Strip an arbitrary literal prefix from every column name that begins with it.
#'
#' A generalised version of strip_wave_prefix() for cases where the prefix is
#' not wave-derived. Currently used to remove the extra H_ prefix from DXA
#' column names at OsteoLaus V5.
#'
#' @param df     Data frame.
#' @param prefix Literal string prefix to strip, e.g. "H_".
#' @return df with prefix stripped from all matching column names.
strip_wave_prefix_literal <- function(df, prefix) {
  cols      <- names(df)
  has_pfx   <- startsWith(cols, prefix)
  new_names <- ifelse(has_pfx, substring(cols, nchar(prefix) + 1L), cols)
  
  dupes <- new_names[duplicated(new_names) & has_pfx]
  if (length(dupes) > 0)
    warning(glue::glue(
      "strip_wave_prefix_literal('{prefix}'): duplicate names after stripping: ",
      "{paste(dupes, collapse = ', ')}"
    ))
  
  stats::setNames(df, new_names)
}

# ── CoLaus harmonisation ------------------------------------------------------

#' Harmonise a single CoLaus wave.
harmonise_colaus <- function(df) {
  
  stopifnot(unique(df$.cohort) == "CoLaus")
  wave   <- unique(df$.wave)
  cohort <- "CoLaus"
  
  # ── Strip wave prefix → base names -----------------------------------------
  # Renames every prefixed column to its base name in one step,
  # e.g. "F1age" → "age". No-op at Baseline where the prefix is empty.
  df <- strip_wave_prefix(df, wave, cohort)
  
  # ── Date & primary key ------------------------------------------------------
  #TODO date_parse_fail
  df <- dplyr::mutate(df,
                      pt              = as.integer(pt),
                      exam_date_iso   = parse_exam_date(datexam),
                      date_parse_fail = is.na(exam_date_iso) & !is.na(datexam)
  )
  
  # ── Continuous variables ----------------------------------------------------
  NUMERIC_COLS <- c(
    "age", "ht", "wt", "BMI", "WHR", "bmpsc",
    "conso_hebdo", "sumalco",
    "esthrpage", "subsp", "subyr", "povdage",
    "handgrip",
    "sumtot1","sumtot3",
    "sumprot1","sumprot3","sumpveg1","sumpveg3","sumpani1","sumpani3",
    "sumgluc1","sumgluc3","sumlipi1","sumlipi3",
    "sumvitd1","sumvitd3","sumcalc3",
    "pct_prot1","pct_prot3","pct_pveg1","pct_pveg3",
    "pct_pani1","pct_pani3","pct_gluc1","pct_gluc3",
    "pct_lipi1","pct_lipi3","pct_alco1",
    "Diet_compl", "mnwlk", "etsem", "etj",
    paste0("FFQ", c(1:8, 52, 53, 63, 68, 71, 82:86), "amount"),
    paste0("freqFFQ", 1:86),
    "PAFQ_SE","PAFQ_SE_pct","PAFQ_LPA","PAFQ_LPA_pct",
    "PAFQ_MPA","PAFQ_MPA_pct","PAFQ_VPA","PAFQ_VPA_pct",
    "agediag_dbts"
  )
  
  df <- dplyr::mutate(df,
                      dplyr::across(dplyr::any_of(NUMERIC_COLS), ~ safe_numeric(.x, dplyr::cur_column()))
  )
  
  # ── Simple Yes/No binaries --------------------------------------------------
  YN_COLS <- c(
    "alcuse", "alcool4", "antiDIAB", "DIAB", "DIAB_Hb",
    "dbtld", "dbdrg", "orldrg", "insn",
    "cdv_event", "cvdbase_adj",
    "antiHTA", "HTA", "hypolip", "hctld",
    "hypolip_drug_status", "corticoids_status", "vitD_status",
    "calcium_status", "benzo_status",
    "esthrp", "bthc",
    "metab_synd", "handgrip_com"
  )
  
  df <- dplyr::mutate(df,
                      dplyr::across(dplyr::any_of(YN_COLS), ~ yn_factor(.x))
  )
  
  # ── CVD component flags (sentinel 9 = does not know) -----------------------
  CVD_FLAGS <- c(
    "miac","strk","chf","cad","angn","cmp",
    "hdc","hdv","artm","vslg","ccth","cabg","pcin"
  )
  
  df <- dplyr::mutate(df,
                      dplyr::across(dplyr::any_of(CVD_FLAGS), ~ yn_factor(.x, sentinel = "9"))
  )
  
  # ── Multi-level factors ----------------------------------------------------
  df <- dplyr::mutate(df,
                      dplyr::across(
                        dplyr::any_of("sbsmk"),
                        ~ factor(sentinel_to_na(.x, "9"),
                                 levels = c("0","1","2"), labels = c("Never","Former","Current"))
                      ),
                      dplyr::across(
                        dplyr::any_of("edtyp4"),
                        ~ factor(.x, levels = c("1","2","3","4"),
                                 labels = c("University","High school","Apprenticeship","Mandatory"))
                      ),
                      dplyr::across(
                        dplyr::any_of("phyact"),
                        ~ factor(sentinel_to_na(.x, "9"),
                                 levels = c("0","1","2"), labels = c("Never","Once/week","Twice/week"))
                      ),
                      dplyr::across(
                        dplyr::any_of("lateralite"),
                        ~ factor(.x, levels = c("1","2","3"), labels = c("Right","Left","Ambidextrous"))
                      ),
                      dplyr::across(
                        dplyr::any_of("sex"),
                        ~ factor(.x, levels = c("0","1"), labels = c("Female","Male"))
                      ),
                      dplyr::across(
                        dplyr::any_of("ethori_self"),
                        ~ factor(.x, levels = c("A","B","W","O","X","K"),
                                 labels = c("Asian","Black/African","White","Other","Unknown","Does not know"))
                      ),
                      dplyr::across(
                        dplyr::any_of("mrtsts2"),
                        ~ factor(.x, levels = c("0","1"), labels = c("Living alone","Living in couple"))
                      ),
                      dplyr::across(
                        dplyr::any_of("crbpmed"),
                        ~ factor(sentinel_to_na(.x, c("8","9")), levels = c("0","1"), labels = c("No","Yes"))
                      ),
                      dplyr::across(
                        dplyr::any_of(paste0("FFQ", 1:86)),
                        ~ factor(.x, levels = as.character(1:7),
                                 labels = c("Never","1/month","2-3/month","1-2/week","3-4/week","1/day","2+/day"))
                      ),
                      dplyr::across(
                        dplyr::any_of(paste0("FFQp", 1:86)),
                        ~ factor(.x, levels = c("1","2","3"), labels = c("Less","Equal","More"))
                      )
  )
  
  # ── DIAB2: wave-specific level structure ------------------------------------
  if ("DIAB2" %in% names(df))
    df <- dplyr::mutate(df, DIAB2 = harmonise_diab2(DIAB2, wave))
  
  return(df)
}

# ── OsteoLaus harmonisation ---------------------------------------------------

#' Harmonise a single OsteoLaus wave.
harmonise_osteo <- function(df) {
  
  stopifnot(unique(df$.cohort) == "OsteoLaus")
  wave   <- unique(df$.wave)
  cohort <- "OsteoLaus"
  
  # ── Strip wave prefix → base names -----------------------------------------
  # Strips e.g. "Bsl_" from "Bsl_SCAN_date" → "SCAN_date" for every column
  # that begins with the wave prefix. No explicit column list required.
  df <- strip_wave_prefix(df, wave, cohort)
  
  # ── V5: strip additional H_ prefix from DXA columns ------------------------
  # At V5 the DXA scanner produced column names with an extra H_ prefix after
  # the wave prefix was removed, e.g. "V5_H_ALM" → "H_ALM" → "ALM".
  # strip_wave_prefix() is reused here with "H_" as an ad-hoc prefix.
  if (wave == "V5")
    df <- strip_wave_prefix_literal(df, "H_")
  
  # ── Date & primary key ------------------------------------------------------
  #TODO date_parse_fail
  df <- dplyr::mutate(df,
                      pt              = as.integer(pt),
                      exam_date_iso   = parse_exam_date(SCAN_date),
                      date_parse_fail = is.na(exam_date_iso) & !is.na(SCAN_date)
  )
  
  # ── Continuous variables ----------------------------------------------------
  NUMERIC_COLS <- c(
    "Age", "Height", "Weight", "BMI",
    "ALM", "ALM_HT2", "ALM_WT", "ALM_BMI",
    "6MGS",
    "HGS_R1","HGS_R2","HGS_R3","HGS_L1","HGS_L2","HGS_L3","HGS_MAX",
    "HEAD_LEAN_MASS","LARM_LEAN_MASS","RARM_LEAN_MASS","ARMS_LEAN_MASS",
    "LLEG_LEAN_MASS","RLEG_LEAN_MASS","LEGS_LEAN_MASS",
    "TRUNK_LEAN_MASS","LTRUNK_LEAN_MASS","RTRUNK_LEAN_MASS",
    "SUBTOT_LEAN_MASS","WBTOT_LEAN_MASS",
    "LTOTAL_LEAN_MASS","RTOTAL_LEAN_MASS",
    "ANDROID_LEAN_MASS","GYNOID_LEAN_MASS","AND_plus_GYN_LEAN_MASS",
    "TUG_TIME", "SARCF_TOTAL",
    "ewgsop2_sarcopenia_stage", "FNIH_sarcopenia"
  )
  
  df <- dplyr::mutate(df,
                      dplyr::across(dplyr::any_of(NUMERIC_COLS), ~ safe_numeric(.x, dplyr::cur_column()))
  )
  
  # ── ewgsop2_sarcopenia_stage: ordered factor --------------------------------
  if ("ewgsop2_sarcopenia_stage" %in% names(df))
    df <- dplyr::mutate(df,
                        ewgsop2_sarcopenia_stage = factor(
                          ewgsop2_sarcopenia_stage,
                          levels = c(0, 1, 2, 3),
                          labels = c("None","Probable","Confirmed","Severe"),
                          ordered = TRUE
                        )
    )
  
  # ── FNIH_sarcopenia: ordered factor --------------------------------
  #TODO NEEDS TO BE CODED
  
  # ── BMI_category ------------------------------------------------------------
  if ("BMI_category" %in% names(df))
    df <- dplyr::mutate(df,
                        BMI_category = factor(BMI_category, levels = c("1","2","3","4"),
                                              labels = c("Underweight","Normal","Overweight","Obese"))
    )
  
  # ── Binary TUG step flags (V4/V5 only) -------------------------------------
  df <- dplyr::mutate(df,
                      dplyr::across(
                        dplyr::any_of(c("TUG_GETUP","TUG_GO","TUG_TURN","TUG_GOBACKSIT")),
                        ~ yn_factor(.x, sentinel = character(0))
                      )
  )
  
  # ── TUG_SCORE: ordinal 0–4 (V4/V5 only) ------------------------------------
  if ("TUG_SCORE" %in% names(df))
    df <- dplyr::mutate(df,
                        TUG_SCORE = factor(TUG_SCORE, levels = as.character(0:4), ordered = TRUE)
    )
  
  # ── SARC-F item factors (V5 only) ------------------------------------------
  df <- dplyr::mutate(df,
                      dplyr::across(
                        dplyr::any_of(c("SARCF_STRENGHT","SARCF_WALK","SARCF_CHAIR","SARCF_STAIRS")),
                        ~ factor(.x, levels = c("0","1","2"),
                                 labels = c("None","Some","A lot or unable"))
                      ),
                      dplyr::across(
                        dplyr::any_of("SARCF_FALL"),
                        ~ factor(.x, levels = c("0","1","2"),
                                 labels = c("None","1-3 falls","4+ falls"))
                      )
  )
  
  return(df)
}

# ── Dispatcher ----------------------------------------------------------------

#' Harmonise a single imported wave — routes to the correct cohort function.
#'
#' @param df Output of import_wave().
#' @return Tibble with correctly typed columns; no columns removed.
harmonise_wave <- function(df) {
  stopifnot(length(unique(df$.wave)) == 1)
  
  cohort <- unique(df$.cohort)
  if (cohort == "CoLaus")    return(harmonise_colaus(df))
  if (cohort == "OsteoLaus") return(harmonise_osteo(df))
  stop(glue::glue("Unknown cohort: '{cohort}'"))
}