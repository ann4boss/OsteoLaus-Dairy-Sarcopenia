# =============================================================================
# R/harmonise_colaus.R
# =============================================================================
# Type coercion, date parsing, and factor coding for a single CoLaus visit.
#
# Column names enter prefixed (e.g. "F1age") and leave in base format ("age")
# after strip_visit_prefix() runs at the top of harmonise_colaus().
# =============================================================================
# TODO add etsem, income2, datquest
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
  paste0("FFQ", 1:100, "amount"),
  paste0("freqFFQ", 1:100),
  
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
  "metab_synd", "crbpmed",
  "cvdbase_adj", "dbtld", "hctld",
  "miac", "strk", "chf", "cad", "angn", "cmp",
  "hdc",  "hdv",  "artm", "vslg", "ccth", "cabg", "pcin"
)



# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------

#' Harmonise a single CoLaus visit.
#'
#' Strips the visit prefix, parses dates, coerces types, codes factors, and
#' applies numeric sentinel recoding. No columns are dropped.
#'
#' @param df Output of import_visit() for a CoLaus visit.
#' @return Tibble with correctly typed columns.
harmonise_colaus <- function(df) {
  
  cohorts <- df |> dplyr::distinct(.cohort) |> dplyr::pull(.cohort)
  visits   <- df |> dplyr::distinct(.visit) |> dplyr::pull(.visit)
  
  stopifnot(cohorts == "CoLaus")
  cohort <- cohorts[1]
  visit   <- visits[1]
  

  # ── Strip visit prefix ------------------------------------------------------
  # e.g. "F1age" -> "age". No-op at Baseline (empty prefix).
  prefix <- COHORT_META[[cohort]][["visit_prefix"]][[visit]]
  df <- strip_prefix(df, prefix)
  
  # ── Start pipeline ---------------------------------------------------------
  df <- df |>
    
    # ── Date & primary key ---------------------------------------------------
    dplyr::mutate(
      pt            = as.integer(pt),
      exam_date_iso = parse_exam_date(datexam)
    ) |>
    dplyr::select(-dplyr::any_of("datexam")) |>
    
    # ── Continuous variables & Sentinels --------------------------------------
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(.COLAUS_NUMERIC_COLS),
        ~ safe_numeric(.x, dplyr::cur_column())
      )
    ) |>
    # change numeric sentinels to NA after numeric coercion
    apply_sentinel_numeric() |>
    
    
    # ── Factors (Binary & Multi-level) ----------------------------------------
    dplyr::mutate(
      # Simple Yes/No
      dplyr::across(dplyr::any_of(.COLAUS_YN_COLS), ~ yn_factor(.x)),
      
      # visit
      dplyr::across(dplyr::any_of(".visit"),
                    ~ factor(.x, levels = c( "1", "2", "3", "4"), labels = c("Baseline", "F1", "F2", "F3"))),
      
      # Smoking
      dplyr::across(dplyr::any_of("sbsmk"),
                    ~ factor(sentinel_to_na(.x, "9"),
                             levels = c("0", "1", "2"), labels = c("Never", "Former", "Current"))),
      
      # Alcohol
      dplyr::across(dplyr::any_of("alcool4"),
                    ~ factor(.x, levels = c("0", "1"), labels = c("Non-drinker", "Drinker"))),
      # Education
      dplyr::across(dplyr::any_of("edtyp4"),
                    ~ factor(.x, levels = c("1", "2", "3", "4"),
                             labels = c("University", "High school", "Apprenticeship", "Mandatory"))),
      
      # Physical Activity
      dplyr::across(dplyr::any_of("phyact"),
                    ~ factor(sentinel_to_na(.x, "9"),
                             levels = c("0", "1", "2"), labels = c("Never", "Once/week", "Twice/week"))),
      # Sex
      dplyr::across(dplyr::any_of("sex"),
                    ~ factor(.x, levels = c("0", "1"), labels = c("Female", "Male"))),
      
      # Dominant hand
      dplyr::across(
        dplyr::any_of("lateralite"),
        ~ factor(.x, levels = c("1", "2", "3"), labels = c("Right", "Left", "Ambidextrous"))
      ),
      
      # Ethnicity (self-reported)
      dplyr::across(
        dplyr::any_of("ethori_self"),
        ~ factor(.x, levels = c("A", "B", "W", "O", "X", "K"),
                 labels = c("Asian", "Black/African", "White", "Other", "Unknown", "Does not know"))
      ),
      
      # Marital status (Living alone vs. Living in couple)
      dplyr::across(
        dplyr::any_of("mrtsts2"),
        ~ factor(.x, levels = c("0", "1"), labels = c("Living alone", "Living in couple"))
      ),
      
      
      # FFQ Items (Frequency 1-7)
      dplyr::across(dplyr::any_of(paste0("FFQ", 1:86)),
                    ~ factor(.x, levels = as.character(1:7),
                             labels = c("Never", "1/month", "2-3/month", "1-2/week",
                                        "3-4/week", "1/day", "2+/day"))),
      
      # FFQ Portions (1-3)
      dplyr::across(dplyr::any_of(paste0("FFQp", 1:86)),
                    ~ factor(.x, levels = c("1", "2", "3"), labels = c("Less", "Equal", "More"))),
      
      
      
      # Dairy intake according to Swiss guidelines compliance (0-1)
      dplyr::across(dplyr::any_of("Dairy_OK"),
                    ~ factor(.x, levels = c("0", "1"), labels = c("< 3 servings/day", ">= 3 servings/day")))
      
    )
    
  
  
  # ── visit-specific logic (Handgrip & Diabetes) -------------------------------
  #TODO: something goes wrong and all values are NA
  # Handgrip
  # Baseline: 0 = No problem, 1 = Yes (problem noted) -> recode to 3 = Yes, unspecified problem.
  # F2:    0 = No problem, 1 = Pain/arthrosis 
  # F3:    0 = No problem, 1 = Pain/arthrosis, 2 = No time/home/rejected.
  if ("handgrip_com" %in% df$vars) {
    df <- df |>
      dplyr::mutate(
        handgrip_com = factor(
          dplyr::case_when(
            visit == "Baseline" & handgrip_com == "0" ~ 0L,
            visit == "Baseline" & handgrip_com == "1" ~ 3L,
            visit == "F2" & handgrip_com == "0" ~ 0L,
            visit == "F2" & handgrip_com == "1" ~ 1L,
            visit == "F3" & handgrip_com == "0" ~ 0L,
            visit == "F3" & handgrip_com == "1" ~ 1L,
            visit == "F3" & handgrip_com == "2" ~ 2L,
            TRUE ~ NA_integer_
          ),
          levels = 0:3,
          labels = c("No problem", "Pain/arthrosis", "No time/home/rejected", "Yes (unspecified problem)")
        )
      )
  }
  
  # Diabetes
  # F1         : 0 = No,     1 = Yes -> recoded to 2 = Diabetes
  # All others : 0 = Normal, 1 = IFG,   2 = Diabetes
  if ("DIAB2" %in% df$vars) {
    df <- df |>
      dplyr::mutate(
        DIAB2 = factor(
          dplyr::case_when(
            visit == "F1" & DIAB2 == "0" ~ 0L,
            visit == "F1" & DIAB2 == "1" ~ 2L,
            visit != "F1" & DIAB2 == "0" ~ 0L,
            visit != "F1" & DIAB2 == "1" ~ 1L,
            visit != "F1" & DIAB2 == "2" ~ 2L,
            TRUE ~ NA_integer_
          ),
          levels = 0:2, labels = c("Normal", "IFG", "Diabetes"), ordered = TRUE
        )
      )
  }
  
  # ── Finalize: Rename and Collect ---------------------------------------------
  out <- df |>
    dplyr::rename(dplyr::any_of(c(
      Age = "age", HGS_MAX = "handgrip", Height = "ht", Weight = "wt"
    ))) |>
    dplyr::as_tibble()
  
  return(out)
}



