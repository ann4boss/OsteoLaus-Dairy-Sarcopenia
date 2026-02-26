# =============================================================================
# Script:   Data Cleaning Pipeline
# Purpose:  This script performs data cleaning and preprocessing tasks to prepare the dataset for analysis.
#           It includes steps such as handling missing values, correcting data types, and creating new variables as needed.
# Input:    Raw datasets of CoLaus and OsteoLaus
# Output:   Cleaned and preprocessed dataset ready for analysis
# =============================================================================


# Load libraries ---------------------------------------------------------------
library(tidyverse)      # data wrangling
library(janitor)        # clean_names, tabyl
library(skimr)          # skim()
library(naniar)         # missingness visualisation
library(data.table)     # rolling date join

# Path configuration -----------------------------------------------------------
PATH_DATA_RAW <- "data/raw/"
PATH_DATA_CLEAN <- "03_data_processed/"
PATH_REPORTS <- "06_outputs/reports/"

# =============================================================================
# IMPORT RAW DATA
# =============================================================================
# Load raw datasets of CoLaus for baseline and follow-ups (f)
baseline <- readr::read_csv(paste0(PATH_DATA_RAW, "baseline.csv")) %>% clean_names()
f1       <- readr::read_csv(paste0(PATH_DATA_RAW, "followup1.csv")) %>% clean_names()
f2       <- readr::read_csv(paste0(PATH_DATA_RAW, "followup2.csv")) %>% clean_names()
f3       <- readr::read_csv(paste0(PATH_DATA_RAW, "followup3.csv")) %>% clean_names()

# Load raw datasets of OsteoLaus for baseline and follow-ups (v)
v1       <- readr::read_csv(paste0(PATH_DATA_RAW, "followup1.csv")) %>% clean_names()
v2       <- readr::read_csv(paste0(PATH_DATA_RAW, "followup2.csv")) %>% clean_names()
v3       <- readr::read_csv(paste0(PATH_DATA_RAW, "followup3.csv")) %>% clean_names()

# Quick import check ------------------------------------------------------------

# Create a summary table to check the number of rows, columns, and unique patient IDs in each dataset
import_check <- tibble(
    dataset  = c("baseline","f1","f2","f3","v3","v4","v5"),
    n_rows   = map_int(list(baseline,f1,f2,f3,v3,v4,v5), nrow),
    n_cols   = map_int(list(baseline,f1,f2,f3,v3,v4,v5), ncol),
    n_pts    = map_int(list(baseline,f1,f2,f3,v3,v4,v5), ~ n_distinct(.x$pt)), # Assuming 'pt' is the participant identifier column in each dataset

    # Are there any missing patient IDs?
    missing_pt = map_int(list(baseline,f1,f2,f3,v3,v4,v5), ~ sum(is.na(.x$pt))),

    # Patient ID overlap with baseline (assuming baseline has all patients)
    pts_in_baseline = map_int(list(baseline,f1,f2,f3,v3,v4,v5), ~ sum(.x$pt %in% baseline$pt)),

    # Patients in this dataset NOT in baseline (potential problem!)
    pts_not_in_baseline = map_int(list(baseline,f1,f2,f3,v3,v4,v5), ~ sum(!.x$pt %in% baseline$pt)),

    # Duplicate rows (by patient ID and visit)
    duplicates = map_int(list(baseline,f1,f2,f3,v3,v4,v5), ~ sum(duplicated(.x$pt))),
    
    # Missing data percentage (overall)
    pct_missing = map_dbl(list(baseline,f1,f2,f3,v3,v4,v5),~ mean(is.na(.x)) * 100),
    
    # Complete cases (rows with no missing data)
    complete_rows = map_int(list(baseline,f1,f2,f3,v3,v4,v5),~ sum(complete.cases(.x))),
    
    )

print(import_check)

# =============================================================================
# STANDARDISE & STACK CoLaus Datasets
# =============================================================================

# Helper: strip wave prefix, add wave label -----------------------------------
strip_prefix <- function(df, prefix, wave_label) {
    df %>%
        rename_with(~ str_remove(., paste0("^", prefix, "_?")),
                    -pt) %>%          # keep pt unchanged
        mutate(wave_s1 = wave_label)
}

# Apply to each follow-up (baseline has no prefix) ----------------------------
baseline_long <- baseline %>% mutate(wave_s1 = "baseline")
f1_long       <- f1 %>% strip_prefix("F1", "F1")
f2_long       <- f2 %>% strip_prefix("F2", "F2")
f3_long       <- f3 %>% strip_prefix("F3", "F3")

# Stack datasets together -----------------------------------------------------
#TODO Assumes all datasets have the same column names/structure
colaus_long <- bind_rows(baseline_long, f1_long, f2_long, f3_long) %>% 
    mutate(wave_colaus = factor(wave_s1, levels = c("baseline","F1","F2","F3")))

# =============================================================================
# STANDARDISE & STACK OsteoLaus Dataset
# =============================================================================
#TODO if standardisation of some sort is needed
#...

# Stack datasets together -----------------------------------------------------
osteolaus_long <- bind_rows(v3, v4, v5) %>%
    mutate(wave_s2 = factor(wave_s2, levels = c("V3","V4","V5")))

# =============================================================================
# TEMPORAL LINKAGE (nearest-date rolling join)
# =============================================================================
# For each OsteoLaus wave measurement, find the closest preceding CoLaus exam
# "Preceding" enforces that exposure is measured BEFORE outcome

# Parse dates -----------------------------------------------------------------
#TODO Adjust date column names if they differ between datasets
colaus_long <- colaus_long %>% mutate(dateexam  = as.Date(dateexam))
osteolaus_long <- osteolaus_long %>% mutate(scan_date = as.Date(scan_date))

# Rolling join via data.table --------------------------------------------------

s1_dt <- colaus_long %>%
    select(pt, wave_s1, dateexam) %>%
    as.data.table() %>%
    setkey(pt, dateexam)

s2_dt <- osteolaus_long %>%
    select(pt, wave_s2, scan_date) %>%
    as.data.table() %>%
    setkey(pt, scan_date)

# roll = Inf: for each scan, look backward to find the nearest prior exam
matched <- s1_dt[s2_dt, roll = Inf, on = .(pt, dateexam = scan_date)] %>%
    as_tibble() %>%
    rename(scan_date = dateexam) %>%
    mutate(days_apart = as.numeric(scan_date - 
                                       colaus_long$dateexam[match(interaction(pt, wave_s1),
                                                              interaction(colaus_long$pt, colaus_long$wave_s1))]))

# days_apart calculation
matched <- matched %>%
    left_join(stream1 %>% select(pt, wave_s1, dateexam), by = c("pt","wave_s1")) %>%
    mutate(days_apart = as.numeric(scan_date - dateexam))


# =============================================================================
# MERGE STREAMS INTO FINAL DATASET
# =============================================================================

final_dataset <- matched %>%
    left_join(colaus_long, by = c("pt", "wave_s1", "dateexam")) %>%
    left_join(osteolaus_long, by = c("pt", "wave_s2", "scan_date"))



# =============================================================================
# MISSINGNESS ASSESSMENT
# =============================================================================

# Overall missigness by variable -----------------------------------------------
miss_summary <- final_dataset %>% miss_var_summary() 
print(miss_summary, n = 30)

# Missingess by wave -----------------------------------------------------------
#TODO define key variables to check missingness on (exposures, outcomes, key covariates)
key_vars <- c("dairy","alm","alm_ht2","wbtot_lean_mass","tug_score",
              "6mgs","hgs_max","sarcf_total","bmi","age")

miss_by_wave <- final_dataset %>%
    group_by(wave_s2) %>%
    summarise(across(any_of(key_vars),
                     ~ mean(is.na(.)) * 100,
                     .names = "pct_miss_{.col}"),
              .groups = "drop")

print(miss_by_wave)

# ── Informative dropout check ────────────────────────────────────────────
# Is missingness on outcome associated with exposure level?
final_dataset %>%
    mutate(missing_alm = is.na(alm)) %>%
    group_by(missing_alm, wave_s2) %>%
    summarise(
        mean_dairy = mean(dairy, na.rm = TRUE),
        mean_age   = mean(age_s1, na.rm = TRUE),
        pct_diab   = mean(diab == 1, na.rm = TRUE) * 100,
        n = n(),
        .groups = "drop"
    ) %>%
    print()


# =============================================================================
# VARIABLE RECODING & HARMONISATION
# =============================================================================
#TODO check these harmonisation and add all other variables needed
final_dataset <- final_dataset %>%
    mutate(
        
        # ── Education ─────────────────────────────────────────────────────────────
        edu = case_when(
            edtyp4 %in% c(0, 1, 2) ~ 1L,
            edtyp4 == 3             ~ 2L,
            edtyp4 == 4             ~ 3L,
            TRUE                    ~ NA_integer_
        ),
        edu = factor(edu,
                     levels = c(1, 2, 3),
                     labels = c("Elementary", "High school", "Superior"),
                     ordered = TRUE),
        
        # ── Ethnicity ─────────────────────────────────────────────────────────────
        # Adjust levels/labels to match your coding scheme
        ethnicity = factor(ethort_self),
        
        # ── Alcohol use ───────────────────────────────────────────────────────────
        alcool4 = factor(alcool4,
                         levels = c(1, 2, 3, 4),
                         labels = c("Non-drinker", "Occasional", "Moderate", "Heavy")),
        
        # ── Smoking status ────────────────────────────────────────────────────────
        smk_status = case_when(
            sbsmk == 0              ~ "Never",
            sbsmk == 1 & equiv == 0 ~ "Former",
            sbsmk == 1 & equiv >  0 ~ "Current",
            TRUE                    ~ NA_character_
        ),
        smk_status = factor(smk_status, levels = c("Never", "Former", "Current")),
        
        # ── Age groups ────────────────────────────────────────────────────────────
        age_group = cut(age_s1,
                        breaks = c(-Inf, 60, 65, 70, 75, Inf),
                        labels = c("<60", "60-64", "65-69", "70-74", "≥75"),
                        right  = FALSE),
        
        # ── BMI categories (WHO) ──────────────────────────────────────────────────
        bmi_cat = case_when(
            bmi < 18.5             ~ "Underweight",
            bmi >= 18.5 & bmi < 25 ~ "Normal",
            bmi >= 25   & bmi < 30 ~ "Overweight",
            bmi >= 30              ~ "Obese",
            TRUE                   ~ NA_character_
        ),
        bmi_cat = factor(bmi_cat,
                         levels = c("Underweight","Normal","Overweight","Obese"),
                         ordered = TRUE),
        
        # ── Sarcopenia classification (EWGSOP2) ───────────────────────────────────
        # Low muscle mass: ALM/height² < 7.0 kg/m² (men) / < 5.5 kg/m² (women)
        # Low muscle strength: HGS < 27 kg (men) / < 16 kg (women)
        # Low physical performance: 6MGS < 0.8 m/s
        # NOTE: Adjust cutoffs by sex when sex variable is available
        sarcopenia = case_when(
            alm_ht2 < 7.0 & (hgs_max < 27 | `6mgs` < 0.8) ~ "Sarcopenia",
            alm_ht2 < 7.0                                   ~ "Low muscle mass",
            TRUE                                             ~ "Normal"
        ),
        sarcopenia = factor(sarcopenia,
                            levels = c("Normal","Low muscle mass","Sarcopenia")),
        
        # ── Binary clinical variables → Yes/No factors ────────────────────────────
        across(all_of(binary_vars_present),
               ~ factor(., levels = c(0, 1), labels = c("No", "Yes")),
               .names = "{.col}_f"),
        
        # ── Hormone therapy (esthrp + age) ────────────────────────────────────────
        hrt = case_when(
            esthrp == 1 & !is.na(esthrpage) ~ "Current/past HRT",
            esthrp == 0                     ~ "No HRT",
            TRUE                            ~ NA_character_
        ),
        hrt = factor(hrt, levels = c("No HRT", "Current/past HRT"))
    )

# ── 14.1 Crosswalk checks: verify recoding is correct ─────────────────────────
cat("\nEducation crosswalk:\n");  print(tabyl(final_dataset, edtyp4, edu))
cat("\nSmoking crosswalk:\n");    print(tabyl(final_dataset, sbsmk, smk_status))
cat("\nBMI category crosswalk:\n"); print(tabyl(final_dataset, bmi_cat))




# =============================================================================
# EXCLUSIONS
# =============================================================================
#TODO define!


# =============================================================================
# FINAL CHECKS ON CLEAN DATASET
# =============================================================================

cat("\n--- SECTION 17: Final dataset checks ---\n")

# ── Post-cleaning skim ───────────────────────────────────────────────────
sink(paste0(PATH_REPORTS, "02_skim_clean.txt"))
skim(final_clean)
sink()
cat("✓ Post-cleaning skim saved\n")

# ── 17.2 Missingness visualisation ───────────────────────────────────────────
png(paste0(PATH_REPORTS, "03_missingness_map.png"),
    width = 1400, height = 900, res = 120)
vis_miss(final_clean %>% select(pt, wave_s2, dairy, dairy_ok,
                                alm, alm_ht2, wbtot_lean_mass,
                                tug_score, `6mgs`, hgs_max, sarcf_total,
                                bmi, age_s1, edu, smk_status),
         warn_large_data = FALSE)
dev.off()
cat("✓ Missingness map saved\n")

# ── 17.3 Key variable summary ─────────────────────────────────────────────────
final_clean %>%
    group_by(wave_s2) %>%
    summarise(
        n               = n(),
        mean_age        = mean(age_s1,    na.rm = TRUE),
        mean_dairy      = mean(dairy,     na.rm = TRUE),
        mean_alm        = mean(alm,       na.rm = TRUE),
        mean_alm_ht2    = mean(alm_ht2,   na.rm = TRUE),
        mean_hgs        = mean(hgs_max,   na.rm = TRUE),
        mean_6mgs       = mean(`6mgs`,    na.rm = TRUE),
        pct_sarcopenia  = mean(sarcopenia == "Sarcopenia", na.rm = TRUE) * 100,
        .groups = "drop"
    ) %>%
    print()


# =============================================================================
# SAVE OUTPUTS
# =============================================================================
#TODO can I safe this since sensitive DATA??
# ── 18.1 Save clean dataset ───────────────────────────────────────────────────
saveRDS(final_clean, paste0(PATH_CLEAN, "final_clean.rds"))
write_csv(final_clean, paste0(PATH_CLEAN, "final_clean.csv"))


# =============================================================================
# WRITE METADATA
# =============================================================================
#TODO check how this works




