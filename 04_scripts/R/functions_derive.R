# =============================================================================
# R/functions_derive.R
# =============================================================================
# Build every derived analytical variable.
# Input: output of clean_data()$data — a merged long tibble where:
#   - education_level : factor Low/Medium/High (ISCED grouping)
#   - sbsmk           : factor Never/Former/Current
#   - exam_date_iso   : Date
#   - .wave           : character wave label ("Baseline", "F1", ...)
#   - .wave_num       : integer (0 = Baseline)
#   - ALM             : Appendicular Lean Mass in GRAMS (OsteoLaus DXA)
#   - ALM_HT2         : Pre-computed ALM/Height^2 in KG/M^2 (OsteoLaus DXA)
#   - HGS_MAX         : Peak grip strength in kg (OsteoLaus V5)
#   - handgrip        : Peak grip strength in kg (CoLaus)

# ── alcohol_category ----------------------------------------------------------

UNIT_TO_GDAY <- 11 / 7   # 1 unit/week -> g/day  (midpoint 10-12 g/unit)

classify_alc <- function(g) {
    dplyr::case_when(
        is.na(g) ~ NA_integer_,
        g == 0   ~ 1L,   # Non-drinker
        g <= 6   ~ 2L,   # Light
        g <= 12  ~ 3L,   # Moderate
        g >  12  ~ 4L    # Heavy
    )
}

derive_alcohol <- function(df) {
    df |>
        dplyr::mutate(
            alc_gday = dplyr::case_when(
                # At Baseline only conso_hebdo (weekly units) is available
                .wave == "Baseline" ~ conso_hebdo * UNIT_TO_GDAY,
                # At follow-ups prefer sumalco (g/day from FFQ); fall back to units
                !is.na(sumalco)     ~ sumalco,
                !is.na(conso_hebdo) ~ conso_hebdo * UNIT_TO_GDAY,
                TRUE                ~ NA_real_
            )
        ) |>
        # Fill remaining gaps across waves (carry from nearest available wave).
        # Per variable_definitions.md: "take category from previous (or next)
        # time point, if this does not exist exclude participant."
        # Filling alc_gday here; participants with ALL waves missing will have
        # NA alcohol_category and can be excluded downstream if needed.
        dplyr::group_by(pt) |>
        dplyr::arrange(.wave_num, .by_group = TRUE) |>
        tidyr::fill(alc_gday, .direction = "downup") |>
        dplyr::ungroup() |>
        dplyr::mutate(
            alcohol_category = factor(
                classify_alc(alc_gday),
                levels = 1:4,
                labels = c("Non-drinker", "Light", "Moderate", "Heavy")
            )
        )
}

# ── diabetes_status -----------------------------------------------------------

derive_diabetes <- function(df) {
    df |> dplyr::mutate(
        diabetes_status = factor(
            dplyr::case_when(
                # Level 3 – Insulin-treated: highest priority
                DIAB == "Yes" & insn == "Yes"                           ~ 3L,
                # Level 2 – Oral/lab-confirmed diabetes
                DIAB == "Yes" | DIAB_Hb == "Yes" |
                    dbtld == "Yes" | antiDIAB == "Yes"                  ~ 2L,
                # Level 4 – Impaired fasting glucose / prediabetes
                !is.na(DIAB2) & as.character(DIAB2) == "IFG"           ~ 4L,
                # Level 1 – No evidence of diabetes
                TRUE                                                    ~ 1L
            ),
            levels = 1:4,
            labels = c("No diabetes", "Diabetes (oral/lab)",
                       "Diabetes (insulin)", "IGT/Prediabetes")
        )
    )
}

# ── cdv_event ----------------------------------------------------------------

CVD_VARS <- c("miac", "strk", "chf", "cad", "angn", "cmp",
              "hdc",  "hdv",  "artm", "vslg", "ccth", "cabg", "pcin")

derive_cvd <- function(df) {
    cvd_cols <- intersect(CVD_VARS, names(df))
    df |>
        dplyr::mutate(
            .cvd_yes = rowSums(
                dplyr::across(dplyr::all_of(cvd_cols), ~ .x == "Yes"),
                na.rm = TRUE
            ),
            .cvd_obs = rowSums(
                !is.na(dplyr::across(dplyr::all_of(cvd_cols)))
            ),
            # Yes if any flag positive; NA if all flags missing; No otherwise.
            cdv_event = factor(
                dplyr::case_when(
                    .cvd_yes > 0  ~ 1L,
                    .cvd_obs == 0 ~ NA_integer_,
                    TRUE          ~ 0L
                ),
                levels = 0:1, labels = c("No", "Yes")
            )
        ) |>
        dplyr::select(-.cvd_yes, -.cvd_obs)
}

# ── hrt_status ---------------------------------------------------------------
# HRT status is derived for women only (OsteoLaus is female-only).
# Variables:
#   bthc    : currently using HRT (Yes/No)  — available at follow-ups (F1+)
#   esthrp  : ever used HRT (Yes/No)        — available at baseline
#   subsp   : year HRT was stopped          — available at baseline/follow-ups
#   exam_date_iso : Date of examination

derive_hrt <- function(df) {
    df |> dplyr::mutate(
        hrt_status = factor(
            dplyr::case_when(
                # ── Follow-up waves: use bthc (current HRT use) ──────────────
                .wave != "Baseline" & bthc == "Yes"                      ~ "Current",
                .wave != "Baseline" & bthc == "No" & esthrp == "Yes"     ~ "Former",
                .wave != "Baseline" & bthc == "No"                       ~ "Never",
                # ── Baseline: infer from esthrp + cessation year ─────────────
                # Never used HRT
                .wave == "Baseline" &
                    (esthrp == "No" | is.na(esthrp))                      ~ "Never",
                # Used HRT but stopped before exam year -> Former
                .wave == "Baseline" & esthrp == "Yes" &
                    !is.na(subsp) & !is.na(exam_date_iso) &
                    subsp < lubridate::year(exam_date_iso)                 ~ "Former",
                # Used HRT and no cessation year recorded -> still Current
                .wave == "Baseline" & esthrp == "Yes"                     ~ "Current",
                TRUE                                                       ~ NA_character_
            ),
            levels = c("Never", "Former", "Current")
        )
    )
}

# ── BMI_category -------------------------------------------------------------

derive_bmi_cat <- function(df) {
    df |> dplyr::mutate(
        BMI_category = factor(
            dplyr::case_when(
                is.na(BMI) ~ NA_character_,
                BMI < 18.5 ~ "Underweight",
                BMI < 25   ~ "Normal",
                BMI < 30   ~ "Overweight",
                TRUE       ~ "Obese"
            ),
            levels = c("Underweight", "Normal", "Overweight", "Obese")
        )
    )
}

# ── Dairy exposure variables --------------------------------------------------
# FFQ item numbers that belong to each dairy sub-category.
# Source: variable_definitions.md
#   Fermented     : FFQ 1-8   (yogurts, cheeses)
#   Non-fermented : FFQ 82, 83, 85, 86  (milk drinks)
#   Low-fat       : FFQ 2, 4, 82, 85   (0% / low-fat products)
#   High-fat      : FFQ 1, 3, 5, 6, 7, 8, 83, 86  (whole-milk / full-fat)
#   Extras        : FFQ 52, 53, 63, 68, 71, 84  (butter, cream, ice cream)
#   Total         : Fermented + Non-fermented + Extras

FFQ_FERMENTED     <- paste0("FFQ", c(1:8),                   "amount")
FFQ_NON_FERMENTED <- paste0("FFQ", c(82, 83, 85, 86),        "amount")
FFQ_LOWFAT        <- paste0("FFQ", c(2, 4, 82, 85),          "amount")
FFQ_HIGHFAT       <- paste0("FFQ", c(1, 3, 5, 6, 7, 8, 83, 86), "amount")
FFQ_EXTRA         <- paste0("FFQ", c(52, 53, 63, 68, 71, 84), "amount")
FFQ_TOTAL         <- unique(c(FFQ_FERMENTED, FFQ_NON_FERMENTED, FFQ_EXTRA))

# Sum FFQ amount items (g/day); return NA only when ALL contributing items missing
safe_ffq_sum <- function(df, cols) {
    present <- intersect(cols, names(df))
    if (length(present) == 0) return(rep(NA_real_, nrow(df)))
    mat   <- as.matrix(dplyr::select(df, dplyr::all_of(present)))
    n_obs <- rowSums(!is.na(mat))
    dplyr::if_else(n_obs == 0L, NA_real_, rowSums(mat, na.rm = TRUE))
}

derive_dairy <- function(df) {
    df |> dplyr::mutate(
        dairy_fermented     = safe_ffq_sum(df, FFQ_FERMENTED),
        dairy_non_fermented = safe_ffq_sum(df, FFQ_NON_FERMENTED),
        dairy_lowfat        = safe_ffq_sum(df, FFQ_LOWFAT),
        dairy_highfat       = safe_ffq_sum(df, FFQ_HIGHFAT),
        dairy_total         = safe_ffq_sum(df, FFQ_TOTAL)
    )
}

# ── EWGSOP2 sarcopenia stage --------------------------------------------------
# Cutoffs (women only — OsteoLaus is female-only):
#   Low muscle strength : HGS < 16 kg
#   Low muscle mass     : ALM/ht^2 < 5.5 kg/m^2
#   Low physical perf.  : 6-metre gait speed <= 0.8 m/s
#
# UNIT NOTE: ALM from OsteoLaus DXA is in GRAMS. ALM_HT2 is the pre-computed
# ratio already in kg/m^2 (i.e. ALM_HT2 = ALM_grams / 1000 / height_m^2).
# We PREFER ALM_HT2 directly from OsteoLaus to avoid unit-conversion errors.
# Only fall back to computing from ALM + ht if ALM_HT2 is absent.

derive_sarcopenia <- function(df) {
    HGS_CUT  <- 16    # kg    (women, EWGSOP2)
    ALM_CUT  <- 5.5   # kg/m^2 (women, EWGSOP2)
    GAIT_CUT <- 0.8   # m/s   (EWGSOP2)
    
    df |> dplyr::mutate(
        # Best available grip strength: prefer OsteoLaus HGS_MAX, fall back
        # to CoLaus handgrip
        hgs_best = dplyr::coalesce(HGS_MAX, handgrip),
        
        # Appendicular lean mass index (kg/m^2).
        # Prefer pre-computed ALM_HT2 from OsteoLaus (already in kg/m^2).
        # Fallback: compute from ALM (grams) + ht (cm), converting to kg/m^2.
        alm_ht2 = dplyr::case_when(
            !is.na(ALM_HT2)                        ~ ALM_HT2,
            !is.na(ALM) & !is.na(ht) & ht > 0      ~ (ALM / 1000) / (ht / 100)^2,
            TRUE                                    ~ NA_real_
        ),
        
        # Prefer the pre-computed ewgsop2_sarcopenia_stage from OsteoLaus
        # harmonisation (harmonise_osteo() reads it directly from the DXA export
        # and casts it to an ordered factor with levels 0-3).
        # For rows where it is absent (CoLaus-only waves, or OsteoLaus waves
        # where the column is missing), derive it from components.
        # The pre-computed column arrives as an ordered factor; coerce to a
        # character string for the case_when() comparison, then re-factor
        # with the pipeline-standard "0-None" / "1-Probable" labels.
        .ewgsop2_precomputed = dplyr::if_else(
            !is.na(ewgsop2_sarcopenia_stage),
            dplyr::case_when(
                as.integer(ewgsop2_sarcopenia_stage) == 0L ~ "0-None",
                as.integer(ewgsop2_sarcopenia_stage) == 1L ~ "1-Probable",
                as.integer(ewgsop2_sarcopenia_stage) == 2L ~ "2-Confirmed",
                as.integer(ewgsop2_sarcopenia_stage) == 3L ~ "3-Severe",
                TRUE ~ NA_character_
            ),
            NA_character_
        ),
        .ewgsop2_derived = dplyr::case_when(
            is.na(hgs_best)
            ~ NA_character_,
            hgs_best >= HGS_CUT
            ~ "0-None",
            hgs_best < HGS_CUT & (is.na(alm_ht2) | alm_ht2 >= ALM_CUT)
            ~ "1-Probable",
            hgs_best < HGS_CUT & alm_ht2 < ALM_CUT &
                (is.na(`6MGS`) | `6MGS` > GAIT_CUT)
            ~ "2-Confirmed",
            hgs_best < HGS_CUT & alm_ht2 < ALM_CUT & `6MGS` <= GAIT_CUT
            ~ "3-Severe",
            TRUE ~ NA_character_
        ),
        ewgsop2_sarcopenia_stage = factor(
            dplyr::coalesce(.ewgsop2_precomputed, .ewgsop2_derived),
            levels = c("0-None", "1-Probable", "2-Confirmed", "3-Severe")
        )
    ) |>
        dplyr::select(-.ewgsop2_precomputed, -.ewgsop2_derived)
}

# ── Master derive function ----------------------------------------------------

#' Build all derived variables
#'
#' Applies each sub-derivation in sequence. Order matters:
#'   derive_bmi_cat()    depends on BMI (cleaned in clean_data()).
#'   derive_sarcopenia() depends on HGS_MAX / handgrip / ALM_HT2 / ALM / 6MGS.
#'
#' @param clean_list Named list output of clean_data() ($data, $log).
#' @return Same named list with $data extended by all derived columns.
derive_variables <- function(clean_list) {
    data <- clean_list$data
    
    data <- data |>
        derive_alcohol()  |>
        derive_diabetes() |>
        derive_cvd()      |>
        derive_hrt()      |>
        derive_bmi_cat()  |>
        derive_dairy()    |>
        derive_sarcopenia()
    
    list(data = data, log = clean_list$log)
}