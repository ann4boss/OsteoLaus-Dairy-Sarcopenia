# =============================================================================
# R/qc_variable.R
# =============================================================================
# QC checks for Height, Weight, and BMI completeness and consistency.
#
# Quality checks include:
#   - Missingness of Height, Weight, BMI per wave
#   - BMI present despite missing Height or Weight (internal inconsistency)
#   - Plausible range for Height and Weight
#
# Each QC check produces a TRUE/FALSE flag per participant row. A wave-level
# and global summary are also returned for reporting.
# =============================================================================

#' Perform QC checks on Height, Weight, and BMI across harmonised waves.
#'
#' @param harmonised_list A named list of data frames, one per wave/cohort,
#'   containing harmonised participant data including `pt`, `.cohort`, `.wave`,
#'   `Height`, `Weight`, and `BMI`.
#'
#' @return A named list with:
#'   - `tbl`          One row per participant per wave with QC flag columns.
#'   - `wave_summary` Aggregated QC counts and ranges grouped by `.wave`.
#'   - `summary`      Global aggregated QC counts and ranges across all waves.
#'
#'   QC flag columns (TRUE = passes check, FALSE = fails):
#'   - `qc_height_missing`      Height is NA.
#'   - `qc_weight_missing`      Weight is NA.
#'   - `qc_bmi_missing`         BMI is NA.
#'   - `qc_bmi_without_inputs`  BMI present but Height or Weight missing.
# =============================================================================
qc_variable <- function(harmonised_list) {
    cli::cli_h1("QC Anthropometrics Report")
    
    # ── Select and bind -------------------------------------------------------
    # Keep only the columns needed for this QC check. Columns absent from a
    # given wave (e.g. Height not collected) are silently skipped by any_of().
    df_all <- harmonised_list |>
        lapply(function(df) {
            df |>
                dplyr::select(
                    dplyr::any_of(c(
                        "pt", ".cohort", ".wave",
                        "Height", "Weight", "BMI",
                        "date_exam_iso", "Age",
                        "HGS_MAX",
                        "PAFQ_SE", "PAFQ_LPA", "PAFQ_MPA", "PAFQ_VPA",
                         "sumtot1", "ALM_HT2"
                    )),
                    dplyr::matches("^FFQ\\d+amount$")
                )
        }) |>
        dplyr::bind_rows() |>
        dtplyr::lazy_dt() |>
        dplyr::arrange(pt, .cohort, .wave)
    
    # ── Core QC flags ---------------------------------------------------------
    # Computed on the lazy_dt for performance; collected once below.
    qc_tbl_variable <- df_all |>
        dplyr::mutate(
            qc_height_missing     = is.na(Height),
            qc_weight_missing     = is.na(Weight),
            qc_bmi_missing        = is.na(BMI),
            
            # BMI recorded despite one or both inputs being absent
            qc_bmi_without_inputs = !is.na(BMI) & (is.na(Height) | is.na(Weight))
        )
    
    # ── Helpers ---------------------------------------------------------------
    safe_min <- function(x) suppressWarnings(min(x, na.rm = TRUE))
    safe_max <- function(x) suppressWarnings(max(x, na.rm = TRUE))
    
    
    # ── Collect ---------------------------------------------------------------
    # Materialise once; all summaries and the CLI block operate on a plain tibble
    # so that $ indexing and dplyr verbs work without lazy evaluation surprises.
    res_tbl <- dplyr::as_tibble(qc_tbl_variable)
    
    # ── Wave summary ----------------------------------------------------------
    wave_summary <- res_tbl |>
        dplyr::group_by(.cohort, .wave) |>
        dplyr::summarise(
            n_total              = dplyr::n(),
            n_height_missing     = sum(qc_height_missing,     na.rm = TRUE),
            n_weight_missing     = sum(qc_weight_missing,     na.rm = TRUE),
            n_bmi_missing        = sum(qc_bmi_missing,        na.rm = TRUE),
            n_bmi_without_inputs = sum(qc_bmi_without_inputs, na.rm = TRUE),
            
            # Anthropometrics
            height_min = safe_min(Height),
            height_max = safe_max(Height),
            weight_min = safe_min(Weight),
            weight_max = safe_max(Weight),
            
            #date_exam_min = safe_min(date_exam_iso),
            #date_exam_max = safe_max(date_exam_iso),
            
            age_min = safe_min(Age),
            age_max = safe_max(Age),
            
            HGS_MAX_min = safe_min(HGS_MAX),
            HGS_MAX_max = safe_max(HGS_MAX),
            
            PAFQ_SE_min  = safe_min(PAFQ_SE),
            PAFQ_SE_max  = safe_max(PAFQ_SE),
            PAFQ_LPA_min = safe_min(PAFQ_LPA),
            PAFQ_LPA_max = safe_max(PAFQ_LPA),
            PAFQ_MPA_min = safe_min(PAFQ_MPA),
            PAFQ_MPA_max = safe_max(PAFQ_MPA),
            PAFQ_VPA_min = safe_min(PAFQ_VPA),
            PAFQ_VPA_max = safe_max(PAFQ_VPA),
            
            sumtot1_min = safe_min(sumtot1),
            sumtot1_max = safe_max(sumtot1),
            
            ALM_HT2_min = safe_min(ALM_HT2),
            ALM_HT2_max = safe_max(ALM_HT2),
            
            # FFQ dynamic range (across all FFQ columns)
            FFQ_min = suppressWarnings(min(dplyr::across(dplyr::matches("^FFQ\\d+amount$")), na.rm = TRUE)),
            FFQ_max = suppressWarnings(max(dplyr::across(dplyr::matches("^FFQ\\d+amount$")), na.rm = TRUE)),
            
            .groups = "drop"
        )
    
    # ── Global summary --------------------------------------------------------
    qc_summary <- res_tbl |>
        dplyr::summarise(
            n_total              = dplyr::n(),
            n_height_missing     = sum(qc_height_missing,     na.rm = TRUE),
            n_weight_missing     = sum(qc_weight_missing,     na.rm = TRUE),
            n_bmi_missing        = sum(qc_bmi_missing,        na.rm = TRUE),
            n_bmi_without_inputs = sum(qc_bmi_without_inputs, na.rm = TRUE),
            
            height_min = safe_min(Height),
            height_max = safe_max(Height),
            weight_min = safe_min(Weight),
            weight_max = safe_max(Weight),
            
            #date_exam_min = safe_min(date_exam_iso),
            #date_exam_max = safe_max(date_exam_iso),
            
            age_min = safe_min(Age),
            age_max = safe_max(Age),
            
            HGS_MAX_min = safe_min(HGS_MAX),
            HGS_MAX_max = safe_max(HGS_MAX),
            
            PAFQ_SE_min  = safe_min(PAFQ_SE),
            PAFQ_SE_max  = safe_max(PAFQ_SE),
            PAFQ_LPA_min = safe_min(PAFQ_LPA),
            PAFQ_LPA_max = safe_max(PAFQ_LPA),
            PAFQ_MPA_min = safe_min(PAFQ_MPA),
            PAFQ_MPA_max = safe_max(PAFQ_MPA),
            PAFQ_VPA_min = safe_min(PAFQ_VPA),
            PAFQ_VPA_max = safe_max(PAFQ_VPA),
            
            sumtot1_min = safe_min(sumtot1),
            sumtot1_max = safe_max(sumtot1),
            
            ALM_HT2_min = safe_min(ALM_HT2),
            ALM_HT2_max = safe_max(ALM_HT2),
            
            FFQ_min = suppressWarnings(min(dplyr::across(dplyr::matches("^FFQ\\d+amount$")), na.rm = TRUE)),
            FFQ_max = suppressWarnings(max(dplyr::across(dplyr::matches("^FFQ\\d+amount$")), na.rm = TRUE))
        )
    
    # ── CLI summary -----------------------------------------------------------
    cli::cli_inform(c(
        "i" = "Total rows: {qc_summary$n_total}",
        "x" = "Missing Height:                        {qc_summary$n_height_missing}",
        "x" = "Missing Weight:                        {qc_summary$n_weight_missing}",
        "x" = "Missing BMI:                           {qc_summary$n_bmi_missing}",
        "!" = "BMI present but Height/Weight missing: {qc_summary$n_bmi_without_inputs}",
        "i" = "Height range: {round(qc_summary$height_min, 2)} - {round(qc_summary$height_max, 2)} cm",
        "i" = "Weight range: {round(qc_summary$weight_min, 2)} - {round(qc_summary$weight_max, 2)} kg",
        "i" = "Exam date range: {qc_summary$date_exam_min} - {qc_summary$date_exam_max}",
        "i" = "Age range: {round(qc_summary$age_min, 1)} - {round(qc_summary$age_max, 1)}",
        "i" = "HGS_MAX range: {round(qc_summary$HGS_MAX_min, 2)} - {round(qc_summary$HGS_MAX_max, 2)}",
        "i" = "PAFQ total ranges (SE/LPA/MPA/VPA): 
      {round(qc_summary$PAFQ_SE_min,2)}-{round(qc_summary$PAFQ_SE_max,2)} /
      {round(qc_summary$PAFQ_LPA_min,2)}-{round(qc_summary$PAFQ_LPA_max,2)} /
      {round(qc_summary$PAFQ_MPA_min,2)}-{round(qc_summary$PAFQ_MPA_max,2)} /
      {round(qc_summary$PAFQ_VPA_min,2)}-{round(qc_summary$PAFQ_VPA_max,2)}",
        "i" = "FFQ amount range: {round(qc_summary$FFQ_min, 2)} - {round(qc_summary$FFQ_max, 2)}",
        "i" = "sumtot1 range: {round(qc_summary$sumtot1_min, 2)} - {round(qc_summary$sumtot1_max, 2)}",
        "i" = "ALM_HT2 range: {round(qc_summary$ALM_HT2_min, 2)} - {round(qc_summary$ALM_HT2_max, 2)}"
    ))
    
    # ── Return ----------------------------------------------------------------
    return(list(
        tbl          = res_tbl,
        wave_summary = wave_summary,
        summary      = qc_summary
    ))
}