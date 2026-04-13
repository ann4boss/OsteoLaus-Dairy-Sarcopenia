# =============================================================================
# R/derive_colaus_diabetes.R
# =============================================================================
# Derives a harmonised diabetes_status variable using multiple sources with a
# predefined hierarchy and provides validation diagnostics.
#
# SOURCE VARIABLES
#   dbtld     — self-reported diabetes diagnosis (Yes/No)
#   DIAB      — clinically assessed diabetes flag (Yes/No)
#   DIAB_Hb   — HbA1c-based diabetes classification (Yes/No)
#
# DERIVATION LOGIC
#
# Diabetes (1):
#   Hierarchical classification:
#     1. DIAB_Hb (highest priority, objective biomarker)
#     2. DIAB (clinical diagnosis)
#     3. dbtld (self-report)
#
#   A participant is classified as diabetic if:
#     - DIAB_Hb == "Yes", OR
#     - DIAB_Hb is missing AND DIAB == "Yes", OR
#     - DIAB_Hb and DIAB are missing AND dbtld == "Yes"
#
# No diabetes (0):
#     - At least one source variable is non-missing
#     - No source indicates "Yes"
#     - At least one source explicitly indicates "No"
#
# Missing:
#   - All source variables are missing OR
#   - Conflicting / insufficient evidence under the above rules
#
# VALIDATION OUTPUT
#   The function reports:
#     - Number of participants with 0, 1, 2, 3 available sources
#     - Overall disagreement between sources
#     - Disagreement between self-report and objective measures
#
#   Note: Some disagreement between dbtld and clinical/lab measures is expected.
#
# =============================================================================

#' Derive diabetes_status for a CoLaus long tibble using dtplyr.
#'
#' @param df CoLaus long tibble (lazy_dt or data.frame).
#' @return df with diabetes_status (factor) and internal flags added.
derive_diabetes <- function(df) {
    
    # ── Ensure Required Columns are available ---------------------------------
    required_cols <- c("dbtld", "DIAB", "DIAB_Hb")
    actual_cols <- df$vars
    missing_cols <- setdiff(required_cols, actual_cols)
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_diabetes: missing required columns: {.val {missing_cols}}. 
        Diabetes status will not be derived."
        )
        return(df)
    }
    
    
    # ── Ensure Lazy State ------------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    # ── Main Calculation Pipeline -----------------------------------------
    df <- df |>
        dplyr::mutate(
            # Logic flags
            is_yes_dbtld   = !is.na(dbtld) & dbtld == "Yes",
            is_yes_DIAB    = !is.na(DIAB)  & DIAB == "Yes",
            is_yes_DIAB_Hb = !is.na(DIAB_Hb) & DIAB_Hb == "Yes",
            
            is_avail_dbtld   = !is.na(dbtld),
            is_avail_DIAB    = !is.na(DIAB),
            is_avail_DIAB_Hb = !is.na(DIAB_Hb),
            
            # Availability counters
            n_sources_available = is_avail_dbtld + is_avail_DIAB + is_avail_DIAB_Hb,
            n_yes               = is_yes_dbtld + is_yes_DIAB + is_yes_DIAB_Hb,
            
            any_yes = is_yes_dbtld | is_yes_DIAB | is_yes_DIAB_Hb,
            any_no  = (!is.na(dbtld) & dbtld == "No") | 
                (!is.na(DIAB)  & DIAB == "No")  | 
                (!is.na(DIAB_Hb) & DIAB_Hb == "No"),
            
            # --- MAIN DERIVATION ---
            diabetes_status_num = dplyr::case_when(
                # YES (hierarchy)
                is_yes_DIAB_Hb ~ 1L,
                !is_avail_DIAB_Hb & is_yes_DIAB ~ 1L,
                !is_avail_DIAB_Hb & !is_avail_DIAB & is_yes_dbtld ~ 1L,
                
                # NO (strict rule)
                n_sources_available > 0 & !any_yes & any_no ~ 0L,
                
                TRUE ~ NA_integer_
            )
        ) |>
        dplyr::mutate(
            # --- VALIDATION FLAGS ---
            disagreement_any = n_sources_available >= 2 & n_yes > 0 & n_yes < n_sources_available,
            
            disagreement_fpg_vs_hba1c = is_avail_DIAB & is_avail_DIAB_Hb & (is_yes_DIAB != is_yes_DIAB_Hb),
            
            disagreement_self_vs_objective = is_avail_dbtld & (
                (is_avail_DIAB & (is_yes_dbtld != is_yes_DIAB)) |
                    (is_avail_DIAB_Hb & (is_yes_dbtld != is_yes_DIAB_Hb))
            )
        ) |>
        dplyr::mutate(
            diabetes_status = factor(
                diabetes_status_num,
                levels = c(0, 1),
                labels = c("No diabetes", "Diabetes")
            )
        )
    
    
    # ── Eager Validation Summary ------------------------------------------------
    summary_stats <- df |>
        dplyr::summarise(
            n             = dplyr::n(),
            n_missing_all = sum(n_sources_available == 0),
            n_one_source  = sum(n_sources_available == 1),
            n_two_sources = sum(n_sources_available == 2),
            n_three_sources = sum(n_sources_available == 3),
            
            # denominators
            n_any_valid       = sum(n_sources_available >= 2),
            n_fpg_hba1c_valid = sum(is_avail_DIAB & is_avail_DIAB_Hb),
            n_self_obj_valid  = sum(is_avail_dbtld & (is_avail_DIAB | is_avail_DIAB_Hb)),
            
            # numerators
            sum_disagreement_any          = sum(disagreement_any, na.rm = TRUE),
            sum_disagreement_fpg_vs_hba1c = sum(disagreement_fpg_vs_hba1c, na.rm = TRUE),
            sum_disagreement_self_vs_obj  = sum(disagreement_self_vs_objective, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::as_tibble()

    
    # ── Reporting ------------------------------------------------
    cli::cli_h2("Derive Diabetes Status")
    cli::cli_inform(c(
        "i" = "Diabetes derivation completed",
        "*" = "Total measurements: {summary_stats$n}",
        "*" = "No data on any source: {summary_stats$n_missing_all} ({round(summary_stats$n_missing_all / summary_stats$n * 100, 1)}%)",
        "*" = "Sources available (1/2/3): {summary_stats$n_one_source} / {summary_stats$n_two_sources} / {summary_stats$n_three_sources}",
        "*" = "Disagreement between any sources: {round(summary_stats$sum_disagreement_any / summary_stats$n_any_valid * 100, 1)}%",
        "*" = "Disagreement between FPG and HbA1c: {round(summary_stats$sum_disagreement_fpg_vs_hba1c / summary_stats$n_fpg_hba1c_valid * 100, 1)}%",
        "*" = "Self-report vs objective disagreement: {round(summary_stats$sum_disagreement_self_vs_obj / summary_stats$n_self_obj_valid * 100, 1)}%"
    ))
    
    # ── Drop source and intermediate variables ------------------------------------
    df <- df |>
        dplyr::select(
            -dplyr::any_of(c(
                "is_yes_dbtld", "is_yes_DIAB", "is_yes_DIAB_Hb",
                "is_avail_dbtld", "is_avail_DIAB", "is_avail_DIAB_Hb",
                "n_sources_available", "n_yes",
                "any_yes", "any_no",
                "diabetes_status_num",
                "disagreement_any", "disagreement_fpg_vs_hba1c", "disagreement_self_vs_objective",
                "dbtld", "DIAB", "DIAB_Hb"
            ))
        )
    
    return(df)
}