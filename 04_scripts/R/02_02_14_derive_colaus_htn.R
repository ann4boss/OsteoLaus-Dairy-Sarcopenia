# =============================================================================
# R/derive_colaus_htn.R
# =============================================================================
# Derives htn_status (hypertension) from three sources using a hierarchy:
#
#   1. HTA     — measured hypertension (BP >= 140/90 mmHg)  [most objective]
#   2. antiHTA — documented antihypertensive treatment
#   3. crbpmed — self-reported antihypertensive medication   [least objective]
#
# Derivation logic (hierarchical):
#   htn_status = "Yes" if HTA == "Yes"
#   htn_status = "Yes" if HTA missing AND antiHTA == "Yes"
#   htn_status = "Yes" if HTA and antiHTA missing AND crbpmed == "Yes"
#   htn_status = "No"  if at least one source is non-NA and none indicate "Yes"
#   htn_status = NA    if all three sources are NA
# =============================================================================

#' Derive htn_status for a CoLaus long tibble.
#'
#' Uses a three-level hierarchy: HTA (measured) > antiHTA (documented
#' treatment) > crbpmed (self-reported). A "Yes" from a higher-priority source
#' takes precedence; "No" is assigned only when no source indicates "Yes" and
#' at least one source is non-missing.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with htn_status (factor: No / Yes) added.
derive_htn <- function(df) {
    
    # ── Ensure Required Columns are available ---------------------------------
    required_cols <- c("antiHTA", "crbpmed", "HTA")
    actual_cols <- names(df)
    missing_cols <- setdiff(required_cols, actual_cols)
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_htn: required column(s) not found: {.val {missing_cols}}. 
        htn_status will not be derived."
        )
        return(df)
    }
    
    
    
    # ── Main derivation (hierarchical) ---------------------------------------
    df <- df |>
        dplyr::mutate(
            # Tier 1: HTA (measured BP)
            is_yes_HTA     = !is.na(HTA)     & HTA     == "Yes",
            is_avail_HTA   = !is.na(HTA),
            
            # Tier 2: antiHTA (documented treatment)
            is_yes_antiHTA  = !is.na(antiHTA) & antiHTA == "Yes",
            is_avail_antiHTA = !is.na(antiHTA),
            
            # Tier 3: crbpmed (self-report)
            is_yes_crbpmed  = !is.na(crbpmed) & crbpmed == "Yes",
            is_avail_crbpmed = !is.na(crbpmed),
            
            any_available = is_avail_HTA | is_avail_antiHTA | is_avail_crbpmed,
            
            htn_status = dplyr::case_when(
                # YES — hierarchical
                is_yes_HTA                                      ~ "Yes",
                !is_avail_HTA & is_yes_antiHTA                  ~ "Yes",
                !is_avail_HTA & !is_avail_antiHTA & is_yes_crbpmed ~ "Yes",
                # NO — at least one source present, none say Yes
                any_available                                   ~ "No",
                TRUE                                            ~ NA_character_
            ) |> factor(levels = c("No", "Yes"))
        ) |>
        dplyr::select(-is_yes_HTA, -is_avail_HTA,
                      -is_yes_antiHTA, -is_avail_antiHTA,
                      -is_yes_crbpmed, -is_avail_crbpmed,
                      -any_available)
    
    # ── Summary --------------------------------------------------------------
    cli::cli_h2("Derive htn Status")
    
    n_rows    <- nrow(df)
    n_yes     <- sum(df$htn_status == "Yes", na.rm = TRUE)
    n_no      <- sum(df$htn_status == "No",  na.rm = TRUE)
    n_missing <- sum(is.na(df$htn_status))
    
    # Which source was decisive for each "Yes" row
    n_yes_hta     <- sum(!is.na(df$HTA)     & df$HTA     == "Yes" & df$htn_status == "Yes", na.rm = TRUE)
    n_yes_antihta <- sum(is.na(df$HTA) & !is.na(df$antiHTA) & df$antiHTA == "Yes" & df$htn_status == "Yes", na.rm = TRUE)
    n_yes_crbpmed <- sum(is.na(df$HTA) & is.na(df$antiHTA)  & !is.na(df$crbpmed) & df$crbpmed == "Yes" & df$htn_status == "Yes", na.rm = TRUE)
    
    cli::cli_inform(c(
        "v" = "htn_status derived.",
        "i" = "Total rows: {n_rows} | Yes: {n_yes} | No: {n_no} | Missing: {n_missing}",
        "i" = "Prevalence: {round(n_yes / (n_yes + n_no) * 100, 1)}% (among non-missing rows)",
        "i" = "Source driving 'Yes' classification:",
        "*" = "HTA (measured BP, tier 1)          : {n_yes_hta}",
        "*" = "antiHTA (documented Rx, tier 2)    : {n_yes_antihta}",
        "*" = "crbpmed (self-reported, tier 3)    : {n_yes_crbpmed}"
    ))
    
    return(df)
}