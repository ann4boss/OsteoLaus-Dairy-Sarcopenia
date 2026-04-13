# =============================================================================
# R/derive_colaus_atc.R
# =============================================================================
# Derives binary medication status flags from raw ATC code columns.
#
# Source columns: ATC1 ... ATC21 (character, one ATC code per column).
# Each row may have up to 21 reported medications. A flag is set to "Yes"
# if ANY of the 21 ATC columns contains a code starting with the relevant
# prefix.
#
# ATC prefix -> derived variable mapping is defined in ATC_PREFIXES
# (00_constants.R):
#   C10   -> hypolip_drug_status   (lipid-lowering drugs)
#   H02   -> corticoids_status     (systemic corticosteroids)
#   A11   -> vitD_status           (vitamin D supplements)
#   A12A  -> calcium_status        (calcium supplements)
#   N05B  -> benzo_status          (benzodiazepines)
#   M05BA -> bisphosphonate_status (bisphosphonates)
#
# Validation:
#   hypolip_drug_status is compared against hypolip (self-reported).
#   Mismatches are warnings for review; some discordance is expected.
# =============================================================================

# ATC column definitions
.ATC_COLS     <- paste0("ATC", 1:21)
.ATC_OTC_COLS <- paste0("ATC_OTC", 1:17)
.ALL_ATC_COLS <- c(.ATC_COLS) # TODO: if I want to add Over-the-counter drugs (ATC_OTC1 ... ATC_OTC17), add .ALT_OTC_COLS to .ALL_ATC_COLS

#' Derive ATC-based medication status flags for a CoLaus long tibble.
#'
#' ATC prefix -> variable name mapping is read from ATC_PREFIXES in
#' 00_constants.R.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with hypolip_drug_status, corticoids_status, vitD_status,
#'   calcium_status, benzo_status, bisphosphonate_status (all Yes/No factors)
#'   added or replaced.
derive_atc <- function(df) {
    
    # ── Check Required Columns ------------------------------------------------
    # .ALL_ATC_COLS is your constant list of atc1, atc2, etc.
    actual_cols <- df$vars
    atc_cols_present <- intersect(.ALL_ATC_COLS, actual_cols)
    
    if (length(atc_cols_present) == 0) {
        cli::cli_warn("derive_atc: No ATC code columns found. Flags not derived.")
        return(df)
    }
    
    # ── Ensure Lazy State ------------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    # ── Build Vectorized Expressions ───────────────────────────────────────
    atc_expressions <- lapply(names(ATC_PREFIXES), function(varname) {
        prefix <- ATC_PREFIXES[[varname]]
        
        # Build logic: (startsWith(atc1, "C10") | startsWith(atc2, "C10") ...)
        detection_logic <- lapply(atc_cols_present, function(col) {
            rlang::expr(startsWith(!!rlang::sym(col), !!prefix))
        }) %>% 
            purrr::reduce(function(a, b) rlang::expr(!!a | !!b))
        
        # Wrap in factor logic
        rlang::expr(factor(
            dplyr::case_when(
                !!detection_logic ~ "Yes",
                # Handle cases where all entries are NA
                dplyr::across(dplyr::all_of(atc_cols_present), ~ is.na(.x)) %>% 
                    rowSums() == !!length(atc_cols_present) ~ NA_character_,
                TRUE ~ "No"
            ),
            levels = c("No", "Yes")
        ))
    })
    names(atc_expressions) <- names(ATC_PREFIXES)
    
    # ── Apply Mutate ------------------------------------------------
    df <- df %>% 
        dplyr::mutate(!!!atc_expressions)
    
    # ── Eager Validation & Reporting ------------------------------------------------
    # We calculate the row counts lazily to avoid a full collection
    report_stats <- df %>%
        dplyr::summarise(
            total_rows = dplyr::n(),
            # Since source cols are dropped, we check the first derived flag for NAs 
            # as a proxy for 'rows with no ATC data'
            n_valid = sum(!is.na(!!rlang::sym(names(ATC_PREFIXES)[1]))),
            .groups = "drop"
        ) %>%
        dplyr::as_tibble()
    
    cli::cli_inform(c(
        "v" = "derive_atc: ATC-based flags derived.",
        "i" = "Processed {report_stats$total_rows} rows; {report_stats$n_valid} rows had valid ATC mapping data."
    ))
    
    # ── Clean up -----------
    df <- df %>% 
        dplyr::select(-dplyr::any_of(c(.ATC_OTC_COLS, .ATC_COLS)))
    
    
    return(df)
}

# -----------------------------------------------------------------------------
# Optimized Validation
# -----------------------------------------------------------------------------
.validate_hypolip_lazy <- function(df) {
    if (!"hypolip" %in% names(df)) return(NULL)
    
    # We summarize to get counts without pulling the whole dataset into memory
    check <- df %>%
        dplyr::filter(!is.na(hypolip_drug_status), !is.na(hypolip)) %>%
        dplyr::summarise(
            yes_no = sum(hypolip_drug_status == "Yes" & hypolip == "No", na.rm = TRUE),
            no_yes = sum(hypolip_drug_status == "No" & hypolip == "Yes", na.rm = TRUE)
        ) %>%
        dplyr::as_tibble()
    
    if (check$yes_no > 0) {
        cli::cli_inform("derive_atc: {check$yes_no} row(s) with ATC C10 but self-reported {.col hypolip} = No.")
    }
    if (check$no_yes > 0) {
        cli::cli_inform("derive_atc: {check$no_yes} row(s) with no ATC C10 but self-reported {.col hypolip} = Yes.")
    }
}