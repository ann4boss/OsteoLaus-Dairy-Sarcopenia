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
#   C10   -> hypolip_drug_status   (lipid-lowering drugs)
#   H02   -> corticoids_status     (systemic corticosteroids)
#   A11   -> vitD_status           (vitamin D supplements)
#   A12A  -> calcium_status        (calcium supplements)
#   N05B  -> benzo_status          (benzodiazepines)
#   M05BA -> bisphosphonate_status (bisphosphonates)
#
# =============================================================================


ATC_PREFIXES <- list(
    hypolip_drug_status   = "C10",
    corticoids_status     = "H02",
    vitD_status           = "A11",
    calcium_status        = "A12A",
    benzo_status          = "N05B",
    bisphosphonate_status = "M05BA"
)


#' Derive ATC-based medication status flags for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with hypolip_drug_status, corticoids_status, vitD_status,
#'   calcium_status, benzo_status, bisphosphonate_status (all Yes/No factors)
#'   added or replaced.
derive_atc <- function(df) {
    
    # ── Check Required Columns ------------------------------------------------
    # ATC column definitions
    .ATC_COLS     <- paste0("ATC", 1:21)
    .ATC_OTC_COLS <- paste0("ATC_OTC", 1:17)
    
    required_cols <- c(.ATC_COLS) # TODO: if I want to add Over-the-counter drugs (ATC_OTC1 ... ATC_OTC17), add .ALT_OTC_COLS to .ALL_ATC_COLS
    actual_cols <- names(df)
    missing_cols <- setdiff(required_cols, actual_cols)
    
    if (length(missing_cols) > 0) {
        cli::cli_warn("derive_atc: No ATC code columns found. Flags not derived.")
        return(df)
    }
    
    # ── Ensure Lazy State ------------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    # ── Build Vectorized Expressions ───────────────────────────────────────
    atc_expressions <- lapply(names(ATC_PREFIXES), function(varname) {
        prefix <- ATC_PREFIXES[[varname]]
        
        # Build logic: (startsWith(atc1, "C10") | startsWith(atc2, "C10") ...)
        detection_logic <- lapply(required_cols, function(col) {
            rlang::expr(startsWith(!!rlang::sym(col), !!prefix))
        }) |> 
            purrr::reduce(function(a, b) rlang::expr(!!a | !!b))
        
        # Wrap in factor logic
        rlang::expr(factor(
            dplyr::case_when(
                !!detection_logic ~ "Yes",
                # Handle cases where all entries are NA
                dplyr::across(dplyr::all_of(required_cols), ~ is.na(.x)) |> 
                    rowSums() == !!length(required_cols) ~ NA_character_,
                TRUE ~ "No"
            ),
            levels = c("No", "Yes")
        ))
    })
    names(atc_expressions) <- names(ATC_PREFIXES)
    
    # ── Apply Mutate ------------------------------------------------
    df <- df |> 
        dplyr::mutate(!!!atc_expressions) |>
        # collect as tibble 
        dplyr::as_tibble()
    
    
    # ── Validation & Reporting ------------------------------------------------
    # We calculate the row counts lazily to avoid a full collection
    report_stats <- df |>
        dplyr::summarise(
            total_rows = dplyr::n(),
            n_valid = sum(!is.na(!!rlang::sym(names(ATC_PREFIXES)[1]))),
            .groups = "drop"
        ) |>
        dplyr::as_tibble()
    
    cli::cli_h2("Derive ATCs")
    cli::cli_inform(c(
        "v" = "derive_atc: ATC-based flags derived.",
        "i" = "Processed {report_stats$total_rows} rows; {report_stats$n_valid} rows had valid ATC mapping data."
    ))
    
    return(df)
}
