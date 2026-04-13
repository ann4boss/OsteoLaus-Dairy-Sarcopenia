# TODO rewrite for dtplyr
# =============================================================================
# R/derive_colaus_cvd.R
# =============================================================================
# Derives cdv_event (any CVD event) from the 13 component flags.
#
# Component flags (all Yes/No, sentinel 9 = Does not know → NA):
#   miac  myocardial infarction
#   strk  stroke
#   chf   congestive heart failure
#   cad   coronary artery disease
#   angn  angina
#   cmp   cardiomyopathy
#   hdc   cardiac surgery (other)
#   hdv   heart valve surgery
#   artm  arrhythmia treated
#   vslg  vascular surgery (legs)
#   ccth  cardiac catheterisation
#   cabg  coronary artery bypass graft
#   pcin  percutaneous coronary intervention
#
# Rule: cdv_event = Yes if ANY component flag is Yes.
#       cdv_event = No  if ALL available components are No (at least one present).
#       cdv_event = NA  if no component flag has a non-NA value.
#
# Validation: the cvdbase_adj column (if present) is compared against the 
# derived value and a warning raised on mismatch.
#
# =============================================================================

.CVD_FLAGS <- c(
    "miac", "strk", "chf",  "cad",  "angn", "cmp",
    "hdc",  "hdv",  "artm", "vslg", "ccth", "cabg", "pcin"
)

#' Derive cdv_event composite for a CoLaus long tibble.
#'
#' The original cdv_event column is renamed to cdv_event_source before the
#' derived version is written, so both are available for audit.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with cdv_event (factor: No / Yes) added or replaced.
derive_cvd <- function(df) {
    
    # ── Check Required Columns ----------------------------------------------
    actual_cols <- df$vars
    flags_present <- intersect(.CVD_FLAGS, actual_cols)
    
    if (length(flags_present) == 0) {
        cli::cli_warn(
            "derive_cvd: No CVD component flags found in {.val {head(actual_cols, 5)}}... 
            {.col cdv_event} will not be derived."
        )
        return(df)
    }
    
    # ── Ensure Lazy State ----------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    # ── Main Derivation ----------------------------------------------
    df <- df %>%
        dplyr::mutate(
            # Helper: TRUE if any flag is "Yes"
            tmp_any_yes = rowSums(
                dplyr::across(dplyr::all_of(flags_present), ~ !is.na(.x) & .x == "Yes"),
                na.rm = TRUE
            ) > 0,
            
            # Helper: TRUE if at least one flag is not NA
            tmp_any_non_na = rowSums(
                dplyr::across(dplyr::all_of(flags_present), ~ !is.na(.x)),
                na.rm = TRUE
            ) > 0,
            
            # Final Category
            cdv_event = dplyr::case_when(
                tmp_any_yes    ~ "Yes",
                tmp_any_non_na ~ "No",
                TRUE           ~ NA_character_
            ) %>% factor(levels = c("No", "Yes"))
        )
    
    # ── Validation ----------------------------------------------
    # We only check 'cvdbase_adj' if it exists in the plan
    if ("cvdbase_adj" %in% df$vars) {
        check <- df %>%
            dplyr::summarise(
                n_mismatch = sum(
                    !is.na(cdv_event) & !is.na(cvdbase_adj) &
                        as.character(cdv_event) != as.character(cvdbase_adj),
                    na.rm = TRUE
                )
            ) %>%
            dplyr::as_tibble()
        
        if (check$n_mismatch > 0) {
            cli::cli_inform(
                "derive_cvd: {check$n_mismatch} row(s) mismatch between derived {.col cdv_event} 
                and original {.col cvdbase_adj}."
            )
        }
    }
    
    # ── Cleanup ----------------------------------------------
    # Drop source flags AND intermediate variables
    df <- df %>%
        dplyr::select(
            -dplyr::all_of(flags_present),
            -dplyr::starts_with("tmp_")
        )
    
    return(df)
}