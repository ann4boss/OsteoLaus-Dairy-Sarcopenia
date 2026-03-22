# =============================================================================
# R/05_derive_colaus_cvd.R
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
# Validation: the pre-existing cdv_event and cvdbase_adj columns (if present)
# are compared against the derived value and a warning raised on mismatch.
#
# Depends on: nothing
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
    
    flags_present <- intersect(.CVD_FLAGS, names(df))
    
    if (length(flags_present) == 0) {
        cli::cli_warn(
            "derive_cvd: no CVD component flag columns found. \\
       {.col cdv_event} will not be derived."
        )
        return(df)
    }
    
    # Preserve original column if it exists
    if ("cdv_event" %in% names(df))
        df <- dplyr::rename(df, cdv_event_source = cdv_event)
    
    df <- dplyr::mutate(df,
                        
                        # TRUE if any flag is Yes, NA if all are NA, FALSE otherwise
                        .any_yes = rowSums(
                            dplyr::across(dplyr::all_of(flags_present), ~ !is.na(.x) & .x == "Yes"),
                            na.rm = TRUE
                        ) > 0,
                        .any_non_na = rowSums(
                            dplyr::across(dplyr::all_of(flags_present), ~ !is.na(.x)),
                            na.rm = TRUE
                        ) > 0,
                        
                        cdv_event = dplyr::case_when(
                            .any_yes               ~ "Yes",
                            .any_non_na            ~ "No",
                            TRUE                   ~ NA_character_
                        ) |> factor(levels = c("No", "Yes"))
    ) |>
        dplyr::select(-.any_yes, -.any_non_na)
    
    # ── Validation against original cdv_event_source ──────────────────────────
    if ("cdv_event_source" %in% names(df)) {
        n_mismatch <- sum(
            !is.na(df$cdv_event) & !is.na(df$cdv_event_source) &
                as.character(df$cdv_event) != as.character(df$cdv_event_source),
            na.rm = TRUE
        )
        if (n_mismatch > 0)
            cli::cli_warn(
                "derive_cvd: {n_mismatch} row(s) where derived {.col cdv_event} \\
         disagrees with original {.col cdv_event_source}. \\
         Review component flags."
            )
    }
    
    return(df)
}