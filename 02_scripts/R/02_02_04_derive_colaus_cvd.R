# =============================================================================
# R/02_02_04_derive_colaus_cvd.R
# =============================================================================
# TODO: get rid of the carrying forward mechanism
# Derives cdv_event (any CVD event) from the 13 component flags. Defines one
# function: derive_cvd().
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
# All flags are only available at Baseline. The derived cdv_event is therefore
# carried forward to all subsequent visits per participant.
#
# Validation: the cvdbase_adj column (if present) is compared against the
# derived value at Baseline and a warning raised on mismatch.
# =============================================================================

# -----------------------------------------------------------------------------
# derive_cvd()
# -----------------------------------------------------------------------------
#' Derive cdv_event composite for a CoLaus long tibble.
#'
#' Derives cdv_event from the 13 CVD component flags (available at Baseline
#' only), then carries the Baseline value forward to all subsequent visits
#' for each participant.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with cdv_event (factor: No / Yes) added.
derive_cvd <- function(df) {
    
    # ── Check required columns -----------------------------------------------
    .CVD_FLAGS <- c("miac", "strk", "chf",  "cad",  "angn", "cmp",
                    "hdc",  "hdv",  "artm", "vslg", "ccth", "cabg", "pcin")
    
    flags_present <- intersect(.CVD_FLAGS, names(df))
    missing_flags <- setdiff(.CVD_FLAGS, names(df))
    
    if (length(flags_present) == 0) {
        cli::cli_warn(
            "derive_cvd: no CVD component flag columns found. \\
             {.col cdv_event} will not be derived."
        )
        return(df)
    }
    if (length(missing_flags) > 0) {
        cli::cli_warn(
            "derive_cvd: {length(missing_flags)} flag(s) not found and will be skipped: \\
             {.val {missing_flags}}"
        )
    }
    
    # ── Ensure plain tibble --------------------------------------------------
    if (inherits(df, "dtplyr_step")) df <- dplyr::as_tibble(df)
    
    # ── Step 1: Derive cdv_event at every row --------------------------------
    # At non-Baseline visits all flags will be NA, so cdv_event will be NA
    # there — filled in by the carry-forward in Step 2.
    df <- df |>
        dplyr::mutate(
            tmp_any_yes = rowSums(
                dplyr::across(dplyr::all_of(flags_present), ~ !is.na(.x) & .x == "Yes"),
                na.rm = TRUE
            ) > 0,
            
            tmp_any_non_na = rowSums(
                dplyr::across(dplyr::all_of(flags_present), ~ !is.na(.x)),
                na.rm = TRUE
            ) > 0,
            
            cdv_event = dplyr::case_when(
                tmp_any_yes    ~ "Yes",
                tmp_any_non_na ~ "No",
                TRUE           ~ NA_character_
            ) |> factor(levels = c("No", "Yes"))
        ) |>
        dplyr::select(-tmp_any_yes, -tmp_any_non_na)
    
    # ── Step 2: Carry Baseline value forward to all visits per participant ---
    df <- df |>
        dplyr::arrange(pt, .visit) |>
        dplyr::group_by(pt) |>
        dplyr::mutate(
            cdv_event = dplyr::coalesce(
                cdv_event[!is.na(cdv_event)][1],
                cdv_event
            )
        ) |>
        dplyr::ungroup()
    
    # ── Validation against cvdbase_adj (Baseline only) -----------------------
    if ("cvdbase_adj" %in% names(df)) {
        check <- df |>
            dplyr::filter(.visit == "Baseline") |>
            dplyr::summarise(
                n_mismatch = sum(
                    !is.na(cdv_event) & !is.na(cvdbase_adj) &
                        as.character(cdv_event) != as.character(cvdbase_adj),
                    na.rm = TRUE
                )
            )
        
        if (check$n_mismatch > 0) {
            cli::cli_inform(
                "derive_cvd: {check$n_mismatch} Baseline row(s) mismatch between \\
                 derived {.col cdv_event} and {.col cvdbase_adj}."
            )
        }
    }
    
    # ── Summary --------------------------------------------------------------
    cli::cli_h2("Derive CVD Status")
    
    n_rows <- nrow(df)
    n_derived <- sum(!is.na(df$cdv_event))
    n_yes     <- sum(df$cdv_event == "Yes", na.rm = TRUE)
    n_no      <- sum(df$cdv_event == "No",  na.rm = TRUE)
    n_missing <- n_rows - n_derived
    n_pts_cvd <- dplyr::n_distinct(df$pt[df$cdv_event == "Yes" & !is.na(df$cdv_event)])
    n_pts_total <- dplyr::n_distinct(df$pt)
    
    cli::cli_inform(c(
        "v" = "cdv_event derived from {length(flags_present)} flag(s) and carried forward.",
        "i" = "Total rows: {n_rows} | derived: {n_derived} | missing: {n_missing}",
        "*" = "Yes (any CVD event): {n_yes} rows | {n_pts_cvd} / {n_pts_total} participants",
        "*" = "No                 : {n_no} rows"
    ))
    
    return(df)
}