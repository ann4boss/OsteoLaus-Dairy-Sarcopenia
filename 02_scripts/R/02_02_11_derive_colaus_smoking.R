# =============================================================================
# R/02_02_11_derive_colaus_smoking.R
# =============================================================================
# Derives smoking_status — a trajectory-corrected version of sbsmk. Defines
# one function: derive_smoking().
#
# Problem
# -------
# Some participants report implausible trajectories across CoLaus visits:
#   (a) Never -> Former or Current    (switching from "never" to a use history)
#   (b) Former or Current -> Never    (recanting a prior history of smoking)
#
# Both transitions are biologically implausible. They most likely reflect
# participants interpreting the question as "do you currently smoke?" rather
# than "have you ever smoked?". The correction assumes the participant does
# have a lifetime smoking history and reclassifies the "Never" report to
# "Former" — the least extreme correction that resolves the implausibility
# without assuming current use.
#
# Correction rule (applied visit-by-visit, within each participant):
#   If a participant has at least one non-NA visit with sbsmk != "Never",
#   any remaining "Never" visits are recoded to "Former".
#
# Output
# ------
#   smoking_status   Factor: Never / Former / Current
#                    Same levels as sbsmk; "Never" entries corrected where
#                    a participant has evidence of lifetime smoking elsewhere.
#
# =============================================================================

# -----------------------------------------------------------------------------
# derive_smoking()
# -----------------------------------------------------------------------------
#' Derive smoking_status (trajectory-corrected sbsmk) for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with smoking_status (factor: Never / Former / Current) added,
#'   plus diagnostic counts emitted as cli messages.
derive_smoking <- function(df) {
    
    required_cols <- c("pt", ".visit", "sbsmk")
    missing_cols <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_smoking: missing required columns: {.val {missing_cols}}."
        )
        return(df)
    }
    
    
    # ── Clean sbsmk (critical) -----------------------------------------------
    df <- df |>
        dplyr::mutate(
            sbsmk = stringr::str_trim(as.character(sbsmk)),
            sbsmk = dplyr::case_when(
                is.na(sbsmk) ~ NA_character_,
                stringr::str_to_lower(sbsmk) == "never"   ~ "Never",
                stringr::str_to_lower(sbsmk) == "former"  ~ "Former",
                stringr::str_to_lower(sbsmk) == "current" ~ "Current",
                TRUE ~ sbsmk
            )
        )
    
    # ── Ever smoker lookup ---------------------------------------------------
    ever_smoked_lookup <- df |>
        dplyr::filter(!is.na(sbsmk), sbsmk != "Never") |>
        dplyr::distinct(pt) |>
        dplyr::mutate(has_ever_smoked = TRUE)
    
    # ── Build smoking_status -------------------------------------------------
    df <- df |>
        dplyr::left_join(ever_smoked_lookup, by = "pt") |>
        dplyr::mutate(
            smoking_status_raw = sbsmk,
            
            smoking_status_new = dplyr::case_when(
                is.na(sbsmk) ~ NA_character_,
                has_ever_smoked == TRUE & sbsmk == "Never" ~ "Former",
                TRUE ~ sbsmk
            ),
            
            smoking_impute_source = dplyr::case_when(
                is.na(smoking_status_raw) ~ NA_character_,
                smoking_status_raw != smoking_status_new ~ "derived",
                TRUE ~ "observed"
            ),
            
            smoking_status = factor(
                smoking_status_new,
                levels = c("Never", "Former", "Current")
            ),
            
            smoking_impute_source = factor(
                smoking_impute_source,
                levels = c("observed", "derived")
            )
        ) |>
        dplyr::select(-has_ever_smoked, -smoking_status_raw, -smoking_status_new) |>
        dplyr::as_tibble()
    
    # ── Diagnostics ----------------------------------------------------------
    cli::cli_h2("Derive Smoking Status")
    
    summary <- df |>
        dplyr::count(smoking_impute_source)
    
    cli::cli_inform(
        paste(capture.output(print(summary)), collapse = "\n")
    )
    
    return(df)
}