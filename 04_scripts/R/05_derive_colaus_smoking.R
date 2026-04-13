# =============================================================================
# R/derive_colaus_smoking.R
# =============================================================================
# Derives smoking_status — a trajectory-corrected version of sbsmk.
#
# Source variable: sbsmk (Factor: Never / Former / Current; per wave)
# Produced by harmonise_colaus() from the raw sbsmk column with sentinel 9 -> NA.
#
# Problem
# -------
# Some participants report implausible trajectories across CoLaus waves:
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
# Correction rule (applied wave-by-wave, within each participant):
#   If a participant has at least one non-NA wave with sbsmk != "Never",
#   any remaining "Never" waves are recoded to "Former".
#

# Output
# ------
#   smoking_status   Factor: Never / Former / Current
#                    Same levels as sbsmk; "Never" entries corrected where
#                    a participant has evidence of lifetime smoking elsewhere.
#
# =============================================================================

#' Derive smoking_status (trajectory-corrected sbsmk) for a CoLaus long tibble.
#'
#' @param df CoLaus long tibble after harmonisation and stacking.
#' @return df with smoking_status (factor: Never / Former / Current) added,
#'   plus diagnostic counts emitted as cli messages.
derive_smoking <- function(df) {
    
    # Ensure source column sbsmk is present
    required_cols <- c("sbsmk", ".wave")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        cli::cli_warn(
            "derive_smoking: missing required columns: {.val {missing_cols}}. 
        Smoking status will not be derived."
        )
        return(df)
    }
    
    
    # ── Ensure Lazy State -------------------------------------------------
    if (!inherits(df, "dtplyr_step")) df <- dtplyr::lazy_dt(df)
    
    
    # ── Trajectory diagnostics on RAW sbsmk ----------------------------------
    smk_seq_lazy <- df |>
        dplyr::filter(!is.na(sbsmk)) |>
        dplyr::arrange(pt, .wave) |>
        dplyr::group_by(pt) |>
        dplyr::filter(dplyr::n() >= 2L) |>
        dplyr::mutate(
            prev       = dplyr::lag(sbsmk),
            to_never   = !is.na(prev) & prev != "Never" & sbsmk == "Never",
            from_never = !is.na(prev) & prev == "Never" & sbsmk != "Never",
            quit       = !is.na(prev) & prev == "Current" & sbsmk == "Former",
            relapse    = !is.na(prev) & prev == "Former"  & sbsmk == "Current"
        ) |>
        dplyr::summarise(
            any_implausible = any(to_never | from_never, na.rm = TRUE),
            any_quit        = any(quit,    na.rm = TRUE),
            any_relapse     = any(relapse, na.rm = TRUE),
            .groups = "drop"
        )
    
    # Trigger collection for diagnostics
    smk_seq <- dplyr::as_tibble(smk_seq_lazy)
    
    n_with_seq     <- nrow(smk_seq)
    n_implausible  <- sum(smk_seq$any_implausible)
    n_quit_relapse <- sum(smk_seq$any_quit &  smk_seq$any_relapse)
    n_quit_only    <- sum(smk_seq$any_quit & !smk_seq$any_relapse)
    n_relapse_only <- sum(!smk_seq$any_quit &  smk_seq$any_relapse)
    n_stable       <- sum(!smk_seq$any_quit & !smk_seq$any_relapse & !smk_seq$any_implausible)
    
    cli::cli_h2("Derive Smoking Status")
    cli::cli_inform(c(
        "i" = "derive_smoking(): trajectory diagnostics on raw sbsmk (n = {n_with_seq}):",
        "*" = "Implausible transitions (Never <-> Former/Current): {n_implausible}",
        "*" = "Quit only: {n_quit_only}",
        "*" = "Relapse only: {n_relapse_only}",
        "*" = "Quit AND relapsed: {n_quit_relapse}",
        "*" = "Stable: {n_stable}"
    ))
    
    # ── Identify Lifetime Smoking Evidence ----------------------------------
    # We create a lazy lookup table instead of pulling a vector
    ever_smoked_lookup <- df |>
        dplyr::filter(!is.na(sbsmk), sbsmk != "Never") |>
        dplyr::distinct(pt) |>
        dplyr::mutate(has_ever_smoked = TRUE)
    
    # Count corrections for the message (requires collection)
    n_corrected <- df |>
        dplyr::left_join(ever_smoked_lookup, by = "pt") |>
        dplyr::filter(has_ever_smoked == TRUE, !is.na(sbsmk), sbsmk == "Never") |>
        dplyr::as_tibble() |>
        nrow()
    
    cli::cli_inform(c("i" = "derive_smoking(): {n_corrected} 'Never' observations to be recoded to 'Former'."))
    
    # ── Build smoking_status ----------------------------------
    df <- df |>
        dplyr::left_join(ever_smoked_lookup, by = "pt") |>
        dplyr::mutate(
            smoking_status = dplyr::case_when(
                is.na(sbsmk) ~ NA_character_,
                has_ever_smoked == TRUE & sbsmk == "Never" ~ "Former",
                TRUE ~ as.character(sbsmk)
            ) |> factor(levels = c("Never", "Former", "Current"))
        ) |>
        dplyr::select(-has_ever_smoked)
    
    # ── Post-correction diagnostics ----------------------------------
    smk_seq_corr_lazy <- df |>
        dplyr::filter(!is.na(smoking_status)) |>
        dplyr::arrange(pt, .wave) |>
        dplyr::group_by(pt) |>
        dplyr::filter(dplyr::n() >= 2L) |>
        dplyr::mutate(
            prev = dplyr::lag(smoking_status),
            to_never   = !is.na(prev) & prev != "Never" & smoking_status == "Never",
            from_never = !is.na(prev) & prev == "Never" & smoking_status != "Never",
            quit       = !is.na(prev) & prev == "Current" & smoking_status == "Former",
            relapse    = !is.na(prev) & prev == "Former"  & smoking_status == "Current"
        ) |>
        dplyr::summarise(
            any_implausible = any(to_never | from_never, na.rm = TRUE),
            any_quit        = any(quit,    na.rm = TRUE),
            any_relapse     = any(relapse, na.rm = TRUE),
            .groups = "drop"
        )
    
    
    smk_seq_corr <- dplyr::as_tibble(smk_seq_corr_lazy)
    
    n_implausible_corr  <- sum(smk_seq_corr$any_implausible)
    n_quit_relapse_corr <- sum(smk_seq_corr$any_quit &  smk_seq_corr$any_relapse)
    n_quit_only_corr    <- sum(smk_seq_corr$any_quit & !smk_seq_corr$any_relapse)
    n_relapse_only_corr <- sum(!smk_seq_corr$any_quit &  smk_seq_corr$any_relapse)
    n_stable_corr       <- sum(!smk_seq_corr$any_quit & !smk_seq_corr$any_relapse & !smk_seq_corr$any_implausible)
    
    cli::cli_inform(c(
        "i" = "derive_smoking(): trajectory diagnostics on corrected status:",
        "*" = "Implausible transitions remaining: {n_implausible_corr}",
        "*" = "Quit only: {n_quit_only_corr}",
        "*" = "Relapse only: {n_relapse_only_corr}",
        "*" = "Quit AND relapsed: {n_quit_relapse_corr}",
        "*" = "Stable: {n_stable_corr}"
    ))
    
    if (n_implausible_corr > 0L) cli::cli_warn("derive_smoking(): {n_implausible_corr} participant(s) still have implausible trajectory.")
    
    # Drop source column for diagnostics (not needed beyond this point)
    df <- df |> dplyr::select(-sbsmk)
    
    
    return(df)
}