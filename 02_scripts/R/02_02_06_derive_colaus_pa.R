# =============================================================================
# R/02_02_06_derive_colaus_pa.R
# =============================================================================
# Derives two parallel PA classifications. Defines one function: derive_pa().
#
#   pa_levels_tertile  Cohort-relative tertile (Low / Medium / High)
#   pa_levels_who      PAFQ-corrected WHO categories, collapsed to 3 levels
#                      (Low / Medium / High)
#
# Source variables (min/day from PAFQ):
#   PAFQ_MPA, PAFQ_VPA
#
# ── Rationale ────────────────────────────────────────────────────────────────
# Verhoog et al. (2019, Maturitas 129:68–75) validated the PAFQ in this same
# CoLaus cohort against 14-day accelerometry (n = 1752). Key findings:
#   - PAFQ overestimates MVPA by a median ratio of ~1.28 (213 vs 166 min/day)
#   - Lin's CCC for MVPA = 0.254; for LPA only 0.075 → LPA excluded
#   - Overestimation increases with activity level (Bland-Altman, Fig. 2)
#   - Sex and age influence over/underestimation; BMI does not
#
# Applying standard WHO thresholds to raw PAFQ data therefore inflates the
# "Sufficient" and "High" categories. Both methods below address this.
#
# METHOD 1 — Tertile (pa_levels_tertile)
#   Raw MVPA (MPA + VPA, min/day).
#   Cut-points fixed at F1 baseline.

# METHOD 2 — Corrected WHO (pa_levels_who)
#   ME-min/week = (MPA + 2xVPA) x 7. WHO 2020 thresholds scaled by 1.28,
#   then collapsed to 3 levels matching the tertile labels:
#     Low    : 0 - 191 ME-min/week  (Inactive + Insufficient; was 0-149)
#     Medium : 192 - 383            (Sufficient;               was 150-299)
#     High   : >= 384               (High;                     was >=300)
#   Note: correction factor is a median ratio and likely conservative for
#   highly active individuals.
#
# Ref: Verhoog et al. (2019) https://doi.org/10.1016/j.maturitas.2019.08.004
# =============================================================================


# -----------------------------------------------------------------------------
# derive_pa()
# -----------------------------------------------------------------------------
#' Derive Physical Activity (PA) classification variables
#'
#' @param df CoLaus long tibble containing `PAFQ_MPA`, `PAFQ_VPA`, `.visit`.
#' @return `df` with `pa_levels_tertile` and `pa_levels_who` added.
derive_pa <- function(df) {
    
    # ── Check required columns ------------------------------------------------
    required_cols <- c("PAFQ_MPA", "PAFQ_VPA", ".visit")
    missing_cols  <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        cli::cli_warn(c(
            "!" = "derive_pa: missing columns: {.val {missing_cols}}.",
            "i" = "PA variables will not be derived."
        ))
        return(df)
    }
    
    if (inherits(df, "dtplyr_step")) df <- dplyr::collect(df)
    
    # ── Shared intermediate: raw MVPA (min/day) ───────────────────────────────
    df <- df |>
        dplyr::mutate(
            mvpa_min_day = dplyr::case_when(
                is.na(PAFQ_MPA) & is.na(PAFQ_VPA) ~ NA_real_,
                TRUE ~ dplyr::coalesce(PAFQ_MPA, 0) +
                    dplyr::coalesce(PAFQ_VPA, 0)
            )
        )
    
    # =========================================================================
    # METHOD 1 — Tertile classification
    # =========================================================================
    f1_mvpa <- df |>
        dplyr::filter(.visit == "F1", !is.na(mvpa_min_day)) |>
        dplyr::pull(mvpa_min_day)
    
    if (length(f1_mvpa) < 10)
        cli::cli_warn("Fewer than 10 F1 observations — tertile cuts may be unstable.")
    
    tertile_cuts <- quantile(f1_mvpa, probs = c(1/3, 2/3), na.rm = TRUE)
    t33 <- tertile_cuts[[1]]
    t67 <- tertile_cuts[[2]]
    
    df <- df |>
        dplyr::mutate(
            pa_levels_tertile = dplyr::case_when(
                is.na(mvpa_min_day)   ~ NA_character_,
                mvpa_min_day <  t33   ~ "Low",
                mvpa_min_day <  t67   ~ "Medium",
                mvpa_min_day >= t67   ~ "High"
            ) |>
                factor(levels = c("Low", "Medium", "High"), ordered = TRUE)
        )
    
    
    # =========================================================================
    # METHOD 2 — Corrected WHO classification
    # =========================================================================
    correction <- 1.28
    adj_cuts   <- round(c(30, 149, 299) * correction)  # 38, 191, 383
    
    df <- df |>
        dplyr::mutate(
            tmp_me_min_week = dplyr::case_when(
                is.na(PAFQ_MPA) & is.na(PAFQ_VPA) ~ NA_real_,
                TRUE ~
                    (dplyr::coalesce(PAFQ_MPA, 0) +
                         2 * dplyr::coalesce(PAFQ_VPA, 0))
            ),
            # Low  = Inactive + Insufficient (below sufficiency threshold)
            # Medium = Sufficient
            # High   = High
            pa_levels_who = dplyr::case_when(
                is.na(tmp_me_min_week)            ~ NA_integer_,
                tmp_me_min_week <= adj_cuts[2]    ~ 1L,   # <= 191 ME-min/week
                tmp_me_min_week <= adj_cuts[3]    ~ 2L,   # 192–383
                tmp_me_min_week >  adj_cuts[3]    ~ 3L    # >= 384
            ) |>
                factor(
                    levels  = 1:3,
                    labels  = c("Low", "Medium", "High"),
                    ordered = TRUE
                )
        ) 
    
    
    # =========================================================================
    # Frozen values
    # =========================================================================
    
    f1_lookup <- df |>
        dplyr::filter(.visit == "F1") |>
        dplyr::select(pt,
                      mvpa_min_day_f1 = mvpa_min_day,  # Add this
                      pa_levels_tertile_f1 = pa_levels_tertile,
                      pa_levels_who_f1     = pa_levels_who)
    
    
    
    df <- df |>
        dplyr::left_join(f1_lookup, by = "pt")
    
    
    f2_lookup <- df |>
        dplyr::filter(.visit == "F2") |>
        dplyr::select(pt,
                      mvpa_min_day_f2 = mvpa_min_day,  # Add this
                      pa_levels_tertile_f2 = pa_levels_tertile,
                      pa_levels_who_f2     = pa_levels_who)
    
    df <- df |>
        dplyr::left_join(f2_lookup, by = "pt")
    # =========================================================================
    # REPORTING
    # =========================================================================
    cli::cli_h2("Physical Activity Derivation Report")
    
    cli::cli_inform(c(
        "i" = "Tertile cut-points (from F1, N = {length(f1_mvpa)}):",
        " " = "Low < {round(t33, 1)} | Medium {round(t33, 1)}-{round(t67, 1)} | High >= {round(t67, 1)} min/day MVPA",
        "i" = "WHO thresholds (x{correction} correction, collapsed to 3 levels):",
        " " = "Low <= {adj_cuts[2]} | Medium {adj_cuts[2]+1}-{adj_cuts[3]} | High > {adj_cuts[3]} ME-min/week"
    ))
    
    # ── Per-visit distribution ────────────────────────────────────────────────
    pa_summary <- df |>
        dplyr::group_by(.visit) |>
        dplyr::summarise(
            n_low_t    = sum(pa_levels_tertile == "Low",    na.rm = TRUE),
            n_medium_t = sum(pa_levels_tertile == "Medium", na.rm = TRUE),
            n_high_t   = sum(pa_levels_tertile == "High",   na.rm = TRUE),
            n_total_t  = sum(!is.na(pa_levels_tertile)),
            n_low_w    = sum(pa_levels_who == "Low",        na.rm = TRUE),
            n_medium_w = sum(pa_levels_who == "Medium",     na.rm = TRUE),
            n_high_w   = sum(pa_levels_who == "High",       na.rm = TRUE),
            n_total_w  = sum(!is.na(pa_levels_who)),
            .groups = "drop"
        )
    
    purrr::pwalk(pa_summary, function(...) {
        r <- list(...)
        pct <- function(x, n) scales::percent(x / n, accuracy = 0.1)
        cli::cli_inform(c(
            "v" = "{.strong Visit: {r$.visit}}",
            "-" = "Tertile (N = {r$n_total_t})",
            " " = "Low {r$n_low_t} ({pct(r$n_low_t, r$n_total_t)})  Medium {r$n_medium_t} ({pct(r$n_medium_t, r$n_total_t)})  High {r$n_high_t} ({pct(r$n_high_t, r$n_total_t)})",
            "-" = "WHO corrected (N = {r$n_total_w})",
            " " = "Low {r$n_low_w} ({pct(r$n_low_w, r$n_total_w)})  Medium {r$n_medium_w} ({pct(r$n_medium_w, r$n_total_w)})  High {r$n_high_w} ({pct(r$n_high_w, r$n_total_w)})",
            ""
        ))
    })
    
    # ── Cross-method alignment summary ────────────────────────────────────────
    cli::cli_h3("Method alignment (tertile vs WHO corrected)")
    
    df |>
        dplyr::filter(!is.na(pa_levels_tertile), !is.na(pa_levels_who)) |>
        dplyr::group_by(.visit) |>
        dplyr::summarise(
            n_agree = sum(pa_levels_tertile == pa_levels_who),
            n_total = dplyr::n(),
            .groups = "drop"
        ) |>
        purrr::pwalk(function(...) {
            r <- list(...)
            cli::cli_inform(c(
                "v" = "Visit {r$.visit}: {r$n_agree}/{r$n_total} in same category ({scales::percent(r$n_agree / r$n_total, accuracy = 0.1)})"
            ))
        })
    
    return(df)
}