# =============================================================================
# R/05_derive_osteo_hgs.R
# =============================================================================
# Derives HGS_peak (peak grip strength, kg) from the six individual measures:
# HGS_R1, HGS_R2, HGS_R3 (right hand) and HGS_L1, HGS_L2, HGS_L3 (left hand).
#
# Each hand was measured only if it was the dominant hand, so typically only
# three of the six columns are non-NA per participant. HGS_peak is the row-wise
# maximum across all non-NA values from both hands.
#
#
# Note: grip strength is only available at V5. For all other waves all six
# source columns will be NA, so HGS_peak will be NA — this is expected.
#
# Relationship to handgrip_max_all:
#   HGS_peak          = OsteoLaus-only peak grip (V5), derived here.
#   handgrip_max_all = HGS_peak coalesced with CoLaus handgrip across all waves,
#                      assembled later in build_sarcopenia() (07b).
#
# =============================================================================

.HGS_COLS <- c("HGS_R1", "HGS_R2", "HGS_R3", "HGS_L1", "HGS_L2", "HGS_L3")

#' Derive HGS_peak from the six individual grip strength measures.
#'
#' @param df OsteoLaus long tibble after harmonisation and stacking.
#' @return df with HGS_peak (numeric, kg) added. If HGS_peak was already present
#'   in the source data it is preserved as HGS_MAX_source for validation.
derive_hgs_max <- function(df) {
    
    cols_present <- intersect(.HGS_COLS, names(df))
    
    if (length(cols_present) == 0) {
        cli::cli_warn(
            "derive_hgs_max: no HGS source columns found \
       ({.val {.HGS_COLS}}). {.col HGS_peak} will not be derived."
        )
        return(df)
    }
    
    
    df <- dplyr::mutate(df,
                        HGS_peak = {
                            mat    <- as.matrix(dplyr::pick(dplyr::all_of(cols_present)))
                            all_na <- apply(is.na(mat), 1, all)
                            # Use pmax via do.call to avoid the base R warning fired by
                            # apply(mat, 1, max, na.rm = TRUE) when all values in a row are NA
                            # (-Inf with a warning). pmax handles all-NA rows silently.
                            out <- do.call(
                                pmax,
                                c(lapply(seq_len(ncol(mat)), function(j) mat[, j]),
                                  list(na.rm = TRUE))
                            )
                            out[all_na] <- NA_real_
                            out
                        }
    )
    
    # ── Validation against HGS_MAX_source ─────────────────────────────────────
    if ("HGS_MAX" %in% names(df)) {
        n_mismatch <- sum(
            !is.na(df$HGS_peak) & !is.na(df$HGS_MAX) &
                abs(df$HGS_peak - df$HGS_MAX) > 0.01,
            na.rm = TRUE
        )
        if (n_mismatch > 0)
            cli::cli_warn(
                "derive_hgs_max: {n_mismatch} row(s) where derived \
         {.col HGS_peak} differs from source {.col HGS_MAX_source} \
         by > 0.01 kg. Check the six individual measures."
            )
        else
            cli::cli_inform(
                c("v" = "derive_hgs_max: derived HGS_peak matches HGS_MAX_source for all non-missing rows.")
            )
    }
    
    n_derived <- sum(!is.na(df$HGS_peak))
    cli::cli_inform(
        c("i" = "derive_hgs_max: HGS_peak derived for {n_derived} / {nrow(df)} rows \
       ({length(cols_present)} source column(s): {.val {cols_present}}).")
    )
    
    return(df)
}