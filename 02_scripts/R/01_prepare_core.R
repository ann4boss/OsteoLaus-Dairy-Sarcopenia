# =============================================================================
# R/01_prepare_core.R
# =============================================================================
# Wrapper function that runs the full static preparation pipeline in one call.
#
# Steps (internal):
#   01  Import raw CSVs
#   02  Harmonise (type coercion, date parsing, factor coding)
#   03  QC – data integrity (participant / visit level)
#   04a Stack visits into long tables
#   04b QC – variable level
#
# @param f_colaus_baseline,f_colaus_f1,f_colaus_f2,f_colaus_f3
#   File paths to CoLaus visit CSVs.
# @param f_osteo_baseline,f_osteo_v2,f_osteo_v3,f_osteo_v4,f_osteo_v5
#   File paths to OsteoLaus visit CSVs.
#
# @return A named list with:
#   $colaus_long         — stacked long tibble for CoLaus (all visits)
#   $osteo_long          — stacked long tibble for OsteoLaus (all visits)
#   $qc_tbl              — participant-level QC flags
#   $qc_summary          — scalar QC summary
# =============================================================================

prepare_core <- function(
        f_colaus_baseline, f_colaus_f1, f_colaus_f2, f_colaus_f3,
        f_osteo_baseline,  f_osteo_v2,  f_osteo_v3,  f_osteo_v4,  f_osteo_v5,
        f_colaus_baseline_add_food, f_colaus_f1_add_food, f_colaus_f2_add_food, f_colaus_f3_add_food, f_colaus_death
) {
    
    # ── 01a. Import ──────────────────────────────────────────────────────────
    cli::cli_h1("01a  Import")
    
    colaus_bsl_raw <- import_visit(f_colaus_baseline, "CoLaus",    "Baseline")
    colaus_f1_raw  <- import_visit(f_colaus_f1,       "CoLaus",    "F1")
    colaus_f2_raw  <- import_visit(f_colaus_f2,       "CoLaus",    "F2")
    colaus_f3_raw  <- import_visit(f_colaus_f3,       "CoLaus",    "F3")
    
    osteo_bsl_raw  <- import_visit(f_osteo_baseline,  "OsteoLaus", "Baseline")
    osteo_v2_raw   <- import_visit(f_osteo_v2,        "OsteoLaus", "V2")
    osteo_v3_raw   <- import_visit(f_osteo_v3,        "OsteoLaus", "V3")
    osteo_v4_raw   <- import_visit(f_osteo_v4,        "OsteoLaus", "V4")
    osteo_v5_raw   <- import_visit(f_osteo_v5,        "OsteoLaus", "V5")
    
    colaus_bsl_raw_add_food <- import_visit(f_colaus_baseline_add_food, "CoLaus", "Baseline", sep = ",")
    colaus_f1_raw_add_food <- import_visit(f_colaus_f1_add_food, "CoLaus", "F1", sep = ",")
    colaus_f2_raw_add_food <- import_visit(f_colaus_f2_add_food, "CoLaus", "F2", sep = ",")
    colaus_f3_raw_add_food <- import_visit(f_colaus_f3_add_food, "CoLaus", "F3", sep = ",")
    
    death <- import_visit(f_colaus_death, "CoLaus", "Baseline", ",")

    
    # ── 01b. add additional columns ──────────────────────────────────────────────────────────
    cli::cli_h1("01b  Concatenate")
    colaus_f1_concatenated <- add_extra_cols(colaus_f1_raw, colaus_f1_raw_add_food, "F1")
    colaus_f2_concatenated <- add_extra_cols(colaus_f2_raw, colaus_f2_raw_add_food, "F2")
    colaus_f3_concatenated <- add_extra_cols(colaus_f3_raw, colaus_f3_raw_add_food, "F3")
    
    colaus_f1_concatenated <- add_ffq_columns(colaus_f1_raw, colaus_f1_raw_add_food, "F1")
    colaus_f2_concatenated <- add_ffq_columns(colaus_f2_raw, colaus_f2_raw_add_food, "F2")
    colaus_f3_concatenated <- add_ffq_columns(colaus_f3_raw, colaus_f3_raw_add_food, "F3")
    
    
    colaus_bsl_concatenated <- add_birth_date(colaus_bsl_raw, colaus_bsl_raw_add_food)
    colaus_bsl_concatenated <- add_death_date(colaus_bsl_concatenated, death)
    osteo_bsl_concatenated <- add_birth_date(osteo_bsl_raw, colaus_bsl_raw_add_food)
    
    # ── 02. Harmonise ───────────────────────────────────────────────────────
    cli::cli_h1("02  Harmonise")
    
    colaus_bsl_harm <- harmonise_colaus(colaus_bsl_concatenated)
    colaus_f1_harm  <- harmonise_colaus(colaus_f1_concatenated)
    colaus_f2_harm  <- harmonise_colaus(colaus_f2_concatenated)
    colaus_f3_harm  <- harmonise_colaus(colaus_f3_concatenated)
    
    osteo_bsl_harm  <- harmonise_osteo(osteo_bsl_concatenated)
    osteo_v2_harm   <- harmonise_osteo(osteo_v2_raw)
    osteo_v3_harm   <- harmonise_osteo(osteo_v3_raw)
    osteo_v4_harm   <- harmonise_osteo(osteo_v4_raw)
    osteo_v5_harm   <- harmonise_osteo(osteo_v5_raw)
    
    # ── 03. QC – data integrity ─────────────────────────────────────────────
    cli::cli_h1("03  QC – data integrity")
    
    qc_out <- qc(list(
        colaus_bsl_harm, colaus_f1_harm, colaus_f2_harm, colaus_f3_harm,
        osteo_bsl_harm,  osteo_v2_harm,  osteo_v3_harm,  osteo_v4_harm,  osteo_v5_harm
    ))
    
    # ── 04. Stack visits ───────────────────────────────────────────────────
    cli::cli_h1("04  Stack visits")
    
    colaus_long <- stack_visits(
        colaus_bsl_harm, colaus_f1_harm,
        colaus_f2_harm,  colaus_f3_harm
    )
    
    osteo_long <- stack_visits(
        osteo_bsl_harm, osteo_v2_harm, osteo_v3_harm,
        osteo_v4_harm,  osteo_v5_harm
    )
    
    
    # ── Done ────────────────────────────────────────────────────────────────
    cli::cli_h1("prepare_core complete")
    cli::cli_inform(c(
        "v" = "colaus_long : {nrow(colaus_long)} rows \u00d7 {ncol(colaus_long)} cols",
        "v" = "osteo_long  : {nrow(osteo_long)} rows \u00d7 {ncol(osteo_long)} cols"
    ))
    
    list(
        colaus_long         = colaus_long,
        osteo_long          = osteo_long,
        qc_tbl              = qc_out$tbl,
        qc_summary          = qc_out$summary
    )
}