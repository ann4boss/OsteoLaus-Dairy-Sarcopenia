qc_tbl_filled <- qc_tbl |>
    
    # ensure Date format
    dplyr::mutate(exam_date_iso = as.Date(exam_date_iso)) |>
    
    # get baseline reference per participant
    dplyr::group_by(pt) |>
    dplyr::mutate(
        baseline_date = exam_date_iso[.wave == "Baseline"][1],
        baseline_age  = Age[.wave == "Baseline"][1]
    ) |>
    dplyr::ungroup() |>
    
    # compute new dates
    dplyr::mutate(
        age_diff_years = Age - baseline_age,
        age_diff_days  = round(age_diff_years * 365.25),
        
        exam_date_imputed = baseline_date + age_diff_days
    ) |>
    
    # fill only where needed
    dplyr::mutate(
        exam_date_iso = dplyr::if_else(
            qc_pt_present & !qc_exam_date,
            exam_date_imputed,
            exam_date_iso
        )
    ) |>
    
    # optional: clean helper columns
    dplyr::select(-baseline_date, -baseline_age,
                  -age_diff_years, -age_diff_days,
                  -exam_date_imputed)