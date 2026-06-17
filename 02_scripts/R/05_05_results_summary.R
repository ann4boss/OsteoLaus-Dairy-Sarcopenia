# =============================================================================
# R/results_summary.R
# =============================================================================
# Manuscript-facing result numbers
# =============================================================================
# library(dplyr)
# library(tidyr)
# library(purrr)


# distribution of number of visit per outcome
analyze_visits_structure <- function(data, pt_col = "pt", visit_col = "time_point", 
                                     imp_col = ".imp", has_imputations = TRUE) {
  
  if(has_imputations) {
    # For imputed data - must group by .imp
    result <- data %>%
      group_by(!!sym(imp_col), !!sym(pt_col)) %>%
      summarise(
        n_visits = n_distinct(!!sym(visit_col)),
        visits = paste(sort(unique(!!sym(visit_col))), collapse = ","),
        .groups = 'drop'
      ) %>%
      group_by(!!sym(imp_col)) %>%
      summarise(
        n_participants_total = n_distinct(!!sym(pt_col)),
        `1_visit` = sum(n_visits == 1),
        `2_visits` = sum(n_visits == 2),
        `3_visits` = sum(n_visits == 3),
        `4+_visits` = sum(n_visits >= 4),
        .groups = 'drop'
      )
  } else {
    # For non-imputed data
    result <- data %>%
      group_by(!!sym(pt_col)) %>%
      summarise(n_visits = n_distinct(!!sym(visit_col)), .groups = 'drop') %>%
      summarise(
        n_participants_total = n(),
        `1_visit` = sum(n_visits == 1),
        `2_visits` = sum(n_visits == 2),
        `3_visits` = sum(n_visits == 3),
        `4+_visits` = sum(n_visits >= 4)
      )
  }
  
  return(result)
}
