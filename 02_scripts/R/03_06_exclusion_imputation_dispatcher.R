run_by_imputation <- function(data, qc_table, imp_col, fun) {
    
    imps <- sort(unique(data[[imp_col]]))
    imps <- imps[imps > 0]
    
    lapply(imps, function(i) {
        
        d_i <- dplyr::filter(data, .data[[imp_col]] == i)
        q_i <- qc_table
        
        fun(d_i, q_i, i)
    })
}