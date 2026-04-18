library(splines)

# Helper: create age groups
add_age_groups <- function(df, cut_points = c(60, 70, 80)) {
    df |>
        dplyr::mutate(
            age_group = cut(Age,
                            breaks = c(-Inf, cut_points, Inf),
                            labels = paste0("Age_", c(seq_along(cut_points) + 59))),
            age_group = factor(age_group)
        )
}

fit_minimal_grip_spline <- function(analysis_long,
                                    age_groups = NULL,
                                    random_slope = TRUE) {
    
    df <- analysis_long |>
        dplyr::filter(!is.na(handgrip_max_all), !is.na(dairy_cumavg))
    
    # Create age group factor if provided
    if (!is.null(age_groups)) {
        df <- df |>
            dplyr::mutate(Age_group = cut(Age,
                                          breaks = c(-Inf, age_groups, Inf),
                                          right = FALSE,
                                          labels = paste0("Age<", c(age_groups, "+"))))
    }
    
    # Standardize covariates
    df <- df |>
        dplyr::mutate(
            Age_z        = as.numeric(scale(Age)),
            BMI_z        = as.numeric(scale(BMI)),
            energy_z     = as.numeric(scale(energy_kcal))
        )
    
    re_term <- if (random_slope) "(1 + time_since_bsl_yr | pt)" else "(1 | pt)"
    
    # Example spline formula with Age group included as factor
    f <- stats::as.formula(
        "handgrip_max_all ~ dairy_cumavg + time_since_bsl_yr + 
     splines::ns(Age_z, df = 3) + BMI_z + energy_z + (1 + time_since_bsl_yr | pt)"
    )
    
    fit <- lmerTest::lmer(f, data = df, REML = TRUE,
                          control = lme4::lmerControl(optimizer = "bobyqa"))
    
    list(fit = fit, data = df, formula = f, n_pt = dplyr::n_distinct(df$pt), n_obs = nrow(df))
}

fit_minimal_alm_spline <- function(analysis_long,
                                   age_groups = NULL,
                                   random_slope = TRUE) {
    df <- analysis_long |>
        dplyr::filter(!is.na(ALM_HT2), !is.na(dairy_cumavg))
    
    if (!is.null(age_groups)) {
        df <- df |>
            dplyr::mutate(Age_group = cut(Age,
                                          breaks = c(-Inf, age_groups, Inf),
                                          right = FALSE,
                                          labels = paste0("Age<", c(age_groups, "+"))))
    }
    
    df <- df |>
        dplyr::mutate(
            Age_z    = as.numeric(scale(Age)),
            Height_z = as.numeric(scale(Height)),
            energy_z = as.numeric(scale(energy_kcal))
        )
    
    re_term <- if (random_slope) "(1 + time_since_bsl_yr | pt)" else "(1 | pt)"
    
    f <- stats::as.formula(
        "ALM_HT2 ~ dairy_cumavg + time_since_bsl_yr +
     splines::ns(Age_z, df = 3) + Height_z + energy_z + (1 + time_since_bsl_yr | pt)"
    )
    
    fit <- lmerTest::lmer(f, data = df, REML = TRUE,
                          control = lme4::lmerControl(optimizer = "bobyqa"))
    
    list(fit = fit, data = df, formula = f, n_pt = dplyr::n_distinct(df$pt), n_obs = nrow(df))
}

fit_minimal_gait_spline <- function(analysis_long,
                                    age_groups = NULL) {
    df <- analysis_long |>
        dplyr::filter(!is.na(gait_speed), !is.na(dairy_cumavg),
                      osteo_wave %in% c("V4", "V5"))
    
    if (!is.null(age_groups)) {
        df <- df |>
            dplyr::mutate(Age_group = cut(Age,
                                          breaks = c(-Inf, age_groups, Inf),
                                          right = FALSE,
                                          labels = paste0("Age<", c(age_groups, "+"))))
    }
    
    df <- df |>
        dplyr::mutate(
            Age_z    = as.numeric(scale(Age)),
            BMI_z    = as.numeric(scale(BMI)),
            energy_z = as.numeric(scale(energy_kcal))
        )
    
    f <- stats::as.formula(
        "gait_speed ~ dairy_cumavg + time_since_bsl_yr +
     splines::ns(Age_z, df = 3) + BMI_z + energy_z + (1 | pt)"
    )
    
    fit <- lmerTest::lmer(f, data = df, REML = TRUE,
                          control = lme4::lmerControl(optimizer = "bobyqa"))
    
    list(fit = fit, data = df, formula = f, n_pt = dplyr::n_distinct(df$pt), n_obs = nrow(df))
}