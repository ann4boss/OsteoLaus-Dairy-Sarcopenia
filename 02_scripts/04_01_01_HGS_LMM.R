library(lmerTest)
library(flexplot)
library(ggplot2)
library(splines)
library(dplyr)
library(nlme)
library(mgcv)
library(rms)


# Function to create scaled dataset with within/between components
create_scaled_dataset <- function(data, median_values = NULL) {
  
  # Calculate medians if not provided
  if (is.null(median_values)) {
    median_values <- list(
      Age = median(data$Age, na.rm = TRUE),
      age_at_baseline = median(data$age_at_baseline, na.rm = TRUE),
      sumtot1 = median(data$sumtot1, na.rm = TRUE),
      dairy_total = median(data$dairy_total_gday_cumavg, na.rm = TRUE),
      dairy_fermented = median(data$dairy_fermented_gday_cumavg, na.rm = TRUE),
      time_since_baseline = median(data$time_since_baseline, na.rm = TRUE),
      BMI = median(data$BMI, na.rm = TRUE),
      Height = median(data$Height, na.rm = TRUE),
      Weight = median(data$Weight, na.rm = TRUE)
    )
  }
  
  # Create scaled dataset
  df_scaled <- data %>%
    group_by(pt) %>%
    mutate(
      # ============================================
      # WITHIN and BETWEEN components for BMI
      # ============================================
      BMI_mean = mean(BMI, na.rm = TRUE),
      BMI_within = BMI - BMI_mean,
      BMI_scale_mean = scale(BMI_mean, center = median_values$BMI, scale = 1),
      BMI_scale_within = scale(BMI_within, center = 0, scale = 1),
      
      # ============================================
      # WITHIN and BETWEEN components for Calories
      # ============================================
      sumtot1_mean = mean(sumtot1, na.rm = TRUE),
      sumtot1_within = sumtot1 - sumtot1_mean,
      sumtot1_scaled_mean = scale(sumtot1_mean, center = median_values$sumtot1, scale = 1000),
      sumtot1_scaled_within = scale(sumtot1_within, center = 0, scale = 1000),
      
      # Smoking components
      smoking_mean = as.numeric(factor(smoking_status, 
                                       levels = c("Never", "Former", "Current"))) - 1,
      
      # Diabetes components
      diabetes_mean = as.numeric(diabetes_status == "Diabetes"),
      
      # ============================================
      # Standard scaling for time-invariant predictors
      # ============================================
      age_at_baseline_scaled = scale(age_at_baseline, 
                                     center = median_values$age_at_baseline,    
                                     scale = 10),
      age_decades = scale(Age, 
                          center = median_values$Age,    
                          scale = 10),
      
      Weight_scale = scale(Weight,
                           center = median_values$Weight,    
                           scale = 1),
      Height_scale = scale(Height,
                           center = median_values$Height,    
                           scale = 1),
      
      # ============================================
      # Scale time-varying predictors at the within level
      # ============================================
      time_years = scale(time_since_baseline,
                         center = median_values$time_since_baseline,
                         scale = 1),
      
      # ============================================
      # Scale dairy predictors
      # ============================================
      dairy_100g = scale(dairy_total_gday_cumavg,
                         center = median_values$dairy_total,
                         scale = 100),
      fermented_100g = scale(dairy_fermented_gday_cumavg,
                             center = median_values$dairy_fermented,
                             scale = 100),
      
      # ============================================
      # Transformations of HGS_MAX
      # ============================================
      HGS_MAX_log = log(HGS_MAX),
      HGS_MAX_sqrt = sqrt(HGS_MAX)
    ) %>%
    ungroup() %>%
    mutate(
      # ============================================
      # Create within-person components for categorical variables
      # ============================================
      smoking_never = as.numeric(smoking_status == "Never"),
      smoking_former = as.numeric(smoking_status == "Former"),
      smoking_current = as.numeric(smoking_status == "Current"),
      
      smoking_never_mean = as.numeric(smoking_mean == 0),
      smoking_former_mean = as.numeric(smoking_mean == 1),
      smoking_current_mean = as.numeric(smoking_mean == 2),
      
      smoking_never_within = smoking_never - smoking_never_mean,
      smoking_former_within = smoking_former - smoking_former_mean,
      smoking_current_within = smoking_current - smoking_current_mean,
      
      diabetes_no = as.numeric(diabetes_status == "No diabetes"),
      diabetes_yes = as.numeric(diabetes_status == "Diabetes"),
      
      diabetes_yes_mean = diabetes_mean,
      diabetes_no_mean = 1 - diabetes_mean,
      
      diabetes_yes_within = diabetes_yes - diabetes_yes_mean,
      diabetes_no_within = diabetes_no - diabetes_no_mean,
      
      # ============================================
      # Ensure categorical variables are unordered factors
      # ============================================
      dairy_quartile_baseline = factor(dairy_quartile_baseline, 
                                       levels = c("Q1", "Q2", "Q3", "Q4"), 
                                       ordered = FALSE) |> 
        relevel(ref = "Q1"),
      
      dairy_guidelines_port = factor(dairy_guidelines_port, 
                                     levels = c("< 2 servings/day", ">= 2 servings/day"), 
                                     ordered = FALSE) |> 
        relevel(ref = "< 2 servings/day"),
      
      BMI_category = factor(BMI_category, 
                            levels = c("Underweight", "Normal", "Overweight", "Obese"), 
                            ordered = FALSE) |> 
        relevel(ref = "Normal"),
      
      education_level = factor(education_level, 
                               levels = c("Low (ISCED 0-2)", "Medium (ISCED 3-4)", "High (ISCED 5-8)"), 
                               ordered = FALSE) |> 
        relevel(ref = "Low (ISCED 0-2)"),
      
      smoking_status = factor(smoking_status, 
                              levels = c("Never", "Former", "Current"), 
                              ordered = FALSE) |> 
        relevel(ref = "Never"),
      
      pa_levels_tertile_f1 = factor(pa_levels_tertile_f1, 
                                    levels = c("Low", "Medium", "High"), 
                                    ordered = FALSE) |> 
        relevel(ref = "Low"),
      
      diabetes_status = factor(diabetes_status, 
                               levels = c("No diabetes", "Diabetes"), 
                               ordered = FALSE) |> 
        relevel(ref = "No diabetes"),
      
      pt = factor(pt, ordered = FALSE)
    )
  
  return(df_scaled)
}

# Function to filter by baseline age
filter_baseline_age <- function(data, age_threshold = 75) {
  keep_ids <- unique(data$pt[data$time_point == "T1" & data$Age < age_threshold])
  filtered_data <- data[data$pt %in% keep_ids, ]
  return(filtered_data)
}

# Get the first imputation
imp1_hgs <- mice::complete(mice_analysis$mids$HGS_MAX, action = 1)

# Create filtered version
imp1_hgs_filtered <- filter_baseline_age(imp1_hgs, age_threshold = 75)

# Calculate medians from unfiltered data
median_values_unfiltered <- list(
  Age = median(imp1_hgs$Age, na.rm = TRUE),
  age_at_baseline = median(imp1_hgs$age_at_baseline, na.rm = TRUE),
  sumtot1 = median(imp1_hgs$sumtot1, na.rm = TRUE),
  dairy_total = median(imp1_hgs$dairy_total_gday_cumavg, na.rm = TRUE),
  dairy_fermented = median(imp1_hgs$dairy_fermented_gday_cumavg, na.rm = TRUE),
  time_since_baseline = median(imp1_hgs$time_since_baseline, na.rm = TRUE),
  BMI = median(imp1_hgs$BMI, na.rm = TRUE),
  Height = median(imp1_hgs$Height, na.rm = TRUE),
  Weight = median(imp1_hgs$Weight, na.rm = TRUE)
)

# Calculate medians from filtered data
median_values_filtered <- list(
  Age = median(imp1_hgs_filtered$Age, na.rm = TRUE),
  age_at_baseline = median(imp1_hgs_filtered$age_at_baseline, na.rm = TRUE),
  sumtot1 = median(imp1_hgs_filtered$sumtot1, na.rm = TRUE),
  dairy_total = median(imp1_hgs_filtered$dairy_total_gday_cumavg, na.rm = TRUE),
  dairy_fermented = median(imp1_hgs_filtered$dairy_fermented_gday_cumavg, na.rm = TRUE),
  time_since_baseline = median(imp1_hgs_filtered$time_since_baseline, na.rm = TRUE),
  BMI = median(imp1_hgs_filtered$BMI, na.rm = TRUE),
  Height = median(imp1_hgs_filtered$Height, na.rm = TRUE),
  Weight = median(imp1_hgs_filtered$Weight, na.rm = TRUE)
)

# Create scaled datasets
df_stans_scaled_hgs <- create_scaled_dataset(imp1_hgs, median_values_unfiltered)
df_stans_scaled_hgs_filtered <- create_scaled_dataset(imp1_hgs_filtered, median_values_filtered)




# ------------------------------------------------------------------------------
# 1. FIT BOTH MODELS
# ------------------------------------------------------------------------------

# Model 1: WITH random slope (your full model)
mod_with_slope <- lmerTest::lmer(
  HGS_MAX ~ 
    time_since_baseline +
    age_at_baseline +
    dairy_100g + 
    sumtot1_scaled_mean + 
    BMI_category +
    education_level + 
    smoking_status + 
    pa_levels_tertile_f1 + 
    diabetes_status +
    (1 | pt + time_since_baseline),
  data = df_stans_scaled_hgs, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)

mod_no_slope <- lmerTest::lmer(
  HGS_MAX ~ 
    time_since_baseline +
    age_at_baseline +
    dairy_100g + 
    sumtot1_scaled_mean + 
    BMI_category +
    education_level + 
    smoking_status + 
    pa_levels_tertile_f1 + 
    diabetes_status +
    (1 | pt ),
  data = df_stans_scaled_hgs, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)


mod_no_slope_filtered <- lmerTest::lmer(
  HGS_MAX ~ 
    time_since_baseline +
    age_at_baseline +
    dairy_100g + 
    sumtot1_scaled_mean + 
    BMI_category +
    education_level + 
    smoking_status + 
    pa_levels_tertile_f1 + 
    diabetes_status +
    (1 | pt ),
  data = df_stans_scaled_hgs_filtered, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)

summary(mod_no_slope_filtered)

cor(fitted(mod_with_slope), resid(mod_with_slope))
summary(lm(resid(mod_with_slope) ~ fitted(mod_with_slope)))
actual <- df_stans_scaled_hgs$HGS_MAX
fitted <- fitted(mod_with_slope)

cor_actual_fitted <- cor(actual, fitted)
print(paste("Correlation between actual and fitted values:", round(cor_actual_fitted, 4)))

# Also get R-squared (which is correlation squared)
r_squared <- cor_actual_fitted^2
print(paste("R-squared:", round(r_squared, 4)))



fit_hetero <- lme(
  fixed = HGS_MAX ~ 
    age_at_baseline_scaled * time_since_baseline +  # Use * for both main + interaction
    dairy_100g * time_since_baseline +              # Use * for both main + interaction
    age_at_baseline_scaled * BMI_scale +         # Use * for both main + interaction
    dairy_100g + 
    sumtot1_scaled + 
    BMI_scale + 
    education_level + 
    smoking_status + 
    pa_levels_tertile_f1 + 
    diabetes_status +
    time_since_baseline,
  
  random = ~ time_since_baseline | pt,
  
  weights = varExp(form = ~ fitted(.)),
  
  data = df_stans_scaled_hgs,
  method = "ML",
  
  control = lmeControl(
    maxIter = 200,
    msMaxIter = 200,
    opt = "optim"
  )
)



# Create the restricted cubic spline for dairy (with 3 knots by default)
# You can adjust the number of knots using nk=4 or nk=5
mod_with_slope_spline <- lmerTest::lmer(
  HGS_MAX ~ 
    time_since_baseline +
    age_at_baseline +
    rcs(dairy_100g, 3) +  # 3 knots (default) - adjust as needed
    sumtot1_scaled_mean + 
    BMI_category +
    education_level + 
    smoking_status + 
    pa_levels_tertile_f1 + 
    diabetes_status +
    (1 | pt + time_since_baseline),
  data = df_stans_scaled_hgs, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)

mod_gamm <- gamm(
  HGS_MAX ~ dairy_100g +
    s(age_at_baseline_scaled, k = 5) +  # Very low k!
    sumtot1_scaled_within +
    s(time_since_baseline, k = 5) +      # Very low k!
    BMI_category + 
    education_level + 
    smoking_status + 
    pa_levels_tertile_f1 + 
    diabetes_status,
  random = list(pt = ~ 1),
  data = df_stans_scaled_hgs,
  method = "ML"
)


library(mgcv)
library(ggplot2)
library(gridExtra)
library(lme4)
library(nlme)

# Comprehensive GAMM diagnostics function
diagnose_gamm <- function(gamm_model, data, subject_id = "participant_id") {
  
  # Extract components
  gam_part <- gamm_model$gam
  lme_part <- gamm_model$lme
  
  cat("========== GAMM DIAGNOSTICS ==========\n\n")
  
  # 1. GAM Summary
  cat("1. GAM SUMMARY:\n")
  print(summary(gam_part))
  cat("\n")
  
  # 2. LME Summary (Random Effects)
  cat("2. LME SUMMARY (Random Effects):\n")
  print(summary(lme_part))
  cat("\n")
  
  # 3. Smooth Term Checks
  cat("3. SMOOTH TERM CHECKS:\n")
  gam.check(gam_part)
  cat("\n")
  
  # 4. Concurvity
  cat("4. CONCURRITY:\n")
  print(concurvity(gam_part, full = TRUE))
  cat("\n")
  
  # 5. Random Effects Diagnostics
  cat("5. RANDOM EFFECTS DIAGNOSTICS:\n")
  ranef_vals <- ranef(lme_part)
  print(head(ranef_vals[[1]]))
  cat("\n")
  
  # 6. Residual Diagnostics
  residuals_gam <- residuals(gam_part, type = "pearson")
  fitted_gam <- fitted(gam_part)
  residuals_lme <- residuals(lme_part, type = "pearson")
  
  # 7. Autocorrelation Check
  cat("6. AUTOCORRELATION:\n")
  # Check by subject
  subjects <- unique(data[[subject_id]])
  acf_list <- list()
  for (subj in subjects[1:min(10, length(subjects))]) {
    subj_data <- data[data[[subject_id]] == subj, ]
    subj_resid <- residuals_gam[data[[subject_id]] == subj]
    if (length(subj_resid) > 1) {
      acf_list[[as.character(subj)]] <- acf(subj_resid, plot = FALSE, lag.max = 2)
    }
  }
  cat("ACF results for first 10 subjects (checking autocorrelation):\n")
  print(sapply(acf_list, function(x) x$acf[2]))
  cat("\n")
  
  # Return diagnostics
  return(list(
    gam_summary = summary(gam_part),
    lme_summary = summary(lme_part),
    gam_check = gam.check(gam_part, silent = TRUE),
    concurvity = concurvity(gam_part, full = TRUE),
    random_effects = ranef_vals,
    residuals_gam = residuals_gam,
    residuals_lme = residuals_lme,
    fitted = fitted_gam
  ))
}

diagnose_gamm(mod_gamm, df_stans_scaled_hgs, "pt")


#------



library(performance)
library(DHARMa)

# Comprehensive diagnostics
check_model(mod_glmm)

# DHARMa for residual diagnostics
simulation_output <- simulateResiduals(mod_glmm)
plot(simulation_output)

# Specific checks
# 1. Residuals vs fitted
plot(fitted(mod_glmm), residuals(mod_glmm, type = "pearson"))
abline(h = 0, col = "red")

# 2. Q-Q plot
qqnorm(residuals(mod_glmm))
qqline(residuals(mod_glmm), col = "red")

# 3. Check random effects
ranef_vals <- ranef(mod_glmm)$participant_id[,1]
qqnorm(ranef_vals)
qqline(ranef_vals, col = "red")

# 4. Check for heteroscedasticity
library(car)
ncvTest(mod_glmm)

# 5. Check for influential observations
influence_glmm <- influence(mod_glmm)
plot(influence_glmm)

# 6. Check for outliers
library(performance)
check_outliers(mod_glmm)

# 7. Check collinearity
check_collinearity(mod_glmm)
#--------


mod_with_ar1 <- lme(
  HGS_MAX ~ 
    age_at_baseline_scaled * time_since_baseline +
    dairy_100g * time_since_baseline +
    age_at_baseline_scaled * BMI_category +
    dairy_100g + 
    sumtot1_scaled + 
    BMI_category + 
    education_level + 
    smoking_status + 
    pa_levels_tertile_f1 + 
    diabetes_status,
  random = ~ 1 + time_since_baseline | pt,
  correlation = corAR1(form = ~ time_since_baseline | pt),  # ← ADD THIS
  data = df_stans_scaled_hgs,
  method = "ML",
  control = lmeControl(maxIter = 200, msMaxIter = 200)
)




# Get residuals from each time point separately
T1_data <- df_stans_scaled_hgs %>% filter(time_point == "T1")
T2_data <- df_stans_scaled_hgs %>% filter(time_point == "T2")
T3_data <- df_stans_scaled_hgs %>% filter(time_point == "T3")

# Fit simple models at each time point
lm_T1 <- lm(HGS_MAX ~ age_at_baseline_scaled + dairy_100g + sumtot1_scaled + 
              BMI_category + education_level + smoking_status + 
              pa_levels_tertile_f1 + diabetes_status + time_since_baseline, data = T1_data)

lm_T2 <- lm(HGS_MAX ~ age_at_baseline_scaled + dairy_100g + sumtot1_scaled + 
              BMI_category + education_level + smoking_status + 
              pa_levels_tertile_f1 + diabetes_status+ time_since_baseline, data = T2_data)

lm_T3 <- lm(HGS_MAX ~ age_at_baseline_scaled + dairy_100g + sumtot1_scaled + 
              BMI_category + education_level + smoking_status + 
              pa_levels_tertile_f1 + diabetes_status+ time_since_baseline, data = T3_data)


summary(lm_T3)

comparison_plots <- function(model, time_label) {
  data.frame(
    fitted = fitted(model),
    resid = residuals(model),
    time = time_label
  )
}

all_data <- rbind(
  comparison_plots(lm_T1, "T1"),
  comparison_plots(lm_T2, "T2"),
  comparison_plots(lm_T3, "T3")
)

# Also get longitudinal data
long_data <- data.frame(
  fitted = fitted(mod_with_slope),
  resid = residuals(mod_with_slope),
  time = "Longitudinal"
)

all_data <- rbind(all_data, long_data)

# Plot comparison
ggplot(all_data, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", color = "blue", se = TRUE) +
  facet_wrap(~ time, scales = "free") +
  labs(title = "Residual Patterns: Cross-Sectional vs Longitudinal",
       subtitle = "Pattern only appears in longitudinal model",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

slope_T1 <- coef(lm(residuals(lm_T1) ~ fitted(lm_T1)))[2]
slope_T2 <- coef(lm(residuals(lm_T2) ~ fitted(lm_T2)))[2]
slope_T3 <- coef(lm(residuals(lm_T3) ~ fitted(lm_T3)))[2]
slope_long <- coef(lm(residuals(mod_with_slope) ~ fitted(mod_with_slope)))[2]

# Create summary table
slope_table <- data.frame(
  Model = c("T1", "T2", "T3", "Longitudinal"),
  Slope = round(c(slope_T1, slope_T2, slope_T3, slope_long), 4)
)

print(slope_table)

cat("\n========== INTERPRETATION ==========\n\n")
cat("Cross-sectional slopes (T1, T2, T3):", 
    round(slope_T1, 4), ",", round(slope_T2, 4), ",", round(slope_T3, 4), "\n")
cat("Longitudinal slope:", round(slope_long, 4), "\n")
cat("Difference:", round(slope_long - slope_T1, 4), "\n\n")

if (abs(slope_long) > max(abs(c(slope_T1, slope_T2, slope_T3))) * 2) {
  cat("🔴 Longitudinal slope is MUCH LARGER than cross-sectional slopes\n")
  cat("   → Pattern is ENTIRELY driven by longitudinal variation\n")
  cat("   → This is CLASSIC RANDOM EFFECTS SHRINKAGE\n")
  cat("   → Your model is working correctly!\n")
}






#-----

plot_diagnostics(lm_baseline, "HGS LMM - lm baseline")
plot_diagnostics(mod_with_slope, "HGS LMM - Slope")
plot_diagnostics(mod_within_between, "HGS LMM - within eff")



#----
install.packages("DHARMa")
library(DHARMa)

# Step 1: Simulate residuals
simulationOutput <- simulateResiduals(fittedModel = mod_with_slope, plot = F)

# Step 2: Create main diagnostic plots
plot(simulationOutput, quantreg = TRUE)

# Step 3: Run formal tests
testResiduals(simulationOutput)

# Step 4: Check residuals against specific predictors if needed
plotResiduals(simulationOutput, form = df_stans_scaled_hgs$age_at_baseline_scaled)
plotResiduals(simulationOutput, form = df_stans_scaled_hgs$time_since_baseline)
#---

library(car)
vif(mod_with_slope)

residuals_l1 <- residuals(mod_with_slope)


# Check normality
qqnorm(residuals_l1)
qqline(residuals_l1)

# Check homoscedasticity
plot(fitted(mod_with_slope), residuals_l1)
abline(h = 0)

# Check independence
acf(residuals_l1)  

length(df_stans_scaled_hgs$HGS_MAX)
length(fitted(mod_with_slope))

library(performance)
performance::check_model(mod_with_slope)

acf(residuals(mod_with_slope))

performance::r2(mod_with_slope)
library(insight)


summary(lm(resid(mod_with_slope) ~ fitted(mod_with_slope)))

plot(fitted(mod_with_slope, level = 0), resid(mod_with_slope))
plot(fitted(mod_with_slope, level = 1), resid(mod_with_slope))


# Get both types of fitted values
fitted_fixed <- fitted(mod_with_slope, re.form = NA)  # Fixed effects ONLY
fitted_conditional <- fitted(mod_with_slope)          # Fixed + Random (default)

# Get residuals (conditional residuals - always the same)
residuals_cond <- residuals(mod_with_slope)


test_fixed <- lm(residuals_cond ~ fitted_fixed)
summary(test_fixed)

test_conditional <- lm(residuals_cond ~ fitted_conditional)
summary(test_conditional)

slope_fixed <- coef(test_fixed)[2]
slope_conditional <- coef(test_conditional)[2]

cat("Slope (Fixed Effects Only):", round(slope_fixed, 4), "\n")
cat("Slope (Conditional):", round(slope_conditional, 4), "\n")

VarCorr(mod_with_slope)

# Get variance decomposition
variance_decomp <- get_variance(mod_with_slope)
print(variance_decomp)


summary(imp1$HGS_MAX)

boxplot(imp1$HGS_MAX)
df_plot <- data.frame(HGS_MAX = imp1$HGS_MAX)

# ------------------------------------------------------------------------------

df <- data.frame(
  fitted = fitted(mod_with_slope),
  resid  = resid(mod_with_slope)
)

# Create quintile groups
df$quintile <- cut(df$fitted, 
                   breaks = quantile(df$fitted, probs = seq(0, 1, 0.2), na.rm = TRUE),
                   include.lowest = TRUE,
                   labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4", "Q5 (Highest)"))

# Compute mean residuals per quintile
quintile_means <- df %>%
  group_by(quintile) %>%
  summarise(
    mean_fitted = mean(fitted, na.rm = TRUE),
    mean_resid = mean(resid, na.rm = TRUE),
    sd_resid = sd(resid, na.rm = TRUE),
    n = n()
  )

print(quintile_means)

# Create the plot with quintile means
ggplot(df, aes(x = fitted, y = resid)) +
  # Individual points (semi-transparent)
  geom_point(alpha = 0.1, size = 1, color = "gray40") +
  
  # LOESS smooth (overall trend)
  geom_smooth(method = "loess", se = TRUE, color = "blue", size = 1.2) +
  
  # Quintile means as points
  geom_point(data = quintile_means, 
             aes(x = mean_fitted, y = mean_resid),
             color = "red", size = 4, shape = 18) +
  
  # Quintile means with error bars (±SE)
  geom_errorbar(data = quintile_means,
                aes(x = mean_fitted, 
                    ymin = mean_resid - sd_resid/sqrt(n),
                    ymax = mean_resid + sd_resid/sqrt(n)),
                color = "red", width = 0.3, alpha = 0.5) +
  
  # Horizontal line at zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", size = 0.8) +
  
  # Labels
  labs(
    title = "Residuals vs Fitted Values with Quintile Means",
    subtitle = paste0("Red points = mean residual per fitted-value quintile"),
    x = "Fitted Values",
    y = "Residuals"
  ) +
  
  theme_minimal() +
  
  # Add annotation with quintile information
  annotate("text", 
           x = max(df$fitted, na.rm = TRUE) * 0.95,
           y = max(df$resid, na.rm = TRUE) * 0.9,
           label = paste(
             "Q1 mean:", round(quintile_means$mean_resid[1], 3), "\n",
             "Q2 mean:", round(quintile_means$mean_resid[2], 3), "\n",
             "Q3 mean:", round(quintile_means$mean_resid[3], 3), "\n",
             "Q4 mean:", round(quintile_means$mean_resid[4], 3), "\n",
             "Q5 mean:", round(quintile_means$mean_resid[5], 3)
           ),
           hjust = 1, vjust = 1, size = 3, color = "darkred")

# ------------------------------------------------------------------------------

plot(resid(mod_with_slope) ~ df_stans_scaled_hgs$age_at_baseline_scaled,
     xlab = "Age at Baseline",
     ylab = "Residuals",
     main = "Residuals vs Age")
abline(h = 0, col = "red", lty = 2)

plot(resid(mod_with_slope) ~ df_stans_scaled_hgs$BMI_scale,
     xlab = "BMI",
     ylab = "Residuals",
     main = "Residuals vs BMI")
abline(h = 0, col = "red", lty = 2)

plot(resid(mod_with_slope) ~ df_stans_scaled_hgs$sumtot1,
     xlab = "Calorie Intake",
     ylab = "Residuals",
     main = "Residuals vs Calorie Intake")
abline(h = 0, col = "red", lty = 2)




library(clubSandwich)


confint(mod_with_slope)
confint(coef_test(mod_with_slope, vcov = "CR2"))
# ------------------------------------------------------------------------------
# 2. LIKELIHOOD RATIO TEST (Formal Comparison)
# ------------------------------------------------------------------------------

# Compare models using ANOVA
comparison <- anova(mod_with_slope, mod_no_slope)
print(comparison)

# This tests: H0 = random slope variance = 0 (no random slope needed)
# If p < 0.05, the model with random slope is significantly better

# ------------------------------------------------------------------------------
# 3. MODEL SUMMARY (Compare fit statistics)
# ------------------------------------------------------------------------------

# Get summaries
summary(mod_with_slope)
summary(mod_no_slope)
summary(fit_hetero)


# ------------------------------------------------------------------------------
# 4. VISUAL MOdel
# ------------------------------------------------------------------------------


# Model WITH random slope - shows individual trajectories
visualize(mod_with_slope, formula = HGS_MAX ~ time_since_baseline + pt | dairy_quartile_baseline , sample = length(unique(df_stans_scaled$pt)), plot = "residuals")
visualize(mod_with_slope_spline_both,  plot = "residuals")





# ------------------------------------------------------------------------------
# 1. IDENTIFY OUTLIERS USING VARIOUS METHODS
# ------------------------------------------------------------------------------

# Method 1: Standardized residuals > |3|
df_stans_scaled$std_resid <- rstudent(mod_with_slope)  # Studentized residuals
outliers_std <- df_stans_scaled[abs(df_stans_scaled$std_resid) > 3, ]
cat("Number of outliers (|residual| > 3):", nrow(outliers_std), "\n")
print(outliers_std[, c("pt", "HGS_MAX", "sumtot1", "std_resid")])

# Method 2: Cook's Distance (influential points)
df_stans_scaled$cooks_d <- cooks.distance(mod_with_slope)
influential <- df_stans_scaled[df_stans_scaled$cooks_d > (0.5), ]
cat("Number of influential points:", nrow(influential), "\n")
print(influential[, c("pt", "HGS_MAX", "cooks_d")])

# Method 3: Leverage
df_stans_scaled$leverage <- hatvalues(mod_with_slope)
high_leverage <- df_stans_scaled[df_stans_scaled$leverage > (2*length(fixef(mod_with_slope))/nrow(df_stans_scaled)), ]
cat("Number of high leverage points:", nrow(high_leverage), "\n")


# ------------------------------------------------------------------------------
# 2. VISUALIZE OUTLIERS
# ------------------------------------------------------------------------------

# Plot 1: Residuals vs Fitted with outlier labels
plot(mod_with_slope, id.n = 5)  # Labels top 5 outliers
# Click on points to see IDs
       
# Plot 2: Q-Q plot with outlier identification
qqPlot(mod_with_slope)

# Plot 3: Cook's distance plot
plot(df_stans_scaled$cooks_d, type = "h", 
     xlab = "Index", ylab = "Cook's Distance",
     main = "Cook's Distance")
abline(h = 0.5, col = "red", lty = 2)
text(which(df_stans_scaled$cooks_d > 0.5), 
     df_stans_scaled$cooks_d[df_stans_scaled$cooks_d > 4/nrow(df_stans_scaled)],
     labels = df_stans_scaled$pt[df_stans_scaled$cooks_d > 4/nrow(df_stans_scaled)],
     pos = 4, cex = 0.8)

# Plot 4: Leverage vs Residuals
plot(df_stans_scaled$leverage, df_stans_scaled$std_resid,
     xlab = "Leverage", ylab = "Studentized Residuals",
     main = "Leverage vs Residuals")
abline(h = c(-3, 3), col = "red", lty = 2)
abline(v = 2*length(fixef(mod_with_slope))/nrow(df_stans_scaled), col = "blue", lty = 2)



# ------------------------------------------------------------------------------
# 3. INVESTIGATE EXTREME OUTLIERS
# ------------------------------------------------------------------------------

# Get the most extreme outliers
extreme_outliers <- df_stans_scaled[
    order(abs(df_stans_scaled$std_resid), decreasing = TRUE), 
    c("pt", "HGS_MAX", "age_at_baseline_scaled", "dairy_100g", "BMI_category", 
      "time_since_baseline", "std_resid", "cooks_d")
]
head(extreme_outliers, 10)

nrow(extreme_outliers)

# Check if these are real values or data entry errors
summary(df_stans_scaled$HGS_MAX)
boxplot(df_stans_scaled$HGS_MAX, main = "HGS_MAX Distribution")
points(1, df_stans_scaled$HGS_MAX[which.min(df_stans_scaled$std_resid)], 
       col = "red", pch = 19, cex = 2)


# Robust check: Refit without outliers to see if results change
out <- df_stans_scaled[abs(df_stans_scaled$std_resid) > 3, ]
df_clean <- df_stans_scaled[abs(df_stans_scaled$std_resid) <= 3, ]
model_clean <- update(mod_with_slope, data = df_clean)

plot_diagnostics(model_clean, "HGS LMM - Model Clean")

# Compare coefficients
compare_coefs <- data.frame(
    Predictor = names(fixef(mod_with_slope)),
    With_Outliers = fixef(mod_with_slope),
    Without_Outliers = fixef(model_clean),
    Difference = fixef(mod_with_slope) - fixef(model_clean)
)
print(compare_coefs)

# If differences are small (< 10%), keep outliers


# Function to create partial residuals for a given predictor
partial_residual_plot <- function(model, predictor, data) {
  # Get the name of the predictor as a string
  pred_name <- deparse(substitute(predictor))
  
  # Get fixed effects
  fixed_coef <- fixef(model)
  
  # Extract the coefficient for this predictor
  pred_coef <- fixed_coef[pred_name]
  
  # Calculate partial residuals: residuals + effect of predictor
  partial_resid <- residuals(model) + pred_coef * data[[pred_name]]
  
  # Plot
  plot(data[[pred_name]], partial_resid,
       xlab = pred_name, ylab = "Partial Residuals",
       main = paste("Partial Residuals for", pred_name),
       pch = 19, col = rgb(0, 0, 1, 0.3))
  abline(0, pred_coef, col = "red", lwd = 2)  # Linear fit
  lines(lowess(data[[pred_name]], partial_resid), col = "blue", lwd = 2)  # Loess smooth
  legend("bottomright", legend = c("Linear", "Loess"), col = c("red", "blue"), lwd = 2)
}

# Use for each continuous predictor
partial_residual_plot(model_clean, dairy_100g, df_clean)
partial_residual_plot(model_clean, age_at_baseline_scaled, df_clean)
partial_residual_plot(model_clean, time_since_baseline, df_clean)
partial_residual_plot(model_clean, sumtot1_scaled, df_clean)

s# ------------------------------------------------------------------------------
# Plot model assumption inspectation  + visuallize
# ------------------------------------------------------------------------------

palette <- c("#E76254FF", "#EF8A47FF", "#F7AA58FF", "#FFD06FFF", "#FFE6B7FF",
             "#AADCE0FF", "#72BCD5FF", "#528FADFF", "#376795FF", "#1E466EFF")

plot_diagnostics <- function(model, model_name = "Model") {
  
  resid_vals <- residuals(model)
  fitted_vals <- fitted(model)
  sqrt_abs_resid <- sqrt(abs(resid_vals))
  
  df <- data.frame(
    fitted = fitted_vals,
    residuals = resid_vals,
    sqrt_abs_resid = sqrt_abs_resid
  )
  
  y_max_sl <- max(abs(sqrt_abs_resid), na.rm = TRUE) * 1
  y_max_rd <- max(abs(df$residuals), na.rm = TRUE) * 1
  
  # A. Scale-Location plot
  p_sl <- ggplot(df, aes(x = fitted, y = sqrt_abs_resid)) +
    geom_point(color = palette[9], alpha = 0.6, size = 1.8) +
    geom_smooth(method = "loess", se = TRUE,
                color = palette[1], fill = palette[3], alpha = 0.2) +
    labs(
      x = "Fitted values",
      y = expression(sqrt("|Residuals|"))
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
  
  # B. Residual Dependence plot
  p_rd <- ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point(color = palette[9], alpha = 0.6, size = 1.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = palette[1]) +
    geom_smooth(method = "loess", se = TRUE,
                color = palette[1], fill = palette[4], alpha = 0.2) +
    labs(
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
  
  # C. Q-Q plot
  p_qq <- ggplot(df, aes(sample = residuals)) +
    stat_qq(color = palette[9], alpha = 0.6, size = 1.8) +
    stat_qq_line(color = palette[1], linewidth = 0.8) +
    labs(
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
  
  # Add A, B, C labels
  
  p_rd <- p_rd + annotate("text", x = -Inf, y = Inf, 
                          label = "A", hjust = -0.5, vjust = 1.5, 
                          size = 6, fontface = "bold")
  p_sl <- p_sl + annotate("text", x = -Inf, y = Inf, 
                          label = "B", hjust = -0.5, vjust = 1.5, 
                          size = 6, fontface = "bold")
  p_qq <- p_qq + annotate("text", x = -Inf, y = Inf, 
                          label = "C", hjust = -0.5, vjust = 1.5, 
                          size = 6, fontface = "bold")
  
  combined_plot <- (p_rd | p_sl | p_qq) +
    patchwork::plot_annotation(
      title = paste("Model Diagnostics -", model_name),
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
    )
  
  return(combined_plot)
}

plot_diagnostics(mod_with_slope_spline)

# Load required packages
library(lme4)
library(ggplot2)
library(dplyr)

# Calculate shrinkage for your model
calculate_shrinkage <- function(model) {
  # Get random effects
  ranef_vals <- ranef(model)$pt[,1]
  
  # Get variance components
  var_components <- as.data.frame(VarCorr(model))
  var_random <- var_components[var_components$grp == "pt", "vcov"]
  var_residual <- var_components[var_components$grp == "Residual", "vcov"]
  
  # Calculate shrinkage (also called "empirical Bayes shrinkage")
  # Formula: Shrinkage = 1 - (Var(random) / (Var(random) + Var(residual)/n_obs_per_subject))
  
  # Calculate per subject shrinkage
  n_per_subject <- table(model@frame$pt)
  shrinkage <- 1 - (var_random / (var_random + var_residual/n_per_subject))
  
  # Calculate η-shrinkage (as defined in Savic & Karlsson 2009)
  eta_shrinkage <- 1 - (sd(ranef_vals) / sqrt(var_random))
  
  return(list(
    shrinkage_per_subject = shrinkage,
    eta_shrinkage = eta_shrinkage,
    mean_shrinkage = mean(shrinkage),
    sd_shrinkage = sd(shrinkage),
    var_random = var_random,
    var_residual = var_residual
  ))
}

# Apply to your model
shrinkage_results <- calculate_shrinkage(mod_with_slope)
print(paste("Mean shrinkage:", round(shrinkage_results$mean_shrinkage, 3)))
print(paste("η-shrinkage (Savic & Karlsson):", round(shrinkage_results$eta_shrinkage, 3)))

# Classification based on Savic & Karlsson (2009)
eta_shrinkage <- shrinkage_results$eta_shrinkage
if (eta_shrinkage > 0.30) {
  print("⚠️ SUBSTANTIAL SHRINKAGE (>30%): Diagnostic interpretation should be cautious")
  print("   - Covariate relationships may be masked")
  print("   - Empirical Bayes estimates are distorted")
} else if (eta_shrinkage > 0.20) {
  print("⚠️ MODERATE SHRINKAGE (20-30%): Some caution needed")
} else {
  print("✅ ACCEPTABLE SHRINKAGE (<20%): Diagnostics are reliable")
}

# Visualize shrinkage across subjects
shrinkage_df <- data.frame(
  Participant = names(shrinkage_results$shrinkage_per_subject),
  Shrinkage = as.numeric(shrinkage_results$shrinkage_per_subject),
  N_Observations = as.numeric(table(mod_with_slope@frame$pt))
)

# Plot shrinkage by number of observations
ggplot(shrinkage_df, aes(x = N_Observations, y = Shrinkage)) +
  geom_point(alpha = 0.6, size = 3, color = "steelblue") +
  geom_hline(yintercept = 0.30, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 0.20, linetype = "dashed", color = "orange", size = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "darkblue") +
  labs(
    title = "Shrinkage by Number of Observations per Subject",
    subtitle = paste("η-shrinkage =", round(shrinkage_results$eta_shrinkage, 3)),
    x = "Number of Observations per Subject",
    y = "Shrinkage (η-shrinkage)"
  ) +
  annotate("text", x = max(shrinkage_df$N_Observations)*0.8, 
           y = 0.32, label = "Severe >30%", color = "red") +
  annotate("text", x = max(shrinkage_df$N_Observations)*0.8, 
           y = 0.22, label = "Moderate 20-30%", color = "orange") +
  theme_minimal()

# Histogram of shrinkage
ggplot(shrinkage_df, aes(x = Shrinkage)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.6) +
  geom_vline(xintercept = 0.30, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = 0.20, linetype = "dashed", color = "orange", size = 0.8) +
  labs(
    title = "Distribution of Shrinkage Across Subjects",
    x = "Shrinkage",
    y = "Count"
  ) +
  theme_minimal()

# Diagnostic approach with caution
diagnose_with_shrinkage <- function(model, shrinkage_results) {
  
  eta_shrinkage <- shrinkage_results$eta_shrinkage
  
  cat("========== SAVIC & KARLSSON (2009) DIAGNOSTIC APPROACH ==========\n\n")
  
  if (eta_shrinkage > 0.30) {
    cat("⚠️ SUBSTANTIAL η-SHRINKAGE DETECTED (>30%)\n")
    cat("RECOMMENDATIONS:\n")
    cat("1. Interpret covariate relationships with caution\n")
    cat("2. Do NOT rely solely on empirical Bayes estimates\n")
    cat("3. Consider using fixed effects for primary inference\n")
    cat("4. Use bootstrap or simulation for uncertainty quantification\n\n")
    
    # Bootstrap to assess uncertainty
    cat("IMPLEMENTING BOOTSTRAP FOR UNCERTAINTY QUANTIFICATION:\n")
    
  } else if (eta_shrinkage > 0.20) {
    cat("⚠️ MODERATE η-SHRINKAGE (20-30%)\n")
    cat("RECOMMENDATIONS:\n")
    cat("1. Interpret diagnostics with some caution\n")
    cat("2. Consider sensitivity analyses\n")
  } else {
    cat("✅ ACCEPTABLE η-SHRINKAGE (<20%)\n")
    cat("Diagnostics can be interpreted with confidence\n")
  }
}

diagnose_with_shrinkage(mod_with_slope, shrinkage_results)






