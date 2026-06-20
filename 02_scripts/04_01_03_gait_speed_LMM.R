library(lmerTest)
library(flexplot)
library(ggplot2)
library(splines)

imp1 <- mice::complete(mice_analysis$mids$gait_speed, action = 1)
df <- imp1



# Calculate medians from your data
median_age <- median(df$Age_lag, na.rm = TRUE)
median_age_baseline <- median(df$age_at_baseline_lag, na.rm = TRUE)
median_sumtot1 <- median(df$sumtot1_lag, na.rm = TRUE)
median_dairy <- median(df$dairy_total_gday_cumavg_lag, na.rm = TRUE)
median_fermented <- median(df$dairy_fermented_gday_cumavg_lag, na.rm = TRUE)
median_time <- median(df$time_since_baseline_lag, na.rm = TRUE)
median_BMI <- median(df$BMI_lag, na.rm = TRUE)

df_stans_scaled <- df %>%
    mutate(
        # scale by 10 years
      age_at_baseline_scaled = scale(age_at_baseline, 
                                     center = median_age_baseline,    
                                     scale = 10),  
      age_at_baseline_scaled = scale(Age, 
                          center = median_age,    
                          scale = 10),  
      
      BMI_scale = scale(BMI, 
                        center = median_BMI,    
                        scale = 1),  
      
      # scale by 100
      sumtot1_scaled = scale(sumtot1,
                             center = median_sumtot1,
                             scale = 1000),

        time_years = scale(time_since_baseline,
                           center = median_time,
                           scale = 1),
        
        # Dairy: scale by 100g
        dairy_100g_lag = scale(dairy_total_gday_cumavg_lag,
                           center = median_dairy,
                           scale = 100),
        
        gait_speed_log = log(gait_speed),
        gait_speed_sqrt = sqrt(gait_speed),
        
        
        # Ensure categorical variables are unordered factors
        dairy_quartile_baseline_lag = factor(dairy_quartile_baseline_lag, 
                                         levels = c("Q1", "Q2", "Q3", "Q4"), 
                                         ordered = FALSE) |> 
          relevel(ref = "Q1"),
        BMI_category_lag = factor(BMI_category_lag, 
                              levels = c("Underweight", "Normal", "Overweight", "Obese"), 
                              ordered = FALSE) |> 
            relevel(ref = "Normal"),
        
        education_level_lag = factor(education_level_lag, 
                                 levels = c("Low (ISCED 0-2)", "Medium (ISCED 3-4)", "High (ISCED 5-8)"), 
                                 ordered = FALSE) |> 
            relevel(ref = "Low (ISCED 0-2)"),
        
        smoking_status_lag = factor(smoking_status_lag, 
                                levels = c("Never", "Former", "Current"), 
                                ordered = FALSE) |> 
            relevel(ref = "Never"),
        
        pa_levels_tertile_f1_lag = factor(pa_levels_tertile_f1_lag, 
                                      levels = c("Low", "Medium", "High"), 
                                      ordered = FALSE) |> 
            relevel(ref = "Low"),
        diabetes_status_lag = factor(diabetes_status_lag, 
                                 levels = c("No diabetes", "Diabetes"), 
                                 ordered = FALSE) |> 
            relevel(ref = "No diabetes",
                    
                    pt = factor(pt, ordered = FALSE)
            )
    )





# ------------------------------------------------------------------------------
# 1. FIT BOTH MODELS
# ------------------------------------------------------------------------------


# Model 1: WITHOUT random slope (only random intercept)
mod_no_slope <- lmerTest::lmer(
  gait_speed ~ dairy_100g_lag + 
    age_at_baseline_scaled + 
    BMI_category_lag + education_level_lag + 
    smoking_status_lag + pa_levels_tertile_f1_lag + 
    diabetes_status_lag + time_since_baseline + (1 | pt),
    data = df_stans_scaled, 
    REML = FALSE,
    control = lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 20000)  # Increase iterations
    )
)

# ------------------------------------------------------------------------------
# Plot model assumption inspectation 
# ------------------------------------------------------------------------------



plot_diagnostics(mod_no_slope, "Gait Speed LMM")



# ------------------------------------------------------------------------------
# 3. MODEL SUMMARY (Compare fit statistics)
# ------------------------------------------------------------------------------


summary(mod_no_slope)
cor(df_stans_scaled$gait_speed, fitted(mod_no_slope), use = "complete.obs")^2



# Get residuals from each time point separately
T3_data <- df_stans_scaled %>% filter(time_point == "T3")
T4_data <- df_stans_scaled %>% filter(time_point == "T4")


# Fit simple models at each time point
lm_T3 <- lm(gait_speed ~ dairy_100g_lag + 
              age_at_baseline_scaled + 
              BMI_category_lag + education_level_lag + 
              smoking_status_lag + pa_levels_tertile_f1_lag + 
              diabetes_status_lag + time_since_baseline , data = T3_data)

lm_T4 <- lm(gait_speed ~ dairy_100g_lag + 
              age_at_baseline_scaled + 
              BMI_category_lag + education_level_lag + 
              smoking_status_lag + pa_levels_tertile_f1_lag + 
              diabetes_status_lag + time_since_baseline , data = T4_data)


summary(lm_T3)

comparison_plots <- function(model, time_label) {
  data.frame(
    fitted = fitted(model),
    resid = residuals(model),
    time = time_label
  )
}

all_data <- rbind(
  comparison_plots(lm_T3, "T3"),
  comparison_plots(lm_T4, "T4")
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
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  facet_wrap(~ time, scales = "free") +
  labs(title = "Residual Patterns: Cross-Sectional vs Longitudinal",
       subtitle = "Pattern only appears in longitudinal model",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

slope_T4 <- coef(lm(residuals(lm_T4) ~ fitted(lm_T4)))[2]
slope_T3 <- coef(lm(residuals(lm_T3) ~ fitted(lm_T3)))[2]
slope_long <- coef(lm(residuals(mod_with_slope) ~ fitted(mod_with_slope)))[2]

# Create summary table
slope_table <- data.frame(
  Model = c("T1", "T2", "T3", "Longitudinal"),
  Slope = round(c(slope_T1, slope_T2, slope_T3, slope_long), 4)
)

print(slope_table)
# ------------------------------------------------------------------------------
# 4. VISUAL COMPARISON OF RANDOM EFFECTS
# ------------------------------------------------------------------------------


length(unique(imp1$pt))
# Model WITHOUT random slope - shows parallel lines
visualize(mod_no_slope, formula = gait_speed ~ time_since_baseline + pt | dairy_quartile_baseline_lag, sample = 827, plot = "model")




# Extract random effects variances
VarCorr(mod_no_slope)


# ------------------------------------------------------------------------------
# 6.RESIDUALS
# ------------------------------------------------------------------------------


resid_without <- residuals(mod_no_slope)

var(resid_without)


# Residuals vs fitted for model without slope
plot(fitted(mod_no_slope), resid_without, 
     main = "Without Random Slope",
     xlab = "Fitted values", 
     ylab = "Residuals")
abline(h = 0, col = "red")





# ------------------------------------------------------------------------------
# 1. IDENTIFY OUTLIERS USING VARIOUS METHODS
# ------------------------------------------------------------------------------

# Method 1: Standardized residuals > |3|
df_stans_scaled$std_resid <- rstudent(mod_no_slope)  # Studentized residuals
outliers_std <- df_stans_scaled[abs(df_stans_scaled$std_resid) > 3, ]
cat("Number of outliers (|residual| > 3):", nrow(outliers_std), "\n")
print(outliers_std[, c("pt", "gait_speed", "std_resid")])

# Method 2: Cook's Distance (influential points)
df_stans_scaled$cooks_d <- cooks.distance(mod_no_slope)
influential <- df_stans_scaled[df_stans_scaled$cooks_d > (4/nrow(df_stans_scaled)), ]
cat("Number of influential points:", nrow(influential), "\n")
print(influential[, c("pt", "gait_speed", "cooks_d")])

# Method 3: Leverage
df_stans_scaled$leverage <- hatvalues(mod_no_slope)
high_leverage <- df_stans_scaled[df_stans_scaled$leverage > (2*length(fixef(mod_no_slope))/nrow(df_stans_scaled)), ]
cat("Number of high leverage points:", nrow(high_leverage), "\n")


# ------------------------------------------------------------------------------
# 2. VISUALIZE OUTLIERS
# ------------------------------------------------------------------------------

# Plot 1: Residuals vs Fitted with outlier labels
plot(mod_no_slope, id.n = 5)  # Labels top 5 outliers
# Click on points to see IDs

# Plot 2: Q-Q plot with outlier identification
qqPlot(mod_no_slope)

# Plot 3: Cook's distance plot
plot(df_stans_scaled$cooks_d, type = "h", 
     xlab = "Index", ylab = "Cook's Distance",
     main = "Cook's Distance")
abline(h = 4/nrow(df_stans_scaled), col = "red", lty = 2)
text(which(df_stans_scaled$cooks_d > 4/nrow(df_stans_scaled)), 
     df_stans_scaled$cooks_d[df_stans_scaled$cooks_d > 4/nrow(df_stans_scaled)],
     labels = df_stans_scaled$pt[df_stans_scaled$cooks_d > 4/nrow(df_stans_scaled)],
     pos = 4, cex = 0.8)

# Plot 4: Leverage vs Residuals
plot(df_stans_scaled$leverage, df_stans_scaled$std_resid,
     xlab = "Leverage", ylab = "Studentized Residuals",
     main = "Leverage vs Residuals")
abline(h = c(-3, 3), col = "red", lty = 2)
abline(v = 2*length(fixef(mod_no_slope))/nrow(df_stans_scaled), col = "blue", lty = 2)



# ------------------------------------------------------------------------------
# 3. INVESTIGATE EXTREME OUTLIERS
# ------------------------------------------------------------------------------

# Get the most extreme outliers
extreme_outliers <- df_stans_scaled[
    order(abs(df_stans_scaled$std_resid), decreasing = TRUE), 
    c("pt", "gait_speed", "age_at_baseline_scaled", "dairy_100g_lag", "BMI_category_lag", 
      "time_since_baseline", "std_resid", "cooks_d")
]
head(extreme_outliers, 10)

# Check if these are real values or data entry errors
summary(df_stans_scaled$gait_speed)
boxplot(df_stans_scaled$gait_speed, main = "Gait Speed Distribution")
points(1, df_stans_scaled$gait_speed[which.min(df_stans_scaled$std_resid)], 
       col = "red", pch = 19, cex = 2)


# Robust check: Refit without outliers to see if results change
df_clean <- df_stans_scaled[abs(df_stans_scaled$std_resid) <= 3, ]
model_clean <- update(mod_no_slope, data = df_clean)

# Compare coefficients
compare_coefs <- data.frame(
    Predictor = names(fixef(mod_no_slope)),
    With_Outliers = fixef(mod_no_slope),
    Without_Outliers = fixef(model_clean),
    Difference = fixef(mod_no_slope) - fixef(model_clean)
)
print(compare_coefs)

# If differences are small (< 10%), keep outliers

