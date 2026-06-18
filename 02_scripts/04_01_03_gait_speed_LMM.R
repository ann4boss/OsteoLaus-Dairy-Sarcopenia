library(lmerTest)
library(flexplot)
library(ggplot2)
library(splines)

imp1 <- mice::complete(mice_analysis$mids$gait_speed, action = 1)
df <- imp1



# Calculate medians from your data
median_age <- median(df$age_at_baseline, na.rm = TRUE)
median_dairy_lag <- median(df$dairy_total_gday_cumavg_lag, na.rm = TRUE)
median_time <- median(df$time_since_baseline, na.rm = TRUE)

df_stans_scaled <- df %>%
    mutate(
        # scale by 10 years
        age_decades = scale(age_at_baseline, 
                            center = median_age,    
                            scale = 10),    # Per 10 years
        
        
        
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
  gait_speed ~ dairy_quartile_baseline_lag + 
    age_decades + 
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
# 3. MODEL SUMMARY (Compare fit statistics)
# ------------------------------------------------------------------------------


summary(mod_no_slope)


# ------------------------------------------------------------------------------
# 4. VISUAL COMPARISON OF RANDOM EFFECTS
# ------------------------------------------------------------------------------


length(unique(imp1$pt))
# Model WITHOUT random slope - shows parallel lines
visualize(mod_no_slope, formula = gait_speed ~ time_since_baseline + pt | dairy_quartile_baseline_lag, sample = 827, plot = "model")



# ------------------------------------------------------------------------------
# 5. COMPARE RANDOM EFFECTS VARIANCES
# ------------------------------------------------------------------------------

# Extract random effects variances
VarCorr(mod_with_slope)
VarCorr(mod_no_slope)

# Check if random slope variance is significant using bootstrap (optional)
# This is more robust than the LRT for boundary effects
if(require(lmerTest)) {
    # Use Kenward-Roger approximation for p-value
    summary(mod_with_slope)
    summary(mod_no_slope)
}

# ------------------------------------------------------------------------------
# 6. COMPARE RESIDUALS
# ------------------------------------------------------------------------------

# Get residuals from both models
resid_with <- residuals(mod_with_slope)
resid_without <- residuals(mod_no_slope)

# Compare residual variances
var(resid_with)
var(resid_without)

# Plot residuals comparison
par(mfrow = c(2, 2))

# Residuals vs fitted for model with slope
plot(fitted(mod_with_slope), resid_with, 
     main = "With Random Slope",
     xlab = "Fitted values", 
     ylab = "Residuals")
abline(h = 0, col = "red")

# Residuals vs fitted for model without slope
plot(fitted(mod_no_slope), resid_without, 
     main = "Without Random Slope",
     xlab = "Fitted values", 
     ylab = "Residuals")
abline(h = 0, col = "red")

# Q-Q plots
qqnorm(resid_with, main = "With Random Slope - Q-Q Plot")
qqline(resid_with, col = "red")

qqnorm(resid_without, main = "Without Random Slope - Q-Q Plot")
qqline(resid_without, col = "red")

# Reset plotting
par(mfrow = c(1, 1))








# ------------------------------------------------------------------------------
# 1. IDENTIFY OUTLIERS USING VARIOUS METHODS
# ------------------------------------------------------------------------------

# Method 1: Standardized residuals > |3|
df_stans_scaled$std_resid <- rstudent(mod_no_slope)  # Studentized residuals
outliers_std <- df_stans_scaled[abs(df_stans_scaled$std_resid) > 3, ]
cat("Number of outliers (|residual| > 3):", nrow(outliers_std), "\n")
print(outliers_std[, c("pt", "HGS_MAX", "std_resid")])

# Method 2: Cook's Distance (influential points)
df_stans_scaled$cooks_d <- cooks.distance(mod_no_slope)
influential <- df_stans_scaled[df_stans_scaled$cooks_d > (4/nrow(df_stans_scaled)), ]
cat("Number of influential points:", nrow(influential), "\n")
print(influential[, c("pt", "HGS_MAX", "cooks_d")])

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
abline(v = 2*length(fixef(mod_with_slope))/nrow(df_stans_scaled), col = "blue", lty = 2)



# ------------------------------------------------------------------------------
# 3. INVESTIGATE EXTREME OUTLIERS
# ------------------------------------------------------------------------------

# Get the most extreme outliers
extreme_outliers <- df_stans_scaled[
    order(abs(df_stans_scaled$std_resid), decreasing = TRUE), 
    c("pt", "gait_speed", "age_decades", "dairy_100g_lag", "BMI_category_lag", 
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

# ------------------------------------------------------------------------------
# Plot model assumption inspectation 
# ------------------------------------------------------------------------------



plot_diagnostics(mod_no_slope, "Gait Speed LMM")
