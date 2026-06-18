library(lmerTest)
library(flexplot)
library(ggplot2)
library(splines)

imp1 <- mice::complete(mice_analysis$mids$ALM_HT2_harmonised, action = 1)
df <- imp1

# Calculate medians from your data
median_age <- median(df$age_at_baseline, na.rm = TRUE)
median_sumtot1 <- median(df$sumtot1, na.rm = TRUE)
median_dairy <- median(df$dairy_total_gday_cumavg, na.rm = TRUE)
median_time <- median(df$time_since_baseline, na.rm = TRUE)

df_stans_scaled <- df %>%
    mutate(
        # scale by 10 years
        age_decades = scale(age_at_baseline, 
                            center = median_age,    
                            scale = 10),    # Per 10 years
        
        # scale by 100
        sumtot1_hundreds = scale(sumtot1,
                                 center = median_sumtot1,
                                 scale = 100),
        
        
        time_years = scale(time_since_baseline,
                           center = median_time,
                           scale = 1),
        
        # Dairy: scale by 100g
        dairy_100g = scale(dairy_total_gday_cumavg,
                           center = median_dairy,
                           scale = 100),
        
        ALM_HT2_log = log(ALM_HT2_harmonised),
        ALM_HT2_sqrt = sqrt(ALM_HT2_harmonised),
        sumtot_spline = ns(sumtot1_hundreds, 2),
        
        # Ensure categorical variables are unordered factors
        dairy_quartile_baseline = factor(dairy_quartile_baseline, 
                                         levels = c("Q1", "Q2", "Q3", "Q4"), 
                                         ordered = FALSE) |> 
            relevel(ref = "Q1"),
        
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
            relevel(ref = "No diabetes",
                    
                    pt = factor(pt, ordered = FALSE)
            )
    )





# ------------------------------------------------------------------------------
# 1. FIT BOTH MODELS
# ------------------------------------------------------------------------------

# Model 1: WITH random slope (your full model)
mod_with_slope <- lmerTest::lmer(
    ALM_HT2_log ~ dairy_quartile_baseline + age_decades +
        BMI_category + education_level + smoking_status +
        pa_levels_tertile_f1 + diabetes_status + sumtot1_hundreds +
      time_since_baseline +  (1 + time_since_baseline | pt),
    data = df_stans_scaled, 
    REML = FALSE,
    control = lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 20000)  # Increase iterations
    )
)

# Model 2: WITHOUT random slope (only random intercept)
mod_no_slope <- lmerTest::lmer(
    ALM_HT2_log ~ dairy_quartile_baseline + age_decades +
        BMI_category + education_level + smoking_status +
        pa_levels_tertile_f1 + diabetes_status + sumtot1_hundreds +
      time_since_baseline + (1 | pt), 
    data = df_stans_scaled, 
    REML = FALSE,
    control = lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 20000)  # Increase iterations
    )
)

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


# ------------------------------------------------------------------------------
# 4. VISUAL COMPARISON OF RANDOM EFFECTS
# ------------------------------------------------------------------------------

# Create side-by-side visualizations
par(mfrow = c(1, 2))

# Model WITH random slope - shows individual trajectories
visualize(mod_with_slope, formula = ALM_HT2_log  ~ time_since_baseline + pt | dairy_quartile_baseline, sample = 1082, plot = "model")

# Model WITHOUT random slope - shows parallel lines
visualize(mod_no_slope, formula = ALM_HT2_log  ~ time_since_baseline + pt | dairy_quartile_baseline, sample = 1082, plot = "model")

# Reset plotting
par(mfrow = c(1, 1))


my_palette <- c("#E76254FF", "#EF8A47FF", "#F7AA58FF", "#FFD06FFF", 
                "#FFE6B7FF", "#AADCE0FF", "#72BCD5FF", "#528FADFF", 
                "#376795FF", "#1E466EFF")

# Create the plot with paper-ready styling
p <- visualize(mod_with_slope, 
               plot = "model",
               formula = ALM_HT2_log ~ dairy_100g + pt, 
               sample = 1082,
               alpha = 0.4,
               method = "lm",
               se = TRUE) +
  # Override the color mapping - use one color from your palette
  geom_point(aes(color = NULL), color = my_palette[8], alpha = 0.4) +
  scale_color_identity() +  # Use colors as-is
  theme_classic(base_size = 14) +
  labs(title = NULL,
       x = "Dairy Intake (100g/day)",
       y = expression(paste("Appendicular Lean Mass (log)", ""))) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none",
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p)




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
df_stans_scaled$std_resid <- rstudent(mod_with_slope)  # Studentized residuals
outliers_std <- df_stans_scaled[abs(df_stans_scaled$std_resid) > 3, ]
cat("Number of outliers (|residual| > 3):", nrow(outliers_std), "\n")
print(outliers_std[, c("pt", "HGS_MAX", "std_resid")])

# Method 2: Cook's Distance (influential points)
df_stans_scaled$cooks_d <- cooks.distance(mod_with_slope)
influential <- df_stans_scaled[df_stans_scaled$cooks_d > (4/nrow(df_stans_scaled)), ]
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
    c("pt", "HGS_MAX", "age_decades", "dairy_100g", "BMI_category", 
      "time_since_baseline", "std_resid", "cooks_d")
]
head(extreme_outliers, 10)

# Check if these are real values or data entry errors
summary(df_stans_scaled$HGS_MAX)
boxplot(df_stans_scaled$HGS_MAX, main = "HGS_MAX Distribution")
points(1, df_stans_scaled$HGS_MAX[which.min(df_stans_scaled$std_resid)], 
       col = "red", pch = 19, cex = 2)


# Robust check: Refit without outliers to see if results change
df_clean <- df_stans_scaled[abs(df_stans_scaled$std_resid) <= 3, ]
model_clean <- update(mod_with_slope, data = df_clean)

# Compare coefficients
compare_coefs <- data.frame(
    Predictor = names(fixef(mod_with_slope)),
    With_Outliers = fixef(mod_with_slope),
    Without_Outliers = fixef(model_clean),
    Difference = fixef(mod_with_slope) - fixef(model_clean)
)
print(compare_coefs)

# If differences are small (< 10%), keep outliers

# ------------------------------------------------------------------------------
# Plot model assumption inspectation 
# ------------------------------------------------------------------------------



library(ggplot2)
library(patchwork)

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
  
  y_max_sl <- max(abs(sqrt_abs_resid), na.rm = TRUE) * 2
  y_max_rd <- max(abs(df$residuals), na.rm = TRUE) * 2
  
  # A. Scale-Location plot
  p_sl <- ggplot(df, aes(x = fitted, y = sqrt_abs_resid)) +
    geom_point(color = palette[9], alpha = 0.6, size = 1.8) +
    geom_smooth(method = "lm", se = TRUE,
                color = palette[1], fill = palette[3], alpha = 0.2) +
    labs(
      x = "Fitted values",
      y = expression(sqrt("|Residuals|"))
    ) +
    theme_minimal(base_size = 12) +
    coord_cartesian(ylim = c(-y_max_sl, y_max_sl)) +
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
    geom_smooth(method = "lm", se = TRUE,
                color = palette[1], fill = palette[4], alpha = 0.2) +
    labs(
      x = "Fitted values",
      y = "Residuals"
    ) +
    coord_cartesian(ylim = c(-y_max_rd, y_max_rd)) +
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
  
  p_rd | p_sl | p_qq
}

# Usage:
plot_diagnostics(mod_with_slope, "ALM_HT2 LMM")
ß