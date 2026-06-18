library(lmerTest)
library(flexplot)
library(ggplot2)
library(splines)
library(dplyr)

imp1 <- mice::complete(mice_analysis$mids$HGS_MAX, action = 1)

imp1 <- imp1 |>
  dplyr::group_by(pt) |>  # or whatever your patient ID column is
  dplyr::filter(!any(dairy_total_gday_cumavg > 750, na.rm = TRUE)) |>
  dplyr::ungroup()


df <- imp1

# Calculate medians from your data
median_age <- median(df$Age, na.rm = TRUE)
median_age_baseline <- median(df$age_at_baseline, na.rm = TRUE)
median_sumtot1 <- median(df$sumtot1, na.rm = TRUE)
median_dairy <- median(df$dairy_total_gday_cumavg, na.rm = TRUE)
median_fermented <- median(df$dairy_fermented_gday_cumavg, na.rm = TRUE)
median_time <- median(df$time_since_baseline, na.rm = TRUE)
median_BMI <- median(df$BMI, na.rm = TRUE)

df_stans_scaled <- df %>%
    mutate(
        # scale by 10 years
        age_baseline = scale(age_at_baseline, 
                            center = median_age_baseline,    
                            scale = 10),  
        age_decades = scale(Age, 
                            center = median_age,    
                            scale = 10),  
        
        BMI_scale = scale(BMI, 
                            center = median_BMI,    
                            scale = 1),  
        
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
        # Dairy: scale by 100g
        fermented_100g = scale(dairy_fermented_gday_cumavg,
                           center = median_fermented,
                           scale = 100),
        
        HGS_MAX_log = log(HGS_MAX),
        HGS_MAX_sqrt = sqrt(HGS_MAX),
        sumtot_spline = ns(sumtot1_hundreds, 2),
        dairy_100g_spline =ns(dairy_100g, 3), 
        time_since_baseline_spline =  ns(time_since_baseline, df = 2),
        BMI_spline = ns(BMI_scale, 3),
        age_decades_spline = ns(age_decades, 3),
        
        # Ensure categorical variables are unordered factors
        dairy_quartile_baseline = factor(dairy_quartile_baseline, 
                                         levels = c("Q1", "Q2", "Q3", "Q4"), 
                                         ordered = FALSE) |> 
            relevel(ref = "Q1"),
        
        dairy_guidelines_port= factor(dairy_quartile_baseline, 
                                         levels = c("< 2 servings/day", "≥ 2 servings/day"), 
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
            relevel(ref = "No diabetes",
        
        pt = factor(pt, ordered = FALSE)
    )
    )
    




# ------------------------------------------------------------------------------
# 1. FIT BOTH MODELS
# ------------------------------------------------------------------------------

# Model 1: WITH random slope (your full model)
mod_with_slope <- lmerTest::lmer(
  HGS_MAX ~ 
    dairy_100g  + age_decades +
    BMI_category + education_level + smoking_status +
    pa_levels_tertile_f1 + diabetes_status + sumtot1_hundreds +
    time_since_baseline  + (1 + time_since_baseline | pt),
  data = df_stans_scaled, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)

mod_no_slope <- lmerTest::lmer(
  HGS_MAX ~ 
    dairy_100g  + age_decades +
    BMI_category + education_level + smoking_status +
    pa_levels_tertile_f1 + diabetes_status + sumtot1_hundreds +
    time_since_baseline  + (1 | pt),
  data = df_stans_scaled, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)


mod_with_slope_spline_both <- lmerTest::lmer(
  HGS_MAX ~ dairy_100g  + age_decades_spline +
    BMI_spline + education_level + smoking_status +
    pa_levels_tertile_f1 + diabetes_status + sumtot1_hundreds +
    time_since_baseline  + (1 + time_since_baseline | pt),
  data = df_stans_scaled, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)

mod_with_slope_spline_BMI <- lmerTest::lmer(
  HGS_MAX ~ dairy_100g  + age_decades +
    BMI_spline + education_level + smoking_status +
    pa_levels_tertile_f1 + diabetes_status + sumtot1_hundreds +
    time_since_baseline  + (1 + time_since_baseline | pt),
  data = df_stans_scaled, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)


mod_with_slope_spline_age <- lmerTest::lmer(
  HGS_MAX ~ dairy_100g  + age_decades_spline +
    BMI_category + education_level + smoking_status +
    pa_levels_tertile_f1 + diabetes_status + sumtot1_hundreds +
    time_since_baseline  + (1 + time_since_baseline | pt),
  data = df_stans_scaled, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)

mod_with_slope_spline_time <- lmerTest::lmer(
  HGS_MAX ~ dairy_100g  + age_decades +
    BMI_category + education_level + smoking_status +
    pa_levels_tertile_f1 + diabetes_status + sumtot1_hundreds +
    time_since_baseline_spline  + (1 + time_since_baseline | pt),
  data = df_stans_scaled, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)

mod_with_slope_spline_time_age <- lmerTest::lmer(
  HGS_MAX ~ dairy_100g  + age_decades_spline +
    BMI_category + education_level + smoking_status +
    pa_levels_tertile_f1 + diabetes_status + sumtot1_hundreds +
    time_since_baseline_spline  + (1 + time_since_baseline | pt),
  data = df_stans_scaled, 
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)


model_poly <- lmerTest::lmer(HGS_MAX ~ 
                     ns(dairy_100g, df = 3) + 
                     ns(age_decades, df = 4) + 
                     poly(time_since_baseline, 2) +
                    age_decades * time_since_baseline +
                     BMI_category + education_level + 
                     smoking_status + pa_levels_tertile_f1 + 
                     diabetes_status + sumtot1_hundreds + 
                     (1 + time_since_baseline | pt),
                     data = df_stans_scaled, 
                     REML = FALSE,
                     control = lmerControl(
                       optimizer = "bobyqa",
                       optCtrl = list(maxfun = 20000)
                       )
)

model_simple <- lmer(HGS_MAX ~ dairy_100g + (1 + time_since_baseline | pt), data = df_stans_scaled)

plot_diagnostics(mod_with_slope, "HGS LMM - Simple")

plot_diagnostics(mod_with_slope_spline_both, "HGS LMM - Spline Both")
plot_diagnostics(mod_with_slope_spline_age, "HGS LMM - Spline only for age at baseline")
plot_diagnostics(mod_with_slope_spline_BMI, "HGS LMM- Spline BMI")
plot_diagnostics(mod_with_slope_spline_time, "HGS LMM - Spline only for time sine baseline")
plot_diagnostics(mod_with_slope_spline_time_age, "HGS LMM - Spline for time sine baseline and age at baseline")



summary(imp1$HGS_MAX
      )

boxplot(imp1$HGS_MAX)
df_plot <- data.frame(HGS_MAX = imp1$HGS_MAX)

ggplot(df_plot, aes(x = "", y = HGS_MAX)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "red", size = 1.5) +
  labs(title = "HGS_MAX Distribution",
       y = "HGS_MAX",
       x = "") +
  theme_minimal()


ggplot(df_stans_scaled, aes(x = dairy_total_gday_cumavg, y = HGS_MAX)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ BMI_category) +
  theme_minimal()

ggplot(df_stans_scaled, aes(x = dairy_total_gday_cumavg, y = HGS_MAX)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ diabetes_status) +
  theme_minimal()

df_stans_scaled$age_group <- cut(df_stans_scaled$age_decades, breaks = c(0, 4, 6, 8, 10))
ggplot(df_stans_scaled, aes(x = dairy_total_gday_cumavg, y = HGS_MAX)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ age_group) +
  theme_minimal()

ggplot(df_stans_scaled, aes(x = dairy_total_gday_cumavg, y = HGS_MAX)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ pa_levels_tertile_f1) +
  theme_minimal()

summary(df_stans_scaled$dairy_total_gday_cumavg)
hist(df_stans_scaled$dairy_total_gday_cumavg, breaks = 50, main = "Distribution of Dairy Intake")
quantile(df$dairy_total_gday_cumavg, probs = c(0.90, 0.95, 0.99, 1))

n_total <- nrow(df_stans_scaled)
n_high_dairy <- sum(df_stans_scaled$dairy_total_gday_cumavg >= 500)
pct_high_dairy <- n_high_dairy / n_total * 100

cat("Total N:", n_total, "\n")
cat("N with ≥500g dairy:", n_high_dairy, "\n")
cat("Percentage:", round(pct_high_dairy, 2), "%\n")

# Look at the distribution of the high dairy group
high_dairy <- df_stans_scaled[df_stans_scaled$dairy_total_gday_cumavg >= 500, ]
summary(high_dairy$dairy_total_gday_cumavg)
hist(high_dairy$dairy_total_gday_cumavg, breaks = 20)

# How many are EXTREMELY high (>800g)?
n_extreme <- sum(df_stans_scaled$dairy_total_gday_cumavg >= 800)
cat("N with ≥800g dairy:", n_extreme, "\n")

df_stans_scaled$dairy_group2 <- ifelse(df_stans_scaled$dairy_total_gday_cumavg >= 500, "High (≥500g)", "Normal (<500g)")

comparison_high <- df_stans_scaled %>%
  group_by(dairy_group2) %>%
  summarise(
    n = n(),
    mean_HGS = mean(HGS_MAX, na.rm = TRUE),
    mean_age = mean(age_decades, na.rm = TRUE),
    mean_BMI = mean(as.numeric(BMI_category), na.rm = TRUE),
    pct_educated = mean(education_level %in% c("High", "University"), na.rm = TRUE),
    pct_diabetes = mean(diabetes_status == "Yes", na.rm = TRUE),
    pct_smoker = mean(smoking_status == "Current", na.rm = TRUE),
    pct_active = mean(pa_levels_tertile_f1 == "High", na.rm = TRUE),
    mean_time = mean(time_since_baseline, na.rm = TRUE)
  )

print(comparison_high)

t.test(HGS_MAX ~ dairy_group2, data = df_stans_scaled)
t.test(age_decades ~ dairy_group2, data = df_stans_scaled)
chisq.test(table(df_stans_scaled$dairy_group2, df_stans_scaled$diabetes_status))
chisq.test(table(df_stans_scaled$dairy_group2, df_stans_scaled$education_level))


range(fitted(mod_with_slope))
hist(fitted(mod_with_slope), breaks = 30)

# Where is the curve happening?
plot(fitted(mod_with_slope), residuals(mod_with_slope))
abline(h = 0, col = "red")
lines(lowess(fitted(mod_with_slope), residuals(mod_with_slope)), col = "blue", lwd = 2)

# Add vertical lines at key points
abline(v = quantile(fitted(mod_with_slope), c(0.25, 0.5, 0.75)), lty = 2, col = "gray")

plot(df_stans_scaled$alcohol_category_conso, residuals(mod_with_slope))
abline(h = 0, col = "red")
lines(lowess(df_stans_scaled$alcohol_category_conso, residuals(mod_with_slope)), col = "blue", lwd = 2)


plot(df_stans_scaled$time_since_baseline, residuals(mod_with_slope))
abline(h = 0, col = "red")
lines(lowess(df_stans_scaled$time_since_baseline, residuals(mod_with_slope)), col = "blue", lwd = 2)


ranef_data <- as.data.frame(ranef(mod_with_slope)$pt)
colnames(ranef_data) <- c("Intercept", "Slope")


plot(ranef_data$Intercept, ranef_data$Slope, 
     xlab = "Random Intercept", ylab = "Random Slope",
     main = "Are the random effects correlated?")
abline(v = 0, h = 0, col = "red")

summary(mod_with_slope)


plot(df_stans_scaled$HGS_MAX, fitted(mod_no_slope), 
     xlab = "Observed HGS", ylab = "Fitted HGS",
     main = "Observed vs Fitted")
abline(0, 1, col = "red")

cor(df_stans_scaled$HGS_MAX, fitted(mod_no_slope), use = "complete.obs")^2


library(lme4)

model_gamma <- glmer(HGS_MAX ~ 
                       ns(dairy_100g, df = 3) + 
                       ns(age_decades, df = 4) + 
                       time_since_baseline + 
                       I(time_since_baseline^2) +
                       BMI_category + education_level + 
                       smoking_status + pa_levels_tertile_f1 + 
                       diabetes_status + sumtot1_hundreds + 
                       (1 + time_since_baseline | pt),
                     data = df_stans_scaled,
                     family = Gamma(link = "log"))



library(clubSandwich)
coef_test(mod_with_slope, vcov = "CR2")
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
    c("pt", "HGS_MAX", "age_decades", "dairy_100g", "BMI_category", 
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
partial_residual_plot(model_clean, age_decades, df_clean)
partial_residual_plot(model_clean, time_since_baseline, df_clean)
partial_residual_plot(model_clean, sumtot1_hundreds, df_clean)

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









