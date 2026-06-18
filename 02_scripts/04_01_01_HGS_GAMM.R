library(mgcv)
library(gratia)   # tidy GAM output + appraise()
library(ggplot2)
library(patchwork)
library(dplyr)

# install.packages(c("mgcv", "gratia", "patchwork"))  # if needed

imp1 <- mice::complete(mice_analysis$mids$HGS_MAX, action = 1)

imp1 <- imp1 |>
    dplyr::group_by(pt) |>
    dplyr::filter(!any(dairy_total_gday_cumavg > 1000, na.rm = TRUE)) |>
    dplyr::ungroup()

df <- imp1

# ------------------------------------------------------------------------------
# DATA PREP  (same scaling logic as LMM script)
# ------------------------------------------------------------------------------

median_age      <- median(df$age_at_baseline,            na.rm = TRUE)
median_sumtot1  <- median(df$sumtot1,                    na.rm = TRUE)
median_dairy    <- median(df$dairy_total_gday_cumavg,    na.rm = TRUE)
median_fermented <- median(df$dairy_fermented_gday_cumavg, na.rm = TRUE)
median_time     <- median(df$time_since_baseline,        na.rm = TRUE)

df_scaled <- df |>
    dplyr::mutate(
        age_decades       = as.numeric(scale(age_at_baseline,
                                             center = median_age, scale = 10)),
        sumtot1_hundreds  = as.numeric(scale(sumtot1,
                                             center = median_sumtot1, scale = 100)),
        time_years        = as.numeric(scale(time_since_baseline,
                                             center = median_time, scale = 1)),
        dairy_100g        = as.numeric(scale(dairy_total_gday_cumavg,
                                             center = median_dairy, scale = 100)),
        fermented_100g    = as.numeric(scale(dairy_fermented_gday_cumavg,
                                             center = median_fermented, scale = 100)),

        HGS_MAX_log  = log(HGS_MAX),
        HGS_MAX_sqrt = sqrt(HGS_MAX),

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
            relevel(ref = "No diabetes"),

        pt = factor(pt, ordered = FALSE)
    )


# ------------------------------------------------------------------------------
# 1.  FIT MODELS
# ------------------------------------------------------------------------------
# mgcv::bam() is preferred over gam() for large repeated-measures data.
# s(pt, bs = "re") = random intercept per subject (equivalent to (1|pt))
# s(time_since_baseline, ...) = non-linear smooth for time trajectory
# dairy_100g kept linear here; swap for s(dairy_100g) to test non-linearity

mod_gamm4 <- gamm4::gamm4(
    HGS_MAX ~ dairy_100g +
        s(time_since_baseline, bs = "tp", k = 5) +
        age_decades + BMI_category + education_level +
        smoking_status + pa_levels_tertile_f1 +
        diabetes_status + sumtot1_hundreds,
    random = ~(1 | pt),
    data = df_scaled
)

summary(mod_gamm4$gam)
summary(mod_gamm4$mer)

library(ggplot2)
ggplot(df_scaled, aes(x = time_since_baseline, y = HGS_MAX)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess") +
    facet_wrap(~pt, scales = "free")




# Model A: non-linear time trajectory, linear dairy, random intercept
mod_gamm_ri <- mgcv::bam(
    HGS_MAX ~ dairy_100g +
        s(time_since_baseline, bs = "tp", k = 5) +
        age_decades + BMI_category + education_level +
        smoking_status + pa_levels_tertile_f1 +
        diabetes_status + sumtot1_hundreds +
        s(pt, bs = "re"),
    data    = df_scaled,
    method  = "fREML",
    discrete = TRUE
)



# Model B: non-linear time + non-linear dairy smooth
mod_gamm_smooth_dairy <- mgcv::bam(
    HGS_MAX ~ s(dairy_100g, bs = "tp", k = 5) +
        s(time_since_baseline, bs = "tp", k = 5) +
        age_decades + BMI_category + education_level +
        smoking_status + pa_levels_tertile_f1 +
        diabetes_status + sumtot1_hundreds +
        s(pt, bs = "re"),
    data    = df_scaled,
    method  = "fREML",
    discrete = TRUE
)

# Model C: tensor product interaction of dairy × time (tests if dairy effect
#          changes non-linearly over follow-up)
mod_gamm_ti <- mgcv::bam(
    HGS_MAX ~ s(dairy_100g, bs = "tp", k = 5) +
        s(time_since_baseline, bs = "tp", k = 5) +
        ti(dairy_100g, time_since_baseline, bs = "tp", k = c(5, 5)) +
        age_decades + BMI_category + education_level +
        smoking_status + pa_levels_tertile_f1 +
        diabetes_status + sumtot1_hundreds +
        s(pt, bs = "re"),
    data    = df_scaled,
    method  = "fREML",
    discrete = TRUE
)


# ------------------------------------------------------------------------------
# 2.  MODEL SUMMARIES
# ------------------------------------------------------------------------------

summary(mod_gamm_ri)
summary(mod_gamm_smooth_dairy)
summary(mod_gamm_ti)

# AIC comparison (lower = better)
AIC(mod_gamm_ri, mod_gamm_smooth_dairy, mod_gamm_ti)

# Likelihood ratio test for nested models (A vs B, A vs C)
anova(mod_gamm_ri, mod_gamm_smooth_dairy, test = "Chisq")
anova(mod_gamm_smooth_dairy, mod_gamm_ti, test = "Chisq")


# ------------------------------------------------------------------------------
# 3.  ASSUMPTION DIAGNOSTICS  (styled to match LMM script)
# ------------------------------------------------------------------------------

palette <- c("#E76254FF", "#EF8A47FF", "#F7AA58FF", "#FFD06FFF", "#FFE6B7FF",
             "#AADCE0FF", "#72BCD5FF", "#528FADFF", "#376795FF", "#1E466EFF")

plot_diagnostics_gam <- function(model, model_name = "GAMM") {

    resid_vals     <- residuals(model, type = "deviance")
    fitted_vals    <- fitted(model)
    sqrt_abs_resid <- sqrt(abs(resid_vals))

    df_diag <- data.frame(
        fitted         = fitted_vals,
        residuals      = resid_vals,
        sqrt_abs_resid = sqrt_abs_resid
    )

    # A. Residuals vs Fitted
    p_rd <- ggplot(df_diag, aes(x = fitted, y = residuals)) +
        geom_point(color = palette[9], alpha = 0.4, size = 1.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = palette[1]) +
        geom_smooth(method = "loess", se = TRUE,
                    color = palette[1], fill = palette[4], alpha = 0.2) +
        labs(x = "Fitted values", y = "Deviance residuals") +
        theme_minimal(base_size = 12) +
        theme(panel.grid = element_blank(),
              axis.line  = element_line(color = "black", linewidth = 0.5),
              axis.ticks = element_line(color = "black"),
              axis.text  = element_text(color = "black"),
              axis.title = element_text(color = "black")) +
        annotate("text", x = -Inf, y = Inf, label = "A",
                 hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold")

    # B. Scale-Location
    p_sl <- ggplot(df_diag, aes(x = fitted, y = sqrt_abs_resid)) +
        geom_point(color = palette[9], alpha = 0.4, size = 1.5) +
        geom_smooth(method = "loess", se = TRUE,
                    color = palette[1], fill = palette[3], alpha = 0.2) +
        labs(x = "Fitted values", y = expression(sqrt("|Residuals|"))) +
        theme_minimal(base_size = 12) +
        theme(panel.grid = element_blank(),
              axis.line  = element_line(color = "black", linewidth = 0.5),
              axis.ticks = element_line(color = "black"),
              axis.text  = element_text(color = "black"),
              axis.title = element_text(color = "black")) +
        annotate("text", x = -Inf, y = Inf, label = "B",
                 hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold")

    # C. Q-Q plot
    p_qq <- ggplot(df_diag, aes(sample = residuals)) +
        stat_qq(color = palette[9], alpha = 0.4, size = 1.5) +
        stat_qq_line(color = palette[1], linewidth = 0.8) +
        labs(x = "Theoretical quantiles", y = "Sample quantiles") +
        theme_minimal(base_size = 12) +
        theme(panel.grid = element_blank(),
              axis.line  = element_line(color = "black", linewidth = 0.5),
              axis.ticks = element_line(color = "black"),
              axis.text  = element_text(color = "black"),
              axis.title = element_text(color = "black")) +
        annotate("text", x = -Inf, y = Inf, label = "C",
                 hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold")

    p_rd | p_sl | p_qq
}

plot_diagnostics_gam(mod_gamm_ri,           "GAMM — random intercept")
plot_diagnostics_gam(mod_gamm_smooth_dairy,  "GAMM — smooth dairy")
plot_diagnostics_gam(mod_gamm_ti,            "GAMM — tensor interaction")

# gratia::appraise() gives a richer 4-panel check (includes random effects QQ)
gratia::appraise(mod_gamm_ri,          method = "simulate")
gratia::appraise(mod_gamm_smooth_dairy, method = "simulate")


# ------------------------------------------------------------------------------
# 4.  SMOOTH TERM VISUALISATION
# ------------------------------------------------------------------------------

# Time trajectory smooth
gratia::draw(mod_gamm_ri, select = "s(time_since_baseline)",
             residuals = TRUE, rug = FALSE) &
    theme_minimal(base_size = 12)

# Dairy smooth (Model B)
gratia::draw(mod_gamm_smooth_dairy, select = "s(dairy_100g)",
             residuals = TRUE, rug = TRUE) &
    labs(x = "Dairy intake (per 100 g, centred)", y = "Partial effect on HGS") &
    theme_minimal(base_size = 12)

# Tensor interaction surface (Model C) — colour = effect of dairy × time
gratia::draw(mod_gamm_ti, select = "ti(dairy_100g,time_since_baseline)") &
    theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# 5.  OUTLIER / INFLUENTIAL OBSERVATION DETECTION
# ------------------------------------------------------------------------------
# Note: mgcv models do not support rstudent() or cooks.distance() directly.
# We use deviance residuals and leverage (hat values) instead.

df_scaled$resid_dev  <- residuals(mod_gamm_ri, type = "deviance")
df_scaled$fitted_val <- fitted(mod_gamm_ri)
df_scaled$leverage   <- influence(mod_gamm_ri)$hat   # requires mgcv >= 1.8

# --- Standardise residuals manually (deviance / sqrt(1 - hat)) ---
df_scaled$std_resid <- df_scaled$resid_dev /
    sqrt(1 - pmin(df_scaled$leverage, 0.999))  # pmin guards against hat = 1

# Outliers: |std_resid| > 3
outliers_std <- df_scaled |>
    dplyr::filter(abs(std_resid) > 3)
cat("Outliers (|std resid| > 3):", nrow(outliers_std), "\n")
print(dplyr::select(outliers_std, pt, HGS_MAX, sumtot1, std_resid))

# High leverage
p_lev  <- 2 * length(coef(mod_gamm_ri)) / nrow(df_scaled)
high_lev <- df_scaled |> dplyr::filter(leverage > p_lev)
cat("High-leverage points:", nrow(high_lev), "\n")


# ------------------------------------------------------------------------------
# 6.  OUTLIER PLOTS
# ------------------------------------------------------------------------------

# Cook's-style influence: resid² × leverage / (1 - leverage)
df_scaled$cooks_approx <- (df_scaled$std_resid^2 * df_scaled$leverage) /
    (1 - df_scaled$leverage)

# Plot: leverage vs standardised residuals
ggplot(df_scaled, aes(x = leverage, y = std_resid)) +
    geom_point(color = palette[8], alpha = 0.5, size = 1.5) +
    geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = palette[1]) +
    geom_vline(xintercept = p_lev, linetype = "dashed", color = palette[3]) +
    ggrepel::geom_text_repel(
        data = dplyr::filter(df_scaled, abs(std_resid) > 3 | leverage > p_lev),
        aes(label = pt), size = 3, max.overlaps = 20
    ) +
    labs(x = "Leverage (hat)", y = "Standardised deviance residual",
         title = "Leverage vs residuals — GAMM") +
    theme_minimal(base_size = 12)

# Cook's-approximate plot
ggplot(df_scaled, aes(x = seq_len(nrow(df_scaled)), y = cooks_approx)) +
    geom_segment(aes(xend = seq_len(nrow(df_scaled)), yend = 0),
                 color = palette[8], alpha = 0.5) +
    geom_hline(yintercept = 4 / nrow(df_scaled),
               linetype = "dashed", color = palette[1]) +
    labs(x = "Index", y = "Approximate Cook's distance",
         title = "Influence — GAMM") +
    theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# 7.  SENSITIVITY: REFIT WITHOUT EXTREME OUTLIERS
# ------------------------------------------------------------------------------

df_clean <- df_scaled |> dplyr::filter(abs(std_resid) <= 3)

mod_gamm_clean <- mgcv::bam(
    HGS_MAX ~ dairy_100g +
        s(time_since_baseline, bs = "tp", k = 5) +
        age_decades + BMI_category + education_level +
        smoking_status + pa_levels_tertile_f1 +
        diabetes_status + sumtot1_hundreds +
        s(pt, bs = "re"),
    data     = df_clean,
    method   = "fREML",
    discrete = TRUE
)

# Compare parametric coefficients
compare_coefs <- data.frame(
    Predictor        = names(coef(mod_gamm_ri)),
    With_Outliers    = coef(mod_gamm_ri),
    Without_Outliers = coef(mod_gamm_clean),
    Difference_pct   = 100 * (coef(mod_gamm_ri) - coef(mod_gamm_clean)) /
        abs(coef(mod_gamm_ri) + 1e-10)
)
print(compare_coefs)
# If |Difference_pct| < 10% for dairy_100g, outliers are not influential
