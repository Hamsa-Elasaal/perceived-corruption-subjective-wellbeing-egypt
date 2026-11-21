# ============================================================
#  EGYPT WVS — COMPLETE ANALYSIS SCRIPT
#  Research Question: Effect of Bribery (Q112) & Wasta (Q110)
#  + Control Variables on Life Satisfaction (Q49) & Happiness (Q46)
#
#  ANALYSES INCLUDED:
#  0.  Packages & Data Preparation
#  1.  Descriptive Statistics
#  2.  Path Analysis (lavaan) — all 4 model structures
#  3.  VGAM Separate Models — Happiness & Life Satisfaction
#      3a. Proportional odds assumption test
#  4.  Interaction Effects — does income moderate bribery & wasta?
#  5.  VGAM Joint Model — both outcomes simultaneously
#      5a. Parallel joint model
#      5b. Non-parallel joint model
#      5c. Model comparison (LR test + AIC)
#      5d. Odds ratio tables
#      5e. Coefficient plot
#  6.  Mediation Analysis — life satisfaction as mediator
#  7.  Latent Class Analysis — hidden subgroups
#  8.  Final Summary Table & Export
# ============================================================


# ─────────────────────────────────────────────────────────────
#  0. PACKAGES & DATA PREPARATION-----
#  Purpose: Install and load all required libraries, load the
#           data, recode missing values, standardise age, and
#           convert variables to the correct types for each
#           analysis. This section must run before everything.
# ─────────────────────────────────────────────────────────────

packages <- c("readr", "dplyr", "lavaan", "semPlot",
              "VGAM", "mediation", "poLCA",
              "ggplot2", "tidyr", "scales")

installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed) install.packages(pkg)
}

library(readr);   library(dplyr);   library(lavaan)
library(semPlot); library(VGAM);    library(mediation)
library(poLCA);   library(ggplot2); library(tidyr)
library(scales)

# ── Load raw data ─────────────────────────────────────────────
# !! Change this path to wherever your file is saved !!
df_raw <- read_csv("C:/Users/admin/OneDrive/Desktop/egypt WVS.csv")

# ── Select variables & recode missing ────────────────────────
df <- df_raw %>%
  dplyr::select(
    Q46,       # Outcome 1  : Happiness         (1=Very happy … 4=Not at all happy)
    Q49,       # Outcome 2  : Life Satisfaction  (1=Dissatisfied … 10=Satisfied)
    Q112,      # Main IV 1  : Bribery perception (1=No corruption … 10=Widespread)
    Q110,      # Main IV 2  : Wasta/luck         (1=Hard work … 10=Luck & wasta)
    Q253,      # Control    : Human rights respect   (1=A lot … 4=No respect)
    Q50,       # Control    : Economic satisfaction  (1–10)
    Q56,       # Control    : Living standard vs parents (1=Better…3=Same)
    Q131,      # Control    : Neighbourhood safety (1=Very safe … 4=Unsafe)
    Q260,      # Control    : Gender              (1=Male, 2=Female)
    Q262,      # Control    : Age                 (continuous)
    Q275,      # Control    : Education level     (0–8 ISCED)
    Q288,      # Control    : Income class        (1–10)
    Q279,      # Control    : Employment status   (1=Full-time … 8=Other)
    Q273,      # Control    : Marital status      (1=Married,3=Divorced,
    #                                               4=Separated,5=Widowed,6=Single)
    Q287,      # Control    : Social class        (2=Upper-mid,3=Lower-mid,
    #                                               4=Working,5=Lower)
    H_URBRURAL # Control    : Urban/Rural         (1=Urban, 2=Rural)
  ) %>%
  mutate(across(everything(),
                ~replace(., . %in% c(-1, -2, -3, -4, -5), NA))) %>%
  mutate(age_z = as.numeric(scale(Q262))) %>%
  dplyr::select(-Q262)
missing_data <- data.frame(
  Missing_Count = colSums(is.na(df)),
  Total = nrow(df)
) %>%
  mutate(
    Missing_Percentage = round((Missing_Count / Total) * 100, 2)
  ) %>%
  dplyr::select(Missing_Count, Missing_Percentage) %>%
  arrange(desc(Missing_Percentage))

print(missing_data)
#duplicated
df[duplicated(df), ]

cat("Total rows (before listwise)  :", nrow(df), "\n")
cat("Complete cases (after listwise):", sum(complete.cases(df)), "\n")

# ── Ordered & factor types (used by lavaan and VGAM) ──────────
ordered_vars <- c("Q46","Q49","Q112","Q110","Q253","Q50","Q56","Q131","Q275","Q288")
factor_vars  <- c("Q260","Q273","Q287","Q279","H_URBRURAL")

df[ordered_vars] <- lapply(df[ordered_vars], ordered)
df[factor_vars]  <- lapply(df[factor_vars],  factor)


# ── Helper: dummy-code categorical predictors ─────────────────
# Purpose: Both lavaan and VGAM work best with explicit 0/1 dummies
#          instead of raw factor variables. This function is applied
#          in sections 2, 3, 4, 5, and 6.
add_dummies <- function(data) {
  data %>%
    mutate(
      female         = as.integer(Q260 == 2),        # ref = Male
      rural          = as.integer(H_URBRURAL == 2),  # ref = Urban
      # Marital status: ref = Married (1)
      # Note: category 2 (living together) absent in Egypt data
      mar_divorced   = as.integer(Q273 == 3),
      mar_separated  = as.integer(Q273 == 4),
      mar_widowed    = as.integer(Q273 == 5),
      mar_single     = as.integer(Q273 == 6),
      # Social class: ref = Lower-middle (3)
      class_uppmid   = as.integer(Q287 == 2),
      class_working  = as.integer(Q287 == 4),
      class_lower    = as.integer(Q287 == 5),
      # Employment: ref = Other (8)  ← تغيّر هنا
      emp_fulltime   = as.integer(Q279 == 1),       
      emp_parttime   = as.integer(Q279 == 2),
      emp_selfempl   = as.integer(Q279 == 3),
      emp_retired    = as.integer(Q279 == 4),
      emp_homemaker  = as.integer(Q279 == 5),
      emp_student    = as.integer(Q279 == 6),
      emp_unemployed = as.integer(Q279 == 7)

    ) %>%
    dplyr::select(-Q260, -H_URBRURAL, -Q273, -Q287, -Q279)
}

# ── Control variable string (reused in all model formulas) ────
ctrl_str <- paste(
  "+ Q253 + Q50 + Q56 + Q131 + Q275 + Q288 + age_z",
  "+ female + rural",
  "+ mar_divorced + mar_separated + mar_widowed + mar_single",
  "+ class_uppmid + class_working + class_lower",
  "+ emp_parttime + emp_selfempl + emp_retired",
  "+ emp_homemaker + emp_student + emp_unemployed +emp_fulltime"
)


# ─────────────────────────────────────────────────────────────
#  1. DESCRIPTIVE STATISTICS ----
#  Purpose: Understand distributions of all key variables
#           before modelling. Always the mandatory first step.
# ─────────────────────────────────────────────────────────────

cat("\n========== 1. DESCRIPTIVE STATISTICS ==========\n")

cat("\n--- Happiness (Q46): 1=Very happy … 4=Not at all happy ---\n")
print(table(df$Q46, useNA = "always"))

cat("\n--- Life Satisfaction (Q49): 1=Dissatisfied … 10=Satisfied ---\n")
print(table(df$Q49, useNA = "always"))

cat("\n--- Bribery Perception (Q112): 1=No corruption … 10=Widespread ---\n")
print(table(df$Q112, useNA = "always"))

cat("\n--- Wasta / Luck (Q110): 1=Hard work matters … 10=Luck & wasta ---\n")
print(table(df$Q110, useNA = "always"))

df_numeric <- df %>% mutate(across(all_of(ordered_vars), as.numeric))
cat("\n--- Summary of key variables ---\n")
print(summary(df_numeric[, c("Q46","Q49","Q112","Q110","Q50","Q288","age_z")]))

cat("\n--- Cross-tab: Bribery (Q112) × Happiness (Q46) ---\n")
print(table(df$Q112, df$Q46))

cat("\n--- Cross-tab: Wasta (Q110) × Life Satisfaction (Q49) ---\n")
print(table(df$Q110, df$Q49))

str(df)
# ─────────────────────────────────────────────────────────────
#  2. PATH ANALYSIS — ALL 4 CAUSAL STRUCTURES -----
#  Purpose: Test direct and indirect relationships between all
#           variables simultaneously using Structural Equation
#           Modelling (SEM). We run all 4 possible causal
#           structures and compare fit indices to find which
#           one the data supports best.
#  Estimator: DWLS — the correct estimator for ordinal data
#  Good fit benchmarks: CFI/TLI > 0.95, RMSEA < 0.06, SRMR < 0.08
# ─────────────────────────────────────────────────────────────

cat("\n========== 2. PATH ANALYSIS ==========\n")

df_path <- df %>%
  mutate(across(all_of(ordered_vars), as.numeric)) %>%
  add_dummies() %>%
  na.omit()

cat("Rows used in path analysis:", nrow(df_path), "\n")

ord_lavaan <- c("Q46","Q49","Q112","Q110","Q253","Q50","Q56","Q131","Q275","Q288")

path_ctrl <- paste(
  "Q253 + Q50 + Q56 + Q131 + Q275 + Q288 + age_z +",
  "female + rural +",
  "mar_divorced + mar_separated + mar_widowed + mar_single +",
  "class_uppmid + class_working + class_lower +",
  "emp_parttime + emp_selfempl + emp_retired +",
  "emp_homemaker + emp_student + emp_unemployed + emp_fulltime"
)

# ── Model A: Bribery & Wasta → Life Satisfaction → Happiness ──
# Life Satisfaction is the MEDIATOR
cat("\n--- Model A: Q49 mediates → Q46 ---\n")
spec_A <- paste0("Q49 ~ Q112+Q110+", path_ctrl, "\n",
                 "Q46 ~ Q49+Q112+Q110+", path_ctrl)
fit_A <- sem(spec_A, data=df_path, ordered=ord_lavaan,
             estimator="DWLS", missing="listwise")
summary(fit_A, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
fitMeasures(fit_A, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr"))

# ── Model B: Bribery & Wasta → Happiness → Life Satisfaction ──
# Happiness is the MEDIATOR
cat("\n--- Model B: Q46 mediates → Q49 ---\n")
spec_B <- paste0("Q46 ~ Q112+Q110+", path_ctrl, "\n",
                 "Q49 ~ Q46+Q112+Q110+", path_ctrl)
fit_B <- sem(spec_B, data=df_path, ordered=ord_lavaan,
             estimator="DWLS", missing="listwise")
summary(fit_B, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
fitMeasures(fit_B, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr"))

# ── Model C: Two separate outcomes, no mediation ──────────────
cat("\n--- Model C: Two separate outcomes (no mediation) ---\n")
spec_C <- paste0("Q49 ~ Q112+Q110+", path_ctrl, "\n",
                 "Q46 ~ Q112+Q110+", path_ctrl, "\n",
                 "Q49 ~~ Q46")
fit_C <- sem(spec_C, data=df_path, ordered=ord_lavaan,
             estimator="DWLS", missing="listwise")
summary(fit_C, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
fitMeasures(fit_C, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr"))

# ── Model D: Full joint model with covariances ────────────────
cat("\n--- Model D: Full joint model with covariances ---\n")
spec_D <- paste0("Q49 ~ Q112+Q110+", path_ctrl, "\n",
                 "Q46 ~ Q49+Q112+Q110+", path_ctrl, "\n",
                 "Q112 ~~ Q110\n",
                 "Q112 ~~ Q50+Q288+Q253\n",
                 "Q110 ~~ Q50+Q288+Q253\n",
                 "Q50  ~~ Q288+Q131")
fit_D <- sem(spec_D, data=df_path, ordered=ord_lavaan,
             estimator="DWLS", missing="listwise")
summary(fit_D, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
fitMeasures(fit_D, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr"))

# ── Fit comparison table ──────────────────────────────────────
# Purpose: Pick the model with highest CFI and lowest RMSEA/SRMR
cat("\n--- Path Model Fit Comparison ---\n")
path_comparison <- data.frame(
  Model = c("A: Q49→Q46 (LifeSat mediates)",
            "B: Q46→Q49 (Happiness mediates)",
            "C: Two separate outcomes",
            "D: Full joint with covariances"),
  CFI   = round(c(fitMeasures(fit_A,"cfi.scaled"),
                  fitMeasures(fit_B,"cfi.scaled"),
                  fitMeasures(fit_C,"cfi.scaled"),
                  fitMeasures(fit_D,"cfi.scaled")), 3),
  RMSEA = round(c(fitMeasures(fit_A,"rmsea.scaled"),
                  fitMeasures(fit_B,"rmsea.scaled"),
                  fitMeasures(fit_C,"rmsea.scaled"),
                  fitMeasures(fit_D,"rmsea.scaled")), 3),
  SRMR  = round(c(fitMeasures(fit_A,"srmr"),
                  fitMeasures(fit_B,"srmr"),
                  fitMeasures(fit_C,"srmr"),
                  fitMeasures(fit_D,"srmr")), 3)
)
print(path_comparison)

# ── Path diagram (Model A — typically the best) ───────────────
semPaths(fit_A, what="std", whatLabels="std", layout="tree2",
         rotation=2, edge.label.cex=0.7, sizeMan=7,
         color=list(man="lightblue"), title=TRUE)
title("Path Model A: Bribery & Wasta → Life Satisfaction → Happiness",
      cex.main=0.85)


# ─────────────────────────────────────────────────────────────
#  3. VGAM — SEPARATE PROPORTIONAL ODDS MODELS -----
#  Purpose: Model each ordinal outcome individually using the
#           proportional odds (cumulative logit) model. This
#           respects the ordered nature of both Q46 and Q49
#           and gives odds ratios for every predictor.
#
#  How to read coefficients:
#    Q46 (Happiness) : positive OR > 1 = MORE likely to be LESS happy
#    Q49 (Life Sat.) : positive OR > 1 = MORE likely to be MORE satisfied
# ─────────────────────────────────────────────────────────────

cat("\n========== 3. VGAM SEPARATE MODELS ==========\n")

df_vgam <- df %>%
  mutate(across(all_of(ordered_vars), as.numeric)) %>%
  add_dummies() %>%
  na.omit()

cat("Rows used in VGAM:", nrow(df_vgam), "\n")

vgam_formula_hap <- as.formula(paste(
  "Q46 ~ Q112 + Q110", ctrl_str))

vgam_formula_ls <- as.formula(paste(
  "Q49 ~ Q112 + Q110", ctrl_str))

# ── Model 1: Happiness (Q46) ──────────────────────────────────
cat("\n--- VGAM Model 1: Happiness (Q46) ---\n")
vgam_happiness <- vglm(vgam_formula_hap,
                       family = cumulative(parallel=TRUE, link="logitlink"),
                       data   = df_vgam)
summary(vgam_happiness)

# Odds Ratios with 95% CI
cat("\n--- Odds Ratios: Happiness (Q46) ---\n")
OR_hap <- data.frame(
  OR       = round(exp(coef(vgam_happiness)), 3),
  CI_lower = round(exp(confint(vgam_happiness))[,1], 3),
  CI_upper = round(exp(confint(vgam_happiness))[,2], 3)
)
print(OR_hap)

# ── Model 2: Life Satisfaction (Q49) ─────────────────────────
cat("\n--- VGAM Model 2: Life Satisfaction (Q49) ---\n")
vgam_lifeSat <- vglm(vgam_formula_ls,
                     family = cumulative(parallel=TRUE, link="logitlink"),
                     data   = df_vgam)
summary(vgam_lifeSat)

# Odds Ratios with 95% CI
cat("\n--- Odds Ratios: Life Satisfaction (Q49) ---\n")
OR_ls <- data.frame(
  OR       = round(exp(coef(vgam_lifeSat)), 3),
  CI_lower = round(exp(confint(vgam_lifeSat))[,1], 3),
  CI_upper = round(exp(confint(vgam_lifeSat))[,2], 3)
)
print(OR_ls)


# ─────────────────────────────────────────────────────────────
#  3a. PROPORTIONAL ODDS ASSUMPTION TEST
#  Purpose: Verify that the parallel lines assumption holds —
#           i.e., each predictor has the same effect across all
#           thresholds of the outcome. We only relax this for
#           our two main IVs (bribery & wasta) to keep the model
#           numerically stable (full non-parallel causes errors
#           with only 4 categories in Q46).
#  Decision rule:
#    p > 0.05 → keep parallel model (assumption holds)
#    p < 0.05 → switch to partial non-parallel model
# ─────────────────────────────────────────────────────────────

cat("\n========== 3a. PROPORTIONAL ODDS ASSUMPTION TEST ==========\n")

# ── Test for Happiness (Q46) ──────────────────────────────────
cat("\n--- Test for Happiness (Q46) ---\n")
vgam_hap_np <- vglm(vgam_formula_hap,
                    family = cumulative(parallel = FALSE ~ Q112 + Q110,
                                        link = "logitlink"),
                    data = df_vgam)

lr_hap   <- 2 * (as.numeric(logLik(vgam_hap_np)) -
                   as.numeric(logLik(vgam_happiness)))
df_hap   <- df.residual(vgam_happiness) - df.residual(vgam_hap_np)
p_hap    <- pchisq(lr_hap, df=df_hap, lower.tail=FALSE)

cat("LR statistic:", round(lr_hap,3), "| df:", df_hap,
    "| p-value:", round(p_hap,4), "\n")
cat("AIC parallel:", round(AIC(vgam_happiness),2),
    "| AIC non-parallel:", round(AIC(vgam_hap_np),2), "\n")
cat(ifelse(p_hap > 0.05,
           "→ p > 0.05: PARALLEL model retained for Q46 ✓\n",
           "→ p < 0.05: Use NON-PARALLEL model for Q46\n"))

# ── Test for Life Satisfaction (Q49) ─────────────────────────
cat("\n--- Test for Life Satisfaction (Q49) ---\n")
vgam_ls_np <- vglm(vgam_formula_ls,
                   family = cumulative(parallel = FALSE ~ Q112 + Q110,
                                       link = "logitlink"),
                   data = df_vgam)

lr_ls    <- 2 * (as.numeric(logLik(vgam_ls_np)) -
                   as.numeric(logLik(vgam_lifeSat)))
df_ls    <- df.residual(vgam_lifeSat) - df.residual(vgam_ls_np)
p_ls     <- pchisq(lr_ls, df=df_ls, lower.tail=FALSE)

cat("LR statistic:", round(lr_ls,3), "| df:", df_ls,
    "| p-value:", round(p_ls,4), "\n")
cat("AIC parallel:", round(AIC(vgam_lifeSat),2),
    "| AIC non-parallel:", round(AIC(vgam_ls_np),2), "\n")
cat(ifelse(p_ls > 0.05,
           "→ p > 0.05: PARALLEL model retained for Q49 ✓\n",
           "→ p < 0.05: Use NON-PARALLEL model for Q49\n"))


# ─────────────────────────────────────────────────────────────
#  4. INTERACTION EFFECTS
#  Purpose: Test whether the effect of bribery and wasta on
#           happiness and life satisfaction DIFFERS by income
#           level. This is called "moderation."
#           If the interaction term Q112:Q288 is significant,
#           it means bribery hurts wellbeing MORE (or less) for
#           people with lower income — a key finding for your
#           research question about inequality and corruption.
# ─────────────────────────────────────────────────────────────

cat("\n========== 4. INTERACTION EFFECTS ==========\n")

# ── 4a. Bribery × Income → Happiness ─────────────────────────
cat("\n--- 4a. Bribery × Income → Happiness (Q46) ---\n")
vgam_int_bri_hap <- vglm(
  as.formula(paste("Q46 ~ Q112*Q288 + Q110", ctrl_str)),
  family = cumulative(parallel=TRUE, link="logitlink"),
  data   = df_vgam)
summary(vgam_int_bri_hap)
# Key: look at Q112:Q288 — significant = income moderates bribery effect

# ── 4b. Wasta × Income → Happiness ───────────────────────────
cat("\n--- 4b. Wasta × Income → Happiness (Q46) ---\n")
vgam_int_was_hap <- vglm(
  as.formula(paste("Q46 ~ Q110*Q288 + Q112", ctrl_str)),
  family = cumulative(parallel=TRUE, link="logitlink"),
  data   = df_vgam)
summary(vgam_int_was_hap)

# ── 4c. Bribery × Income → Life Satisfaction ─────────────────
cat("\n--- 4c. Bribery × Income → Life Satisfaction (Q49) ---\n")
vgam_int_bri_ls <- vglm(
  as.formula(paste("Q49 ~ Q112*Q288 + Q110", ctrl_str)),
  family = cumulative(parallel=TRUE, link="logitlink"),
  data   = df_vgam)
summary(vgam_int_bri_ls)

# ── 4d. Wasta × Income → Life Satisfaction ───────────────────
cat("\n--- 4d. Wasta × Income → Life Satisfaction (Q49) ---\n")
vgam_int_was_ls <- vglm(
  as.formula(paste("Q49 ~ Q110*Q288 + Q112", ctrl_str)),
  family = cumulative(parallel=TRUE, link="logitlink"),
  data   = df_vgam)
summary(vgam_int_was_ls)

# ── 4e. Visualise: Bribery × Income → Happiness ──────────────
# Purpose: Show predicted P(Very Happy) across bribery levels
#          separately for low / middle / high income groups
cat("\n--- Visualising Bribery × Income → Happiness ---\n")

pred_grid <- expand.grid(
  Q112 = 1:10, Q288 = c(2,5,9),
  Q110=median(df_vgam$Q110,na.rm=T), Q253=median(df_vgam$Q253,na.rm=T),
  Q50=median(df_vgam$Q50,na.rm=T),   Q56=median(df_vgam$Q56,na.rm=T),
  Q131=median(df_vgam$Q131,na.rm=T), Q275=median(df_vgam$Q275,na.rm=T),
  age_z=0, female=0, rural=0,
  mar_divorced=0, mar_separated=0, mar_widowed=0, mar_single=0,
  class_uppmid=0, class_working=0, class_lower=0,
  emp_parttime=0, emp_selfempl=0, emp_retired=0,
  emp_homemaker=0, emp_student=0, emp_unemployed=0, emp_other=0
)

pred_probs <- predictvglm(vgam_int_bri_hap, newdata=pred_grid, type="response")
pred_grid$prob_very_happy <- pred_probs[,1]
pred_grid$Income <- factor(pred_grid$Q288, levels=c(2,5,9),
                           labels=c("Low income","Middle income","High income"))

ggplot(pred_grid, aes(x=Q112, y=prob_very_happy, color=Income, group=Income)) +
  geom_line(linewidth=1.2) + geom_point(size=2) +
  scale_x_continuous(breaks=1:10) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  labs(title    = "Interaction: Bribery × Income → P(Very Happy)",
       subtitle = "Does corruption perception hurt happiness more for low-income Egyptians?",
       x="Bribery Perception (1=No corruption, 10=Widespread)",
       y="Predicted Probability of Being Very Happy",
       color="Income Level") +
  theme_minimal(base_size=13) + theme(legend.position="bottom")

ggsave("interaction_bribery_income_happiness.png", width=8, height=5, dpi=150)
cat("Saved: interaction_bribery_income_happiness.png\n")


# ─────────────────────────────────────────────────────────────
#  5. VGAM JOINT MODEL — BOTH OUTCOMES SIMULTANEOUSLY
#  Purpose: Instead of two separate models, the joint model
#           fits Happiness AND Life Satisfaction at the same
#           time. This is better because:
#           (1) accounts for correlation between the two outcomes
#           (2) gives more efficient parameter estimates
#           (3) lets you directly compare bribery's effect on
#               happiness vs life satisfaction in one model
#           (4) each predictor gets one coefficient per outcome
#
#  The data is prepared separately here using readable variable
#  names (happiness, life_satisfaction, bribery, wasta, etc.)
#  to make the joint model output easier to read and report.
# ─────────────────────────────────────────────────────────────

cat("\n========== 5. VGAM JOINT MODEL ==========\n")

# ── Prepare data with readable names for joint model ─────────
df_joint <- df_raw %>%
  dplyr::select(Q46,Q49,Q112,Q110,Q253,Q50,Q56,Q131,
                Q260,Q262,Q275,Q288,Q279,Q273,Q287,H_URBRURAL) %>%
  mutate(across(everything(),
                ~replace(., . %in% c(-1,-2,-3,-4,-5), NA))) %>%
  rename(happiness=Q46, life_satisfaction=Q49,
         bribery=Q112,  wasta=Q110,
         human_rights=Q253, economic_sat=Q50,
         living_standard=Q56, safety=Q131,
         gender=Q260, age=Q262, education=Q275,
         income=Q288, employment=Q279,
         marital=Q273, social_class=Q287,
         urban_rural=H_URBRURAL) %>%
  mutate(age_z = as.numeric(scale(age))) %>%
  dplyr::select(-age) %>%
  mutate(
    female        = as.integer(gender == 2),
    rural         = as.integer(urban_rural == 2),
    mar_divorced  = as.integer(marital == 3),
    mar_separated = as.integer(marital == 4),
    mar_widowed   = as.integer(marital == 5),
    mar_single    = as.integer(marital == 6),
    class_uppmid  = as.integer(social_class == 2),
    class_working = as.integer(social_class == 4),
    class_lower   = as.integer(social_class == 5),
    emp_fulltime  = as.integer(employment == 1),
    emp_parttime  = as.integer(employment == 2),
    emp_selfempl  = as.integer(employment == 3),
    emp_retired   = as.integer(employment == 4),
    emp_homemaker = as.integer(employment == 5),
    emp_student   = as.integer(employment == 6),
    emp_unemployed= as.integer(employment == 7)
  ) %>%
  dplyr::select(-gender,-urban_rural,-marital,-social_class,-employment) %>%
  na.omit()

cat("Rows used in joint model:", nrow(df_joint), "\n")

# Joint model control string (uses readable names)
joint_ctrl <- paste(
  "+ human_rights + economic_sat + living_standard + safety",
  "+ age_z + female + rural + education + income",
  "+ mar_divorced + mar_separated + mar_widowed + mar_single",
  "+ class_uppmid + class_working + class_lower",
  "+ emp_parttime + emp_selfempl + emp_retired",
  "+ emp_homemaker + emp_student + emp_unemployed + emp_fulltime"
)

# ── 5a. Parallel joint model ──────────────────────────────────
# Purpose: One shared set of coefficients for both outcomes.
#          Simpler and more parsimonious.
cat("\n--- 5a. Joint Model — Parallel (shared coefficients) ---\n")

joint_parallel <- vglm(
  as.formula(paste("cbind(happiness, life_satisfaction) ~ bribery + wasta", joint_ctrl)),
  family = cumulative(parallel=TRUE, link="logitlink"),
  data   = df_joint
)
summary(joint_parallel)
# Output shows TWO sets of intercept thresholds (3 for happiness, 9 for life_sat)
# but ONE shared slope for each predictor

# ── 5b. Non-parallel joint model ─────────────────────────────
# Purpose: Each predictor gets TWO coefficients:
#          predictor:1 = effect on Happiness
#          predictor:2 = effect on Life Satisfaction
#          This directly answers whether bribery affects
#          happiness differently than life satisfaction
cat("\n--- 5b. Joint Model — Non-Parallel (outcome-specific coefficients) ---\n")

joint_nonparallel <- vglm(
  as.formula(paste("cbind(happiness, life_satisfaction) ~ bribery + wasta", joint_ctrl)),
  family = cumulative(parallel=FALSE, link="logitlink"),
  data   = df_joint
)
summary(joint_nonparallel)
# Look for bribery:1 vs bribery:2 and wasta:1 vs wasta:2
# to compare effects across the two outcomes

# ── 5c. Model comparison: which joint model is better? ────────
# Purpose: LR test + AIC to decide whether the non-parallel
#          (more complex) joint model is justified
cat("\n--- 5c. Joint Model Comparison: Parallel vs Non-Parallel ---\n")

lr_joint <- 2 * (as.numeric(logLik(joint_nonparallel)) -
                   as.numeric(logLik(joint_parallel)))
df_joint_lr <- df.residual(joint_parallel) - df.residual(joint_nonparallel)
p_joint  <- pchisq(lr_joint, df=df_joint_lr, lower.tail=FALSE)

cat("LR test statistic :", round(lr_joint, 3), "\n")
cat("Degrees of freedom:", df_joint_lr, "\n")
cat("p-value           :", round(p_joint, 4), "\n")
cat("AIC parallel      :", round(AIC(joint_parallel), 2), "\n")
cat("AIC non-parallel  :", round(AIC(joint_nonparallel), 2), "\n")
cat(ifelse(p_joint > 0.05,
           "→ p > 0.05: PARALLEL joint model is sufficient ✓\n",
           "→ p < 0.05: NON-PARALLEL joint model is preferred\n"))

# ── 5d. Odds Ratio tables ─────────────────────────────────────
# Purpose: Export clean OR tables with CIs and significance
#          stars for both joint models — ready for your thesis

make_OR_table <- function(model) {
  coefs <- coef(summary(model))
  pred_rows <- !grepl("Intercept", rownames(coefs))
  data.frame(
    Predictor = rownames(coefs)[pred_rows],
    Estimate  = round(coefs[pred_rows,"Estimate"], 4),
    SE        = round(coefs[pred_rows,"Std. Error"], 4),
    z         = round(coefs[pred_rows,"z value"], 3),
    p_value   = round(coefs[pred_rows,"Pr(>|z|)"], 4),
    OR        = round(exp(coefs[pred_rows,"Estimate"]), 4),
    OR_lower  = round(exp(coefs[pred_rows,"Estimate"] -
                            1.96*coefs[pred_rows,"Std. Error"]), 4),
    OR_upper  = round(exp(coefs[pred_rows,"Estimate"] +
                            1.96*coefs[pred_rows,"Std. Error"]), 4),
    Sig = ifelse(coefs[pred_rows,"Pr(>|z|)"]<0.001,"***",
                 ifelse(coefs[pred_rows,"Pr(>|z|)"]<0.01, "**",
                        ifelse(coefs[pred_rows,"Pr(>|z|)"]<0.05, "*",
                               ifelse(coefs[pred_rows,"Pr(>|z|)"]<0.10, ".", ""))))
  )
}

cat("\n--- OR Table: Parallel Joint Model ---\n")
OR_parallel <- make_OR_table(joint_parallel)
print(OR_parallel)

cat("\n--- OR Table: Non-Parallel Joint Model ---\n")
cat("(:1 = effect on Happiness | :2 = effect on Life Satisfaction)\n\n")
OR_nonparallel <- make_OR_table(joint_nonparallel)
print(OR_nonparallel)

# ── 5e. Coefficient plot: Bribery & Wasta effects ────────────
# Purpose: Visual comparison of bribery and wasta effects on
#          both outcomes — the key figure for your thesis
cat("\n--- 5e. Coefficient plot ---\n")

key_vars  <- c("bribery:1","bribery:2","wasta:1","wasta:2")
plot_data <- OR_nonparallel %>%
  filter(Predictor %in% key_vars) %>%
  mutate(
    Variable = ifelse(grepl("bribery", Predictor), "Bribery (Q112)", "Wasta (Q110)"),
    Outcome  = ifelse(grepl(":1", Predictor),
                      "Happiness (Q46)", "Life Satisfaction (Q49)")
  )

ggplot(plot_data, aes(x=OR, y=Variable, color=Outcome, shape=Outcome)) +
  geom_point(size=4, position=position_dodge(width=0.5)) +
  geom_errorbarh(aes(xmin=OR_lower, xmax=OR_upper),
                 height=0.2, position=position_dodge(width=0.5)) +
  geom_vline(xintercept=1, linetype="dashed", color="gray40") +
  scale_color_manual(values=c("Happiness (Q46)"="steelblue",
                              "Life Satisfaction (Q49)"="tomato")) +
  labs(title    = "Effect of Bribery & Wasta on Happiness vs Life Satisfaction",
       subtitle = "Odds Ratios with 95% CI | Non-Parallel Joint VGAM Model",
       x="Odds Ratio  (OR > 1 = higher odds of worse outcome)",
       y="", color="Outcome", shape="Outcome",
       caption="Egypt WVS Wave 7 (2018)") +
  theme_minimal(base_size=13) + theme(legend.position="bottom")

ggsave("joint_model_OR_plot.png", width=8, height=5, dpi=150)
cat("Saved: joint_model_OR_plot.png\n")


# ─────────────────────────────────────────────────────────────
#  6. MEDIATION ANALYSIS
#  Purpose: Test whether Life Satisfaction (Q49) MEDIATES the
#           relationship between Bribery/Wasta and Happiness.
#           Does bribery reduce happiness BECAUSE it first
#           reduces life satisfaction?
#
#  Decomposes the total effect into:
#    ACME (indirect) : Bribery → Q49 → Happiness
#    ADE  (direct)   : Bribery → Happiness (bypassing Q49)
#    Total           : ACME + ADE
#    Prop. Mediated  : % of total going through life satisfaction
#
#  Method: Bootstrap (1000 samples) for robust CIs
# ─────────────────────────────────────────────────────────────

cat("\n========== 6. MEDIATION ANALYSIS ==========\n")

df_med <- df_vgam %>%
  mutate(Q46_num=as.numeric(Q46), Q49_num=as.numeric(Q49),
         Q112_num=as.numeric(Q112), Q110_num=as.numeric(Q110))

med_ctrl <- paste("Q253+Q50+Q56+Q131+Q275+Q288+age_z+female+rural+",
                  "mar_divorced+mar_separated+mar_widowed+mar_single+",
                  "class_uppmid+class_working+class_lower+",
                  "emp_parttime+emp_selfempl+emp_retired+",
                  "emp_homemaker+emp_student+emp_unemployed+emp_fulltime")

# ── 6a. Bribery → Life Satisfaction → Happiness ──────────────
cat("\n--- 6a. Bribery (Q112) → Q49 → Happiness (Q46) ---\n")

med_m_bri <- lm(as.formula(paste("Q49_num ~ Q112_num +", med_ctrl)), data=df_med)
out_m_bri <- lm(as.formula(paste("Q46_num ~ Q49_num + Q112_num +", med_ctrl)), data=df_med)

set.seed(42)
med_bribery <- mediate(model.m=med_m_bri, model.y=out_m_bri,
                       treat="Q112_num", mediator="Q49_num",
                       boot=TRUE, sims=1000)
summary(med_bribery)
plot(med_bribery, main="Mediation: Bribery → Life Satisfaction → Happiness")

# ── 6b. Wasta → Life Satisfaction → Happiness ────────────────
cat("\n--- 6b. Wasta (Q110) → Q49 → Happiness (Q46) ---\n")

med_m_was <- lm(as.formula(paste("Q49_num ~ Q110_num +", med_ctrl)), data=df_med)
out_m_was <- lm(as.formula(paste("Q46_num ~ Q49_num + Q110_num +", med_ctrl)), data=df_med)

set.seed(42)
med_wasta <- mediate(model.m=med_m_was, model.y=out_m_was,
                     treat="Q110_num", mediator="Q49_num",
                     boot=TRUE, sims=1000)
summary(med_wasta)
plot(med_wasta, main="Mediation: Wasta → Life Satisfaction → Happiness")


# ─────────────────────────────────────────────────────────────
#  7. LATENT CLASS ANALYSIS (LCA)
#  Purpose: Discover hidden subgroups of respondents who share
#           similar patterns across bribery, wasta, happiness,
#           life satisfaction, income, and safety. Unlike cluster
#           analysis, LCA is model-based and gives each person
#           a probability of belonging to each class.
#           We test 2–5 classes and select using BIC.
# ─────────────────────────────────────────────────────────────

cat("\n========== 7. LATENT CLASS ANALYSIS ==========\n")

df_lca <- df %>%
  mutate(across(all_of(ordered_vars), as.numeric)) %>%
  dplyr::select(Q46,Q49,Q112,Q110,Q50,Q131,Q288) %>%
  na.omit() %>%
  # Collapse 10-point scales into 3 categories to reduce sparsity
  # and help LCA converge to stable solutions
  mutate(
    Q49_3  = case_when(Q49<=3~1L, Q49<=6~2L, TRUE~3L),  # Low/Med/High life sat
    Q50_3  = case_when(Q50<=3~1L, Q50<=6~2L, TRUE~3L),  # Low/Med/High econ sat
    Q112_3 = case_when(Q112<=3~1L,Q112<=6~2L,TRUE~3L),  # Low/Med/High bribery
    Q110_3 = case_when(Q110<=3~1L,Q110<=6~2L,TRUE~3L),  # Hard work/Mid/Wasta
    Q288_3 = case_when(Q288<=3~1L,Q288<=6~2L,TRUE~3L),  # Low/Med/High income
    Q46_r  = as.integer(Q46),   # Keep 1-4 as is
    Q131_r = as.integer(Q131)   # Keep 1-4 as is
  ) %>%
  dplyr::select(Q46_r,Q49_3,Q112_3,Q110_3,Q50_3,Q288_3,Q131_r) %>%
  na.omit()

cat("Rows used in LCA:", nrow(df_lca), "\n")

f_lca <- cbind(Q46_r,Q49_3,Q112_3,Q110_3,Q50_3,Q288_3,Q131_r) ~ 1

# Fit 2 to 5 classes and record BIC for each
lca_results <- list()
lca_bic     <- numeric(4)

set.seed(42)
for (k in 2:5) {
  cat("\nFitting LCA with", k, "classes...\n")
  lca_results[[k]] <- poLCA(f_lca, data=df_lca, nclass=k,
                            maxiter=1000, nrep=5, verbose=FALSE)
  lca_bic[k-1] <- lca_results[[k]]$bic
  cat("  BIC:", round(lca_bic[k-1],1), "\n")
}

best_k   <- which.min(lca_bic) + 1
best_lca <- lca_results[[best_k]]

cat("\nOptimal number of classes:", best_k,
    "| BIC values (2–5):", round(lca_bic,1), "\n")
print(best_lca)

# BIC selection plot
bic_df <- data.frame(Classes=2:5, BIC=lca_bic)
ggplot(bic_df, aes(x=Classes, y=BIC)) +
  geom_line(color="steelblue", linewidth=1.2) +
  geom_point(color="steelblue", size=3) +
  geom_point(data=bic_df[bic_df$Classes==best_k,],
             color="red", size=5, shape=18) +
  annotate("text", x=best_k+0.15, y=bic_df$BIC[best_k-1],
           label=paste("Best:", best_k, "classes"),
           color="red", hjust=0) +
  labs(title="LCA Model Selection: BIC by Number of Classes",
       subtitle="Lower BIC = better model  |  Red diamond = selected model",
       x="Number of Latent Classes", y="BIC") +
  theme_minimal(base_size=13)
ggsave("LCA_BIC_plot.png", width=7, height=5, dpi=150)

# Class profiles
df_lca$predicted_class <- best_lca$predclass
class_profile <- df_lca %>%
  group_by(predicted_class) %>%
  summarise(n=n(),
            Happiness  = round(mean(Q46_r), 2),
            LifeSat    = round(mean(Q49_3), 2),
            Bribery    = round(mean(Q112_3),2),
            Wasta      = round(mean(Q110_3),2),
            EconSat    = round(mean(Q50_3), 2),
            Income     = round(mean(Q288_3),2),
            Safety     = round(mean(Q131_r),2))
cat("\n--- Class profiles ---\n")
print(class_profile)

# Profile plot
profile_long <- class_profile %>%
  pivot_longer(-c(predicted_class,n), names_to="Variable", values_to="Mean") %>%
  mutate(Class=paste0("Class ",predicted_class," (n=",n,")"))

ggplot(profile_long, aes(x=Variable, y=Mean, group=Class, color=Class)) +
  geom_line(linewidth=1.1) + geom_point(size=3) + coord_flip() +
  labs(title="Latent Class Profiles",
       subtitle="Mean scores on key variables per class",
       x="", y="Mean score", color="Latent Class") +
  theme_minimal(base_size=12) + theme(legend.position="bottom")
ggsave("LCA_class_profiles.png", width=9, height=6, dpi=150)
cat("Saved: LCA_BIC_plot.png & LCA_class_profiles.png\n")


# ─────────────────────────────────────────────────────────────
#  8. FINAL SUMMARY TABLE & EXPORT
#  Purpose: Extract key predictor results (bribery, wasta,
#           income, safety, economic satisfaction) from the
#           separate VGAM models and export to CSV for your
#           thesis tables.
# ─────────────────────────────────────────────────────────────

cat("\n========== 8. FINAL SUMMARY TABLE ==========\n")

key_preds <- c("Q112","Q110","Q50","Q288","Q131","Q253",
               "Q56","Q275","age_z","female","rural")

extract_key <- function(model, outcome_name) {
  coefs <- summary(model)@coef3
  mask  <- rownames(coefs) %in% key_preds
  data.frame(
    Outcome   = outcome_name,
    Predictor = rownames(coefs)[mask],
    Estimate  = round(coefs[mask,"Estimate"], 3),
    SE        = round(coefs[mask,"Std. Error"], 3),
    z         = round(coefs[mask,"z value"], 3),
    p_value   = round(coefs[mask,"Pr(>|z|)"], 4),
    OR        = round(exp(coefs[mask,"Estimate"]), 3),
    Sig = ifelse(coefs[mask,"Pr(>|z|)"]<0.001,"***",
                 ifelse(coefs[mask,"Pr(>|z|)"]<0.01, "**",
                        ifelse(coefs[mask,"Pr(>|z|)"]<0.05, "*",
                               ifelse(coefs[mask,"Pr(>|z|)"]<0.10, ".", ""))))
  )
}

summary_table <- rbind(
  extract_key(vgam_happiness, "Happiness (Q46)"),
  extract_key(vgam_lifeSat,   "Life Satisfaction (Q49)")
)
print(summary_table)

# Export all tables
write.csv(summary_table,    "VGAM_separate_key_results.csv",   row.names=FALSE)
write.csv(OR_parallel,      "joint_model_parallel_OR.csv",     row.names=FALSE)
write.csv(OR_nonparallel,   "joint_model_nonparallel_OR.csv",  row.names=FALSE)

cat("\n✓ All analyses complete. Files saved:\n")
cat("  - VGAM_separate_key_results.csv\n")
cat("  - joint_model_parallel_OR.csv\n")
cat("  - joint_model_nonparallel_OR.csv\n")
cat("  - interaction_bribery_income_happiness.png\n")
cat("  - joint_model_OR_plot.png\n")
cat("  - LCA_BIC_plot.png\n")
cat("  - LCA_class_profiles.png\n")

#___________________________________________#

# ─────────────────────────────────────────────────────────────
#  3b. ASSUMPTIONS CHECK — SEPARATE MODELS-----
#  Purpose: Verify all key assumptions for ordinal logistic
#           regression for both Happiness and Life Satisfaction
# ─────────────────────────────────────────────────────────────

cat("\n========== 3b. ASSUMPTIONS CHECK — SEPARATE MODELS ==========\n")

library(MASS)
library(car)

# ── polr versions (for crPlot & VIF — car doesn't work with vglm) ──
df_vgam <- df %>%
  mutate(across(all_of(ordered_vars), as.numeric)) %>%
  add_dummies() %>%
  na.omit()

polr_hap <- polr(
  as.formula(paste("factor(Q46) ~ Q112 + Q110", ctrl_str)),
  data = df_vgam, Hess = TRUE
)

polr_ls <- polr(
  as.formula(paste("factor(Q49) ~ Q112 + Q110", ctrl_str)),
  data = df_vgam, Hess = TRUE
)

cont_vars <- c("Q112", "Q110", "Q253", "Q50", "Q56",
               "Q131", "Q275", "Q288", "age_z")

# ── ASSUMPTION 1: Linearity (Component + Residual Plots) ──────----
# ─────────────────────────────────────────────────────────────
#  ASSUMPTION 1: BOX-TIDWELL — Linearity of log-odds
#  Purpose: Test whether each continuous predictor has a linear
#           relationship with the log-odds of the outcome.
#           Method: add x*log(x) interaction term for each cont var.
#           If p > 0.05 → linearity holds ✓
#           If p < 0.05 → non-linearity detected ✗
# ─────────────────────────────────────────────────────────────

cat("\n--- Assumption 1: BOX-TIDWELL — Linearity ---\n")

cont_vars <- c("Q112", "Q110", "Q253", "Q50", "Q56",
               "Q131", "Q275", "Q288", "age_z")

run_boxtidwell <- function(polr_model, df, outcome_var, cont_vars, model_name) {
  
  cat("\n======", model_name, "======\n")
  
  # نحتاج القيم positive لـ log(x) — age_z ممكن يكون سالب
  # الحل: نشيل age_z من الـ Box-Tidwell ونتحقق منه بشكل منفصل
  bt_vars <- cont_vars[cont_vars != "age_z"]
  
  # نفلتر الصفوف اللي فيها قيم > 0 لكل الـ cont vars
  df_bt <- df %>%
    filter(if_all(all_of(bt_vars), ~ . > 0)) %>%
    as.data.frame()
  
  cat("Rows used in Box-Tidwell:", nrow(df_bt), "\n\n")
  
  # نضيف الـ interaction terms: x * log(x)
  for (v in bt_vars) {
    df_bt[[paste0(v, "_logx")]] <- df_bt[[v]] * log(df_bt[[v]])
  }
  
  # نبني الـ formula: الـ original vars + الـ interaction terms
  orig_terms <- paste(cont_vars, collapse = " + ")
  bt_terms   <- paste(paste0(bt_vars, "_logx"), collapse = " + ")
  
  # باقي الـ controls (categorical dummies)
  cat_controls <- paste(
    "female + rural",
    "+ mar_divorced + mar_separated + mar_widowed + mar_single",
    "+ class_uppmid + class_working + class_lower",
    "+ emp_fulltime + emp_parttime + emp_selfempl + emp_retired",
    "+ emp_homemaker + emp_student + emp_unemployed",
    sep = " "
  )
  
  bt_formula <- as.formula(paste(
    outcome_var, "~",
    orig_terms, "+",
    bt_terms, "+",
    cat_controls
  ))
  
  # نشغّل الـ polr مع الـ interaction terms
  bt_fit <- tryCatch(
    polr(bt_formula, data = df_bt, Hess = TRUE),
    error = function(e) { cat("Model failed:", e$message, "\n"); return(NULL) }
  )
  
  if (is.null(bt_fit)) return(invisible(NULL))
  
  # نستخرج بس الـ interaction terms من النتايج
  coef_bt <- summary(bt_fit)$coefficients
  bt_rows <- grep("_logx", rownames(coef_bt))
  bt_results <- coef_bt[bt_rows, , drop = FALSE]
  
  # نحسب p-values
  p_vals <- 2 * pnorm(abs(bt_results[, "t value"]), lower.tail = FALSE)
  
  results_df <- data.frame(
    Variable    = gsub("_logx", "", rownames(bt_results)),
    Coefficient = round(bt_results[, "Value"], 4),
    SE          = round(bt_results[, "Std. Error"], 4),
    t_value     = round(bt_results[, "t value"], 3),
    p_value     = round(p_vals, 4),
    Linearity   = ifelse(p_vals > 0.05, "✓ Linear", "✗ Non-linear")
  )
  
  print(results_df, row.names = FALSE)
  
  cat("\n── age_z note ──────────────────────────────────────────\n")
  cat("age_z excluded from Box-Tidwell (can be negative after scaling).\n")
  cat("Linearity of age_z assessed via partial residual plot below.\n")
  
  return(results_df)
}

# ── Happiness (Q46) ───────────────────────────────────────────
bt_hap <- run_boxtidwell(
  polr_model = polr_hap,
  df         = df_vgam,
  outcome_var = "factor(Q46)",
  cont_vars  = cont_vars,
  model_name = "Box-Tidwell: Happiness (Q46)"
)

# ── Life Satisfaction (Q49) ───────────────────────────────────
bt_ls <- run_boxtidwell(
  polr_model = polr_ls,
  df         = df_vgam,
  outcome_var = "factor(Q49)",
  cont_vars  = cont_vars,
  model_name = "Box-Tidwell: Life Satisfaction (Q49)"
)

cat("\n── How to read Box-Tidwell results ─────────────────────\n")
cat("p > 0.05 → interaction term NOT significant → linearity holds ✓\n")
cat("p < 0.05 → non-linearity detected → consider log(x) or x^2 ✗\n")

# ─────────────────────────────────────────────────────────────
#  ASSUMPTION 1: BOX-TIDWELL COMPLETE-----
#  Happiness + Life Sat + Joint Parallel + age_z fix
# ─────────────────────────────────────────────────────────────
joint_ctrl <- paste(
  "+ human_rights + economic_sat + living_standard + safety",
  "+ age_z + female + rural + education + income",
  "+ mar_divorced + mar_separated + mar_widowed + mar_single",
  "+ class_uppmid + class_working + class_lower",
  "+ emp_parttime + emp_selfempl + emp_retired",
  "+ emp_homemaker + emp_student + emp_unemployed + emp_fulltime"
)
# ── polr للـ joint (بنستخدم happiness كـ proxy للـ predictors) ──
polr_joint <- polr(
  as.formula(paste("factor(happiness) ~ bribery + wasta", joint_ctrl)),
  data = df_joint, Hess = TRUE
)

# ── Box-Tidwell للـ joint parallel ───────────────────────────
bt_joint <- run_boxtidwell(
  polr_model  = polr_joint,
  df          = df_joint,
  outcome_var = "factor(happiness)",
  cont_vars   = c("bribery", "wasta", "human_rights", "economic_sat",
                  "living_standard", "safety", "education", "income"),
  model_name  = "Box-Tidwell: Joint Parallel Model"
)

# ── age_z — FIXED partial residual plot ──────────────────────
cat("\n--- age_z Linearity (Fixed) ---\n")

par(mfrow = c(1, 3))

for (info in list(
  list(polr_hap,   "Happiness", "steelblue"),
  list(polr_ls,    "Life Sat",  "tomato"),
  list(polr_joint, "Joint Par", "darkgreen")
)) {
  model  <- info[[1]]
  label  <- info[[2]]
  color  <- info[[3]]
  
  # FIX: نحسب الـ linear predictor يدوياً = X %*% beta
  x_vals    <- model$model[["age_z"]]
  beta_age  <- coef(model)["age_z"]
  
  # نبني الـ model matrix ونضرب في الـ coefficients
  X         <- model.matrix(model)[, -1]  # بدون intercept
  lp        <- as.numeric(X %*% coef(model))
  
  y_num     <- as.numeric(model$model[[1]])
  
  # نتأكد إن الأطوال متساوية
  n         <- min(length(x_vals), length(y_num), length(lp))
  x_vals    <- x_vals[1:n]
  partial_r <- (y_num[1:n] - lp[1:n]) + beta_age * x_vals[1:n]
  
  plot(x_vals, partial_r,
       col  = adjustcolor(color, alpha.f = 0.3),
       pch  = 16, cex = 0.6,
       main = paste("age_z —", label),
       xlab = "age_z",
       ylab = "Partial Residual")
  
  abline(lm(partial_r ~ x_vals), col = "blue", lwd = 2)
  
  lo    <- loess(partial_r ~ x_vals, span = 0.75)
  x_seq <- seq(min(x_vals), max(x_vals), length.out = 100)
  lines(x_seq, predict(lo, x_seq), col = "red", lwd = 2)
  
  legend("topright",
         legend = c("Linear", "Smooth"),
         col    = c("blue", "red"),
         lwd    = 2, cex = 0.7)
}

par(mfrow = c(1, 1))

# ── إزاي تقري الـ plot ────────────────────────────────────────
# الخط الأزرق  = علاقة linear مفترضة
# الخط الأحمر  = علاقة فعلية في البيانات (smooth)
# لو الاتنين قريبين من بعض → العلاقة linear ✓
# لو في انحناء واضح       → محتاج quadratic term

# ── ASSUMPTION 2: Multicollinearity (VIF) ────────────────────----
cat("\n--- Assumption 2: Multicollinearity — VIF ---\n")
cat("Rule: VIF < 5 = acceptable | VIF > 10 = serious problem\n\n")

vif_hap <- vif(polr_hap)
vif_ls  <- vif(polr_ls)

vif_table <- data.frame(
  Predictor    = names(vif_hap),
  VIF_Happiness = round(vif_hap, 3),
  VIF_LifeSat   = round(vif_ls,  3),
  Flag_Hap      = ifelse(vif_hap > 10, "PROBLEM",
                         ifelse(vif_hap > 5, "Check", "OK")),
  Flag_LS       = ifelse(vif_ls  > 10, "PROBLEM",
                         ifelse(vif_ls  > 5, "Check", "OK"))
)
print(vif_table)

# ── بنبني dataframe مؤقت فيه الـ dummies + factor(Q279) ──────
df_vif <- df %>%
  mutate(across(all_of(ordered_vars), as.numeric)) %>%
  mutate(
    female        = as.integer(Q260 == 2),
    rural         = as.integer(H_URBRURAL == 2),
    mar_divorced  = as.integer(Q273 == 3),
    mar_separated = as.integer(Q273 == 4),
    mar_widowed   = as.integer(Q273 == 5),
    mar_single    = as.integer(Q273 == 6),
    class_uppmid  = as.integer(Q287 == 2),
    class_working = as.integer(Q287 == 4),
    class_lower   = as.integer(Q287 == 5),
    emp_f         = factor(Q279)   # ← factor بدل dummies يدوية
    # ref = category 1 (fulltime) تلقائياً
    # أو غيّريها: relevel(factor(Q279), ref="8")
  ) %>%
  na.omit()

cat("Rows:", nrow(df_vif), "\n")

# ── Formula مشتركة ────────────────────────────────────────────
vif_formula_hap <- as.formula(paste(
  "factor(Q46) ~ Q112 + Q110 + Q253 + Q50 + Q56 + Q131 + Q275 + Q288 + age_z",
  "+ female + rural",
  "+ mar_divorced + mar_separated + mar_widowed + mar_single",
  "+ class_uppmid + class_working + class_lower",
  "+ emp_f"
))

vif_formula_ls <- as.formula(paste(
  "factor(Q49) ~ Q112 + Q110 + Q253 + Q50 + Q56 + Q131 + Q275 + Q288 + age_z",
  "+ female + rural",
  "+ mar_divorced + mar_separated + mar_widowed + mar_single",
  "+ class_uppmid + class_working + class_lower",
  "+ emp_f"
))

# ── polr models ───────────────────────────────────────────────
polr_hap_vif   <- polr(vif_formula_hap, data = df_vif, Hess = TRUE)
polr_ls_vif    <- polr(vif_formula_ls,  data = df_vif, Hess = TRUE)

# ── Joint proxy ───────────────────────────────────────────────
df_joint_vif <- df_joint %>%
  mutate(emp_f = factor(
    case_when(
      emp_fulltime   == 1 ~ 1,
      emp_parttime   == 1 ~ 2,
      emp_selfempl   == 1 ~ 3,
      emp_retired    == 1 ~ 4,
      emp_homemaker  == 1 ~ 5,
      emp_student    == 1 ~ 6,
      emp_unemployed == 1 ~ 7,
      TRUE           ~ 8   # other = reference
    )
  ))

polr_joint_vif <- polr(
  as.formula(paste(
    "factor(happiness) ~ bribery + wasta",
    "+ human_rights + economic_sat + living_standard + safety",
    "+ age_z + female + rural + education + income",
    "+ mar_divorced + mar_separated + mar_widowed + mar_single",
    "+ class_uppmid + class_working + class_lower",
    "+ emp_f"
  )),
  data = df_joint_vif, Hess = TRUE
)

# ── ASSUMPTION 3: Influential Observations (Cook's Distance) ──----
cat("\n--- Assumption 3: Influential Observations ---\n")
cat("Rule: Cook's D > 4/n = potentially influential\n\n")

# نستخدم lm على numeric outcome كـ approximation لتحديد outliers
lm_hap <- lm(as.formula(paste("as.numeric(Q46) ~ Q112 + Q110", ctrl_str)),
             data = df_vgam)
lm_ls  <- lm(as.formula(paste("as.numeric(Q49) ~ Q112 + Q110", ctrl_str)),
             data = df_vgam)

cooks_hap <- cooks.distance(lm_hap)
cooks_ls  <- cooks.distance(lm_ls)
threshold <- 4 / nrow(df_vgam)

cat("Threshold (4/n):", round(threshold, 5), "\n")
cat("Influential obs — Happiness:", sum(cooks_hap > threshold), "\n")
cat("Influential obs — Life Sat :", sum(cooks_ls  > threshold), "\n")

# Cook's Distance plots
par(mfrow = c(1, 2))

plot(cooks_hap, type = "h", col = "steelblue",
     main = "Cook's Distance — Happiness",
     ylab = "Cook's D", xlab = "Observation")
abline(h = threshold, col = "red", lty = 2)
text(which(cooks_hap > threshold),
     cooks_hap[cooks_hap > threshold],
     labels = which(cooks_hap > threshold),
     pos = 3, cex = 0.7, col = "red")

plot(cooks_ls, type = "h", col = "tomato",
     main = "Cook's Distance — Life Satisfaction",
     ylab = "Cook's D", xlab = "Observation")
abline(h = threshold, col = "red", lty = 2)
text(which(cooks_ls > threshold),
     cooks_ls[cooks_ls > threshold],
     labels = which(cooks_ls > threshold),
     pos = 3, cex = 0.7, col = "red")

par(mfrow = c(1, 1))

# ─────────────────────────────────────────────────────────────
#  ASSUMPTION 3 COMPLETE: Cook's Distance للـ 3 موديلز
#  + Sensitivity Analysis (الموديل مع وبدون الـ influential obs)
# ─────────────────────────────────────────────────────────────

# ── Joint Parallel Cook's Distance ────────────────────────────
lm_joint <- lm(
  as.formula(paste("as.numeric(happiness) ~ bribery + wasta", joint_ctrl)),
  data = df_joint
)

cooks_joint <- cooks.distance(lm_joint)
threshold_j <- 4 / nrow(df_joint)

cat("Threshold Joint (4/n):", round(threshold_j, 5), "\n")
cat("Influential obs — Joint:", sum(cooks_joint > threshold_j), "\n")

# ── Plot الـ 3 موديلز ─────────────────────────────────────────
par(mfrow = c(1, 3))

for (info in list(
  list(cooks_hap,   threshold,   "Happiness (Q46)",       "steelblue"),
  list(cooks_ls,    threshold,   "Life Satisfaction (Q49)","tomato"),
  list(cooks_joint, threshold_j, "Joint Parallel",         "darkgreen")
)) {
  cd <- info[[1]]; thr <- info[[2]]; label <- info[[3]]; color <- info[[4]]
  
  plot(cd, type = "h", col = color,
       main = paste("Cook's D —", label),
       ylab = "Cook's D", xlab = "Observation",
       cex.main = 0.9)
  abline(h = thr, col = "red", lty = 2)
  text(which(cd > thr), cd[cd > thr],
       labels = which(cd > thr),
       pos = 3, cex = 0.6, col = "red")
}

par(mfrow = c(1, 1))

# ── Sensitivity Analysis ──────────────────────────────────────باااااااااااااظ
# نشوف هل الـ influential observations بتغير النتايج لو شلناهم
cat("\n--- Sensitivity Analysis ---\n")
cat("Comparing coefficients WITH vs WITHOUT influential observations\n\n")

# Happiness — شيل الـ influential obs
inf_hap    <- which(cooks_hap > threshold)
df_hap_clean <- df_vgam[-inf_hap, ]

vgam_hap_full  <- vglm(
  as.formula(paste("Q46 ~ Q112 + Q110", ctrl_str)),
  family = cumulative(parallel = TRUE, link = "logitlink"),
  data   = df_vgam
)
vgam_hap_clean <- vglm(
  as.formula(paste("Q46 ~ Q112 + Q110", ctrl_str)),
  family = cumulative(parallel = TRUE, link = "logitlink"),
  data   = df_hap_clean
)

# Life Sat — شيل الـ influential obs
inf_ls       <- which(cooks_ls > threshold)
df_ls_clean  <- df_vgam[-inf_ls, ]

vgam_ls_full  <- vglm(
  as.formula(paste("Q49 ~ Q112 + Q110", ctrl_str)),
  family = cumulative(parallel = TRUE, link = "logitlink"),
  data   = df_vgam
)
vgam_ls_clean <- vglm(
  as.formula(paste("Q49 ~ Q112 + Q110", ctrl_str)),
  family = cumulative(parallel = TRUE, link = "logitlink"),
  data   = df_ls_clean
)

# ── مقارنة الـ key predictors فقط ────────────────────────────
key_preds <- c("Q112", "Q110", "Q50", "Q288")

compare_coefs <- function(full_model, clean_model, label, n_full, n_clean) {
  cf_full  <- coef(summary(full_model))
  cf_clean <- coef(summary(clean_model))
  
  rows <- intersect(rownames(cf_full), key_preds)
  
  cat("\n===", label, "===\n")
  cat(sprintf("%-15s %10s %10s %10s %s\n",
              "Predictor", "Coef Full", "Coef Clean", "Diff", "Stable?"))
  cat(strrep("-", 60), "\n")
  
  for (r in rows) {
    b_full  <- cf_full[r,  "Estimate"]
    b_clean <- cf_clean[r, "Estimate"]
    diff    <- abs(b_full - b_clean)
    pct     <- diff / abs(b_full) * 100
    stable  <- ifelse(pct < 10, "✓ Stable", ifelse(pct < 20, "~ Check", "✗ Sensitive"))
    cat(sprintf("%-15s %10.4f %10.4f %10.4f  %s (%.1f%%)\n",
                r, b_full, b_clean, b_full - b_clean, stable, pct))
  }
  cat(sprintf("\nN full: %d | N clean: %d | Removed: %d obs\n",
              n_full, n_clean, n_full - n_clean))
}

compare_coefs(vgam_hap_full,  vgam_hap_clean,
              "Happiness", nrow(df_vgam), nrow(df_hap_clean))

compare_coefs(vgam_ls_full,   vgam_ls_clean,
              "Life Sat",  nrow(df_vgam), nrow(df_ls_clean))


# ─────────────────────────────────────────────────────────────
#  SENSITIVITY ANALYSIS — الحل الصح
#  بدل ما نشيل observations، نستخدم robust SEs
#  ونحدد مين الـ top influential observations ونفهمهم
# ─────────────────────────────────────────────────────────────

library(sandwich)
library(lmtest)

# ── STEP 1: افهم مين هم الـ top influential obs ──────────────
cat("\n--- Top Influential Observations ---\n")

# Happiness — top 10
top_hap <- sort(cooks_hap, decreasing = TRUE)[1:10]
cat("\nTop 10 — Happiness:\n")
print(round(top_hap, 5))

# Life Sat — top 10  
top_ls <- sort(cooks_ls, decreasing = TRUE)[1:10]
cat("\nTop 10 — Life Sat:\n")
print(round(top_ls, 5))

# Joint — top 10
top_joint <- sort(cooks_joint, decreasing = TRUE)[1:10]
cat("\nTop 10 — Joint:\n")
print(round(top_joint, 5))

# ── STEP 2: شوف الـ obs دي إيه اللي فيها ────────────────────
cat("\n--- Profile of Top Influential Obs (Happiness) ---\n")
top_ids_hap <- as.integer(names(top_hap))
print(df_vgam[top_ids_hap, c("Q46","Q49","Q112","Q110","Q50","Q288","emp_fulltime","emp_homemaker")])

cat("\n--- Profile of Top Influential Obs (Life Sat) ---\n")
top_ids_ls <- as.integer(names(top_ls))
print(df_vgam[top_ids_ls, c("Q46","Q49","Q112","Q110","Q50","Q288","emp_fulltime","emp_homemaker")])

# ── STEP 3: Robust SE بدل حذف الـ observations ──────────────
# نستخدم lm مع robust SEs كـ sensitivity check
cat("\n--- Robust SE Sensitivity Check ---\n")

robust_check <- function(lm_model, label) {
  cat("\n===", label, "===\n")
  
  # Normal SEs
  normal <- coeftest(lm_model)
  # Robust SEs (HC3)
  robust <- coeftest(lm_model, vcov = vcovHC(lm_model, type = "HC3"))
  
  key <- c("Q112", "Q110", "Q50", "Q288")
  
  cat(sprintf("%-12s %10s %10s %10s %s\n",
              "Predictor", "Normal SE", "Robust SE", "Ratio", "Stable?"))
  cat(strrep("-", 55), "\n")
  
  for (k in key) {
    if (k %in% rownames(normal)) {
      se_n <- normal[k, "Std. Error"]
      se_r <- robust[k, "Std. Error"]
      ratio <- se_r / se_n
      stable <- ifelse(ratio < 1.2, "✓ Stable",
                       ifelse(ratio < 1.5, "~ Mild inflation", "✗ Sensitive"))
      cat(sprintf("%-12s %10.4f %10.4f %10.2f  %s\n",
                  k, se_n, se_r, ratio, stable))
    }
  }
}

robust_check(lm_hap,   "Happiness (Q46)")
robust_check(lm_ls,    "Life Satisfaction (Q49)")
robust_check(lm_joint, "Joint Parallel")

# ── STEP 4: هل الـ max Cook's D خطير فعلاً؟ ─────────────────
cat("\n--- Are the influential obs truly problematic? ---\n")
cat("Rule of thumb: Cook's D > 1.0 = definitely problematic\n")
cat("              Cook's D > 0.5  = worth investigating\n")
cat("              Cook's D < 0.5  = minor influence only\n\n")

cat("Max Cook's D — Happiness :", round(max(cooks_hap),   4), "\n")
cat("Max Cook's D — Life Sat  :", round(max(cooks_ls),    4), "\n")
cat("Max Cook's D — Joint     :", round(max(cooks_joint), 4), "\n")

