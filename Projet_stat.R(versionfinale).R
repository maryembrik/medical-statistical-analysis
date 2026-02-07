#Libraries
#install.packages("BSDA")
#install.packages("car")
#install.packages("BSDA")
#install.packages("copula")
#install.packages("readr")
#install.packages("corrplot")
#install.packages("psych")
#install.packages("lsr")
#install.packages("skimr")
#install.packages("GGally")
#install.packages("skimr")

#library(corrplot)
#library(dplyr)
#library(car)
#library(BSDA)
#library(copula)
#library(readr)
#library(tidyr)
#library(ggplot2)
#library(psych)
#library(lsr)


                                    #Chapter 1


# -------------------------------------------------------------------------
# *** Helper Functions
# -------------------------------------------------------------------------

# IQR outliers
compute_iqr_bounds <- function(x, k = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  c(lower = q1 - k * iqr, upper = q3 + k * iqr)
}

# Winsorization
clip_to_bounds <- function(x, bounds) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x < bounds["lower"] ~ bounds["lower"],
    x > bounds["upper"] ~ bounds["upper"],
    TRUE ~ x
  )
}

# -------------------------------------------------------------------------
# *** Load Raw Data
# -------------------------------------------------------------------------

raw_path <- "C:/Users/Huawei/Documents/Projet Stat/Projet StatVm/patients_medical_data.csv"

message("Reading raw data from: ", raw_path)

df_raw <- read_delim(
  raw_path,
  delim = ";",
  locale = locale(decimal_mark = ".", grouping_mark = ""),
  show_col_types = FALSE
)

message("Initial dataset shape: ", nrow(df_raw), " rows and ", ncol(df_raw), " columns")
glimpse(df_raw)
summary(df_raw)

# -------------------------------------------------------------------------
# *** Rename Columns
# -------------------------------------------------------------------------

df <- df_raw %>%
  rename(
    patient_id = ID,
    age = Age,
    sex = Sexe,
    weight_kg = Poids,
    systolic_bp = Tension,
    cholesterol_mg_dl = Cholesterol,
    treatment_group = Groupe_Traitement,
    followup_days = Suivi_Jours,
    symptom_score = Symptom_Score
  )

# -------------------------------------------------------------------------
# *** Data Types
# -------------------------------------------------------------------------

df <- df %>%
  mutate(
    patient_id = as.integer(patient_id),
    age = as.integer(age),
    weight_kg = as.numeric(weight_kg),
    systolic_bp = as.numeric(systolic_bp),
    cholesterol_mg_dl = as.numeric(cholesterol_mg_dl),
    followup_days = as.integer(followup_days),
    symptom_score = as.integer(symptom_score),
    sex = factor(trimws(sex), levels = c("F", "M")),
    treatment_group = factor(trimws(treatment_group))
  )

# Trim whitespace in character columns
df[sapply(df, is.character)] <-
  lapply(df[sapply(df, is.character)], trimws)

# -------------------------------------------------------------------------
# *** Duplicate Removal
# -------------------------------------------------------------------------

dups <- df %>% filter(duplicated(patient_id))
if (nrow(dups) > 0)
  message("Found duplicates: ", nrow(dups))

df <- df %>% distinct(patient_id, .keep_all = TRUE)

# -------------------------------------------------------------------------
# *** Missing Value Summary
# -------------------------------------------------------------------------

na_summary <- df %>% summarise(across(everything(), ~sum(is.na(.))))
print(na_summary)

# -------------------------------------------------------------------------
# *** CORRELATION CHECK 
# -------------------------------------------------------------------------

numeric_cols <- df %>%
  select(where(is.numeric)) %>%
  select(-patient_id) %>%   # IMPORTANT: exclude ID
  names()

cor_matrix <- cor(df[numeric_cols], use = "pairwise.complete.obs")

if (!dir.exists("plots")) dir.create("plots")

png("plots/correlation_matrix.png", width = 900, height = 700)
corrplot(
  cor_matrix,
  method = "color",
  addCoef.col = "black",
  tl.cex = 0.8,
  number.cex = 0.5,
  title = "Exploratory Correlation Matrix (Numeric Variables)",
  mar = c(0,0,2,0)
)
dev.off()

message("Exploratory correlation matrix saved.")

# -------------------------------------------------------------------------
#  Outlier Treatment BEFORE Imputation
# -------------------------------------------------------------------------

for (col in numeric_cols) {
  bounds <- compute_iqr_bounds(df[[col]])
  df[[col]] <- clip_to_bounds(df[[col]], bounds)
}

# -------------------------------------------------------------------------
#  Missing Value Imputation AFTER Outliers Removed
# -------------------------------------------------------------------------

df <- df %>%
  mutate(across(
    all_of(numeric_cols),
    ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x)
  ))

# -------------------------------------------------------------------------
#  Log-transform highly skewed features 
# -------------------------------------------------------------------------

skewed_cols <- c("cholesterol_mg_dl", "symptom_score", "systolic_bp")

for (col in skewed_cols) {
  df[[paste0(col, "_log")]] <- log(df[[col]] + 1)
}

# -------------------------------------------------------------------------
#  Derived Features
# -------------------------------------------------------------------------

# BMI calculation ONLY if height column exists
if ("height_cm" %in% names(df)) {
  df <- df %>%
    mutate(
      height_m = height_cm / 100,
      bmi = weight_kg / (height_m^2),
      bmi_category = case_when(
        bmi < 18.5 ~ "underweight",
        bmi < 25 ~ "normal",
        bmi < 30 ~ "overweight",
        TRUE ~ "obese"
      )
    )
} else {
  # fallback: weight categories (your original)
  df <- df %>%
    mutate(
      bmi_category = case_when(
        weight_kg < 50 ~ "underweight",
        weight_kg < 70 ~ "healthy",
        weight_kg < 90 ~ "overweight",
        TRUE ~ "obese"
      )
    )
}

df <- df %>%
  mutate(hypertension_flag = if_else(systolic_bp >= 140, 1L, 0L))

# -------------------------------------------------------------------------
# Descriptive Stats
# -------------------------------------------------------------------------

numeric_summary <- df %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd   = ~sd(.x, na.rm = TRUE))))

print(numeric_summary)

# -------------------------------------------------------------------------
#  Histograms
# -------------------------------------------------------------------------

if (!dir.exists("plots")) dir.create("plots")

for (metric in numeric_cols) {
  p <- ggplot(df, aes(x = .data[[metric]])) +
    geom_histogram(bins = 20, fill = "#2E86AB", color = "black") +
    labs(title = paste("Distribution of", metric)) +
    theme_minimal()
  
  ggsave(paste0("plots/", metric, "_hist.png"),
         plot = p, width = 6, height = 4, dpi = 120)
}

# -------------------------------------------------------------------------
#  Save Clean Data
# -------------------------------------------------------------------------

write_csv(df, "patients_medical_data_cleanedFinal.csv")
message("Cleaned dataset saved.")

# -------------------------------------------------------------------------
#  Profile Summary
# -------------------------------------------------------------------------

data_profile <- df %>%
  summarise(
    n_patients = n(),
    n_female = sum(sex == "F", na.rm = TRUE),
    n_male = sum(sex == "M", na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    mean_weight = mean(weight_kg, na.rm = TRUE),
    mean_systolic_bp = mean(systolic_bp, na.rm = TRUE),
    mean_cholesterol = mean(cholesterol_mg_dl, na.rm = TRUE),
    mean_symptom_score = mean(symptom_score, na.rm = TRUE)
  )

write_csv(data_profile, "patients_medical_data_profileFinal.csv")
message("Profile summary saved.")

message("Data cleaning complete.")


                                       


                     # CHAPTER 2 : Parametric Hypothesis Tests



#  NORMALITY CHECK (numeric only)
shapiro.test(df$weight_kg)
shapiro.test(df$age)
shapiro.test(df$systolic_bp)
shapiro.test(df$cholesterol_mg_dl)
shapiro.test(df$followup_days)
shapiro.test(df$symptom_score)

# -----------------------------
# *** PARAMETRIC TESTS (Normal variables)
# -----------------------------

# *** Weight by Sex Weight by Sex — t-test
# Check equality of variances
var.test(weight_kg ~ sex, data = df)

# Perform t-test (pooled if equal variances)
t.test(weight_kg ~ sex, data = df, var.equal = TRUE)

 
  
# Visualization
par(mfrow = c(1,2))
hist(df$weight_kg,
     main = "Histogram of Weight",
     col = "lightblue",
     xlab = "Weight (kg)")
qqnorm(df$weight_kg, main = "Q-Q Plot of Weight")
qqline(df$weight_kg, col = "red", lwd = 2)
par(mfrow = c(1,1))

boxplot(weight_kg ~ sex, data = df,
        col = c("lightgreen", "lightpink"),
        main = "Weight by Sex",
        ylab = "Weight (kg)")

# Density plot
ggplot(df, aes(x = weight_kg, fill = sex)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Density Plot of Weight by Sex", x = "Weight (kg)")



# *** Cholesterol by Sex — t-test

t.test(cholesterol_mg_dl ~ sex, data = df, var.equal = TRUE)

boxplot(cholesterol_mg_dl ~ sex, data = df,
        col = c("lightblue", "lightpink"),
        main = "Cholesterol by Sex",
        ylab = "Cholesterol (mg/dL)")


# -----------------------------
# *** NON-PARAMETRIC TESTS (Other numeric variables)
# -----------------------------
wilcox.test(age ~ sex, data = df)
wilcox.test(systolic_bp ~ sex, data = df)
wilcox.test(followup_days ~ sex, data = df)
wilcox.test(symptom_score ~ sex, data = df)

# Boxplots
boxplot(age ~ sex, data = df,
        col = c("lightblue", "lightpink"),
        main = "Age by Sex",
        ylab = "Age (years)")

boxplot(systolic_bp ~ sex, data = df,
        col = c("lightgreen", "lightpink"),
        main = "Systolic BP by Sex",
        ylab = "Systolic BP (mmHg)")



# -----------------------------
# *** COMPARISON OF PROPORTIONS
# -----------------------------
df$High_Chol <- ifelse(df$cholesterol_mg_dl > 200, 1, 0)

x <- c(sum(df$High_Chol[df$sex == "M"], na.rm = TRUE),
       sum(df$High_Chol[df$sex == "F"], na.rm = TRUE))

n <- c(sum(df$sex == "M", na.rm = TRUE),
       sum(df$sex == "F", na.rm = TRUE))

# Proportion test
prop.test(x, n, correct = FALSE)

# Barplot
tab <- table(df$sex, df$High_Chol)
prop <- prop.table(tab, margin = 1)
barplot(t(prop),
        beside = TRUE,
        col = c("steelblue", "tomato"),
        legend = c("Low/Normal", "High Cholesterol"),
        main = "Proportion of High Cholesterol by Sex",
        xlab = "Sex")

# -----------------------------
# *** Z-test (if known variances, optional)
# -----------------------------
group_male <- df$weight_kg[df$sex == "M"]
group_female <- df$weight_kg[df$sex == "F"]
z.test(group_male, group_female, sigma.x = 8, sigma.y = 7, alternative = "two.sided")





                                     #Chapter 3
# Régression simple : systolic BP ~ Age
#  Predict systolic BP from Age
simple_model <- lm(systolic_bp ~ age, data = df)
#lm(systolic_bp ~ weight_kg, data = df)
#lm(systolic_bp ~ cholesterol_mg_dl, data = df)

# 1. View summary
summary(simple_model)

# 2. Plot regression line
plot(df$age, df$systolic_bp,
     main = "Systolic BP vs Age",
     xlab = "Age",
     ylab = "Systolic BP",
     pch = 19, col = "blue")
abline(simple_model, col = "red", lwd = 2)

# 3. Check residuals
par(mfrow = c(1,2))
plot(simple_model, which = 1)  # Residuals vs Fitted
plot(simple_model, which = 2)  # Normal Q-Q
par(mfrow = c(1,1))

# 4. Shapiro-Wilk test for residual normality
shapiro.test(residuals(simple_model))

# multiple Linear Regression
# Ensure categorical variables are factors
df <- df %>%
  mutate(
    sex = factor(sex),
    treatment_group = factor(treatment_group)
  )
# Multiple Linear Regression: Predicting Systolic Blood Pressure
model <- lm(systolic_bp ~ age + weight_kg + cholesterol_mg_dl + sex + treatment_group,
            data = df)

# 1 Summary of the model
cat("\n--- Multiple Linear Regression Model Summary ---\n")
summary(model)

# 2 Check residual normality
shapiro_res <- shapiro.test(residuals(model))
cat("\n--- Shapiro-Wilk Test for Normality of Residuals ---\n")
print(shapiro_res)

# 3 Residuals vs Fitted Plot
plot(model, which = 1, main = "Residuals vs Fitted")

# 4️ Normal Q-Q Plot
plot(model, which = 2, main = "Normal Q-Q")

# 5️ Scale-Location Plot (to check homoscedasticity)
plot(model, which = 3, main = "Scale-Location")

# 6️ Cook's Distance Plot (influential observations)
plot(model, which = 4, main = "Cook's Distance")

# 7 Non-constant Variance Test (Breusch-Pagan)
library(car)
ncv_result <- ncvTest(model)
cat("\n--- Non-constant Variance Test (Breusch-Pagan) ---\n")
print(ncv_result)

# 8 plot individual predictors vs response with regression line
library(ggplot2)
ggplot(df, aes(x = age, y = systolic_bp)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  ggtitle("Systolic BP vs Age")

ggplot(df, aes(x = weight_kg, y = systolic_bp)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  ggtitle("Systolic BP vs Weight")
#Correlation
library(corrplot)
 num_vars <- df %>% 
   select(systolic_bp, age, weight_kg, cholesterol_mg_dl)

cor_matrix <- cor(num_vars)
corrplot(cor_matrix, method = "circle")
#Correlation Between Numeric Variables
num_df <- df %>% select(where(is.numeric))

cor_matrix <- cor(num_df, use = "complete.obs")
cor_matrix



corrplot(cor_matrix, method = "color", addCoef.col = "black")

#Correlation Between Numeric & Categorical Variables

library(psych)
biserial(df$age, df$sex == "M")
library(lsr)
etaSquared(aov(systolic_bp ~ treatment_group, data = df))
library(skimr)
skim(df)
library(GGally)

GGally::ggpairs(
  df %>% select(
    age, weight_kg, systolic_bp, cholesterol_mg_dl, 
    sex, treatment_group
  )
)
# 9. Check for Multicollinearity (VIF)
# NOTE: VIF values > 5 or > 10 usually indicate problematic multicollinearity.
vif_results <- car::vif(model)
cat("\n--- Variance Inflation Factor (VIF) Results ---\n")
print(vif_results)



                                      #Chapter 4
# ---------------------------------------------------------------
# 1. Assumption Check: Homogeneity of Variances
# ---------------------------------------------------------------

leveneTest(cholesterol_mg_dl ~ bmi_category, data = df)
# Interpretation:
# If p-value > 0.05 → variances are homogeneous → ANOVA applicable

# ---------------------------------------------------------------
# 2. One-Way ANOVA
# ---------------------------------------------------------------

anova_model <- aov(cholesterol_mg_dl ~ bmi_category, data = df)

cat("\n--- One-Way ANOVA Model Summary (BMI Category) ---\n")
summary(anova_model)
# Interpretation:
# If p-value < 0.05 → at least one group mean is significantly different

# ---------------------------------------------------------------
# 3. Post-hoc Analysis (Tukey HSD)
# ---------------------------------------------------------------

tukey_hsd_result <- TukeyHSD(anova_model)

cat("\n--- Tukey HSD Post-hoc Test ---\n")
print(tukey_hsd_result)
# Interpretation:
# Look for adjusted p-values < 0.05 to identify specific group differences

# ---------------------------------------------------------------
# 4. Model Validation: Normality of Residuals
# ---------------------------------------------------------------

res_anova <- residuals(anova_model)

# Shapiro-Wilk normality test
shapiro.test(res_anova)
# If p-value > 0.05 → residuals approximately normal

# Q-Q plot
qqnorm(res_anova, main = "Q-Q Plot of ANOVA Residuals")
qqline(res_anova, col = "red", lwd = 2)

# ---------------------------------------------------------------
# 5. Visualization
# ---------------------------------------------------------------

boxplot(
  cholesterol_mg_dl ~ bmi_category,
  data = df,
  main = "Cholesterol Levels by BMI Category",
  xlab = "BMI Category",
  ylab = "Cholesterol (mg/dL)",
  col = c("red", "lightgreen", "yellow", "orange")
)

                         #Chapter 5
# Justification: symptom_score is discrete and not normally distributed
shapiro.test(df$symptom_score)

# ---------------------------------------------------------------
# 1) CONFORMITY TESTS
# ---------------------------------------------------------------

# Sign Test (median = 5)
SIGN.test(df$symptom_score, md = 5)

# Wilcoxon Signed-Rank Test (one sample)
wilcox.test(df$symptom_score, mu = 5)

# ---------------------------------------------------------------
# 2) HOMOGENEITY TESTS
# ---------------------------------------------------------------

# Mann-Whitney U Test (two independent groups)
wilcox.test(symptom_score ~ sex, data = df)

# Kruskal-Wallis Test (k independent groups)
kruskal.test(symptom_score ~ bmi_category, data = df)

# ---------------------------------------------------------------
# 3) Visual Support
# ---------------------------------------------------------------

boxplot(symptom_score ~ sex, data = df,
        main = "Symptom Score by Sex",
        ylab = "Symptom Score",
        col = c("lightblue", "lightpink"))

boxplot(symptom_score ~ bmi_category, data = df,
        main = "Symptom Score by BMI Category",
        ylab = "Symptom Score",
        col = c("red", "lightgreen", "yellow", "orange"))

# -------------------------------------------------------------------------
#           CHAPTER 6: MEASURING LINEAR & NONLINEAR ASSOCIATION
# -------------------------------------------------------------------------
# Selection of quantitative variables
numeric_vars <- df %>%
  select(age, weight_kg, systolic_bp, cholesterol_mg_dl, followup_days, symptom_score)

# ---------------------------------------------------------------
# 1. Pearson Correlation (Linear Association)
# ---------------------------------------------------------------

cor_matrix_pearson <- cor(numeric_vars, use = "complete.obs", method = "pearson")
cat("\n--- Pearson Correlation Matrix ---\n")
print(cor_matrix_pearson)

# ---------------------------------------------------------------
# 2. Spearman Correlation (Monotonic Association)
# ---------------------------------------------------------------

cor_matrix_spearman <- cor(numeric_vars, use = "complete.obs", method = "spearman")
cat("\n--- Spearman Correlation Matrix ---\n")
print(cor_matrix_spearman)

# ---------------------------------------------------------------
# 3. Kendall Correlation (Rank-Based)
# ---------------------------------------------------------------

cor_matrix_kendall <- cor(numeric_vars, use = "complete.obs", method = "kendall")
cat("\n--- Kendall Correlation Matrix ---\n")
print(cor_matrix_kendall)

# ---------------------------------------------------------------
# 4. Correlation Significance Tests
# ---------------------------------------------------------------

cor.test(df$age, df$systolic_bp, method = "pearson")
cor.test(df$age, df$systolic_bp, method = "spearman")
cor.test(df$age, df$systolic_bp, method = "kendall")

cor.test(df$weight_kg, df$cholesterol_mg_dl, method = "spearman")
cor.test(df$systolic_bp, df$symptom_score, method = "spearman")

# ---------------------------------------------------------------
# 5. Graphical Representation
# ---------------------------------------------------------------

pairs(numeric_vars,
      main = "Scatterplot Matrix of Quantitative Variables",
      pch = 19,
      col = rgb(0, 0, 1, 0.4))

# -------------------------------------------------------------------------
#             CHAPTER 7: MEASUREMENT OF LINEAR DEPENDENCIES (COPULAS)
# -------------------------------------------------------------------------

# ---------------------------------------------------------------
# 1. Variable Selection and Pseudo-Observations
# ---------------------------------------------------------------

df_cop <- df %>%
  select(age, symptom_score) %>%
  na.omit()

U <- pobs(as.matrix(df_cop))  # Empirical CDFs (Sklar’s theorem)

# ---------------------------------------------------------------
# 2. Copula Model Fitting
# ---------------------------------------------------------------

# Gaussian Copula (Elliptic)
normal_fit <- fitCopula(
  normalCopula(dim = 2),
  U,
  method = "ml"
)

# Gumbel Copula (Archimedean – upper tail dependence)
gumbel_fit <- fitCopula(
  gumbelCopula(dim = 2),
  U,
  method = "ml"
)

# ---------------------------------------------------------------
# 3. Estimated Parameters
# ---------------------------------------------------------------

cat("\n=========== AGE vs SYMPTOM SCORE ===========\n")

cat("\n>> Gaussian Copula\n")
print(normal_fit)

cat("\n>> Gumbel Copula\n")
print(gumbel_fit)

# ---------------------------------------------------------------
# 4. Model Comparison
# ---------------------------------------------------------------

cat("\n--- Log-Likelihood Comparison ---\n")
print(logLik(normal_fit))
print(logLik(gumbel_fit))

cat("\n--- AIC Comparison ---\n")
AIC(normal_fit, gumbel_fit)

# ---------------------------------------------------------------
# 5. Visualization: Pseudo-Observations
# ---------------------------------------------------------------

pairs(U, main = "Pseudo-Observations: Age vs Symptom Score")

# ---------------------------------------------------------------
# 6. Simulation from Fitted Copulas
# ---------------------------------------------------------------

# Gaussian Copula Simulation
sim_normal <- rCopula(500, normal_fit@copula)
plot(sim_normal,
     col = "blue", pch = 19,
     main = "Simulation from Gaussian Copula")

# Gumbel Copula Simulation
sim_gumbel <- rCopula(500, gumbel_fit@copula)
plot(sim_gumbel,
     col = "red", pch = 19,
     main = "Simulation from Gumbel Copula")