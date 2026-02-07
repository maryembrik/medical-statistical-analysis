# medical-statistical-analysis

# Statistical Analysis of Medical Patient Data (R)

📊 **Academic Project – Medical Statistics & R Programming**

This repository contains an academic statistics project based on the article:

> Najmi A., Sadasivam B., Ray A. (2021).  
> *How to choose and interpret a statistical test?*  
> Journal of Family Medicine and Primary Care.

The project applies appropriate **parametric and non-parametric statistical tests** to a real medical dataset using **R**, following the methodological framework proposed in the article.

---

## 📌 Objectives

- Apply correct statistical test selection based on:
  - Type of variables
  - Distribution assumptions
  - Study design
  - Number of groups
- Perform exploratory data analysis and data cleaning
- Implement:
  - Hypothesis testing
  - Linear regression
  - ANOVA
  - Non-parametric tests
  - Correlation analysis
  - Copula-based dependence modeling
- Interpret results from both **statistical** and **practical (clinical)** perspectives

---

## 📁 Dataset

- **100 medical patients**
- Variables include:
  - Age, sex, weight
  - Systolic blood pressure
  - Cholesterol level
  - Treatment group
  - Follow-up duration
  - Symptom severity score

---

## 🛠️ Methods Used

✔ Data cleaning & imputation  
✔ Outlier detection (IQR + winsorization)  
✔ Normality tests (Shapiro–Wilk)  
✔ Student’s t-test & Mann–Whitney test  
✔ One-way ANOVA & Kruskal–Wallis test  
✔ Linear & multiple regression  
✔ Pearson & Spearman correlation  
✔ Copula-based dependence modeling  

All analyses were performed using **R**.

---

## 📊 Project Structure

- `data/` → raw and cleaned datasets  
- `scripts/` → R scripts organized by chapter  
- `figures/` → generated plots and diagnostics  
- `report/` → final PDF report  
- `references/` → scientific article used as methodology reference  

---

## ▶️ How to Run the Project

1. Clone the repository:
```bash
git clone https://github.com/your-username/medical-statistical-analysis-R.git
