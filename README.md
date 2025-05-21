# Employment Demographics Analysis Using CPS Data

This project explores employment disparities in the U.S. labor force using a binomial logistic regression model applied to IPUMS CPS microdata. The analysis focuses on the effect of Mexican nativity, gender, and immigration status on the probability of employment, leveraging over 989,000 observations.

## 🎯 Objective

To identify and quantify the influence of key demographic characteristics—such as nativity, gender, and age—on employment outcomes, and provide actionable insights for policymakers and labor market analysts.

## 📊 Dataset

- **Source:** [IPUMS CPS](https://cps.ipums.org/cps/)
- **Sample Size:** 989,997 observations
- **Key Features:**
  - Employment status (binary)
  - Mexican nativity
  - Gender
  - Immigration year availability
  - Weeks worked last year (categorized)
  - Total income
  - Age

## 🧠 Methodology

- Data cleaning and transformation using `dplyr`, `tidyverse`, and `haven`
- Feature engineering for binary indicators of Mexican origin, gender, and immigration
- Exploratory data analysis (EDA) using `ggplot2`
- Logistic regression modeling:
  - Stepwise model selection (AIC/BIC)
  - Chi-square tests for group differences
  - Evaluation via accuracy, precision, recall, F1-score, pseudo R²

## 📈 Key Results

- **Mexican-born individuals** had 21% lower odds of employment than non-Mexican-born
- **Males** had 20.6% higher odds of being employed than females
- Model accuracy: ~66.8% (test set)
- Pseudo R² (McFadden): 0.0366

## 📌 Insights & Implications

- Employment disparities persist for Mexican immigrants and women, even in a high-employment labor pool.
- Policymakers can use these results to design more equitable employment access programs and address systemic disparities in the labor market.

## 📁 Repository Structure
```bash
employment-demographics-analysis/
├── final_project_with_annotations.R     # R script with full analysis and annotations
├── Stats_final.pptx.pdf                 # Final presentation slides (PDF)
├── README.md                            # Project overview and documentation



