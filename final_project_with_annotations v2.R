library(tidyverse)
library(ggplot2)
library(haven)
library(stats)
library(broom)
library(dplyr)
library(MASS)
library(caret)
library(Metrics)
library(pscl)

#Data loading
cps_00013 <- read_dta("cps_00013.dta")

#install.packages('skimr')
library(skimr)  
#install.packages('psych')
library(psych)  

#Overview of Dataset
cat("Total Number of Observations:", nrow(cps_clean), "\n")

# Descriptive Stats
income_stats <- data.frame(
  Statistic = c("Total Observations", "Minimum", "Maximum", "Mean", "Median", "Standard Deviation"),
  Value = c(
    sum(!is.na(cps_clean$inctot)),
    min(cps_clean$inctot, na.rm = TRUE),
    max(cps_clean$inctot, na.rm = TRUE),
    mean(cps_clean$inctot, na.rm = TRUE),
    median(cps_clean$inctot, na.rm = TRUE),
    sd(cps_clean$inctot, na.rm = TRUE)
  )
)
colnames(income_stats) <- c("Income Statistic", "Value")
print("Income Descriptive Statistics:")
print(income_stats)

# Demographic
demo_stats <- data.frame(
  Characteristic = c(
    "Total Observations", 
    "Minimum Age", 
    "Maximum Age", 
    "Mean Age", 
    "Median Age", 
    "Standard Deviation of Age",
    "Proportion Mexican (%)",
    "Proportion Immigrant (%)",
    "Proportion Male (%)"
  ),
  Value = c(
    nrow(cps_clean),
    min(cps_clean$age, na.rm = TRUE),
    max(cps_clean$age, na.rm = TRUE),
    mean(cps_clean$age, na.rm = TRUE),
    median(cps_clean$age, na.rm = TRUE),
    sd(cps_clean$age, na.rm = TRUE),
    mean(cps_clean$mexican) * 100,
    mean(cps_clean$inmigra) * 100,
    mean(cps_clean$men) * 100
  )
)
colnames(demo_stats) <- c("Demographic Statistic", "Value")
print("\nDemographic Descriptive Statistics:")
print(demo_stats)
    
# Weeks Worked Descriptive Statistics
weeks_worked_numeric <- as.numeric(as.character(cps_clean$wkswork2))

weeks_stats <- data.frame(
  Statistic = c(
    "Total Observations", 
    "Minimum Weeks", 
    "Maximum Weeks", 
    "Mean Weeks", 
    "Median Weeks", 
    "Standard Deviation of Weeks"
  ),
  Value = c(
    sum(!is.na(weeks_worked_numeric)),
    min(weeks_worked_numeric, na.rm = TRUE),
    max(weeks_worked_numeric, na.rm = TRUE),
    mean(weeks_worked_numeric, na.rm = TRUE),
    median(weeks_worked_numeric, na.rm = TRUE),
    sd(weeks_worked_numeric, na.rm = TRUE)
  )
)
colnames(weeks_stats) <- c("Weeks Worked Statistic", "Value")
print("\nWeeks Worked Descriptive Statistics:")
print(weeks_stats)
      
# Frequency Tables for Categorical Variables
print("\nFrequency Tables:")
print("Mexican Origin:")
print(table(cps_clean$mexican))

print("\nImmigrant Status:")
print(table(cps_clean$inmigra))

print("\nGender:")
print(table(cps_clean$men))

print("\nEmployment Status:")
print(table(cps_clean$employed_binary))

skim(cps_clean)

#Binary employment variable (employed vs unemployed)
#Transforming demographic characteristics and employment variables to binary variables. We also catagorized employment status in binary 
#for future regression modelling. 
cps_00013 <- cps_00013 %>%
  mutate(
    # Basic demographic variables
    mexhisp = if_else(hispan == 100, 1, 0),
    mexborn = if_else(bpl == 20000, 1, 0),
    mexican = if_else(mexhisp == 1 & mexborn == 1, 1, 0),
    age16 = if_else(age >= 16, 1, 0),
    hispano = if_else(hispan >= 100 & hispan <= 500, 1, 0),
    inmigra = if_else(yrimmig != 0, 1, 0),
    men = if_else(sex == 1, 1, 0),
    woman = if_else(sex == 2, 1, 0),
    wkswork2 = as.factor(wkswork2),
    
# Binary employment status (1 = employed, 0 = unemployed)
    employed_binary = case_when(
      wkstat %in% c(11, 14, 15) ~ 1,  # Employed
      wkstat %in% c(12, 20, 21, 22) ~ 0,  # Unemployed
      TRUE ~ NA_real_
    )
  )

#Creating a new dataset excluding rows with NA values and those not in labor force
cps_clean <- cps_00013 %>%
  filter(!is.na(employed_binary), !is.na(mexican), !is.na(age16), age >= 16, !is.na(inmigra), !is.na(wkswork2), !is.na(inctot))

cps_clean <- cps_clean %>% mutate(row_id = row_number())

# Summary stats
summary_stats <- cps_clean %>%
  summarise(
    total_observations = n(),
    employment_rate_mean = mean(employed_binary) * 100,
    pct_mexican_mean = mean(mexican) * 100,
    pct_immigrant_mean = mean(inmigra) * 100,
    pct_male_mean = mean(men) * 100,
    mean_age_mean = mean(age, na.rm = TRUE),
    median_age_mean = median(age, na.rm = TRUE),
    mean_wksworked = mean(wkswork2),
    mean_inctot = mean(inctot)
    
  )

#summary stats show: 
# 1. Around 88% of the sample is employed, indicating strong employment rate
# 2. Only around 5.58% of the sample is Mexican
# 3. 19.4% are immigrants indicating that non-Mexican immigrants are about 13.82% (19.4% - 5.58%)
# 4. 55.5% are males, indicating a slight skew
# 5. Middle-aged population shown by mean age of 42
print(summary_stats)

counts <- table(cps_clean$wkswork2)
print(counts)

#Train Test Split (Tr=0.8, Te=0.2)
#Training dataset rows = 791,714, columns = 45
#Testing dataset rows = 197,929, columns = 45
training_dataset  <- cps_clean %>% dplyr::sample_frac(0.8) 
print (training_dataset) 
print ("Testing Dataset") 
testing_dataset <- dplyr::anti_join(cps_clean, 
                                      training_dataset, by = 'row_id') 
print (testing_dataset)

#Employment % by demography
demographic_summary <- cps_clean %>%
  summarise(
    overall_emp_rate = mean(employed_binary) * 100,
    mexican_emp_rate = mean(employed_binary[mexican == 1]) * 100,
    nonmexican_emp_rate = mean(employed_binary[mexican == 0]) * 100,
    immigrant_emp_rate = mean(employed_binary[inmigra == 1]) * 100,
    nonimmigrant_emp_rate = mean(employed_binary[inmigra == 0]) * 100,
    male_emp_rate = mean(employed_binary[men == 1]) * 100,
    female_emp_rate = mean(employed_binary[men == 0]) * 100
  )

print(demographic_summary)
#Summary stats show: 
#1. The overall employment rate of this sample is 88.1% indicting high employment rates. 
#The breakdown of overall employment rate shows mexican-born are employed at the rate of 83.7%, 
#while non mexican born are employed at a rate of 88.4%. There's a 4.7 percentage point lower 
#employment rate for Mexican-born individuals compared to non-Mexicans/ 
#2. The overall employment of immigrants is 87%, while the employment rate of non-immigrants is 88.4%. 
#There's a small 1.4 percentage point difference, with immigrants having slightly lower employment rates. 
#3. All groups maintain relatively high employment rates above 86% and since these rates are all relatively high, 
#it suggests our sample is focused on labor force participants.

# Age group
age_analysis <- cps_clean %>%
  mutate(age_group = cut(age, 
                         breaks = c(0, 25, 35, 45, 55, Inf),
                         labels = c("16-25", "26-35", "36-45", "46-55", "55+"))) %>%
  group_by(age_group) %>%
  summarise(
    employment_rate = mean(employed_binary) * 100,
    n = n()
  )

print(age_analysis)
#Stats show: 
#1. Expected age progression in employment rates lowest for youngest, peaks at middle age
#2. Individuals in the age range of 46-55 have the highest employment rates (89.4%) with those 
# between ages 36-45 being a close second
#3. Ages 16-25 have the lowest employment rates (81.1%)
#4. There is a gradual decline of employment rates after 55 years old which can be due to retirement 
#5. The sample is fairly evenly distributed across working-age adults, with smaller representation of young workers

# Binomial logistic regression
#full model
full_model <- glm(employed_binary ~ wkswork2 + inctot + mexican + age + inmigra + men, family = binomial(link = "logit"), data = training_dataset)

#null model
null_model <- glm(employed_binary ~ 1, family = binomial(link = "logit"), data = training_dataset)

#stepwise model
stepmodcheck2 = step(null_model, direction='both', scope=formula(full_model), trace=1, k = log(nrow(cps_clean)))

# Model summary with odds ratios
model_summary <- tidy(stepmodcheck2) %>%
  mutate(
    odds_ratio = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  )

print("\nLogistic Regression Results (From Null on):")
print(model_summary)
print(summary(stepmodcheck2))

#Likelihood Ratio Test
anova(null_model, stepmodcheck2, test="LRT")

#Summary stats show: 
#1. Lower weeks worked have lesser odds of being employed, higher weeks worked (25,26) have 
# significantly higher odds of employment 
#2. Total income has a small positive effect on employment 
#3. 20% increase in employment odds for men compared to women 
#4. Mexican origin decreases employment odds by 21.7% compared to those with non-Mexican origins
#5. In summary: weeks worked and gender are the strongest predictors of employment 

classification_report <- function(y_true, y_pred) {
  # Check inputs
  if (!is.factor(y_true)) y_true <- as.factor(y_true)
  if (!is.factor(y_pred)) y_pred <- as.factor(y_pred)
  
  # Confusion Matrix
  cm <- table(y_true, y_pred)
  print(length(y_true))
  print(length(y_pred))
  cm_get <- caret::confusionMatrix(cm, mode = "everything")
  print("GOT HERE")
  print(cm_get)
  
  print(Metrics::recall(y_true, y_pred))
  
  # to calculate precision
  print(Metrics::precision(y_true, y_pred))
y_true
  # to calculate f1_score
  print(Metrics::f1(y_true, y_pred))
  
  return(cm_get)
  
}



train_data = training_dataset %>% dplyr::select(wkswork2, inctot, men, mexican)
train_pred_data = if_else(predict(stepmodcheck2, train_data, type="response") >= sum(training_dataset$employed_binary == 1) / nrow(training_dataset), 1, 0)

print("TRAIN SET RESULTS")
report <- classification_report(training_dataset$employed_binary, train_pred_data)
#1. Dataset: 34.16% employment indicating imbalanced dataset (skewed towards unemployed cases)
#2. Sensitivity: The sensitivity of this model is 19.06% showing a slight struggle to correctly 
# predict employed individuals (might be due to skewed dataset)
#3. Specificity: Model does well with predicting unemployed individuals (shown by 91.81%)
#4. Precision: 54.71% showing struggle with predicting employed 
#5. Accuracy: 66.96% high but class imbalance leads to misleading stats 
#6. F1: 28.27% indicating room for improvement for predicting employed individuals 



test_data = testing_dataset %>% dplyr::select(wkswork2, inctot, men, mexican)
test_pred_data = if_else(predict(stepmodcheck2, test_data, type="response") >= sum(testing_dataset$employed_binary == 1) / nrow(testing_dataset), 1, 0)
#print(test_pred_data)
#print(testing_dataset$employed_binary)
print("TEST SET RESULTS")
report <- classification_report(testing_dataset$employed_binary, test_pred_data)
#1. Imbalanced dataset: low prevalance of employed individuals 
#2. Low sensitivity and high specificity (high number of false negatives): might be due to dataset being biased 
#3. 55.07% predictions are correct
#4. Accuracy is 66.82% 
#5. Balanced accuracy of 55.46% shows model performs better than an unbiased coin 


loglik <- logLik(stepmodcheck2)
n   <- attributes(loglik)$nobs # following user20650 recommendation 
p   <- attributes(loglik)$df # following user20650 recommendation
dev <- -2*as.numeric(loglik)
my_AIC  <- dev + 2 * p
my_AICc <- my_AIC + (2 * p^2 + 2 * p)/(n - p - 1)
my_BIC  <- dev +  p * log(n)
print(my_AICc)
print(my_BIC)
#1. AIC: 554735.1 
#2. BIC: 554850.9

# Employment rates by demography
cps_plot <- ggplot(cps_clean) +
  geom_bar(aes(x = factor(mexican), fill = factor(employed_binary)), 
           position = "fill") +
  scale_fill_discrete(name = "Employment Status",
                      labels = c("Unemployed", "Employed")) +
  labs(title = "Employment Status by Mexican Origin",
       x = "Mexican Origin (1 = Yes, 0 = No)",
       y = "Proportion") +
  theme_minimal()

print(cps_plot)

#Analysis based on Current Population Survey (CPS) data
#Random sample of 100,000 working-age individuals
#Employed binomial logistic regression to analyze employment probability focusing on labor force participants employed vs. unemployed
#All major demographic differences are statistically significant, with gender showing the strongest relationship, followed by Mexican origin.
#CPS plot demonstrates the employment gap between Mexican-born and non-Mexican workers, while maintaining high overall employment rates for both groups.

#These findings could be useful for:
#Identifying barriers to employment for Mexican-born workers
#Creating more effective job placement services for immigrant populations
#Addressing potential discrimination or structural barriers in hiring practices
#Assessing the impact of immigration on local labor markets
#Contributing to understanding of immigrant labor market integration

#Testing if the demographic summary differences are statistically significant with a Chi-square tests for independence
#Mexican vs Non-Mexican
mexican_test <- chisq.test(table(cps_clean$employed_binary, cps_clean$mexican))

#Immigrant vs Non-immigrant
immigrant_test <- chisq.test(table(cps_clean$employed_binary, cps_clean$inmigra))

#Male vs Female
gender_test <- chisq.test(table(cps_clean$employed_binary, cps_clean$men))


# Create summary table of results
test_results <- tibble(
  Comparison = c("Mexican vs Non-Mexican", "Immigrant vs Non-immigrant", "Male vs Female"),
  Chi_Square = c(mexican_test$statistic,
                 immigrant_test$statistic,
                 gender_test$statistic),
  P_Value = c(mexican_test$p.value,
              immigrant_test$p.value,
              gender_test$p.value),
  Significant = P_Value < 0.05
)

print(test_results)
#All differences are statistically significant
#Gender shows the strongest relationship with the largest chi-square
#Mexican status shows a moderate relationship
#Immigrant status shows the weakest relationship


#MORE EDA

# Age Distribution Histogram with Employment Status as Color
ggplot(cps_clean, aes(x = age, fill = as.factor(employed_binary))) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "blue"), 
                    name = "Employment Status", 
                    labels = c("Unemployed", "Employed")) +
  labs(title = "Age Distribution by Employment Status",
       x = "Age",
       y = "Count") +
  theme_minimal()

# Age Distribution Density Plot with Employment Status as Color
ggplot(cps_clean, aes(x = age, color = as.factor(employed_binary), fill = as.factor(employed_binary))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("red", "blue"), 
                    name = "Employment Status", 
                    labels = c("Unemployed", "Employed")) +
  scale_color_manual(values = c("red", "blue"), 
                     name = "Employment Status", 
                     labels = c("Unemployed", "Employed")) +
  labs(title = "Density Plot of Age by Employment Status",
       x = "Age",
       y = "Density") +
  theme_minimal()

# Employment Status by Gender
ggplot(cps_clean, aes(x = as.factor(men), fill = as.factor(employed_binary))) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("red", "blue"), 
                    name = "Employment Status", 
                    labels = c("Unemployed", "Employed")) +
  scale_x_discrete(labels = c("0" = "Women", "1" = "Men")) +
  labs(title = "Employment Status by Gender",
       x = "Gender",
       y = "Proportion") +
  theme_minimal()

#Employment Status by Immigration Status
ggplot(cps_clean, aes(x = as.factor(inmigra), fill = as.factor(employed_binary))) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("red", "blue"), 
                    name = "Employment Status", 
                    labels = c("Unemployed", "Employed")) +
  scale_x_discrete(labels = c("0" = "NIU", "1" = "Available")) +
  labs(title = "Employment Status by Immigration Status",
       x = "Immigration Year Availability",
       y = "Proportion") +
  theme_minimal()

#Insights: 
#1. A larger proportion of the male and female groups are employed, consistent with the employment rates previously explored
#2. Men have a slightly higher employment rate compared to women, and women have a higher rate of unemployment
#3. positive indication that employment dates mostly overpower unemployment



#Finding the AIC and Pseudo R^2
#install.packages("pscl")

# AIC
aic_result <- AIC(stepmodcheck2)
cat("AIC:", aic_result, "\n")

# Pseudo-R²
library(pscl)  # Load the pscl package for Pseudo-R²
pseudo_r2 <- pR2(stepmodcheck2)
cat("Pseudo-R² (McFadden):", pseudo_r2["McFadden"], "\n")
cat("Pseudo-R² (Cox and Snell):", pseudo_r2["Cox and Snell"], "\n")
cat("Pseudo-R² (Nagelkerke):", pseudo_r2["Nagelkerke"], "\n")
