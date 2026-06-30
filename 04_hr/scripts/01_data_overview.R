# ============================================================
# 01_data_overview.R
# HR Employee Attrition - Initial Data Overview
#
# Purpose:
# Explore the dataset before cleaning, analysis, visualisation,
# DAX measures, or Power BI dashboard development.
# ============================================================


# ============================================================
# 1. Load required packages
# ============================================================

library(tidyverse)


# ============================================================
# 2. Load the dataset from the data folder
# ============================================================

data_path <- "data/HR-Employee-Attrition.csv"

if (!file.exists(data_path)) {
  data_path <- "../data/HR-Employee-Attrition.csv"
}

hr_data <- readr::read_csv(data_path, show_col_types = FALSE)


# ============================================================
# 3. Preview the dataset
# ============================================================

# View the first few rows
cat("\nFIRST FEW ROWS\n")
print(head(hr_data))

# Display the last few rows
cat("\nLAST FEW ROWS\n")
print(tail(hr_data))

# Check dimensions
cat("\nDATASET DIMENSIONS\n")
cat("Rows:", nrow(hr_data), "\n")
cat("Columns:", ncol(hr_data), "\n")

# Display column names
cat("\nCOLUMN NAMES\n")
print(names(hr_data))


# ============================================================
# 4. Understand the dataset structure
# ============================================================

# Quick structure view
cat("\nGLIMPSE\n")
glimpse(hr_data)

# Check variable types
cat("\nVARIABLE TYPES\n")
variable_types <- tibble(
  variable = names(hr_data),
  type = map_chr(hr_data, ~ class(.x)[1])
)
print(variable_types)

# Generate summary statistics
cat("\nSUMMARY STATISTICS\n")
print(summary(hr_data))


# ============================================================
# 5. Data quality assessment
# ============================================================

# Missing values by column
cat("\nMISSING VALUES BY COLUMN\n")
missing_values <- hr_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  ) %>%
  arrange(desc(missing_values))

print(missing_values)

# Duplicate rows
cat("\nDUPLICATE ROWS\n")
duplicate_rows <- nrow(hr_data) - nrow(distinct(hr_data))
cat("Duplicate rows:", duplicate_rows, "\n")

# Count unique values for every variable
cat("\nUNIQUE VALUES PER VARIABLE\n")
unique_values <- hr_data %>%
  summarise(across(everything(), n_distinct)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "unique_values"
  ) %>%
  arrange(unique_values)

print(unique_values)

# Identify columns with only one unique value
cat("\nCOLUMNS WITH ONLY ONE UNIQUE VALUE\n")
single_value_columns <- unique_values %>%
  filter(unique_values == 1)

print(single_value_columns)

# Identify likely identifier columns
cat("\nLIKELY IDENTIFIER COLUMNS\n")
likely_identifier_columns <- unique_values %>%
  filter(unique_values == nrow(hr_data))

print(likely_identifier_columns)

# Check for obvious inconsistencies in common business fields
cat("\nOBVIOUS INCONSISTENCY CHECKS\n")

cat("Attrition values:\n")
print(hr_data %>% count(Attrition))

cat("\nAge range:\n")
print(hr_data %>% summarise(min_age = min(Age), max_age = max(Age)))

cat("\nMonthly income range:\n")
print(hr_data %>% summarise(
  min_monthly_income = min(MonthlyIncome),
  max_monthly_income = max(MonthlyIncome),
  avg_monthly_income = mean(MonthlyIncome)
))

cat("\nYears at company range:\n")
print(hr_data %>% summarise(
  min_years_at_company = min(YearsAtCompany),
  max_years_at_company = max(YearsAtCompany),
  avg_years_at_company = mean(YearsAtCompany)
))

cat("\nConstant fields to review:\n")
print(single_value_columns)


# ============================================================
# 6. Explore categorical variables
# ============================================================

# Frequency tables for key categorical variables
cat("\nATTRITION FREQUENCY\n")
print(hr_data %>% count(Attrition) %>% mutate(percent = n / sum(n)))

cat("\nDEPARTMENT FREQUENCY\n")
print(hr_data %>% count(Department) %>% mutate(percent = n / sum(n)))

cat("\nJOB ROLE FREQUENCY\n")
print(hr_data %>% count(JobRole) %>% mutate(percent = n / sum(n)))

cat("\nGENDER FREQUENCY\n")
print(hr_data %>% count(Gender) %>% mutate(percent = n / sum(n)))

cat("\nOVERTIME FREQUENCY\n")
print(hr_data %>% count(OverTime) %>% mutate(percent = n / sum(n)))

cat("\nBUSINESS TRAVEL FREQUENCY\n")
print(hr_data %>% count(BusinessTravel) %>% mutate(percent = n / sum(n)))

cat("\nMARITAL STATUS FREQUENCY\n")
print(hr_data %>% count(MaritalStatus) %>% mutate(percent = n / sum(n)))

cat("\nEDUCATION FIELD FREQUENCY\n")
print(hr_data %>% count(EducationField) %>% mutate(percent = n / sum(n)))

# Identify class imbalance for the main outcome variable
cat("\nATTRITION CLASS BALANCE\n")
attrition_balance <- hr_data %>%
  count(Attrition) %>%
  mutate(percent = n / sum(n))

print(attrition_balance)


# ============================================================
# 7. Explore numerical variables
# ============================================================

# Basic descriptive statistics for numeric variables
cat("\nNUMERIC VARIABLE SUMMARY\n")
numeric_summary <- hr_data %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    average = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(variable)

print(numeric_summary)

# Identify potential outliers using a simple IQR rule.
# This is only a flag for review, not a cleaning decision.
cat("\nPOTENTIAL OUTLIERS USING IQR RULE\n")
outlier_summary <- hr_data %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower_bound = q1 - 1.5 * iqr,
    upper_bound = q3 + 1.5 * iqr,
    is_potential_outlier = value < lower_bound | value > upper_bound
  ) %>%
  summarise(
    potential_outliers = sum(is_potential_outlier, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(potential_outliers))

print(outlier_summary)


# ============================================================
# 8. Initial business observations
# ============================================================

# The dataset appears to represent employee-level HR records.
# Each row is one employee, with information about demographics,
# job role, department, compensation, satisfaction, overtime,
# training, tenure, promotion history, and attrition status.

# The main business outcome variable is likely Attrition, because it
# indicates whether an employee left the organisation. Other useful
# business outcomes or monitoring fields may include JobSatisfaction,
# WorkLifeBalance, PerformanceRating, MonthlyIncome, and OverTime.

# Variables likely to be useful for analysis include:
# - Department and JobRole for workforce segmentation
# - Attrition for turnover analysis
# - MonthlyIncome, JobLevel, PercentSalaryHike, and StockOptionLevel
#   for compensation and reward analysis
# - OverTime, BusinessTravel, DistanceFromHome, and WorkLifeBalance
#   for work conditions and retention risk
# - JobSatisfaction, EnvironmentSatisfaction, JobInvolvement, and
#   RelationshipSatisfaction for employee experience analysis
# - YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion,
#   and YearsWithCurrManager for tenure and career progression

# Variables that may not add analytical value:
# - EmployeeCount has only one value
# - Over18 has only one value
# - StandardHours has only one value
# - EmployeeNumber is useful as an identifier, but should not be used
#   as a business driver or chart category

# Data quality concerns noticed during this exploration:
# - No missing values were found
# - No duplicate rows were found
# - Attrition is imbalanced, with many more current employees than
#   employees who left
# - Some fields are coded rating scales, so their meaning should be
#   documented before dashboard storytelling
# - DailyRate, HourlyRate, and MonthlyRate may need business definition
#   checks before being used as headline compensation metrics
