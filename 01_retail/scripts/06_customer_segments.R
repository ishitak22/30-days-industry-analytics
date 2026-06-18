# Day 01 - Retail
# Customer segment analysis
# Focus: customer value and behaviour by gender and age group
# No time trend analysis, charts, or dashboard code.

library(tidyverse)

# Load engineered dataset created in scripts/03_feature_engineering.R.
retail_sales_engineered <- read_csv(
  "outputs/retail_sales_engineered.csv",
  show_col_types = FALSE
)

# 1. Revenue by gender
# Business interpretation:
# Shows which gender segment contributes more total sales revenue.
# Dashboard use:
# Feed customer segment KPI cards, tables, or comparison views.
revenue_by_gender <- retail_sales_engineered %>%
  group_by(gender) %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    total_units_sold = sum(quantity),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

write_csv(revenue_by_gender, "outputs/customer_revenue_by_gender.csv")

# 2. Revenue by age group
# Business interpretation:
# Shows which age groups contribute the most revenue to the business.
# Dashboard use:
# Support customer value segmentation and age-based targeting views.
revenue_by_age_group <- retail_sales_engineered %>%
  group_by(age_group) %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    total_units_sold = sum(quantity),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

write_csv(revenue_by_age_group, "outputs/customer_revenue_by_age_group.csv")

# 3. Average order value by gender
# Business interpretation:
# Shows whether one gender segment generates higher revenue per transaction.
# Dashboard use:
# Help distinguish customer value from customer volume.
average_order_value_by_gender <- retail_sales_engineered %>%
  group_by(gender) %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    average_order_value = total_revenue / total_transactions,
    .groups = "drop"
  ) %>%
  arrange(desc(average_order_value))

write_csv(
  average_order_value_by_gender,
  "outputs/customer_average_order_value_by_gender.csv"
)

# 4. Average order value by age group
# Business interpretation:
# Shows which age groups spend more per transaction.
# Dashboard use:
# Identify high-value age segments for executive customer insights.
average_order_value_by_age_group <- retail_sales_engineered %>%
  group_by(age_group) %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    average_order_value = total_revenue / total_transactions,
    .groups = "drop"
  ) %>%
  arrange(desc(average_order_value))

write_csv(
  average_order_value_by_age_group,
  "outputs/customer_average_order_value_by_age_group.csv"
)

# 5. Category preferences by gender
# Business interpretation:
# Shows how each gender segment distributes revenue and transactions across
# product categories.
# Dashboard use:
# Feed customer preference tables for segment-specific merchandising insights.
category_preferences_by_gender <- retail_sales_engineered %>%
  group_by(gender, product_category) %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    total_units_sold = sum(quantity),
    average_order_value = total_revenue / total_transactions,
    .groups = "drop"
  ) %>%
  group_by(gender) %>%
  mutate(
    revenue_share_within_gender = total_revenue / sum(total_revenue),
    preference_rank = min_rank(desc(total_revenue))
  ) %>%
  ungroup() %>%
  arrange(gender, preference_rank)

write_csv(
  category_preferences_by_gender,
  "outputs/customer_category_preferences_by_gender.csv"
)

# 6. Category preferences by age group
# Business interpretation:
# Shows which product categories matter most within each age group.
# Dashboard use:
# Support age-segment merchandising and targeting views in the final dashboard.
category_preferences_by_age_group <- retail_sales_engineered %>%
  group_by(age_group, product_category) %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    total_units_sold = sum(quantity),
    average_order_value = total_revenue / total_transactions,
    .groups = "drop"
  ) %>%
  group_by(age_group) %>%
  mutate(
    revenue_share_within_age_group = total_revenue / sum(total_revenue),
    preference_rank = min_rank(desc(total_revenue))
  ) %>%
  ungroup() %>%
  arrange(age_group, preference_rank)

write_csv(
  category_preferences_by_age_group,
  "outputs/customer_category_preferences_by_age_group.csv"
)
