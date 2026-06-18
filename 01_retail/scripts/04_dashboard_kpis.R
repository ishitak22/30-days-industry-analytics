# Day 01 - Retail
# Dashboard KPI calculations
# This script creates reusable KPI tables for Quarto and Shiny.
# No charts or additional business analysis are created here.

library(tidyverse)

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Load engineered dataset created in scripts/03_feature_engineering.R.
retail_sales_engineered <- read_csv(
  "outputs/retail_sales_engineered.csv",
  show_col_types = FALSE
)

# 1. Overall dashboard KPIs
# Use this table for executive KPI cards in the final dashboard.
dashboard_kpis <- retail_sales_engineered %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    total_units_sold = sum(quantity),
    average_order_value = total_revenue / total_transactions
  )

write_csv(dashboard_kpis, "outputs/dashboard_kpis.csv")

# 2. Revenue by category
# Use this table for product category KPI cards, tables, or dashboard filters.
revenue_by_category <- retail_sales_engineered %>%
  group_by(product_category) %>%
  summarise(
    total_revenue = sum(total_amount),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

write_csv(revenue_by_category, "outputs/kpi_revenue_by_category.csv")

# 3. Revenue by gender
# Use this table for customer segment KPI summaries in the dashboard.
revenue_by_gender <- retail_sales_engineered %>%
  group_by(gender) %>%
  summarise(
    total_revenue = sum(total_amount),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

write_csv(revenue_by_gender, "outputs/kpi_revenue_by_gender.csv")

# 4. Revenue by age group
# Use this table for age-segment KPI summaries in the dashboard.
revenue_by_age_group <- retail_sales_engineered %>%
  group_by(age_group) %>%
  summarise(
    total_revenue = sum(total_amount),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

write_csv(revenue_by_age_group, "outputs/kpi_revenue_by_age_group.csv")

# Print outputs for a quick analyst review when running the script.
dashboard_kpis
revenue_by_category
revenue_by_gender
revenue_by_age_group
