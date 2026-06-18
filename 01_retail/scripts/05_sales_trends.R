# Day 01 - Retail
# Sales trends analysis
# Focus: time-based sales performance only
# No customer analysis, product category analysis, charts, or dashboard code.

library(tidyverse)

# Load engineered dataset created in scripts/03_feature_engineering.R.
retail_sales_engineered <- read_csv(
  "outputs/retail_sales_engineered.csv",
  show_col_types = FALSE
)

# 1. Monthly revenue
# Business purpose:
# Shows how total sales value changes over time and identifies peak or weak
# revenue months for executive performance tracking.
monthly_revenue <- retail_sales_engineered %>%
  group_by(year_month) %>%
  summarise(
    total_revenue = sum(total_amount),
    .groups = "drop"
  ) %>%
  arrange(year_month)

write_csv(monthly_revenue, "outputs/monthly_revenue.csv")

# 2. Monthly transactions
# Business purpose:
# Shows whether changes in sales performance are linked to more or fewer
# customer purchases.
monthly_transactions <- retail_sales_engineered %>%
  group_by(year_month) %>%
  summarise(
    total_transactions = n_distinct(transaction_id),
    .groups = "drop"
  ) %>%
  arrange(year_month)

write_csv(monthly_transactions, "outputs/monthly_transactions.csv")

# 3. Monthly units sold
# Business purpose:
# Shows product movement over time and helps distinguish revenue changes caused
# by volume from changes caused by order value.
monthly_units_sold <- retail_sales_engineered %>%
  group_by(year_month) %>%
  summarise(
    total_units_sold = sum(quantity),
    .groups = "drop"
  ) %>%
  arrange(year_month)

write_csv(monthly_units_sold, "outputs/monthly_units_sold.csv")

# 4. Monthly average order value
# Business purpose:
# Shows how much revenue each transaction generates on average each month.
# This helps identify whether revenue changes are driven by larger baskets.
monthly_average_order_value <- retail_sales_engineered %>%
  group_by(year_month) %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    average_order_value = total_revenue / total_transactions,
    .groups = "drop"
  ) %>%
  arrange(year_month)

write_csv(
  monthly_average_order_value,
  "outputs/monthly_average_order_value.csv"
)

# 5. Combined monthly sales trends table
# Business purpose:
# Creates one reusable time-based table for Quarto narrative sections and
# future Shiny dashboard trend cards or tables.
monthly_sales_trends <- retail_sales_engineered %>%
  group_by(year_month) %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    total_units_sold = sum(quantity),
    average_order_value = total_revenue / total_transactions,
    .groups = "drop"
  ) %>%
  arrange(year_month)

write_csv(monthly_sales_trends, "outputs/monthly_sales_trends.csv")

# 6. Strongest and weakest months
# Business purpose:
# Identifies the highest and lowest revenue periods for stakeholder reporting.
strongest_month <- monthly_sales_trends %>%
  slice_max(total_revenue, n = 1, with_ties = FALSE)

weakest_month <- monthly_sales_trends %>%
  slice_min(total_revenue, n = 1, with_ties = FALSE)

write_csv(strongest_month, "outputs/strongest_sales_month.csv")
write_csv(weakest_month, "outputs/weakest_sales_month.csv")

# 7. Performance driver comparison
# Business purpose:
# Compares the strongest and weakest revenue months across transaction volume,
# units sold, and average order value to understand what may be driving the gap.
performance_driver_comparison <- bind_rows(
  strongest_month %>% mutate(month_type = "Strongest month"),
  weakest_month %>% mutate(month_type = "Weakest month")
) %>%
  select(
    month_type,
    year_month,
    total_revenue,
    total_transactions,
    total_units_sold,
    average_order_value
  )

write_csv(
  performance_driver_comparison,
  "outputs/performance_driver_comparison.csv"
)

performance_driver_summary <- performance_driver_comparison %>%
  summarise(
    revenue_gap = total_revenue[month_type == "Strongest month"] -
      total_revenue[month_type == "Weakest month"],
    transaction_gap = total_transactions[month_type == "Strongest month"] -
      total_transactions[month_type == "Weakest month"],
    units_gap = total_units_sold[month_type == "Strongest month"] -
      total_units_sold[month_type == "Weakest month"],
    average_order_value_gap =
      average_order_value[month_type == "Strongest month"] -
      average_order_value[month_type == "Weakest month"]
  ) %>%
  mutate(
    transaction_pct_gap = transaction_gap /
      performance_driver_comparison$total_transactions[
        performance_driver_comparison$month_type == "Weakest month"
      ],
    units_pct_gap = units_gap /
      performance_driver_comparison$total_units_sold[
        performance_driver_comparison$month_type == "Weakest month"
      ],
    average_order_value_pct_gap = average_order_value_gap /
      performance_driver_comparison$average_order_value[
        performance_driver_comparison$month_type == "Weakest month"
      ]
  ) %>%
  mutate(
    primary_driver = case_when(
      abs(transaction_pct_gap) >= abs(units_pct_gap) &
        abs(transaction_pct_gap) >= abs(average_order_value_pct_gap) ~
        "Transaction volume",
      abs(units_pct_gap) >= abs(transaction_pct_gap) &
        abs(units_pct_gap) >= abs(average_order_value_pct_gap) ~ "Units sold",
      TRUE ~ "Average order value"
    )
  )

write_csv(
  performance_driver_summary,
  "outputs/performance_driver_summary.csv"
)