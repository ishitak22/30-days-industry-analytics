# Day 01 - Retail
# Category performance analytics layer
# Focus: product category contribution to business performance

library(tidyverse)

# Create outputs folder for dashboard-ready summary tables.
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

if (!exists("retail_sales_clean")) {
  retail_sales_clean <- read_csv("01_retail/data/retail_sales.csv", show_col_types = FALSE) %>%
    rename_with(
      ~ .x %>%
        str_to_lower() %>%
        str_replace_all("[^a-z0-9]+", "_") %>%
        str_replace_all("^_|_$", "")
    ) %>%
    drop_na() %>%
    distinct()
}

# 1. Revenue by category
# Business interpretation:
# Shows which product categories generate the most total sales revenue.
# Dashboard use:
# Feed an executive category revenue bar chart and identify the top revenue driver.
revenue_by_category <- retail_sales_clean %>%
  group_by(product_category) %>%
  summarise(
    total_revenue = sum(total_amount),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

write_csv(revenue_by_category, "outputs/revenue_by_category.csv")

# 2. Transactions by category
# Business interpretation:
# Shows which product categories attract the highest number of purchases.
# Dashboard use:
# Compare transaction volume against revenue to separate popular categories
# from high-value categories.
transactions_by_category <- retail_sales_clean %>%
  group_by(product_category) %>%
  summarise(
    total_transactions = n_distinct(transaction_id),
    .groups = "drop"
  ) %>%
  arrange(desc(total_transactions))

write_csv(transactions_by_category, "outputs/transactions_by_category.csv")

# 3. Units sold by category
# Business interpretation:
# Shows which categories move the highest product volume.
# Dashboard use:
# Support merchandising and inventory views by highlighting volume-heavy categories.
units_sold_by_category <- retail_sales_clean %>%
  group_by(product_category) %>%
  summarise(
    units_sold = sum(quantity),
    .groups = "drop"
  ) %>%
  arrange(desc(units_sold))

write_csv(units_sold_by_category, "outputs/units_sold_by_category.csv")

# 4. Average order value by category
# Business interpretation:
# Shows the average revenue generated per transaction in each category.
# Dashboard use:
# Reveal whether a category wins through higher-value baskets rather than volume.
average_order_value_by_category <- retail_sales_clean %>%
  group_by(product_category) %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    average_order_value = total_revenue / total_transactions,
    .groups = "drop"
  ) %>%
  arrange(desc(average_order_value))

write_csv(
  average_order_value_by_category,
  "outputs/average_order_value_by_category.csv"
)

# 5. Revenue share by category
# Business interpretation:
# Shows each category's percentage contribution to total business revenue.
# Dashboard use:
# Feed a category revenue share card, table, or composition chart.
revenue_share_by_category <- retail_sales_clean %>%
  group_by(product_category) %>%
  summarise(
    total_revenue = sum(total_amount),
    .groups = "drop"
  ) %>%
  mutate(
    revenue_share = total_revenue / sum(total_revenue)
  ) %>%
  arrange(desc(revenue_share))

write_csv(revenue_share_by_category, "outputs/revenue_share_by_category.csv")

# 6. Category ranking table
# Business interpretation:
# Combines revenue, transaction volume, units sold, average order value,
# and revenue share to rank categories by overall business contribution.
# Dashboard use:
# Power a category performance table in the final Quarto report or Shiny dashboard.
category_ranking <- retail_sales_clean %>%
  group_by(product_category) %>%
  summarise(
    total_revenue = sum(total_amount),
    total_transactions = n_distinct(transaction_id),
    units_sold = sum(quantity),
    average_order_value = total_revenue / total_transactions,
    .groups = "drop"
  ) %>%
  mutate(
    revenue_share = total_revenue / sum(total_revenue),
    revenue_rank = min_rank(desc(total_revenue)),
    transaction_rank = min_rank(desc(total_transactions)),
    units_rank = min_rank(desc(units_sold)),
    aov_rank = min_rank(desc(average_order_value))
  ) %>%
  arrange(revenue_rank)

write_csv(category_ranking, "outputs/category_ranking.csv")


