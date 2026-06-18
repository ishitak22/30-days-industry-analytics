# Day 01 - Retail
# Feature engineering for Quarto and Shiny dashboard development
# No analysis or visualisations are created in this script.

library(tidyverse)
library(lubridate)

# Load and prepare the cleaned dataset for downstream dashboard work.
retail_sales_engineered <- read_csv("01_retail/data/retail_sales.csv", show_col_types = FALSE) %>%
  rename_with(
    ~ .x %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_replace_all("^_|_$", "")
  ) %>%
  drop_na() %>%
  distinct() %>%
  mutate(
    # Month supports monthly filters, grouped summaries, and dashboard trend views.
    month = month(date),

    # Month name gives readable labels for reports, slicers, and chart axes.
    month_name = month(date, label = TRUE, abbr = FALSE),

    # Year-month provides a stable monthly period key for Quarto and Shiny summaries.
    year_month = format(date, "%Y-%m"),

    # Quarter enables executive-level period filtering and quarterly summaries.
    quarter = paste0("Q", quarter(date)),

    # Age group turns individual ages into business-friendly customer segments.
    age_group = case_when(
      age >= 18 & age <= 24 ~ "18-24",
      age >= 25 & age <= 34 ~ "25-34",
      age >= 35 & age <= 44 ~ "35-44",
      age >= 45 & age <= 54 ~ "45-54",
      age >= 55 & age <= 64 ~ "55-64",
      age >= 65 ~ "65+",
      TRUE ~ "Unknown"
    ),

    # Transaction value band separates lower, mid, and high-value purchases.
    transaction_value_band = case_when(
      total_amount < 100 ~ "Low",
      total_amount >= 100 & total_amount < 500 ~ "Medium",
      total_amount >= 500 ~ "High",
      TRUE ~ "Unknown"
    ),

    # Revenue per unit supports pricing and basket-value comparisons in the dashboard.
    revenue_per_unit = total_amount / quantity
  )

write_csv(retail_sales_engineered, "outputs/retail_sales_engineered.csv")

glimpse(retail_sales_engineered)
