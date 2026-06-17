# Day 01 - Retail
# Data cleaning only: no analysis or visualisations

library(tidyverse)

# 1. Load dataset
retail_sales_raw <- read_csv("01_retail/data/retail_sales.csv", show_col_types = FALSE)

# 2. Inspect raw data
glimpse(retail_sales_raw)
summary(retail_sales_raw)

# 3. Clean column names
retail_sales_clean <- retail_sales_raw %>%
  rename_with(
    ~ .x %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_replace_all("^_|_$", "")
  )

# 4. Handle missing values
retail_sales_clean <- retail_sales_clean %>%
  drop_na()

# 5. Remove duplicate rows
retail_sales_clean <- retail_sales_clean %>%
  distinct()

# 6. Inspect cleaned data
glimpse(retail_sales_clean)
