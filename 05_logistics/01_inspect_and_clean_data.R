# ============================================================
# 01_inspect_and_clean_data.R
# Logistics & Supply Chain - Initial Data Inspection
#
# ============================================================
# 1. Load required packages
# ============================================================

library(tidyverse)
library(lubridate)


# ============================================================
# 2. Load the dataset from the data folder
# ============================================================

data_path <- "data/DataCoSupplyChainDataset.csv"

if (!file.exists(data_path)) {
  data_path <- "05_logistics/data/DataCoSupplyChainDataset.csv"
}

data_folder <- dirname(data_path)

supply_chain <- readr::read_csv(
  data_path,
  locale = readr::locale(encoding = "Latin1"),
  na = c("", "NA"),
  show_col_types = FALSE
)


# ============================================================
# 3. Basic structure checks
# ============================================================

cat("\nDATASET DIMENSIONS\n")
cat("Rows:", nrow(supply_chain), "\n")
cat("Columns:", ncol(supply_chain), "\n")

cat("\nCOLUMN NAMES\n")
print(names(supply_chain))

cat("\nDATA TYPES\n")
column_types <- tibble(
  column = names(supply_chain),
  type = map_chr(supply_chain, ~ class(.x)[1])
)
print(column_types)

cat("\nGLIMPSE\n")
glimpse(supply_chain)


# ============================================================
# 4. Missing values and duplicate rows
# ============================================================

cat("\nMISSING VALUES BY COLUMN\n")
missing_values <- supply_chain %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column",
    values_to = "missing_values"
  ) %>%
  arrange(desc(missing_values))

print(missing_values)

cat("\nDUPLICATE ROWS\n")
duplicate_rows <- nrow(supply_chain) - nrow(distinct(supply_chain))
cat("Duplicate rows:", duplicate_rows, "\n")


# ============================================================
# 5. Date columns
# ============================================================

cat("\nDATE COLUMN CHECKS\n")

date_columns <- names(supply_chain)[str_detect(str_to_lower(names(supply_chain)), "date")]

date_checks <- map_dfr(date_columns, function(column_name) {
  parsed_dates <- lubridate::mdy_hm(supply_chain[[column_name]], quiet = TRUE)

  tibble(
    column = column_name,
    missing_raw_values = sum(is.na(supply_chain[[column_name]])),
    unparsed_non_missing_values = sum(is.na(parsed_dates) & !is.na(supply_chain[[column_name]])),
    min_date = min(parsed_dates, na.rm = TRUE),
    max_date = max(parsed_dates, na.rm = TRUE)
  )
})

print(date_checks)


# ============================================================
# 6. Numeric columns
# ============================================================

cat("\nNUMERIC COLUMN SUMMARY\n")

numeric_summary <- supply_chain %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column",
    values_to = "value"
  ) %>%
  group_by(column) %>%
  summarise(
    missing_values = sum(is.na(value)),
    min = min(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    average = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(column)

print(numeric_summary)


# ============================================================
# 7. Categorical columns
# ============================================================

cat("\nCATEGORICAL COLUMN SUMMARY\n")

categorical_summary <- supply_chain %>%
  select(where(~ is.character(.x) || is.factor(.x))) %>%
  summarise(across(everything(), ~ n_distinct(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column",
    values_to = "unique_values"
  ) %>%
  arrange(unique_values)

print(categorical_summary)

cat("\nKEY CATEGORICAL VALUE COUNTS\n")

key_categorical_columns <- c(
  "Type",
  "Delivery Status",
  "Customer Country",
  "Customer Segment",
  "Market",
  "Order Status",
  "Shipping Mode"
)

for (column_name in key_categorical_columns) {
  cat("\n", column_name, "\n", sep = "")
  print(
    supply_chain %>%
      count(.data[[column_name]], sort = TRUE)
  )
}


# ============================================================
# 8. Obvious inconsistent value checks
# ============================================================

cat("\nOBVIOUS INCONSISTENT VALUE CHECKS\n")

problem_checks <- tibble(
  check = c(
    "Negative real shipping days",
    "Negative scheduled shipping days",
    "Late delivery risk outside 0/1",
    "Negative item quantity",
    "Negative sales",
    "Negative product price",
    "Negative discount",
    "Discount rate outside 0 to 1",
    "Latitude outside -90 to 90",
    "Longitude outside -180 to 180",
    "Product status outside 0/1",
    "Unparsed non-missing date values"
  ),
  issue_count = c(
    sum(supply_chain[["Days for shipping (real)"]] < 0, na.rm = TRUE),
    sum(supply_chain[["Days for shipment (scheduled)"]] < 0, na.rm = TRUE),
    sum(!supply_chain[["Late_delivery_risk"]] %in% c(0, 1), na.rm = TRUE),
    sum(supply_chain[["Order Item Quantity"]] < 0, na.rm = TRUE),
    sum(supply_chain[["Sales"]] < 0, na.rm = TRUE),
    sum(supply_chain[["Product Price"]] < 0, na.rm = TRUE),
    sum(supply_chain[["Order Item Discount"]] < 0, na.rm = TRUE),
    sum(
      supply_chain[["Order Item Discount Rate"]] < 0 |
        supply_chain[["Order Item Discount Rate"]] > 1,
      na.rm = TRUE
    ),
    sum(
      supply_chain[["Latitude"]] < -90 |
        supply_chain[["Latitude"]] > 90,
      na.rm = TRUE
    ),
    sum(
      supply_chain[["Longitude"]] < -180 |
        supply_chain[["Longitude"]] > 180,
      na.rm = TRUE
    ),
    sum(!supply_chain[["Product Status"]] %in% c(0, 1), na.rm = TRUE),
    sum(date_checks$unparsed_non_missing_values)
  )
)

print(problem_checks)


# ============================================================
# 9. Cleaning decision
# ============================================================

# Inspection notes from the current dataset:
# - The file contains 180,519 rows and 53 columns.
# - No duplicate rows were found.
# - The two date columns parse successfully.
# - Basic numeric ranges are plausible for this supply chain dataset.
# - Product Description is fully missing, and Order Zipcode is mostly missing.
#   These are coverage limitations, not issues that can be reliably cleaned.
# - Customer Email and Customer Password are masked as XXXXXXXXX, which is
#   appropriate for privacy and should not be changed.
#
# Because no clearly justified cleaning step was found, the raw dataset is
# intentionally left unchanged. No cleaned CSV is exported by this script.
