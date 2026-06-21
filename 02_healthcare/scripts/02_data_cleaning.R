# Day 02 - Healthcare
# Data cleaning

library(tidyverse)
library(janitor)

# Load data
elective_surgery <- readr::read_csv(
  "02_healthcare/data/aihw_elective_surgery.csv",
  show_col_types = FALSE
) %>%
  clean_names()

# Create working copy for cleaning
healthcare_clean <- elective_surgery

# Standardise missing text values
healthcare_clean <- healthcare_clean %>%
  mutate(
    across(
      where(is.character),
      ~ str_squish(.) %>%
        na_if("")
    )
  )

# Convert date fields
healthcare_clean <- healthcare_clean %>%
  mutate(
    reporting_start = as.Date(reporting_start),
    reporting_end = as.Date(reporting_end)
  )

# Convert numeric measure fields
healthcare_clean <- healthcare_clean %>%
  mutate(
    across(
      c(value, lower_value, upper_value, peer_value),
      as.numeric
    )
  )

# Remove exact duplicate rows
healthcare_clean <- healthcare_clean %>%
  distinct()

# Keep analysis-ready fields
healthcare_clean <- healthcare_clean %>%
  select(
    mapped_state,
    reporting_unit_code,
    reporting_unit_name,
    reporting_unit_type_code,
    reporting_unit_type_name,
    reporting_start,
    reporting_end,
    measure_category_code,
    measure_code,
    measure_name,
    reported_measure_category_code,
    reported_measure_category_name,
    reported_measure_category_two_code,
    reported_measure_category_two_name,
    reported_measure_category_three_code,
    reported_measure_category_three_name,
    reported_measure_code,
    reported_measure_name,
    units_display,
    units_name,
    value,
    lower_value,
    upper_value,
    caveat,
    caveat_codes,
    caveat_footnotes,
    suppression,
    suppression_codes,
    peer_group_name,
    peer_value
  )

# Final quality check
glimpse(healthcare_clean)

summary(healthcare_clean)

# Save cleaned dataset
dir.create("outputs", showWarnings = FALSE)

readr::write_csv(
  healthcare_clean,
  "outputs/elective_surgery_clean.csv"
)