# Day 02 - Healthcare
# Initial data quality checks

library(tidyverse)
library(janitor)

# Data Loading & Structure
elective_surgery <- readr::read_csv(
  "02_healthcare/data/aihw_elective_surgery.csv",
  show_col_types = FALSE
) %>%
  clean_names()

glimpse(elective_surgery)

names(elective_surgery)

# Missing Values
missing_summary <- elective_surgery %>%
  summarise(across(everything(), ~ sum(is.na(.))))

missing_summary

missing_summary_sorted <- sort(unlist(missing_summary), decreasing = TRUE)

missing_summary_sorted

# Duplicates
duplicate_count <- sum(duplicated(elective_surgery))

duplicate_count

duplicate_rows <- janitor::get_dupes(elective_surgery)

duplicate_rows
