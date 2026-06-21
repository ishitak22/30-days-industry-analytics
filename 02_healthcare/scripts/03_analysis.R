# Day 02 - Healthcare
# Analysis: elective surgery wait-time pressure by procedure

library(tidyverse)

# Load cleaned data
elective_surgery_clean <- readr::read_csv(
  "02_healthcare/data/elective_surgery_clean.csv",
  show_col_types = FALSE
)

# Focus on median waiting time records
median_wait_records <- elective_surgery_clean %>%
  filter(
    measure_name == "Median waiting time for elective surgery",
    !is.na(value)
  )

# Summarise waiting time pressure by reported procedure or surgical category
wait_time_by_procedure <- median_wait_records %>%
  group_by(reported_measure_name) %>%
  summarise(
    records = n(),
    reporting_hospitals = n_distinct(reporting_unit_name),
    average_median_wait_days = mean(value, na.rm = TRUE),
    highest_median_wait_days = max(value, na.rm = TRUE),
    latest_reporting_end = max(reporting_end, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(records >= 10) %>%
  arrange(desc(average_median_wait_days))

# Identify the procedures/categories with the highest sustained wait-time pressure
highest_wait_pressure_procedures <- wait_time_by_procedure %>%
  slice_head(n = 10)

# Business interpretation:
# These results show which elective surgery procedures or categories have the
# highest average median waiting times across available hospital reporting.
# For healthcare executives, this helps identify where access pressure is most
# persistent and where waitlist management or theatre capacity planning may need
# closer attention.

wait_time_by_procedure
highest_wait_pressure_procedures

# State-level wait-time analysis
wait_time_by_state <- median_wait_records %>%
  group_by(mapped_state) %>%
  summarise(
    average_median_wait_days = mean(value, na.rm = TRUE),
    maximum_median_wait_days = max(value, na.rm = TRUE),
    reporting_hospitals = n_distinct(reporting_unit_name),
    .groups = "drop"
  ) %>%
  arrange(desc(average_median_wait_days))

states_highest_average_wait <- wait_time_by_state %>%
  slice_head(n = 5)

states_lowest_average_wait <- wait_time_by_state %>%
  arrange(average_median_wait_days) %>%
  slice_head(n = 5)

# Business interpretation:
# This extends the wait-time story from specific procedures to geography.
# It helps show whether access pressure is concentrated in particular states,
# and whether those differences may need executive attention at a system level.

wait_time_by_state
states_highest_average_wait
states_lowest_average_wait