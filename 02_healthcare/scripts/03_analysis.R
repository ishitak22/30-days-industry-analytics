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

# State-level disparity analysis
highest_average_wait_state <- wait_time_by_state %>%
  slice_max(average_median_wait_days, n = 1, with_ties = FALSE)

lowest_average_wait_state <- wait_time_by_state %>%
  slice_min(average_median_wait_days, n = 1, with_ties = FALSE)

state_wait_time_difference <- highest_average_wait_state$average_median_wait_days -
  lowest_average_wait_state$average_median_wait_days

state_wait_time_inequality_ratio <- highest_average_wait_state$average_median_wait_days /
  lowest_average_wait_state$average_median_wait_days

overall_average_wait_time <- wait_time_by_state %>%
  summarise(overall_average_median_wait_days = mean(average_median_wait_days, na.rm = TRUE))

states_above_national_average <- wait_time_by_state %>%
  filter(average_median_wait_days > overall_average_wait_time$overall_average_median_wait_days) %>%
  summarise(states_above_national_average = n())

states_below_national_average <- wait_time_by_state %>%
  filter(average_median_wait_days < overall_average_wait_time$overall_average_median_wait_days) %>%
  summarise(states_below_national_average = n())

state_wait_time_ranking <- wait_time_by_state %>%
  arrange(desc(average_median_wait_days)) %>%
  mutate(wait_time_rank = row_number()) %>%
  select(
    wait_time_rank,
    mapped_state,
    average_median_wait_days,
    maximum_median_wait_days,
    reporting_hospitals
  )

# Business interpretation:
# This quantifies inequality in elective surgery access across states by showing
# the gap between the highest- and lowest-wait states, the relative worst-to-best
# ratio, and how many states sit above or below the national average wait time.

highest_average_wait_state
lowest_average_wait_state
state_wait_time_difference
state_wait_time_inequality_ratio
overall_average_wait_time
states_above_national_average
states_below_national_average
state_wait_time_ranking