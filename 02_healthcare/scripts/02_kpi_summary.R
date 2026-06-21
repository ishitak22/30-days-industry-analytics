# Day 02 - Healthcare
# KPI summary

library(tidyverse)

# Load cleaned data
elective_surgery_clean <- readr::read_csv(
  "data/elective_surgery_clean.csv",
  show_col_types = FALSE
)

# Base KPI section
total_elective_surgeries <- elective_surgery_clean %>%
  filter(measure_name == "Number of elective surgeries") %>%
  summarise(
    total_elective_surgeries = sum(value, na.rm = TRUE)
  )

number_reporting_hospitals <- elective_surgery_clean %>%
  summarise(number_reporting_hospitals = n_distinct(reporting_unit_name))

number_states <- elective_surgery_clean %>%
  summarise(number_states = n_distinct(mapped_state))

total_elective_surgeries
number_reporting_hospitals
number_states

# Access and waiting time KPIs
median_waiting_time <- elective_surgery_clean %>%
  filter(str_detect(measure_name, regex("median waiting time", ignore_case = TRUE))) %>%
  summarise(median_waiting_time = mean(value, na.rm = TRUE))

patients_waiting_over_365_days <- elective_surgery_clean %>%
  filter(str_detect(measure_name, regex("waited longer than 365 days", ignore_case = TRUE))) %>%
  summarise(patients_waiting_over_365_days = mean(value, na.rm = TRUE))

patients_treated_within_recommended_time <- elective_surgery_clean %>%
  filter(str_detect(measure_name, regex("clinically recommended times", ignore_case = TRUE))) %>%
  summarise(patients_treated_within_recommended_time = mean(value, na.rm = TRUE))

median_waiting_time
patients_waiting_over_365_days
patients_treated_within_recommended_time

# Benchmarking KPIs
average_value_vs_peer <- elective_surgery_clean %>%
  summarise(
    average_value = mean(value, na.rm = TRUE),
    average_peer_value = mean(peer_value, na.rm = TRUE)
  )

overall_performance_gap <- elective_surgery_clean %>%
  summarise(overall_performance_gap = mean(value - peer_value, na.rm = TRUE))

records_above_peer_benchmark <- elective_surgery_clean %>%
  filter(!is.na(value), !is.na(peer_value)) %>%
  summarise(records_above_peer_benchmark = sum(value > peer_value))

records_below_peer_benchmark <- elective_surgery_clean %>%
  filter(!is.na(value), !is.na(peer_value)) %>%
  summarise(records_below_peer_benchmark = sum(value < peer_value))

average_value_vs_peer
overall_performance_gap
records_above_peer_benchmark
records_below_peer_benchmark
