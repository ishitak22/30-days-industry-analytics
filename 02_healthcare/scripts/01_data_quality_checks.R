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
