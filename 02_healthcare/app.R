# Day 02 - Healthcare
# Shiny dashboard skeleton

library(shiny)
library(bslib)
library(tidyverse)

elective_surgery_clean <- readr::read_csv(
  "data/elective_surgery_clean.csv",
  show_col_types = FALSE
)

total_elective_surgeries <- elective_surgery_clean %>%
  filter(measure_name == "Number of elective surgeries") %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  pull(value)

reporting_hospitals <- elective_surgery_clean %>%
  summarise(value = n_distinct(reporting_unit_name)) %>%
  pull(value)

median_waiting_time <- elective_surgery_clean %>%
  filter(str_detect(measure_name, regex("median waiting time", ignore_case = TRUE))) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  pull(value)

treated_within_recommended_time <- elective_surgery_clean %>%
  filter(str_detect(measure_name, regex("clinically recommended times", ignore_case = TRUE))) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  pull(value)

ui <- page_navbar(
  title = "Healthcare Elective Surgery Performance Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  nav_panel(
    "Executive Overview",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      card(
        card_header("Total Elective Surgeries"),
        h2(format(round(total_elective_surgeries), big.mark = ",")),
        p("Reported elective surgery activity")
      ),
      card(
        card_header("Reporting Hospitals"),
        h2(format(reporting_hospitals, big.mark = ",")),
        p("Hospitals included in the dataset")
      ),
      card(
        card_header("Median Waiting Time"),
        h2(paste0(round(median_waiting_time, 1), " days")),
        p("Average of reported median wait times")
      ),
      card(
        card_header("Treated Within Recommended Time"),
        h2(paste0(round(treated_within_recommended_time, 1), "%")),
        p("Average reported performance")
      )
    )
  ),
  nav_panel(
    "Procedure Performance",
    layout_columns(
      card(
        card_header("Procedure Performance"),
        "Placeholder for procedure-level elective surgery performance analysis."
      )
    )
  ),
  nav_panel(
    "State Comparison",
    layout_columns(
      card(
        card_header("State Comparison"),
        "Placeholder for state-level elective surgery comparison."
      )
    )
  ),
  nav_panel(
    "Trends Over Time",
    layout_columns(
      card(
        card_header("Trends Over Time"),
        "Placeholder for elective surgery wait-time trends."
      )
    )
  ),
  nav_panel(
    "Executive Insights",
    layout_columns(
      card(
        card_header("Executive Insights"),
        "Placeholder for consolidated executive insights and key pressure points."
      )
    )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)
