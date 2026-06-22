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

average_median_waiting_time <- elective_surgery_clean %>%
  filter(str_detect(measure_name, regex("median waiting time", ignore_case = TRUE))) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  pull(value)

treated_within_recommended_time <- elective_surgery_clean %>%
  filter(str_detect(measure_name, regex("clinically recommended times", ignore_case = TRUE))) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  pull(value)

top_wait_time_procedures <- elective_surgery_clean %>%
  filter(
    measure_name == "Median waiting time for elective surgery",
    !is.na(value)
  ) %>%
  group_by(reported_measure_name) %>%
  summarise(
    average_median_wait_days = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(average_median_wait_days)) %>%
  slice_head(n = 10)

state_wait_time_summary <- elective_surgery_clean %>%
  filter(
    measure_name == "Median waiting time for elective surgery",
    !is.na(value)
  ) %>%
  group_by(mapped_state) %>%
  summarise(
    average_median_wait_days = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(average_median_wait_days)) %>%
  mutate(
    performance_group = case_when(
      average_median_wait_days == max(average_median_wait_days, na.rm = TRUE) ~ "Highest wait",
      average_median_wait_days == min(average_median_wait_days, na.rm = TRUE) ~ "Lowest wait",
      TRUE ~ "Other states"
    )
  )

ui <- page_navbar(
  title = "Healthcare Elective Surgery Performance Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  nav_panel(
    "Executive Overview",
    layout_column_wrap(
      width = "260px",
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
        card_header("Average Median Waiting Time"),
        h2(paste0(round(average_median_waiting_time, 1), " days")),
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
        card_header("Top 10 Procedures by Average Median Waiting Time"),
        plotOutput("procedure_wait_time_chart", height = "520px")
      )
    )
  ),
  nav_panel(
    "State Comparison",
    layout_columns(
      card(
        card_header("State Variation in Average Median Waiting Time"),
        plotOutput("state_wait_time_chart", height = "420px")
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
  output$procedure_wait_time_chart <- renderPlot({
    ggplot(
      top_wait_time_procedures,
      aes(
        x = reorder(reported_measure_name, average_median_wait_days),
        y = average_median_wait_days
      )
    ) +
      geom_col(fill = "#2c7fb8", width = 0.72) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Average median waiting time (days)"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.margin = margin(10, 20, 10, 10)
      )
  })

  output$state_wait_time_chart <- renderPlot({
    ggplot(
      state_wait_time_summary,
      aes(
        x = reorder(mapped_state, average_median_wait_days),
        y = average_median_wait_days,
        fill = performance_group
      )
    ) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_fill_manual(
        values = c(
          "Highest wait" = "#b2182b",
          "Lowest wait" = "#1a9850",
          "Other states" = "#9ecae1"
        )
      ) +
      labs(
        x = NULL,
        y = "Average median waiting time (days)",
        fill = NULL
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.margin = margin(10, 20, 10, 10)
      )
  })
}

shinyApp(ui = ui, server = server)
