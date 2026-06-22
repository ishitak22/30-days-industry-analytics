# Day 02 - Healthcare
# Shiny dashboard skeleton

library(shiny)
library(bslib)
library(tidyverse)

elective_surgery_clean <- readr::read_csv(
  "data/elective_surgery_clean.csv",
  show_col_types = FALSE
)

elective_surgery_wait_times_national <- readr::read_csv(
  "data/elective_surgery_wait_times_national.csv",
  show_col_types = FALSE
) %>%
  janitor::clean_names()

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

state_wait_time_summary <- elective_surgery_wait_times_national %>%
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

wait_time_trend_over_time <- elective_surgery_clean %>%
  filter(
    measure_name == "Median waiting time for elective surgery",
    !is.na(value)
  ) %>%
  mutate(reporting_start = as.Date(reporting_start)) %>%
  group_by(reporting_start) %>%
  summarise(
    average_median_wait_days = mean(value, na.rm = TRUE),
    reporting_units = n_distinct(reporting_unit_name),
    .groups = "drop"
  ) %>%
  arrange(reporting_start)

worst_wait_time_period <- wait_time_trend_over_time %>%
  slice_max(average_median_wait_days, n = 1, with_ties = FALSE) %>%
  mutate(period_label = paste0("Highest: ", round(average_median_wait_days, 1), " days"))

best_wait_time_period <- wait_time_trend_over_time %>%
  slice_min(average_median_wait_days, n = 1, with_ties = FALSE) %>%
  mutate(period_label = paste0("Lowest: ", round(average_median_wait_days, 1), " days"))

highest_wait_procedure <- top_wait_time_procedures %>%
  slice_max(average_median_wait_days, n = 1, with_ties = FALSE)

highest_wait_state <- state_wait_time_summary %>%
  slice_max(average_median_wait_days, n = 1, with_ties = FALSE)

lowest_wait_state <- state_wait_time_summary %>%
  slice_min(average_median_wait_days, n = 1, with_ties = FALSE)

state_wait_gap <- highest_wait_state$average_median_wait_days -
  lowest_wait_state$average_median_wait_days

latest_wait_period <- wait_time_trend_over_time %>%
  slice_max(reporting_start, n = 1, with_ties = FALSE)

earliest_wait_period <- wait_time_trend_over_time %>%
  slice_min(reporting_start, n = 1, with_ties = FALSE)

trend_direction <- case_when(
  latest_wait_period$average_median_wait_days > earliest_wait_period$average_median_wait_days ~ "deteriorated",
  latest_wait_period$average_median_wait_days < earliest_wait_period$average_median_wait_days ~ "improved",
  TRUE ~ "remained stable"
)

business_questions <- tibble::tribble(
  ~`Business Question`, ~`Why It Matters`,
  "Which procedures have the longest waiting times?",
  "Identifies clinical areas where capacity, staffing, or theatre time may need attention.",
  "Which states experience the highest access pressure?",
  "Shows where elective surgery access is most constrained and where support may be needed.",
  "Are waiting times improving or worsening over time?",
  "Helps leaders judge whether access initiatives are reducing waitlist pressure.",
  "How do hospitals perform against peer benchmarks?",
  "Supports fairer performance review by comparing hospitals with similar peers.",
  "What actions should healthcare leaders prioritise?",
  "Turns analysis into decisions about backlog reduction, resource allocation, and service planning."
)

ui <- page_navbar(
  title = "Healthcare Elective Surgery Performance Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  header = tags$style(
    HTML("
      .kpi-card .card-body {
        min-height: 170px;
        padding: 1.25rem;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }

      .kpi-value {
        font-size: 2.8rem;
        font-weight: 700;
        line-height: 1;
        margin-bottom: 0.75rem;
        color: #20384f;
      }

      .kpi-card p {
        font-size: 1rem;
        font-weight: 500;
        color: #52616f;
        margin-bottom: 0;
      }

      .insight-card h4 {
        font-size: 1.35rem;
        font-weight: 700;
        margin-bottom: 0.75rem;
        color: #20384f;
      }

      .insight-card p {
        font-size: 1rem;
        line-height: 1.45;
        color: #3f4d5a;
        margin-bottom: 0;
      }

      .insight-card .card-body {
        min-height: 210px;
        padding: 1.25rem;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
    ")
  ),
  nav_panel(
    "Executive Overview",
    layout_columns(
      col_widths = 12,
      layout_column_wrap(
        width = "260px",
        card(
          class = "kpi-card",
          card_header("Total Elective Surgeries"),
          div(class = "kpi-value", format(round(total_elective_surgeries), big.mark = ",")),
          p("Reported elective surgery activity")
        ),
        card(
          class = "kpi-card",
          card_header("Reporting Hospitals"),
          div(class = "kpi-value", format(reporting_hospitals, big.mark = ",")),
          p("Hospitals included in the dataset")
        ),
        card(
          class = "kpi-card",
          card_header("Average Median Waiting Time"),
          div(class = "kpi-value", paste0(round(average_median_waiting_time, 1), " days")),
          p("Average of reported median wait times")
        ),
        card(
          class = "kpi-card",
          card_header("Treated Within Recommended Time"),
          div(class = "kpi-value", paste0(round(treated_within_recommended_time, 1), "%")),
          p("Average reported performance")
        )
      ),
      card(
        card_header("Business Questions This Dashboard Answers"),
        tableOutput("business_questions_table")
      ),
      card(
        card_header("About the Analysis"),
        layout_column_wrap(
          width = "300px",
          value_box(
            title = "Datasets Used",
            value = "AIHW MyHospitals elective surgery data"
          ),
          value_box(
            title = "What Was Analysed",
            value = "Surgery volumes, wait times, state variation, trends, and peer benchmarks"
          ),
          value_box(
            title = "Decisions Supported",
            value = "Capacity planning, backlog reduction, access improvement, and performance review"
          )
        )
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
        card_header("Average Median Waiting Time Over Time"),
        plotOutput("wait_time_trend_chart", height = "460px")
      )
    )
  ),
  nav_panel(
    "Executive Insights",
    layout_column_wrap(
      width = "380px",
      card(
        class = "insight-card",
        card_header("Prioritise the highest-pressure procedure"),
        h4(highest_wait_procedure$reported_measure_name),
        p(
          paste0(
            "This procedure has the longest average median wait at ",
            round(highest_wait_procedure$average_median_wait_days, 1),
            " days, so theatre time, specialist availability, and waitlist triage should be reviewed first."
          )
        )
      ),
      card(
        class = "insight-card",
        card_header("Target support to the highest-wait state"),
        h4(highest_wait_state$mapped_state),
        p(
          paste0(
            "This state records the highest average median wait at ",
            round(highest_wait_state$average_median_wait_days, 1),
            " days, suggesting a need for focused capacity planning and backlog reduction."
          )
        )
      ),
      card(
        class = "insight-card",
        card_header("Use the lowest-wait state as a benchmark"),
        h4(lowest_wait_state$mapped_state),
        p(
          paste0(
            "This state records the lowest average median wait at ",
            round(lowest_wait_state$average_median_wait_days, 1),
            " days, making it useful for comparing referral pathways, scheduling practices, and theatre utilisation."
          )
        )
      ),
      card(
        class = "insight-card",
        card_header("Reduce the access inequality gap"),
        h4(paste0(round(state_wait_gap, 1), " days")),
        p(
          "This gap between the highest- and lowest-wait states shows unequal access and can guide where additional funding or workforce support may have the largest impact."
        )
      ),
      card(
        class = "insight-card",
        card_header("Watch the direction of system performance"),
        h4(str_to_sentence(trend_direction)),
        p(
          paste0(
            "Average median waiting time has ",
            trend_direction,
            " from ",
            round(earliest_wait_period$average_median_wait_days, 1),
            " days to ",
            round(latest_wait_period$average_median_wait_days, 1),
            " days, helping leaders judge whether current access initiatives are working."
          )
        )
      ),
      card(
        class = "insight-card",
        card_header("Learn from the worst pressure period"),
        h4(format(worst_wait_time_period$reporting_start, "%Y-%m-%d")),
        p(
          paste0(
            "The worst period recorded an average median wait of ",
            round(worst_wait_time_period$average_median_wait_days, 1),
            " days, so leaders should examine what operational constraints or demand shocks were present then."
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$business_questions_table <- renderTable(
    business_questions,
    striped = TRUE,
    bordered = FALSE,
    spacing = "m",
    width = "100%"
  )

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
        ),
        labels = c(
          "Highest wait" = "Highest wait pressure",
          "Lowest wait" = "Lowest wait pressure",
          "Other states" = "Other states"
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

  output$wait_time_trend_chart <- renderPlot({
    ggplot(
      wait_time_trend_over_time,
      aes(
        x = reporting_start,
        y = average_median_wait_days
      )
    ) +
      geom_line(color = "#2c7fb8", linewidth = 1.1) +
      geom_point(color = "#2c7fb8", size = 2.8) +
      geom_point(
        data = worst_wait_time_period,
        color = "#b2182b",
        size = 4
      ) +
      geom_point(
        data = best_wait_time_period,
        color = "#1a9850",
        size = 4
      ) +
      geom_label(
        data = worst_wait_time_period,
        aes(label = period_label),
        nudge_y = 5,
        fill = "#b2182b",
        color = "white",
        label.size = 0,
        size = 3.6
      ) +
      geom_label(
        data = best_wait_time_period,
        aes(label = period_label),
        nudge_y = -5,
        fill = "#1a9850",
        color = "white",
        label.size = 0,
        size = 3.6
      ) +
      labs(
        x = NULL,
        y = "Average median waiting time (days)"
      ) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      scale_y_continuous(expand = expansion(mult = c(0.08, 0.14))) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.margin = margin(10, 20, 10, 10)
      )
  })
}

shinyApp(ui = ui, server = server)
