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

average_procedure_wait_time <- top_wait_time_procedures %>%
  summarise(value = mean(average_median_wait_days, na.rm = TRUE)) %>%
  pull(value)

procedure_wait_gap <- highest_wait_procedure$average_median_wait_days -
  average_procedure_wait_time

top_three_procedures <- top_wait_time_procedures %>%
  slice_head(n = 3) %>%
  pull(reported_measure_name) %>%
  paste(collapse = ", ")

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

wait_time_trend_reporting_end <- elective_surgery_clean %>%
  filter(
    measure_name == "Median waiting time for elective surgery",
    !is.na(value)
  ) %>%
  mutate(reporting_end = as.Date(reporting_end)) %>%
  group_by(reporting_end) %>%
  summarise(
    average_median_wait_days = mean(value, na.rm = TRUE),
    reporting_units = n_distinct(reporting_unit_name),
    .groups = "drop"
  ) %>%
  arrange(reporting_end)

worst_trend_period <- wait_time_trend_reporting_end %>%
  slice_max(average_median_wait_days, n = 1, with_ties = FALSE) %>%
  mutate(period_label = paste0("Highest: ", round(average_median_wait_days, 1), " days"))

best_trend_period <- wait_time_trend_reporting_end %>%
  slice_min(average_median_wait_days, n = 1, with_ties = FALSE) %>%
  mutate(period_label = paste0("Lowest: ", round(average_median_wait_days, 1), " days"))

first_trend_period <- wait_time_trend_reporting_end %>%
  slice_min(reporting_end, n = 1, with_ties = FALSE)

latest_trend_period <- wait_time_trend_reporting_end %>%
  slice_max(reporting_end, n = 1, with_ties = FALSE)

trend_direction_reporting_end <- case_when(
  latest_trend_period$average_median_wait_days > first_trend_period$average_median_wait_days ~ "worsening",
  latest_trend_period$average_median_wait_days < first_trend_period$average_median_wait_days ~ "improving",
  TRUE ~ "stable"
)

trend_variability <- wait_time_trend_reporting_end %>%
  summarise(
    variability = sd(average_median_wait_days, na.rm = TRUE),
    wait_range = max(average_median_wait_days, na.rm = TRUE) -
      min(average_median_wait_days, na.rm = TRUE)
  )

trend_variability_label <- if_else(
  trend_variability$wait_range >= 50,
  "volatile",
  "relatively stable"
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

executive_evidence_summary <- tibble::tribble(
  ~Finding, ~Evidence,
  "High-pressure procedures identified",
  paste0(
    highest_wait_procedure$reported_measure_name,
    " has the longest average median wait at ",
    round(highest_wait_procedure$average_median_wait_days, 1),
    " days."
  ),
  "Highest state wait-time pressure",
  paste0(
    highest_wait_state$mapped_state,
    " has the highest average median wait at ",
    round(highest_wait_state$average_median_wait_days, 1),
    " days."
  ),
  "Trend direction",
  paste0(
    "Average median waiting time is ",
    trend_direction_reporting_end,
    " across the available reporting periods."
  ),
  "Access inequality",
  paste0(
    "The gap between the highest and lowest-wait states is ",
    round(state_wait_gap, 1),
    " days."
  )
)

executive_action_plan <- tibble::tribble(
  ~Priority, ~Action, ~Reason, ~`Expected Impact`,
  "High",
  "Increase capacity for high-pressure procedures",
  "The longest-wait procedure is creating concentrated access pressure.",
  "Reduce patient wait times in the most constrained surgical pathway.",
  "High",
  "Target high-wait states for intervention",
  "State-level variation shows access pressure is not evenly distributed.",
  "Direct funding, workforce, and backlog programs to the areas with greatest need.",
  "Medium",
  "Monitor deterioration periods",
  "Trend analysis shows whether access pressure is improving or worsening over time.",
  "Create early warning signals before waitlists become harder to manage.",
  "Medium",
  "Benchmark low-wait states for best practice",
  "Lower-wait states may reveal better scheduling, referral, or theatre utilisation practices.",
  "Transfer effective operational practices to higher-wait areas.",
  "Low",
  "Review peer benchmark gaps",
  "Peer comparisons support fairer performance review across different hospital contexts.",
  "Focus governance conversations on realistic and comparable performance expectations."
)

executive_snapshot <- tibble::tribble(
  ~Area, ~KeyMessage,
  "Procedure pressure",
  paste0(
    highest_wait_procedure$reported_measure_name,
    " has the longest average median wait at ",
    round(highest_wait_procedure$average_median_wait_days, 1),
    " days."
  ),
  "Geographic pressure",
  paste0(
    highest_wait_state$mapped_state,
    " has the highest state-level wait pressure; the state gap is ",
    round(state_wait_gap, 1),
    " days."
  ),
  "System direction",
  paste0(
    "Waiting time performance is ",
    trend_direction_reporting_end,
    " across the available reporting periods."
  )
)

executive_priority_actions <- tibble::tribble(
  ~Priority, ~Decision, ~Why Now,
  "High",
  "Add capacity to the highest-wait procedures",
  "Procedure bottlenecks are the clearest pressure point.",
  "High",
  "Target support to the highest-wait state",
  "Access pressure is geographically uneven.",
  "Medium",
  "Monitor trend deterioration",
  "Earlier intervention can prevent backlog pressure from becoming harder to manage."
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
      col_widths = 12,
      card(
        card_header("Business Question"),
        h4("Which elective surgery procedures create the greatest access pressure?"),
        p("This section identifies the procedures where patients face the longest typical waits for elective surgery.")
      ),
      card(
        card_header("Analysis Approach"),
        tags$ul(
          tags$li("Filtered the dataset to median waiting time records."),
          tags$li("Grouped records by procedure using reported_measure_name."),
          tags$li("Calculated the average median waiting time for each procedure."),
          tags$li("Ranked procedures from highest to lowest wait-time pressure.")
        )
      ),
      card(
        card_header("Top 10 Procedures by Average Median Waiting Time"),
        plotOutput("procedure_wait_time_chart", height = "520px")
      ),
      layout_column_wrap(
        width = "340px",
        card(
          card_header("Key Findings"),
          tags$ul(
            tags$li(
              paste0(
                "Highest pressure procedure: ",
                highest_wait_procedure$reported_measure_name,
                " at ",
                round(highest_wait_procedure$average_median_wait_days, 1),
                " days."
              )
            ),
            tags$li(
              paste0(
                "The top procedure is ",
                round(procedure_wait_gap, 1),
                " days above the average of the top 10 pressure procedures."
              )
            ),
            tags$li(
              paste0(
                "The wait-time burden is concentrated in a small group of procedures, led by ",
                top_three_procedures,
                "."
              )
            )
          )
        ),
        card(
          card_header("Healthcare Implications"),
          p(
            "High procedure-level wait times suggest that access pressure is not uniform across elective surgery. These bottlenecks may reflect constrained theatre time, specialist availability, referral demand, or recovery capacity for specific surgical pathways."
          )
        ),
        card(
          card_header("Recommended Actions"),
          tags$ul(
            tags$li("Increase theatre capacity or session allocation for the highest-pressure procedures."),
            tags$li("Review scheduling, referral, and triage pathways for procedures with sustained long waits."),
            tags$li("Target backlog reduction programs toward the procedures with the greatest patient access pressure."),
            tags$li("Monitor whether added capacity reduces average median waits in future reporting periods.")
          )
        )
      )
    )
  ),
  nav_panel(
    "State Comparison",
    layout_columns(
      col_widths = 12,
      card(
        card_header("Business Question"),
        h4("Where is elective surgery access most unequal across states?"),
        p("This section compares average median waiting times across states and territories to identify geographic access pressure.")
      ),
      card(
        card_header("Analysis Approach"),
        tags$ul(
          tags$li("Filtered the dataset to median waiting time records."),
          tags$li("Grouped records by state using mapped_state."),
          tags$li("Calculated average median waiting time for each state."),
          tags$li("Compared states to identify variation in elective surgery access pressure.")
        )
      ),
      card(
        card_header("State Variation in Average Median Waiting Time"),
        plotOutput("state_wait_time_chart", height = "420px")
      ),
      layout_column_wrap(
        width = "340px",
        card(
          card_header("Key Findings"),
          tags$ul(
            tags$li(
              paste0(
                "Highest wait state: ",
                highest_wait_state$mapped_state,
                " at ",
                round(highest_wait_state$average_median_wait_days, 1),
                " days."
              )
            ),
            tags$li(
              paste0(
                "Lowest wait state: ",
                lowest_wait_state$mapped_state,
                " at ",
                round(lowest_wait_state$average_median_wait_days, 1),
                " days."
              )
            ),
            tags$li(
              paste0(
                "The access gap between highest and lowest states is ",
                round(state_wait_gap, 1),
                " days."
              )
            ),
            tags$li(
              if (state_wait_gap >= 50) {
                "Variation appears large and may indicate meaningful geographic inequality in elective surgery access."
              } else {
                "Variation appears moderate, but still points to measurable differences in access between states."
              }
            )
          )
        ),
        card(
          card_header("Healthcare Implications"),
          p(
            "Geographic differences in waiting times suggest that patients may experience different access to elective surgery depending on where they live. Persistent gaps can affect patient outcomes, system fairness, and pressure on hospitals in high-wait regions."
          )
        ),
        card(
          card_header("Recommended Actions"),
          tags$ul(
            tags$li("Investigate high-wait states for theatre capacity, workforce, referral, and backlog constraints."),
            tags$li("Use low-wait states as benchmarks to understand scheduling, pathway, and capacity practices."),
            tags$li("Consider targeted funding, workforce support, or backlog programs where access pressure is highest."),
            tags$li("Monitor state-level wait gaps over time to assess whether inequality is widening or narrowing.")
          )
        )
      )
    )
  ),
  nav_panel(
    "Trends Over Time",
    layout_columns(
      col_widths = 12,
      card(
        card_header("Business Question"),
        h4("Are elective surgery waiting times improving or worsening over time?"),
        p("This section monitors whether average median waiting times are moving in a positive or negative direction across reporting periods.")
      ),
      card(
        card_header("Analysis Approach"),
        tags$ul(
          tags$li("Used time-based elective surgery median waiting time records."),
          tags$li("Grouped records by reporting_end to represent each reporting period."),
          tags$li("Calculated average median waiting time over time."),
          tags$li("Identified the best and worst performance periods.")
        )
      ),
      card(
        card_header("Average Median Waiting Time Over Time"),
        plotOutput("wait_time_trend_chart", height = "460px")
      ),
      layout_column_wrap(
        width = "340px",
        card(
          card_header("Key Findings"),
          tags$ul(
            tags$li(
              paste0(
                "Highest waiting time period: ",
                format(worst_trend_period$reporting_end, "%Y-%m-%d"),
                " at ",
                round(worst_trend_period$average_median_wait_days, 1),
                " days."
              )
            ),
            tags$li(
              paste0(
                "Lowest waiting time period: ",
                format(best_trend_period$reporting_end, "%Y-%m-%d"),
                " at ",
                round(best_trend_period$average_median_wait_days, 1),
                " days."
              )
            ),
            tags$li(
              paste0(
                "Overall trend direction is ",
                trend_direction_reporting_end,
                "."
              )
            ),
            tags$li(
              paste0(
                "The system appears ",
                trend_variability_label,
                ", with a ",
                round(trend_variability$wait_range, 1),
                "-day range across reporting periods."
              )
            )
          )
        ),
        card(
          card_header("Healthcare Implications"),
          p(
            paste0(
              "A ",
              trend_direction_reporting_end,
              " trend indicates whether elective surgery access pressure is easing or building over time. Sustained increases may signal backlog growth, capacity constraints, or delays in patient access, while improvement suggests that operational interventions may be having an effect."
            )
          )
        ),
        card(
          card_header("Recommended Actions"),
          tags$ul(
            tags$li("Investigate peak pressure periods to understand demand, capacity, and workforce constraints."),
            tags$li("If the trend worsens, identify whether deterioration is linked to backlog growth or reduced theatre availability."),
            tags$li("If improvement is observed, sustain the policies or operational changes linked to better performance."),
            tags$li("Monitor reporting-period trends as an early warning signal for future access pressure.")
          )
        )
      )
    )
  ),
  nav_panel(
    "Executive Insights",
    layout_columns(
      col_widths = 12,
      card(
        card_header("Executive Question"),
        h3("What should leaders act on first?"),
        p("The dashboard points to three immediate priorities: procedure bottlenecks, geographic access pressure, and worsening wait-time performance.")
      ),
      layout_column_wrap(
        width = "300px",
        card(
          class = "insight-card",
          card_header("1. Procedure bottleneck"),
          h4(highest_wait_procedure$reported_measure_name),
          p(paste0(
            round(highest_wait_procedure$average_median_wait_days, 1),
            " days average median wait. Prioritise theatre time and specialist capacity here."
          ))
        ),
        card(
          class = "insight-card",
          card_header("2. Access inequality"),
          h4(highest_wait_state$mapped_state),
          p(paste0(
            "Highest state wait pressure, with a ",
            round(state_wait_gap, 1),
            "-day gap to the lowest-wait state. Target support geographically."
          ))
        ),
        card(
          class = "insight-card",
          card_header("3. System direction"),
          h4(str_to_sentence(trend_direction_reporting_end)),
          p(paste0(
            "Wait-time performance is ",
            trend_direction_reporting_end,
            ". Monitor deterioration early and investigate peak pressure periods."
          ))
        )
      ),
      card(
        card_header("Priority Actions"),
        tableOutput("executive_priority_action_table")
      ),
      card(
        card_header("Executive Takeaway"),
        tags$ul(
          tags$li(
            paste0(
              "Biggest operational pressure: ",
              highest_wait_procedure$reported_measure_name,
              "."
            )
          ),
          tags$li(
            paste0(
              "Most urgent geographic focus: ",
              highest_wait_state$mapped_state,
              "."
            )
          ),
          tags$li(
            paste0(
              "Leadership focus: targeted capacity, backlog reduction, and early trend monitoring."
            )
          )
        )
      ),
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

  output$executive_evidence_table <- renderTable(
    executive_evidence_summary,
    striped = TRUE,
    bordered = FALSE,
    spacing = "m",
    width = "100%"
  )

  output$executive_action_table <- renderTable(
    executive_action_plan,
    striped = TRUE,
    bordered = FALSE,
    spacing = "m",
    width = "100%"
  )

  output$executive_priority_action_table <- renderTable(
    executive_priority_actions,
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
      wait_time_trend_reporting_end,
      aes(
        x = reporting_end,
        y = average_median_wait_days
      )
    ) +
      geom_line(color = "#2c7fb8", linewidth = 1.1) +
      geom_point(color = "#2c7fb8", size = 2.8) +
      geom_point(
        data = worst_trend_period,
        color = "#b2182b",
        size = 4
      ) +
      geom_point(
        data = best_trend_period,
        color = "#1a9850",
        size = 4
      ) +
      geom_label(
        data = worst_trend_period,
        aes(label = period_label),
        nudge_y = 5,
        fill = "#b2182b",
        color = "white",
        label.size = 0,
        size = 3.6
      ) +
      geom_label(
        data = best_trend_period,
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
