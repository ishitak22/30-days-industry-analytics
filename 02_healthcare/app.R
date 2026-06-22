# Day 02 - Healthcare
# Shiny dashboard skeleton

library(shiny)
library(bslib)

ui <- page_navbar(
  title = "Healthcare Elective Surgery Performance Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  nav_panel(
    "Executive Overview",
    layout_columns(
      card(
        card_header("Executive Overview"),
        "Placeholder for executive summary KPIs and high-level performance context."
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
