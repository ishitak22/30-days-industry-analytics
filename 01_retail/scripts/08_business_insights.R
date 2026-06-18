# Day 01 - Retail
# Final business insights
# Uses only existing output CSV files from prior KPI, category, sales trend,
# and customer segment scripts.
# No new analysis, charts, or previous script modifications are performed here.

library(tidyverse)

# Load existing dashboard-ready outputs.
dashboard_kpis <- read_csv("outputs/dashboard_kpis.csv", show_col_types = FALSE)
category_ranking <- read_csv("outputs/category_ranking.csv", show_col_types = FALSE)
monthly_sales_trends <- read_csv(
  "outputs/monthly_sales_trends.csv",
  show_col_types = FALSE
)
strongest_sales_month <- read_csv(
  "outputs/strongest_sales_month.csv",
  show_col_types = FALSE
)
weakest_sales_month <- read_csv(
  "outputs/weakest_sales_month.csv",
  show_col_types = FALSE
)
performance_driver_summary <- read_csv(
  "outputs/performance_driver_summary.csv",
  show_col_types = FALSE
)
kpi_revenue_by_gender <- read_csv(
  "outputs/kpi_revenue_by_gender.csv",
  show_col_types = FALSE
)
kpi_revenue_by_age_group <- read_csv(
  "outputs/kpi_revenue_by_age_group.csv",
  show_col_types = FALSE
)

# Extract values from existing outputs for executive narrative text.
total_revenue <- dashboard_kpis$total_revenue[1]
total_transactions <- dashboard_kpis$total_transactions[1]
total_units_sold <- dashboard_kpis$total_units_sold[1]
average_order_value <- dashboard_kpis$average_order_value[1]

top_revenue_category <- category_ranking %>%
  arrange(revenue_rank) %>%
  slice(1)

top_volume_category <- category_ranking %>%
  arrange(units_rank) %>%
  slice(1)

top_aov_category <- category_ranking %>%
  arrange(aov_rank) %>%
  slice(1)

top_gender <- kpi_revenue_by_gender %>%
  arrange(desc(total_revenue)) %>%
  slice(1)

top_age_group <- kpi_revenue_by_age_group %>%
  arrange(desc(total_revenue)) %>%
  slice(1)

strongest_month <- strongest_sales_month %>%
  slice(1)

weakest_month <- weakest_sales_month %>%
  slice(1)

primary_time_driver <- performance_driver_summary$primary_driver[1]

# Ranked executive insights for the final Quarto report and Shiny dashboard.
business_insights <- tibble(
  rank = 1:7,
  insight = c(
    "Electronics is the leading revenue category",
    "Category roles are clearly different across revenue, volume, and premium value",
    "Sales performance is driven more by volume than by basket size",
    "May 2023 is the strongest sales month",
    "January 2024 should be treated as a partial-month performance caveat",
    "Female customers contribute slightly more revenue than male customers",
    "The 45-54 age group is the highest revenue age segment"
  ),
  evidence = c(
    paste0(
      top_revenue_category$product_category,
      " generated $",
      round(top_revenue_category$total_revenue, 0),
      ", representing ",
      round(top_revenue_category$revenue_share * 100, 1),
      "% of total revenue."
    ),
    paste0(
      top_revenue_category$product_category,
      " leads revenue, ",
      top_volume_category$product_category,
      " leads units sold, and ",
      top_aov_category$product_category,
      " has the highest average order value."
    ),
    paste0(
      "The strongest-to-weakest month comparison identifies ",
      primary_time_driver,
      " as the main performance driver."
    ),
    paste0(
      strongest_month$year_month,
      " generated $",
      round(strongest_month$total_revenue, 0),
      " from ",
      strongest_month$total_transactions,
      " transactions and ",
      strongest_month$total_units_sold,
      " units sold."
    ),
    paste0(
      weakest_month$year_month,
      " generated $",
      round(weakest_month$total_revenue, 0),
      " from only ",
      weakest_month$total_transactions,
      " transactions."
    ),
    paste0(
      top_gender$gender,
      " customers generated $",
      round(top_gender$total_revenue, 0),
      " in revenue."
    ),
    paste0(
      "The ",
      top_age_group$age_group,
      " age group generated $",
      round(top_age_group$total_revenue, 0),
      " in revenue."
    )
  ),
  business_meaning = c(
    "The business has a clear top category by revenue, making Electronics the main commercial revenue driver.",
    "The categories play different business roles: one leads revenue, one leads purchase volume, and one attracts higher-value baskets.",
    "Revenue changes appear to depend more on the number of purchases and units sold than on customers spending more per order.",
    "May 2023 is the strongest observed sales period and can be used as a benchmark for strong monthly performance.",
    "January 2024 has very low transaction volume, so it should not be interpreted as a normal weak month without noting the partial-period issue.",
    "Female customers are the slightly higher-value gender segment in the available KPI outputs.",
    "The 45-54 segment is the strongest age-based revenue contributor and should be considered a priority customer group."
  ),
  recommended_action = c(
    "Prioritise Electronics in revenue-focused campaigns, homepage placements, and stock availability planning.",
    "Use different strategies by category: protect Electronics revenue, use Clothing for volume-led promotions, and position Beauty as a premium basket-value opportunity.",
    "Focus sales initiatives on increasing traffic, conversion, and units per order rather than relying only on higher prices.",
    "Review what could explain May performance and use it as a planning reference for promotions, inventory, and campaign timing.",
    "Flag January 2024 as a partial period in dashboards and avoid using it as a direct benchmark against complete months.",
    "Use gender-level revenue differences to inform customer messaging, while avoiding over-targeting until category preference tables are reviewed.",
    "Build age-group targeting and campaign messaging around the 45-54 segment while comparing its product preferences before final recommendations."
  )
)

write_csv(business_insights, "outputs/business_insights.csv")

# One-row executive summary for dashboard headers or Quarto callout boxes.
executive_summary <- tibble(
  total_revenue = total_revenue,
  total_transactions = total_transactions,
  total_units_sold = total_units_sold,
  average_order_value = average_order_value,
  top_revenue_category = top_revenue_category$product_category,
  top_volume_category = top_volume_category$product_category,
  top_aov_category = top_aov_category$product_category,
  strongest_month = strongest_month$year_month,
  weakest_month = weakest_month$year_month,
  primary_time_driver = primary_time_driver,
  top_gender_segment = top_gender$gender,
  top_age_group = top_age_group$age_group
)

write_csv(executive_summary, "outputs/executive_summary.csv")

# Analyst review outputs.
business_insights
executive_summary
