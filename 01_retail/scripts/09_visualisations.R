# Day 01 - Retail
# Executive dashboard visualisations
# Uses existing analysis outputs only.
# No new metrics or analytical summaries are created in this script.

library(ggplot2)
library(readr)
library(dplyr)

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

if (!dir.exists("outputs/visualisations")) {
  dir.create("outputs/visualisations")
}

required_outputs <- c(
  "outputs/category_ranking.csv",
  "outputs/revenue_share_by_category.csv",
  "outputs/monthly_sales_trends.csv",
  "outputs/kpi_revenue_by_age_group.csv",
  "outputs/kpi_revenue_by_gender.csv",
  "outputs/customer_category_preferences_by_age_group.csv"
)

missing_outputs <- required_outputs[!file.exists(required_outputs)]

if (length(missing_outputs) > 0) {
  stop(
    paste(
      "The following existing analysis outputs are required before running this script:",
      paste(missing_outputs, collapse = ", ")
    )
  )
}

category_ranking <- read_csv("outputs/category_ranking.csv", show_col_types = FALSE)
revenue_share_by_category <- read_csv(
  "outputs/revenue_share_by_category.csv",
  show_col_types = FALSE
)
monthly_sales_trends <- read_csv(
  "outputs/monthly_sales_trends.csv",
  show_col_types = FALSE
)
revenue_by_age_group <- read_csv(
  "outputs/kpi_revenue_by_age_group.csv",
  show_col_types = FALSE
)
revenue_by_gender <- read_csv(
  "outputs/kpi_revenue_by_gender.csv",
  show_col_types = FALSE
)
category_preferences_by_age_group <- read_csv(
  "outputs/customer_category_preferences_by_age_group.csv",
  show_col_types = FALSE
)
dashboard_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    plot.caption = element_text(color = "grey40", size = 8)
  )

category_palette <- c(
  "Electronics" = "#1F77B4",
  "Clothing" = "#2CA02C",
  "Beauty" = "#FF7F0E"
)

format_dollar <- function(x) {
  paste0("$", format(round(x, 0), big.mark = ",", scientific = FALSE))
}

format_percent <- function(x) {
  paste0(round(x * 100, 1), "%")
}

format_number <- function(x) {
  format(round(x, 0), big.mark = ",", scientific = FALSE)
}

# 1. Revenue contribution by category
# Business question:
# Which categories contribute the largest share of total revenue?
revenue_contribution_by_category_plot <- ggplot(
  revenue_share_by_category,
  aes(
    x = "Total Revenue",
    y = revenue_share,
    fill = product_category
  )
) +
  geom_col(width = 0.45, color = "white") +
  geom_text(
    aes(label = format_percent(revenue_share)),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(labels = format_percent) +
  scale_fill_manual(values = category_palette) +
  labs(
    title = "Revenue Contribution by Category",
    subtitle = "Category share of total retail revenue",
    caption = "Source: revenue_share_by_category.csv"
  ) +
  dashboard_theme +
  theme(axis.text.y = element_blank())

ggsave(
  "outputs/visualisations/01_revenue_contribution_by_category.png",
  revenue_contribution_by_category_plot,
  width = 9,
  height = 4,
  dpi = 300
)

# 2. Category performance ranking
# Business question:
# Which product categories rank highest by total revenue?
category_performance_ranking_plot <- ggplot(
  category_ranking,
  aes(
    x = reorder(product_category, total_revenue),
    y = total_revenue,
    color = product_category
  )
) +
  geom_segment(
    aes(xend = product_category, y = 0, yend = total_revenue),
    linewidth = 1.2
  ) +
  geom_point(size = 6) +
  geom_text(
    aes(label = format_dollar(total_revenue)),
    hjust = -0.15,
    fontface = "bold",
    size = 3.8,
    color = "grey15"
  ) +
  coord_flip() +
  scale_y_continuous(labels = format_dollar, expand = expansion(mult = c(0, 0.2))) +
  scale_color_manual(values = category_palette) +
  labs(
    title = "Category Performance Ranking",
    subtitle = "Revenue ranking highlights the top commercial category",
    caption = "Source: category_ranking.csv"
  ) +
  dashboard_theme +
  theme(legend.position = "none")

ggsave(
  "outputs/visualisations/02_category_performance_ranking.png",
  category_performance_ranking_plot,
  width = 9,
  height = 5,
  dpi = 300
)

# 3. Category value vs volume comparison
# Business question:
# Which categories win through volume, and which win through value per order?
category_value_vs_volume_plot <- ggplot(
  category_ranking,
  aes(
    x = units_sold,
    y = average_order_value,
    size = total_revenue,
    color = product_category
  )
) +
  geom_point(alpha = 0.85) +
  geom_text(
    aes(label = product_category),
    vjust = -1.2,
    fontface = "bold",
    size = 3.8,
    show.legend = FALSE
  ) +
  scale_x_continuous(labels = format_number) +
  scale_y_continuous(labels = format_dollar) +
  scale_size_continuous(range = c(10, 18), labels = format_dollar) +
  scale_color_manual(values = category_palette) +
  labs(
    title = "Category Value vs Volume",
    subtitle = "Bubble size represents total revenue",
    caption = "Source: category_ranking.csv"
  ) +
  dashboard_theme +
  theme(legend.position = "bottom")

ggsave(
  "outputs/visualisations/03_category_value_vs_volume.png",
  category_value_vs_volume_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)

# 4. Monthly performance heatmap
# Business question:
# Which months were strongest or weakest by revenue intensity?
monthly_performance_heatmap <- monthly_sales_trends %>%
  mutate(month_label = factor(year_month, levels = year_month)) %>%
  ggplot(aes(x = month_label, y = "Revenue", fill = total_revenue)) +
  geom_tile(color = "white", linewidth = 1.2, height = 0.75) +
  geom_text(
    aes(label = format_dollar(total_revenue)),
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  scale_fill_gradient(low = "#D9EAF7", high = "#145DA0", labels = format_dollar) +
  labs(
    title = "Monthly Revenue Heatmap",
    subtitle = "Darker months generated higher revenue",
    caption = "Source: monthly_sales_trends.csv"
  ) +
  dashboard_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  "outputs/visualisations/04_monthly_performance_heatmap.png",
  monthly_performance_heatmap,
  width = 10,
  height = 4.5,
  dpi = 300
)

# 5. Customer segment contribution heatmap
# Business question:
# Which customer segments contribute the most revenue by age group and gender?
# Note: This uses existing age-group and gender revenue outputs side by side.
# It is a dashboard-ready segment contribution view, not a cross-tab analysis.
customer_segment_contribution <- bind_rows(
  revenue_by_age_group %>%
    transmute(segment_type = "Age Group", segment = age_group, total_revenue),
  revenue_by_gender %>%
    transmute(segment_type = "Gender", segment = gender, total_revenue)
)

customer_segment_contribution_heatmap <- ggplot(
  customer_segment_contribution,
  aes(
    x = segment,
    y = segment_type,
    fill = total_revenue
  )
) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(
    aes(label = format_dollar(total_revenue)),
    color = "white",
    fontface = "bold",
    size = 3.3
  ) +
  scale_fill_gradient(low = "#E8F3EC", high = "#1F7A4D", labels = format_dollar) +
  labs(
    title = "Customer Segment Revenue Contribution",
    subtitle = "Revenue contribution across existing age-group and gender outputs",
    caption = "Source: kpi_revenue_by_age_group.csv; kpi_revenue_by_gender.csv"
  ) +
  dashboard_theme +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  )

ggsave(
  "outputs/visualisations/05_customer_segment_contribution_heatmap.png",
  customer_segment_contribution_heatmap,
  width = 9,
  height = 5,
  dpi = 300
)

# 6. Revenue share breakdown
# Business question:
# What portion of total revenue does each category represent?
revenue_share_breakdown_plot <- ggplot(
  revenue_share_by_category,
  aes(
    x = 2,
    y = revenue_share,
    fill = product_category
  )
) +
  geom_col(width = 1, color = "white") +
  geom_text(
    aes(label = paste0(product_category, "\n", format_percent(revenue_share))),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.8
  ) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = category_palette) +
  labs(
    title = "Revenue Share Breakdown",
    subtitle = "Donut view of category contribution to revenue",
    caption = "Source: revenue_share_by_category.csv"
  ) +
  dashboard_theme +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

ggsave(
  "outputs/visualisations/06_revenue_share_breakdown.png",
  revenue_share_breakdown_plot,
  width = 7,
  height = 6,
  dpi = 300
)

# 7. Category x customer segment interaction heatmap
# Business question:
# Which categories are most important within each customer age segment?
category_customer_segment_heatmap <- ggplot(
  category_preferences_by_age_group,
  aes(
    x = product_category,
    y = age_group,
    fill = total_revenue
  )
) +
  geom_tile(color = "white", linewidth = 1.1) +
  geom_text(
    aes(label = format_dollar(total_revenue)),
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  scale_fill_gradient(low = "#F4E7D3", high = "#A64B2A", labels = format_dollar) +
  labs(
    title = "Category x Customer Segment Revenue",
    subtitle = "Revenue intensity by age group and product category",
    caption = "Source: customer_category_preferences_by_age_group.csv"
  ) +
  dashboard_theme +
  theme(legend.position = "bottom")

ggsave(
  "outputs/visualisations/07_category_customer_segment_heatmap.png",
  category_customer_segment_heatmap,
  width = 9,
  height = 6,
  dpi = 300
)
