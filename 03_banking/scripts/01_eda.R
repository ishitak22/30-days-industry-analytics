# Day 03 - Banking

library(tidyverse)
library(janitor)
library(tibble)
library(scales)

banking_transactions <- read_csv(
  "03_banking/data/banking_transactions.csv",
  show_col_types = FALSE
) %>%
  clean_names()

glimpse(banking_transactions)
names(banking_transactions)
dim(banking_transactions)

# Data Quality Assessment 

# 1. Missing Data Analysis
missing_counts <- banking_transactions %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# 2. Duplicate Records
duplicate_rows <- sum(duplicated(banking_transactions))

# 3. Data Structure Validation
data_type_validation <- tibble(
  column_name = names(banking_transactions),
  data_type = map_chr(banking_transactions, ~ paste(class(.x), collapse = ", ")),
  expected_role = case_when(
    str_detect(column_name, "date|time") ~ "date_or_time_field",
    str_detect(column_name, "amount|balance|score|count|frequency|age|hour|distance|duration|attempt") ~ "numeric_transaction_value",
    str_detect(column_name, "channel|type|flag|authentication") ~ "categorical_or_flag",
    TRUE ~ "identifier_or_other"
  )
)

date_columns <- names(banking_transactions)[
  map_lgl(banking_transactions, ~ inherits(.x, c("Date", "POSIXct", "POSIXt")))
]

numeric_columns <- names(banking_transactions)[
  map_lgl(banking_transactions, is.numeric)
]

categorical_columns <- names(banking_transactions)[
  map_lgl(banking_transactions, ~ is.character(.x) || is.factor(.x) || is.logical(.x))
]

# 4. Basic Distribution Checks
numeric_distribution_summary <- banking_transactions %>%
  select(where(is.numeric)) %>%
  summary()

potential_outliers <- banking_transactions %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    everything(),
    names_to = "column_name",
    values_to = "value"
  ) %>%
  group_by(column_name) %>%
  summarise(
    minimum_value = min(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    maximum_value = max(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE),
    lower_outlier_count = sum(value < q1 - 1.5 * iqr, na.rm = TRUE),
    upper_outlier_count = sum(value > q3 + 1.5 * iqr, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Business Data Health Check
customer_columns <- names(banking_transactions)[
  str_detect(names(banking_transactions), "customer_id|customer_number|customer_no")
]

account_columns <- names(banking_transactions)[
  str_detect(names(banking_transactions), "account_id|account_number|account_no")
]

transaction_type_columns <- names(banking_transactions)[
  str_detect(names(banking_transactions), "transaction_type|payment_channel")
]

business_health_summary <- tibble(
  total_transactions = nrow(banking_transactions),
  unique_customers = if (length(customer_columns) > 0) {
    n_distinct(banking_transactions[[customer_columns[1]]])
  } else {
    NA_integer_
  },
  unique_accounts = if (length(account_columns) > 0) {
    n_distinct(banking_transactions[[account_columns[1]]])
  } else {
    NA_integer_
  },
  earliest_transaction_date = if (length(date_columns) > 0) {
    min(banking_transactions[[date_columns[1]]], na.rm = TRUE)
  } else {
    as.Date(NA)
  },
  latest_transaction_date = if (length(date_columns) > 0) {
    max(banking_transactions[[date_columns[1]]], na.rm = TRUE)
  } else {
    as.Date(NA)
  }
)

transaction_type_counts <- if (length(transaction_type_columns) > 0) {
  banking_transactions %>%
    count(
      transaction_type = .data[[transaction_type_columns[1]]],
      name = "transaction_count",
      sort = TRUE
    )
} else {
  tibble(
    transaction_type = character(),
    transaction_count = integer()
  )
}

# Business Data Inventory -------------------------------------------------

dataset_fields <- names(banking_transactions)

customer_related_fields <- dataset_fields[
  str_detect(dataset_fields, "customer|client|user")
]

account_related_fields <- dataset_fields[
  str_detect(dataset_fields, "account")
]

transaction_related_fields <- dataset_fields[
  str_detect(dataset_fields, "transaction|transfer|payment")
]

monetary_value_related_fields <- dataset_fields[
  str_detect(dataset_fields, "amount|balance|value|fee|charge|cost|revenue")
]

date_time_fields <- dataset_fields[
  str_detect(dataset_fields, "date|time|hour|day|month|year")
]

payment_channel_fields <- dataset_fields[
  str_detect(dataset_fields, "payment|channel|card|authentication")
]

location_related_fields <- dataset_fields[
  str_detect(dataset_fields, "location|geo|distance|country|city|state|region|ip")
]

fraud_risk_related_fields <- dataset_fields[
  str_detect(dataset_fields, "fraud|risk|anomaly|suspicious|failed|velocity")
]

demographic_fields <- dataset_fields[
  str_detect(dataset_fields, "age|gender|income|occupation|segment|demographic")
]

inventory_grouped_fields <- c(
  customer_related_fields,
  account_related_fields,
  transaction_related_fields,
  monetary_value_related_fields,
  date_time_fields,
  payment_channel_fields,
  location_related_fields,
  fraud_risk_related_fields,
  demographic_fields
) %>%
  unique()

other_fields <- setdiff(dataset_fields, inventory_grouped_fields)

business_data_inventory <- tibble(
  category = c(
    "customer_related_fields",
    "account_related_fields",
    "transaction_related_fields",
    "monetary_value_related_fields",
    "date_time_fields",
    "payment_channel_fields",
    "location_related_fields",
    "fraud_risk_related_fields",
    "demographic_fields",
    "other_fields"
  ),
  field_names = list(
    customer_related_fields,
    account_related_fields,
    transaction_related_fields,
    monetary_value_related_fields,
    date_time_fields,
    payment_channel_fields,
    location_related_fields,
    fraud_risk_related_fields,
    demographic_fields,
    other_fields
  )
) %>%
  mutate(
    number_of_fields = map_int(field_names, length),
    field_names = map_chr(field_names, ~ paste(.x, collapse = ", "))
  ) %>%
  select(category, number_of_fields, field_names)

# Business Question Discovery ---------------------------------------------

business_questions <- tibble(
  business_question = c(
    "Which payment channels are used most often for banking transactions?",
    "How does transaction value differ across payment channels?",
    "Are higher-value transactions associated with stronger fraud or risk indicators?",
    "Which authentication types are most common across transactions?",
    "Do international transactions show different fraud or risk patterns than domestic transactions?",
    "Are card-present and card-not-present transactions associated with different risk profiles?",
    "How does transaction activity vary by hour of day?",
    "Are transactions from greater geographic distances associated with higher risk scores?",
    "Do accounts with different account ages show different transaction or risk patterns?",
    "Are customers with higher average monthly balances associated with different transaction values or risk indicators?"
  ),
  business_objective = c(
    "Understand customer channel usage across the transaction base.",
    "Identify whether transaction value differs by banking access channel.",
    "Assess whether transaction size aligns with known fraud and risk signals.",
    "Understand authentication usage across the transaction base.",
    "Compare risk patterns between international and domestic transaction activity.",
    "Review risk exposure across card-present and card-not-present activity.",
    "Understand operational activity patterns across transaction hours.",
    "Assess whether location distance is a useful signal for transaction risk.",
    "Explore whether account tenure relates to transaction behaviour or risk.",
    "Understand whether balance levels relate to transaction size or risk exposure."
  ),
  potential_business_value = c(
    "Support channel planning, service design, and operational staffing decisions.",
    "Help retail banking teams understand where higher-value activity occurs.",
    "Support risk teams in prioritising risk indicators for monitoring.",
    "Inform authentication policy and customer experience review.",
    "Help risk and operations teams focus controls on higher-risk transaction contexts.",
    "Support card risk monitoring and control design.",
    "Help operations teams understand peak activity windows.",
    "Support fraud monitoring rules that consider geographic transaction behaviour.",
    "Help customer strategy and risk teams understand account lifecycle patterns.",
    "Support customer strategy by linking balance profile to activity and risk."
  ),
  likely_fields_used = c(
    "payment_channel",
    "transaction_amount, payment_channel",
    "transaction_amount, fraud_flag, anomaly_score, device_risk_score, transaction_velocity_score",
    "authentication_type",
    "international_transaction_flag, fraud_flag, anomaly_score, device_risk_score, transaction_velocity_score",
    "card_present_flag, fraud_flag, anomaly_score, device_risk_score, transaction_velocity_score",
    "transaction_time_hour, daily_transaction_count, transfer_frequency",
    "geo_distance_km, fraud_flag, anomaly_score, device_risk_score, suspicious_ip_flag",
    "account_age_days, transaction_amount, transfer_frequency, fraud_flag, anomaly_score",
    "avg_monthly_balance, transaction_amount, fraud_flag, anomaly_score, device_risk_score"
  )
)

# Analysis 1: Transaction Value Distribution ------------------------------

transaction_value_column <- "transaction_amount"

transaction_value_summary <- banking_transactions %>%
  summarise(
    minimum_transaction_value = min(.data[[transaction_value_column]], na.rm = TRUE),
    average_transaction_value = mean(.data[[transaction_value_column]], na.rm = TRUE),
    median_transaction_value = median(.data[[transaction_value_column]], na.rm = TRUE),
    maximum_transaction_value = max(.data[[transaction_value_column]], na.rm = TRUE),
    standard_deviation = sd(.data[[transaction_value_column]], na.rm = TRUE)
  )

transaction_value_quantiles <- banking_transactions %>%
  summarise(
    q0 = quantile(.data[[transaction_value_column]], 0, na.rm = TRUE),
    q25 = quantile(.data[[transaction_value_column]], 0.25, na.rm = TRUE),
    q50 = quantile(.data[[transaction_value_column]], 0.50, na.rm = TRUE),
    q75 = quantile(.data[[transaction_value_column]], 0.75, na.rm = TRUE),
    q100 = quantile(.data[[transaction_value_column]], 1, na.rm = TRUE)
  )

transaction_value_iqr <- IQR(
  banking_transactions[[transaction_value_column]],
  na.rm = TRUE
)

transaction_value_extreme_thresholds <- banking_transactions %>%
  summarise(
    q1 = quantile(.data[[transaction_value_column]], 0.25, na.rm = TRUE),
    q3 = quantile(.data[[transaction_value_column]], 0.75, na.rm = TRUE),
    iqr = transaction_value_iqr,
    lower_extreme_threshold = q1 - 1.5 * iqr,
    upper_extreme_threshold = q3 + 1.5 * iqr
  )

transaction_value_extreme_values <- banking_transactions %>%
  filter(
    .data[[transaction_value_column]] <
      transaction_value_extreme_thresholds$lower_extreme_threshold |
      .data[[transaction_value_column]] >
      transaction_value_extreme_thresholds$upper_extreme_threshold
  ) %>%
  select(transaction_id, transaction_amount, payment_channel, fraud_flag)

transaction_value_boxplot <- banking_transactions %>%
  ggplot(aes(y = .data[[transaction_value_column]])) +
  geom_boxplot(
    fill = "#3B82F6",
    color = "#1F2937",
    outlier.color = "#DC2626",
    outlier.fill = "#FEE2E2",
    outlier.shape = 21,
    outlier.size = 2.4,
    width = 0.28,
    alpha = 0.82
  ) +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(
    title = "Transaction Value Distribution",
    subtitle = "Outliers highlight unusually high or low transaction values",
    x = NULL,
    y = "Transaction value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "#4B5563"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()
  )

transaction_value_density_plot <- banking_transactions %>%
  ggplot(aes(x = .data[[transaction_value_column]])) +
  geom_density(
    fill = "#14B8A6",
    color = "#0F766E",
    linewidth = 1,
    alpha = 0.35
  ) +
  geom_vline(
    xintercept = transaction_value_summary$median_transaction_value,
    color = "#7C2D12",
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  scale_x_continuous(labels = scales::label_dollar()) +
  labs(
    title = "Transaction Value Concentration",
    subtitle = "Density curve shows where transaction values cluster across the portfolio",
    x = "Transaction value",
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "#4B5563"),
    panel.grid.minor = element_blank()
  )

transaction_value_insight <- tibble(
  insight_title = "Transaction values show portfolio concentration and high-value behaviour",
  insight_description = "The distribution of transaction amounts helps identify whether most activity is concentrated in routine transaction bands while a smaller set of high-value transactions sits in the upper tail.",
  business_implication = "Banking teams should review high-value transaction behaviour separately from everyday activity because it may carry different operational, service, and risk considerations."
)

# Business meaning: transaction value distribution shows whether everyday
# banking activity is broadly balanced or whether a smaller group of large
# transactions contributes a disproportionate share of value.
