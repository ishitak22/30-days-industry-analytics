# Day 03 - Banking

library(tidyverse)
library(janitor)

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
