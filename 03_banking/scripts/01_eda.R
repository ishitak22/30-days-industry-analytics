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

# Analysis 1: Transaction Value Distribution

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

# Analysis 2: Transaction Channel Usage Patterns

transaction_channel_column <- "payment_channel"

channel_usage_summary <- banking_transactions %>%
  count(
    channel = .data[[transaction_channel_column]],
    name = "total_transactions",
    sort = TRUE
  ) %>%
  mutate(
    percentage_share = total_transactions / sum(total_transactions) * 100,
    usage_rank = row_number()
  )

channel_usage_bar_chart <- channel_usage_summary %>%
  ggplot(aes(
    x = reorder(channel, total_transactions),
    y = total_transactions
  )) +
  geom_col(
    fill = "#2563EB",
    width = 0.68
  ) +
  geom_text(
    aes(label = comma(total_transactions)),
    hjust = -0.15,
    size = 3.7,
    color = "#1F2937"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Transaction Channel Usage",
    subtitle = "Channels ranked by total transaction volume",
    x = NULL,
    y = "Total transactions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "#4B5563"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

channel_usage_composition_chart <- channel_usage_summary %>%
  mutate(channel = fct_reorder(channel, percentage_share)) %>%
  ggplot(aes(
    x = "All transactions",
    y = percentage_share,
    fill = channel
  )) +
  geom_col(width = 0.48, color = "white", linewidth = 0.6) +
  coord_flip() +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Share of Transaction Activity by Channel",
    subtitle = "Composition view of total transaction volume",
    x = NULL,
    y = "Share of transactions",
    fill = "Channel"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "#4B5563"),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

channel_usage_insight <- tibble(
  insight_title = "Transaction activity is concentrated across customer access channels",
  insight_description = "Channel usage patterns show which customer interaction points carry the highest transaction load and whether activity is spread evenly or concentrated in a smaller number of channels.",
  business_implication = "Retail banking and operations teams can use channel concentration to prioritise capacity planning, service support, and digital channel strategy."
)

# Analysis 3: Transaction Risk and Behaviour Patterns

transaction_value_risk_column <- if ("transaction_amount" %in% names(banking_transactions)) {
  "transaction_amount"
} else {
  NA_character_
}

risk_indicator_column <- case_when(
  "device_risk_score" %in% names(banking_transactions) ~ "device_risk_score",
  "anomaly_score" %in% names(banking_transactions) ~ "anomaly_score",
  "transaction_velocity_score" %in% names(banking_transactions) ~ "transaction_velocity_score",
  TRUE ~ NA_character_
)

fraud_indicator_column <- if ("fraud_flag" %in% names(banking_transactions)) {
  "fraud_flag"
} else {
  NA_character_
}

authentication_type_column <- if ("authentication_type" %in% names(banking_transactions)) {
  "authentication_type"
} else {
  NA_character_
}

international_indicator_column <- if ("international_transaction_flag" %in% names(banking_transactions)) {
  "international_transaction_flag"
} else {
  NA_character_
}

risk_analysis_available <- !is.na(transaction_value_risk_column) &&
  !is.na(risk_indicator_column)

if (risk_analysis_available) {
  risk_lower_threshold <- quantile(
    banking_transactions[[risk_indicator_column]],
    0.33,
    na.rm = TRUE
  )

  risk_upper_threshold <- quantile(
    banking_transactions[[risk_indicator_column]],
    0.67,
    na.rm = TRUE
  )

  transaction_risk_behaviour_data <- banking_transactions %>%
    mutate(
      risk_category = case_when(
        .data[[risk_indicator_column]] <= risk_lower_threshold ~ "Low risk",
        .data[[risk_indicator_column]] <= risk_upper_threshold ~ "Medium risk",
        TRUE ~ "High risk"
      ),
      fraud_indicator = if (!is.na(fraud_indicator_column)) {
        case_when(
          is.na(.data[[fraud_indicator_column]]) ~ NA_integer_,
          as.character(.data[[fraud_indicator_column]]) %in%
            c("TRUE", "True", "true", "1") ~ 1L,
          TRUE ~ 0L
        )
      } else {
        NA_integer_
      },
      transaction_scope = if (!is.na(international_indicator_column)) {
        if_else(
          as.character(.data[[international_indicator_column]]) %in%
            c("TRUE", "True", "true", "1"),
          "International",
          "Domestic"
        )
      } else {
        NA_character_
      }
    )

  risk_transaction_value_summary <- transaction_risk_behaviour_data %>%
    group_by(risk_category) %>%
    summarise(
      total_transactions = n(),
      average_transaction_value = mean(
        .data[[transaction_value_risk_column]],
        na.rm = TRUE
      ),
      median_transaction_value = median(
        .data[[transaction_value_risk_column]],
        na.rm = TRUE
      ),
      average_risk_score = mean(.data[[risk_indicator_column]], na.rm = TRUE),
      fraud_rate = mean(fraud_indicator, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(average_risk_score))

  authentication_risk_summary <- if (!is.na(authentication_type_column)) {
    transaction_risk_behaviour_data %>%
      group_by(
        authentication_type = .data[[authentication_type_column]],
        risk_category
      ) %>%
      summarise(
        total_transactions = n(),
        average_risk_score = mean(.data[[risk_indicator_column]], na.rm = TRUE),
        fraud_rate = mean(fraud_indicator, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      group_by(authentication_type) %>%
      mutate(
        risk_distribution_share =
          total_transactions / sum(total_transactions) * 100
      ) %>%
      ungroup()
  } else {
    tibble(
      authentication_type = character(),
      risk_category = character(),
      total_transactions = integer(),
      average_risk_score = numeric(),
      fraud_rate = numeric(),
      risk_distribution_share = numeric()
    )
  }

  domestic_international_risk_summary <- if (!is.na(international_indicator_column)) {
    transaction_risk_behaviour_data %>%
      group_by(transaction_scope) %>%
      summarise(
        total_transactions = n(),
        average_transaction_value = mean(
          .data[[transaction_value_risk_column]],
          na.rm = TRUE
        ),
        median_transaction_value = median(
          .data[[transaction_value_risk_column]],
          na.rm = TRUE
        ),
        average_risk_score = mean(.data[[risk_indicator_column]], na.rm = TRUE),
        fraud_rate = mean(fraud_indicator, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(average_risk_score))
  } else {
    tibble(
      transaction_scope = character(),
      total_transactions = integer(),
      average_transaction_value = numeric(),
      median_transaction_value = numeric(),
      average_risk_score = numeric(),
      fraud_rate = numeric()
    )
  }

  risk_value_boxplot <- transaction_risk_behaviour_data %>%
    mutate(
      risk_category = factor(
        risk_category,
        levels = c("Low risk", "Medium risk", "High risk")
      )
    ) %>%
    ggplot(aes(
      x = risk_category,
      y = .data[[transaction_value_risk_column]],
      fill = risk_category
    )) +
    geom_boxplot(
      color = "#1F2937",
      outlier.color = "#DC2626",
      outlier.fill = "#FEE2E2",
      outlier.shape = 21,
      outlier.size = 2,
      alpha = 0.82
    ) +
    scale_y_continuous(labels = label_dollar()) +
    scale_fill_manual(
      values = c(
        "Low risk" = "#60A5FA",
        "Medium risk" = "#FBBF24",
        "High risk" = "#EF4444"
      )
    ) +
    labs(
      title = "Transaction Value by Risk Category",
      subtitle = "Distribution of transaction values across device risk bands",
      x = NULL,
      y = "Transaction value",
      fill = "Risk category"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "#4B5563"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )

  authentication_risk_heatmap <- if (!is.na(authentication_type_column)) {
    authentication_risk_summary %>%
      mutate(
        risk_category = factor(
          risk_category,
          levels = c("Low risk", "Medium risk", "High risk")
        )
      ) %>%
      ggplot(aes(
        x = authentication_type,
        y = risk_category,
        fill = risk_distribution_share
      )) +
      geom_tile(color = "white", linewidth = 0.7) +
      geom_text(
        aes(label = label_percent(scale = 1, accuracy = 0.1)(risk_distribution_share)),
        size = 3.4,
        color = "#111827"
      ) +
      scale_fill_gradient(
        low = "#DBEAFE",
        high = "#1D4ED8",
        labels = label_percent(scale = 1)
      ) +
      labs(
        title = "Authentication Type and Risk Distribution",
        subtitle = "Share of each authentication type by risk category",
        x = "Authentication type",
        y = "Risk category",
        fill = "Share"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "#4B5563"),
        panel.grid = element_blank()
      )
  } else {
    NULL
  }

  highest_risk_segment <- risk_transaction_value_summary %>%
    slice_max(average_risk_score, n = 1, with_ties = FALSE)

  lowest_risk_segment <- risk_transaction_value_summary %>%
    slice_min(average_risk_score, n = 1, with_ties = FALSE)

  transaction_risk_insight <- tibble(
    insight_title = "Risk patterns can be linked to transaction behaviour",
    insight_description = "Transaction value, authentication type, fraud flags, and international activity provide a practical view of where risk indicators concentrate across the transaction base.",
    business_implication = "Risk and operations teams can use these patterns to focus monitoring on higher-risk transaction behaviours while protecting lower-risk customer journeys from unnecessary friction."
  )
} else {
  transaction_risk_behaviour_data <- tibble()
  risk_transaction_value_summary <- tibble()
  authentication_risk_summary <- tibble()
  domestic_international_risk_summary <- tibble()
  risk_value_boxplot <- NULL
  authentication_risk_heatmap <- NULL
  highest_risk_segment <- tibble()
  lowest_risk_segment <- tibble()

  transaction_risk_insight <- tibble(
    insight_title = "Transaction risk analysis requires value and risk fields",
    insight_description = "The dataset does not contain both a transaction value field and a usable risk indicator field for this analysis.",
    business_implication = "Risk behaviour analysis should wait until core transaction value and risk indicator fields are available."
  )
}

