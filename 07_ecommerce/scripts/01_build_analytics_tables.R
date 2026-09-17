# Phase 2: build analysis-ready ecommerce tables from unchanged Olist CSVs.
# The script keeps raw files immutable and builds tables only after all expected
# source files are present.

expected_files <- c(
  customers = "olist_customers_dataset.csv",
  geolocation = "olist_geolocation_dataset.csv",
  items = "olist_order_items_dataset.csv",
  payments = "olist_order_payments_dataset.csv",
  reviews = "olist_order_reviews_dataset.csv",
  orders = "olist_orders_dataset.csv",
  products = "olist_products_dataset.csv",
  sellers = "olist_sellers_dataset.csv",
  translation = "product_category_name_translation.csv"
)

required_packages <- c("readr", "dplyr", "tibble", "lubridate")

industry_dir <- function() {
  if (file.exists("30-days-industry-analytics.Rproj") &&
      dir.exists("07_ecommerce")) return("07_ecommerce")
  if (basename(getwd()) == "07_ecommerce" && dir.exists("scripts")) return(".")
  stop("Run from the repository root or 07_ecommerce directory.", call. = FALSE)
}

money_cents <- function(x) {
  valid <- !is.na(x) & grepl("^-?[0-9]+(\\.[0-9]{1,2})?$", x)
  out <- rep(NA_real_, length(x))
  for (i in which(valid)) {
    parts <- strsplit(sub("^-", "", x[i]), ".", fixed = TRUE)[[1]]
    fraction <- if (length(parts) == 2L) {
      as.numeric(paste0(parts[2], if (nchar(parts[2]) == 1L) "0" else ""))
    } else {
      0
    }
    amount <- as.numeric(parts[1]) * 100 + fraction
    if (is.finite(amount) && amount <= 2^52) {
      out[i] <- amount * if (startsWith(x[i], "-")) -1 else 1
    }
  }
  out
}

write_phase2_summary <- function(output_dir, rows) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    rows,
    file.path(output_dir, "phase2_summary.csv"),
    row.names = FALSE,
    na = ""
  )
}

write_empty_inventory <- function(output_dir) {
  utils::write.csv(
    data.frame(
      table_name = character(),
      grain = character(),
      rows = integer(),
      columns = integer(),
      path = character()
    ),
    file.path(output_dir, "phase2_table_inventory.csv"),
    row.names = FALSE,
    na = ""
  )
}

read_sources <- function(raw_dir) {
  paths <- file.path(raw_dir, unname(expected_files))
  names(paths) <- names(expected_files)
  lapply(paths, function(path) {
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      na = c("", "NA"),
      trim_ws = TRUE,
      show_col_types = FALSE,
      progress = FALSE
    )
  })
}

write_table <- function(df, table_name, grain, processed_dir) {
  path <- file.path(processed_dir, paste0(table_name, ".csv"))
  readr::write_csv(df, path, na = "")
  data.frame(
    table_name = table_name,
    grain = grain,
    rows = nrow(df),
    columns = ncol(df),
    path = path
  )
}

first_day <- function(x) {
  as.Date(format(x, "%Y-%m-01"))
}

run_phase2_build <- function(raw_dir, processed_dir, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- file.path(raw_dir, unname(expected_files))
  missing_files <- unname(expected_files)[!file.exists(paths)]
  if (length(missing_files)) {
    write_phase2_summary(output_dir, data.frame(
      metric = c("phase2_state", "missing_source_files", "processed_tables"),
      value = c("NOT RUN", paste(missing_files, collapse = "|"), "0"),
      interpretation = c(
        "Raw Olist CSVs are not available, so analytics tables were not built.",
        "Download the unchanged source files documented in DATASET.md.",
        "No processed outputs were created or refreshed."
      )
    ))
    write_empty_inventory(output_dir)
    stop(
      paste0(
        "Missing original Olist CSVs in ", raw_dir, ":\n",
        paste(missing_files, collapse = "\n"),
        "\nPhase 2 tables were not built."
      ),
      call. = FALSE
    )
  }

  missing_packages <- required_packages[!vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]
  if (length(missing_packages)) {
    write_phase2_summary(output_dir, data.frame(
      metric = c("phase2_state", "missing_packages", "processed_tables"),
      value = c("NOT RUN", paste(missing_packages, collapse = "|"), "0"),
      interpretation = c(
        "Required R packages are missing.",
        "Install these packages before building processed tables.",
        "No processed outputs were created or refreshed."
      )
    ))
    write_empty_inventory(output_dir)
    stop(
      paste("Install required R packages first:", paste(missing_packages, collapse = ", ")),
      call. = FALSE
    )
  }

  suppressPackageStartupMessages({
    library(dplyr)
    library(lubridate)
  })

  src <- read_sources(raw_dir)

  customers <- src$customers %>%
    transmute(
      customer_id,
      customer_unique_id,
      customer_zip_code_prefix,
      customer_city,
      customer_state
    )

  products <- src$products %>%
    left_join(src$translation, by = "product_category_name") %>%
    transmute(
      product_id,
      product_category_name,
      product_category_name_english = if_else(
        is.na(product_category_name_english) | product_category_name_english == "",
        product_category_name,
        product_category_name_english
      ),
      product_name_lenght = suppressWarnings(as.integer(product_name_lenght)),
      product_description_lenght = suppressWarnings(as.integer(product_description_lenght)),
      product_photos_qty = suppressWarnings(as.integer(product_photos_qty)),
      product_weight_g = suppressWarnings(as.numeric(product_weight_g)),
      product_length_cm = suppressWarnings(as.numeric(product_length_cm)),
      product_height_cm = suppressWarnings(as.numeric(product_height_cm)),
      product_width_cm = suppressWarnings(as.numeric(product_width_cm))
    )

  sellers <- src$sellers %>%
    transmute(
      seller_id,
      seller_zip_code_prefix,
      seller_city,
      seller_state
    )

  order_items <- src$items %>%
    mutate(
      order_item_id = suppressWarnings(as.integer(order_item_id)),
      shipping_limit_timestamp = lubridate::ymd_hms(shipping_limit_date, tz = "UTC", quiet = TRUE),
      price_cents = money_cents(price),
      freight_cents = money_cents(freight_value),
      item_gmv_cents = price_cents + freight_cents
    ) %>%
    left_join(products %>% select(product_id, product_category_name_english), by = "product_id") %>%
    transmute(
      order_id,
      order_item_id,
      product_id,
      seller_id,
      product_category_name_english,
      shipping_limit_timestamp,
      price_cents,
      freight_cents,
      item_gmv_cents
    )

  payments <- src$payments %>%
    transmute(
      order_id,
      payment_sequential = suppressWarnings(as.integer(payment_sequential)),
      payment_type,
      payment_installments = suppressWarnings(as.integer(payment_installments)),
      payment_value_cents = money_cents(payment_value)
    )

  reviews <- src$reviews %>%
    transmute(
      review_id,
      order_id,
      review_score = suppressWarnings(as.integer(review_score)),
      review_creation_date = as.Date(review_creation_date),
      review_answer_timestamp = lubridate::ymd_hms(review_answer_timestamp, tz = "UTC", quiet = TRUE),
      review_score_valid = !is.na(review_score) & review_score >= 1 & review_score <= 5
    )

  item_order <- order_items %>%
    group_by(order_id) %>%
    summarise(
      item_count = n(),
      item_price_cents = sum(price_cents, na.rm = TRUE),
      freight_cents = sum(freight_cents, na.rm = TRUE),
      order_gmv_cents = sum(item_gmv_cents, na.rm = TRUE),
      distinct_products = n_distinct(product_id),
      distinct_sellers = n_distinct(seller_id),
      distinct_categories = n_distinct(product_category_name_english),
      .groups = "drop"
    )

  payment_order <- payments %>%
    group_by(order_id) %>%
    summarise(
      payment_records = n(),
      payment_total_cents = sum(payment_value_cents, na.rm = TRUE),
      payment_types = paste(sort(unique(payment_type)), collapse = "|"),
      max_payment_installments = suppressWarnings(max(payment_installments, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(max_payment_installments = if_else(is.infinite(max_payment_installments), NA_integer_, max_payment_installments))

  review_order <- reviews %>%
    group_by(order_id) %>%
    summarise(
      review_records = n(),
      valid_review_records = sum(review_score_valid, na.rm = TRUE),
      avg_review_score = if_else(
        valid_review_records > 0,
        mean(review_score[review_score_valid], na.rm = TRUE),
        NA_real_
      ),
      min_review_score = suppressWarnings(min(review_score[review_score_valid], na.rm = TRUE)),
      max_review_score = suppressWarnings(max(review_score[review_score_valid], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      min_review_score = if_else(is.infinite(min_review_score), NA_integer_, min_review_score),
      max_review_score = if_else(is.infinite(max_review_score), NA_integer_, max_review_score)
    )

  orders_base <- src$orders %>%
    transmute(
      order_id,
      customer_id,
      order_status,
      order_purchase_timestamp = lubridate::ymd_hms(order_purchase_timestamp, tz = "UTC", quiet = TRUE),
      order_approved_at = lubridate::ymd_hms(order_approved_at, tz = "UTC", quiet = TRUE),
      order_delivered_carrier_date = lubridate::ymd_hms(order_delivered_carrier_date, tz = "UTC", quiet = TRUE),
      order_delivered_customer_date = lubridate::ymd_hms(order_delivered_customer_date, tz = "UTC", quiet = TRUE),
      order_estimated_delivery_date = lubridate::ymd_hms(order_estimated_delivery_date, tz = "UTC", quiet = TRUE)
    )

  orders <- orders_base %>%
    left_join(customers, by = "customer_id") %>%
    left_join(item_order, by = "order_id") %>%
    left_join(payment_order, by = "order_id") %>%
    left_join(review_order, by = "order_id") %>%
    mutate(
      purchase_date = as.Date(order_purchase_timestamp),
      purchase_month = first_day(purchase_date),
      delivered_date = as.Date(order_delivered_customer_date),
      estimated_delivery_date = as.Date(order_estimated_delivery_date),
      is_delivered = order_status == "delivered",
      delivery_days = as.numeric(delivered_date - purchase_date),
      delivery_delay_days = as.numeric(delivered_date - estimated_delivery_date),
      is_late_delivery = !is.na(delivery_delay_days) & delivery_delay_days > 0,
      payment_item_difference_cents = payment_total_cents - order_gmv_cents
    )

  customer_sequence <- orders %>%
    filter(!is.na(customer_unique_id), !is.na(order_purchase_timestamp)) %>%
    arrange(customer_unique_id, order_purchase_timestamp, order_id) %>%
    group_by(customer_unique_id) %>%
    mutate(
      order_number_all_observed = row_number(),
      previous_order_timestamp = lag(order_purchase_timestamp),
      days_since_previous_order = as.numeric(as.Date(order_purchase_timestamp) - as.Date(previous_order_timestamp))
    ) %>%
    ungroup() %>%
    select(order_id, order_number_all_observed, previous_order_timestamp, days_since_previous_order)

  fact_orders <- orders %>%
    left_join(customer_sequence, by = "order_id") %>%
    mutate(
      is_first_observed_order = order_number_all_observed == 1,
      is_repeat_observed_order = order_number_all_observed > 1
    ) %>%
    transmute(
      order_id,
      customer_id,
      customer_unique_id,
      order_status,
      purchase_date,
      purchase_month,
      order_purchase_timestamp,
      order_approved_at,
      order_delivered_carrier_date,
      order_delivered_customer_date,
      order_estimated_delivery_date,
      item_count,
      item_price_cents,
      freight_cents,
      order_gmv_cents,
      payment_total_cents,
      payment_item_difference_cents,
      payment_records,
      payment_types,
      max_payment_installments,
      distinct_products,
      distinct_sellers,
      distinct_categories,
      review_records,
      valid_review_records,
      avg_review_score,
      min_review_score,
      max_review_score,
      is_delivered,
      delivered_date,
      estimated_delivery_date,
      delivery_days,
      delivery_delay_days,
      is_late_delivery,
      order_number_all_observed,
      is_first_observed_order,
      is_repeat_observed_order,
      previous_order_timestamp,
      days_since_previous_order
    )

  customer_history <- fact_orders %>%
    group_by(customer_unique_id) %>%
    summarise(
      order_count_observed = n_distinct(order_id),
      delivered_order_count = sum(is_delivered, na.rm = TRUE),
      first_observed_purchase_date = min(purchase_date, na.rm = TRUE),
      last_observed_purchase_date = max(purchase_date, na.rm = TRUE),
      observed_order_gmv_cents = sum(order_gmv_cents, na.rm = TRUE),
      observed_payment_cents = sum(payment_total_cents, na.rm = TRUE),
      avg_observed_review_score = mean(avg_review_score, na.rm = TRUE),
      has_repeat_observed_order = order_count_observed > 1,
      .groups = "drop"
    ) %>%
    mutate(
      first_observed_purchase_date = if_else(
        is.infinite(first_observed_purchase_date),
        as.Date(NA),
        first_observed_purchase_date
      ),
      last_observed_purchase_date = if_else(
        is.infinite(last_observed_purchase_date),
        as.Date(NA),
        last_observed_purchase_date
      ),
      avg_observed_review_score = if_else(is.nan(avg_observed_review_score), NA_real_, avg_observed_review_score)
    )

  monthly_kpis <- fact_orders %>%
    filter(!is.na(purchase_month)) %>%
    group_by(purchase_month) %>%
    summarise(
      orders = n_distinct(order_id),
      delivered_orders = sum(is_delivered, na.rm = TRUE),
      customers = n_distinct(customer_unique_id),
      repeat_orders = sum(is_repeat_observed_order, na.rm = TRUE),
      new_customer_orders = sum(is_first_observed_order, na.rm = TRUE),
      order_gmv_cents = sum(order_gmv_cents, na.rm = TRUE),
      payment_total_cents = sum(payment_total_cents, na.rm = TRUE),
      avg_order_value_cents = if_else(orders > 0, order_gmv_cents / orders, NA_real_),
      repeat_order_rate = if_else(orders > 0, repeat_orders / orders, NA_real_),
      avg_review_score = mean(avg_review_score, na.rm = TRUE),
      late_delivery_rate = mean(is_late_delivery, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      avg_review_score = if_else(is.nan(avg_review_score), NA_real_, avg_review_score),
      late_delivery_rate = if_else(is.nan(late_delivery_rate), NA_real_, late_delivery_rate)
    )

  category_performance <- order_items %>%
    left_join(
      fact_orders %>%
        select(order_id, customer_unique_id, purchase_month, order_status, avg_review_score, is_late_delivery),
      by = "order_id"
    ) %>%
    group_by(product_category_name_english) %>%
    summarise(
      orders = n_distinct(order_id),
      customers = n_distinct(customer_unique_id),
      item_count = n(),
      item_price_cents = sum(price_cents, na.rm = TRUE),
      freight_cents = sum(freight_cents, na.rm = TRUE),
      item_gmv_cents = sum(item_gmv_cents, na.rm = TRUE),
      avg_item_price_cents = mean(price_cents, na.rm = TRUE),
      avg_review_score = mean(avg_review_score, na.rm = TRUE),
      late_delivery_rate = mean(is_late_delivery, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(item_gmv_cents)) %>%
    mutate(
      gmv_rank = row_number(),
      avg_item_price_cents = if_else(is.nan(avg_item_price_cents), NA_real_, avg_item_price_cents),
      avg_review_score = if_else(is.nan(avg_review_score), NA_real_, avg_review_score),
      late_delivery_rate = if_else(is.nan(late_delivery_rate), NA_real_, late_delivery_rate)
    )

  min_date <- suppressWarnings(min(fact_orders$purchase_date, na.rm = TRUE))
  max_date <- suppressWarnings(max(fact_orders$purchase_date, na.rm = TRUE))
  dim_date <- if (is.finite(min_date) && is.finite(max_date)) {
    tibble::tibble(date = seq.Date(min_date, max_date, by = "day")) %>%
      mutate(
        year = lubridate::year(date),
        quarter = lubridate::quarter(date),
        month = lubridate::month(date),
        month_start = first_day(date),
        month_label = format(date, "%Y-%m"),
        week_start = lubridate::floor_date(date, "week", week_start = 1),
        weekday = weekdays(date)
      )
  } else {
    tibble::tibble(
      date = as.Date(character()),
      year = integer(),
      quarter = integer(),
      month = integer(),
      month_start = as.Date(character()),
      month_label = character(),
      week_start = as.Date(character()),
      weekday = character()
    )
  }

  inventory <- dplyr::bind_rows(
    write_table(customers, "dim_customers", "one row per order-specific customer_id", processed_dir),
    write_table(products, "dim_products", "one row per product_id", processed_dir),
    write_table(sellers, "dim_sellers", "one row per seller_id", processed_dir),
    write_table(dim_date, "dim_date", "one row per calendar date in observed purchase range", processed_dir),
    write_table(order_items, "fact_order_items", "one row per order_id and order_item_id", processed_dir),
    write_table(payments, "fact_payments", "one row per order payment component", processed_dir),
    write_table(reviews, "fact_reviews", "one row per source review record", processed_dir),
    write_table(fact_orders, "fact_orders", "one row per order_id with order-level aggregates", processed_dir),
    write_table(customer_history, "customer_order_history", "one row per observed customer_unique_id", processed_dir),
    write_table(monthly_kpis, "monthly_kpis", "one row per purchase month", processed_dir),
    write_table(category_performance, "category_performance", "one row per translated product category", processed_dir)
  )

  readr::write_csv(inventory, file.path(output_dir, "phase2_table_inventory.csv"), na = "")
  write_phase2_summary(output_dir, data.frame(
    metric = c(
      "phase2_state",
      "processed_tables",
      "fact_orders_rows",
      "fact_order_items_rows",
      "customer_history_rows",
      "primary_grain_guard"
    ),
    value = c(
      "COMPLETED",
      nrow(inventory),
      nrow(fact_orders),
      nrow(order_items),
      nrow(customer_history),
      "order/items/payments/reviews aggregated separately before order-level joins"
    ),
    interpretation = c(
      "Processed tables were rebuilt from unchanged raw Olist CSVs.",
      "Number of CSV tables written to data/processed.",
      "Main order-grain table available for KPI and cohort work.",
      "Item-grain table available for product/category analysis.",
      "Observed persistent-customer table available for repeat behaviour analysis.",
      "This prevents item-payment-review fan-out from inflating GMV or payments."
    )
  ))

  invisible(list(
    inventory = inventory,
    fact_orders = fact_orders,
    customer_history = customer_history,
    monthly_kpis = monthly_kpis,
    category_performance = category_performance
  ))
}

if (!isTRUE(getOption("ecommerce.phase2.no_run"))) {
  root <- industry_dir()
  run_phase2_build(
    raw_dir = file.path(root, "data", "raw"),
    processed_dir = file.path(root, "data", "processed"),
    output_dir = file.path(root, "outputs")
  )
}
