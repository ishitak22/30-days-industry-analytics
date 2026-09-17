options(ecommerce.phase2.no_run = TRUE)
source("07_ecommerce/scripts/01_build_analytics_tables.R")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

make_fixture <- function(base_dir) {
  raw_dir <- file.path(base_dir, "raw")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  utils::write.csv(data.frame(
    customer_id = c("c1", "c2", "c3"),
    customer_unique_id = c("u1", "u1", "u2"),
    customer_zip_code_prefix = c("1000", "1000", "2000"),
    customer_city = c("sao paulo", "sao paulo", "rio"),
    customer_state = c("SP", "SP", "RJ")
  ), file.path(raw_dir, "olist_customers_dataset.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    order_id = c("o1", "o2", "o3"),
    customer_id = c("c1", "c2", "c3"),
    order_status = c("delivered", "delivered", "canceled"),
    order_purchase_timestamp = c("2017-01-01 10:00:00", "2017-02-10 10:00:00", "2017-02-15 10:00:00"),
    order_approved_at = c("2017-01-01 11:00:00", "2017-02-10 12:00:00", NA),
    order_delivered_carrier_date = c("2017-01-02 10:00:00", "2017-02-12 10:00:00", NA),
    order_delivered_customer_date = c("2017-01-05 10:00:00", "2017-02-25 10:00:00", NA),
    order_estimated_delivery_date = c("2017-01-08 10:00:00", "2017-02-20 10:00:00", "2017-03-01 10:00:00")
  ), file.path(raw_dir, "olist_orders_dataset.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    order_id = c("o1", "o1", "o2"),
    order_item_id = c("1", "2", "1"),
    product_id = c("p1", "p2", "p1"),
    seller_id = c("s1", "s1", "s2"),
    shipping_limit_date = c("2017-01-04 10:00:00", "2017-01-04 10:00:00", "2017-02-15 10:00:00"),
    price = c("10.00", "20.00", "30.00"),
    freight_value = c("1.00", "2.00", "3.00")
  ), file.path(raw_dir, "olist_order_items_dataset.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    order_id = c("o1", "o1", "o2"),
    payment_sequential = c("1", "2", "1"),
    payment_type = c("credit_card", "voucher", "credit_card"),
    payment_installments = c("1", "1", "2"),
    payment_value = c("25.00", "8.00", "33.00")
  ), file.path(raw_dir, "olist_order_payments_dataset.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    review_id = c("r1", "r2", "r3"),
    order_id = c("o1", "o1", "o2"),
    review_score = c("5", "4", "2"),
    review_creation_date = c("2017-01-06", "2017-01-07", "2017-02-26"),
    review_answer_timestamp = c("2017-01-07 10:00:00", "2017-01-08 10:00:00", "2017-02-27 10:00:00")
  ), file.path(raw_dir, "olist_order_reviews_dataset.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    product_id = c("p1", "p2"),
    product_category_name = c("beleza_saude", "cama_mesa_banho"),
    product_name_lenght = c("10", "20"),
    product_description_lenght = c("100", "200"),
    product_photos_qty = c("1", "2"),
    product_weight_g = c("100", "200"),
    product_length_cm = c("10", "20"),
    product_height_cm = c("5", "8"),
    product_width_cm = c("4", "7")
  ), file.path(raw_dir, "olist_products_dataset.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    seller_id = c("s1", "s2"),
    seller_zip_code_prefix = c("3000", "4000"),
    seller_city = c("campinas", "curitiba"),
    seller_state = c("SP", "PR")
  ), file.path(raw_dir, "olist_sellers_dataset.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    geolocation_zip_code_prefix = c("1000", "2000"),
    geolocation_lat = c("-23.5", "-22.9"),
    geolocation_lng = c("-46.6", "-43.2")
  ), file.path(raw_dir, "olist_geolocation_dataset.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    product_category_name = c("beleza_saude", "cama_mesa_banho"),
    product_category_name_english = c("health_beauty", "bed_bath_table")
  ), file.path(raw_dir, "product_category_name_translation.csv"), row.names = FALSE)

  raw_dir
}

tmp <- tempfile("phase2_fixture_")
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

missing_output <- file.path(tmp, "missing_outputs")
invisible(tryCatch(
  run_phase2_build(
    raw_dir = file.path(tmp, "missing_raw"),
    processed_dir = file.path(tmp, "missing_processed"),
    output_dir = missing_output
  ),
  error = function(e) NULL
))
missing_summary <- read.csv(file.path(missing_output, "phase2_summary.csv"))
assert(missing_summary$value[missing_summary$metric == "phase2_state"] == "NOT RUN",
       "Missing raw files should produce a NOT RUN summary.")
assert(nrow(read.csv(file.path(missing_output, "phase2_table_inventory.csv"))) == 0,
       "Missing raw files should produce an empty table inventory.")

raw_dir <- make_fixture(tmp)
processed_dir <- file.path(tmp, "processed")
output_dir <- file.path(tmp, "outputs")
result <- run_phase2_build(raw_dir, processed_dir, output_dir)

inventory <- read.csv(file.path(output_dir, "phase2_table_inventory.csv"))
assert(nrow(inventory) == 11, "Phase 2 should write 11 processed tables.")
assert(all(file.exists(inventory$path)), "Every inventory path should exist.")

fact_orders <- read.csv(file.path(processed_dir, "fact_orders.csv"))
customer_history <- read.csv(file.path(processed_dir, "customer_order_history.csv"))
monthly_kpis <- read.csv(file.path(processed_dir, "monthly_kpis.csv"))
category_performance <- read.csv(file.path(processed_dir, "category_performance.csv"))

assert(nrow(fact_orders) == 3, "fact_orders should stay at one row per order.")
assert(fact_orders$order_gmv_cents[fact_orders$order_id == "o1"] == 3300,
       "Order GMV should aggregate item price plus freight before payment/review joins.")
assert(fact_orders$payment_total_cents[fact_orders$order_id == "o1"] == 3300,
       "Multiple payment rows should aggregate to order grain.")
assert(fact_orders$review_records[fact_orders$order_id == "o1"] == 2,
       "Multiple review rows should be counted without multiplying order GMV.")
assert(fact_orders$is_repeat_observed_order[fact_orders$order_id == "o2"],
       "Second observed order for the same persistent customer should be repeat.")
assert(fact_orders$is_late_delivery[fact_orders$order_id == "o2"],
       "Late delivery should be flagged when delivered after estimated date.")
assert(customer_history$order_count_observed[customer_history$customer_unique_id == "u1"] == 2,
       "Customer history should use customer_unique_id, not order-specific customer_id.")
assert(sum(monthly_kpis$orders) == 3, "Monthly KPIs should include all fixture orders.")
assert(category_performance$item_gmv_cents[category_performance$product_category_name_english == "health_beauty"] == 4400,
       "Category performance should aggregate item-grain GMV.")

message("PASS: Phase 2 missing-input guard and synthetic processed tables validated.")
