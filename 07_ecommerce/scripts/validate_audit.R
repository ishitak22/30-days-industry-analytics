# Synthetic test fixtures only. These are not Olist data or portfolio findings.
old_option <- getOption("ecommerce.audit.no_run")
options(ecommerce.audit.no_run = TRUE)
project <- if (dir.exists("07_ecommerce/scripts")) "07_ecommerce" else "."
source(file.path(project, "scripts", "00_dataset_overview.R"))
options(ecommerce.audit.no_run = old_option)

validate_audit <- function() {
  sandbox <- tempfile("olist-audit-tests-")
  dir.create(sandbox)
  on.exit(unlink(sandbox, recursive = TRUE), add = TRUE)
  raw <- file.path(sandbox, "synthetic_inputs")
  out <- file.path(sandbox, "test_outputs")
  dir.create(raw)
  expect_error <- function(expression, pattern) {
    error <- tryCatch({ force(expression); NULL }, error = identity)
    stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
  }
  read_output <- function(file) read.csv(file.path(out, file), stringsAsFactors = FALSE)
  expect_error(run_audit(raw, out), "Missing original Olist CSVs")
  stopifnot(nrow(read_output("source_manifest.csv")) == 9,
            all(!read_output("source_manifest.csv")$exists),
            nrow(read_output("audit_details.csv")) == 0,
            read_output("audit_summary.csv")$value[1] == "NOT RUN")
  stopifnot(identical(money_cents(c("10", "10.1", "10.01", "-1.25", "0",
                                    "1.001", "x", NA_character_)),
                      c(1000, 1010, 1001, -125, 0, NA_real_, NA_real_, NA_real_)))
  cat("PASS: missing-source guard, output schemas and exact-centavo parsing.\n")
  dependencies <- c("readr", "dplyr", "purrr", "tibble", "lubridate")
  stopifnot(all(vapply(dependencies, requireNamespace, logical(1), quietly = TRUE)))

  # 120 orders / 60 persistent customers, two items each.
  # First order has two payments and reviews: theoretical inner join = 8 rows.
  n <- 120L
  ids <- sprintf("TEST_ORDER_%03d", seq_len(n))
  cids <- sprintf("TEST_ORDER_CUSTOMER_%03d", seq_len(n))
  purchase <- as.POSIXct("2017-01-01 12:00:00", tz = "UTC") +
    (seq_len(n) - 1) * 86400
  stamp <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  orders <- data.frame(
    order_id = ids, customer_id = cids, order_status = "delivered",
    order_purchase_timestamp = stamp(purchase),
    order_approved_at = stamp(purchase + 3600),
    order_delivered_carrier_date = stamp(purchase + 86400),
    order_delivered_customer_date = stamp(purchase + 4 * 86400),
    order_estimated_delivery_date = stamp(purchase + 5 * 86400)
  )
  orders$order_delivered_customer_date[5] <- stamp(purchase[5] - 86400)
  customers <- data.frame(
    customer_id = cids,
    customer_unique_id = sprintf("TEST_PERSON_%03d", rep(1:60, 2)),
    customer_zip_code_prefix = "01234", customer_city = "TEST_CITY",
    customer_state = "XX")
  items <- data.frame(
    order_id = rep(ids, each = 2), order_item_id = rep(1:2, n),
    product_id = rep(c("TEST_P1", "TEST_P2"), n),
    seller_id = rep(c("TEST_S1", "TEST_S2"), n),
    shipping_limit_date = stamp(rep(purchase + 2 * 86400, each = 2)),
    price = "10.00", freight_value = "1.00")
  payments <- data.frame(
    order_id = rep(ids, each = 2), payment_sequential = rep(1:2, n),
    payment_type = "credit_card", payment_installments = 1,
    payment_value = "11.00")
  payments$payment_value[3] <- "11.01"
  payments$payment_value[5] <- "11.02"
  payments$payment_value[7] <- NA_character_
  reviews <- data.frame(
    review_id = paste0("TEST_REVIEW_", seq_len(n + 1)),
    order_id = c(ids, ids[1]), review_score = 5,
    review_creation_date = stamp(c(purchase, purchase[1]) + 5 * 86400),
    review_answer_timestamp = stamp(c(purchase, purchase[1]) + 6 * 86400),
    review_comment_message = NA_character_)
  products <- data.frame(product_id = c("TEST_P1", "TEST_P2"),
                         product_category_name = c("TEST_C1", "TEST_C2"))
  sellers <- data.frame(seller_id = c("TEST_S1", "TEST_S2"),
                        seller_zip_code_prefix = "01234",
                        seller_city = "TEST_CITY", seller_state = "XX")
  translation <- data.frame(product_category_name = c("TEST_C1", "TEST_C2"),
                             product_category_name_english = c("TEST_ONE", "TEST_TWO"))
  geolocation <- data.frame(geolocation_zip_code_prefix = c("01234", "01234"),
                            geolocation_lat = c("-23", "-23.1"),
                            geolocation_lng = c("-46", "-46.1"))
  sources <- list(customers = customers, orders = orders, items = items,
                  payments = payments, reviews = reviews, products = products,
                  sellers = sellers, translation = translation, geolocation = geolocation)
  write_sources <- function() for (nm in names(sources))
    write.csv(sources[[nm]], file.path(raw, expected_files[nm]), row.names = FALSE, na = "")
  write_sources()
  before <- tools::md5sum(file.path(raw, expected_files))
  invisible(capture.output(run_audit(raw, out)))
  stopifnot(identical(before, tools::md5sum(file.path(raw, expected_files))))
  manifest <- read_output("source_manifest.csv")
  q <- read_output("data_quality.csv")
  details <- read_output("audit_details.csv")
  summary <- read_output("audit_summary.csv")
  number <- function(section, metric, group = NULL) {
    rows <- details$section == section & details$metric == metric
    if (!is.null(group)) rows <- rows & details$group == group
    as.numeric(details$value[rows])
  }
  stopifnot(nrow(manifest) == 9, all(manifest$exists), all(!is.na(manifest$rows)),
            all(q$status %in% c("PASS", "WARN", "FAIL", "INFO")),
            all(q$severity %in% c("LOW", "MEDIUM", "HIGH", "BLOCKING")),
            all(is.na(q$affected_pct) | (q$affected_pct >= 0 & q$affected_pct <= 100)),
            summary$value[summary$metric == "audit_state"] == "COMPLETED",
            summary$value[summary$metric == "dataset_decision"] == "GO WITH LIMITATIONS",
            summary$value[summary$metric == "delivered_snapshot:repeat_identities"] == "60",
            number("reconciliation", "result:count", "exact") == 117,
            number("reconciliation", "result:count", "within_1_cent_nonexact") == 1,
            number("reconciliation", "result:count", "material_difference") == 1,
            number("reconciliation", "result:count", "non_comparable") == 1,
            number("join_fanout", "inner_join_rows", ids[1]) == 8,
            summary$value[summary$metric == "delivery_review_overlap"] == "119",
            any(q$affected_rows[q$check == "order_delivered_customer_date_before_order_purchase_timestamp"] == 1))
  windows <- details[details$section == "follow_up" &
                       details$scope == "all_observed" &
                       details$metric == "fully_observed_customers", ]
  stopifnot(as.numeric(windows$value[grepl(":180$", windows$group)]) == 0,
            as.numeric(windows$value[grepl(":90$", windows$group)]) == 30)
  cat("PASS: complete synthetic audit, source immutability, customer identity,\n",
      "follow-up, reconciliation, review coverage and 8-row fan-out.\n")

  # Broken keys must stop before any analytical joins.
  sources$orders <- rbind(orders, orders[1, ])
  write_sources()
  expect_error(run_audit(raw, out), "Key contracts failed")
  stopifnot(any(read_output("data_quality.csv")$blocks_analysis),
            nrow(read_output("audit_details.csv")) < nrow(details))
  sources$orders <- orders
  sources$orders$order_id <- NULL
  write_sources()
  expect_error(run_audit(raw, out), "Source schema/read checks failed")
  sources$orders <- orders
  sources$orders$order_purchase_timestamp[1] <- "2017-02-30 12:00:00"
  sources$reviews$review_score[1] <- "bad"
  write_sources()
  invisible(capture.output(run_audit(raw, out)))
  q <- read_output("data_quality.csv")
  stopifnot(any(q$affected_rows[q$check == "malformed:order_purchase_timestamp"] == 1),
            any(q$affected_rows[q$check == "invalid_or_missing_score"] == 1))
  # A structurally readable dataset with no delivered orders is a real NO-GO,
  # not a crash or an invented delivery percentage.
  sources$orders <- orders
  sources$orders$order_status <- "canceled"
  write_sources()
  invisible(capture.output(run_audit(raw, out)))
  summary <- read_output("audit_summary.csv")
  stopifnot(summary$value[summary$metric == "dataset_decision"] == "NO-GO",
            summary$value[summary$metric == "delivery_review_overlap"] == "0")
  cat("PASS: conflicting keys, missing schema, malformed dates and invalid scores.\n",
      "PASS: empty delivered population produces an evidence-based NO-GO.\n",
      "All fixtures were synthetic, isolated and removed; no Olist findings generated.\n")
}
validate_audit()
