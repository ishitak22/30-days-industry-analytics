# Phase 1: read-only suitability inspection. No raw rows are repaired or removed.
# Counts are audit evidence, not final business KPIs.
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

required_columns <- list(
  customers = c("customer_id", "customer_unique_id", "customer_zip_code_prefix",
                "customer_city", "customer_state"),
  geolocation = c("geolocation_zip_code_prefix", "geolocation_lat", "geolocation_lng"),
  items = c("order_id", "order_item_id", "product_id", "seller_id",
            "shipping_limit_date", "price", "freight_value"),
  payments = c("order_id", "payment_sequential", "payment_type",
               "payment_installments", "payment_value"),
  reviews = c("review_id", "order_id", "review_score", "review_creation_date",
              "review_answer_timestamp"),
  orders = c("order_id", "customer_id", "order_status", "order_purchase_timestamp",
             "order_approved_at", "order_delivered_carrier_date",
             "order_delivered_customer_date", "order_estimated_delivery_date"),
  products = c("product_id", "product_category_name"),
  sellers = c("seller_id", "seller_zip_code_prefix", "seller_city", "seller_state"),
  translation = c("product_category_name", "product_category_name_english")
)

industry_dir <- function() {
  if (file.exists("30-days-industry-analytics.Rproj") &&
      dir.exists("07_ecommerce")) return("07_ecommerce")
  if (basename(getwd()) == "07_ecommerce" && dir.exists("scripts")) return(".")
  stop("Run from the repository root or 07_ecommerce directory.", call. = FALSE)
}

# Parse decimal text into exact whole centavos within the safe integer range.
# Invalid precision is reported, not rounded or assumed to be a refund.
money_cents <- function(x) {
  valid <- !is.na(x) & grepl("^-?[0-9]+(\\.[0-9]{1,2})?$", x)
  out <- rep(NA_real_, length(x))
  for (i in which(valid)) {
    parts <- strsplit(sub("^-", "", x[i]), ".", fixed = TRUE)[[1]]
    fraction <- if (length(parts) == 2L) as.numeric(paste0(parts[2], if (
      nchar(parts[2]) == 1L) "0" else "")) else 0
    amount <- as.numeric(parts[1]) * 100 + fraction
    if (is.finite(amount) && amount <= 2^52) {
      out[i] <- amount * if (startsWith(x[i], "-")) -1 else 1
    }
  }
  out
}

run_audit <- function(raw_dir, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  quality <- list()
  detail <- list()
  summary <- data.frame(metric = character(), value = character(),
                        interpretation = character())
  paths <- file.path(raw_dir, unname(expected_files))
  manifest <- data.frame(
    source_file = unname(expected_files), exists = file.exists(paths),
    file_size_bytes = NA_real_, rows = NA_real_, columns = NA_real_,
    hash = NA_character_, column_names = NA_character_,
    inferred_types = NA_character_, min_date = NA_character_,
    max_date = NA_character_, notes = "Not read", stringsAsFactors = FALSE
  )
  # Four stable output schemas also describe failed/preflight runs truthfully.
  quality_empty <- data.frame(
    check_id = character(), table = character(), check = character(),
    status = character(), affected_rows = numeric(), denominator = numeric(),
    affected_pct = numeric(), severity = character(),
    blocks_analysis = logical(), recommended_treatment = character(),
    notes = character()
  )
  detail_empty <- data.frame(section = character(), scope = character(),
                            group = character(), metric = character(),
                            value = character(), notes = character())
  add_check <- function(table, check, n, total, status = NULL,
                        severity = "MEDIUM", treatment = "Retain and investigate",
                        notes = "", blocking = FALSE) {
    if (is.null(status)) status <- if (n > 0) "WARN" else "PASS"
    quality[[length(quality) + 1L]] <<- data.frame(
      check_id = sprintf("Q%04d", length(quality) + 1L), table = table,
      check = check, status = status, affected_rows = n, denominator = total,
      affected_pct = if (total > 0) 100 * n / total else NA_real_,
      severity = if (status == "PASS") "LOW" else severity,
      blocks_analysis = blocking, recommended_treatment = treatment, notes = notes
    )
  }
  add_detail <- function(section, scope, group, metric, value, notes = "") {
    if (!length(group) || !length(metric) || !length(value)) return(invisible(NULL))
    detail[[length(detail) + 1L]] <<- data.frame(
      section = section, scope = scope, group = as.character(group),
      metric = metric, value = as.character(value), notes = notes
    )
  }
  add_summary <- function(metric, value, interpretation) {
    summary[nrow(summary) + 1L, ] <<- list(metric, as.character(value), interpretation)
  }
  save_outputs <- function() {
    write.csv(manifest, file.path(output_dir, "source_manifest.csv"),
              row.names = FALSE, na = "")
    write.csv(if (length(quality)) do.call(rbind, quality) else quality_empty,
              file.path(output_dir, "data_quality.csv"), row.names = FALSE, na = "")
    write.csv(if (length(detail)) do.call(rbind, detail) else detail_empty,
              file.path(output_dir, "audit_details.csv"), row.names = FALSE, na = "")
    write.csv(summary, file.path(output_dir, "audit_summary.csv"),
              row.names = FALSE, na = "")
  }
  abort_audit <- function(reason) {
    add_summary("audit_state", "NOT COMPLETED", reason)
    add_summary("dataset_decision", "NO-GO",
                "Do not proceed to Phase 2; dataset suitability remains unassessed.")
    save_outputs()
    stop(reason, call. = FALSE)
  }

  # A: fingerprint original inputs before reading any analytical observations.
  for (i in seq_along(paths)) {
    if (manifest$exists[i]) {
      manifest$file_size_bytes[i] <- file.info(paths[i])$size
      manifest$hash[i] <- unname(tools::md5sum(paths[i]))
    }
    add_check(names(expected_files)[i], "source_file_present",
              as.integer(!manifest$exists[i]), 1,
              if (manifest$exists[i]) "PASS" else "FAIL", "BLOCKING",
              "Obtain unchanged CSV from the original Olist dataset",
              expected_files[i], !manifest$exists[i])
  }
  if (any(!manifest$exists)) {
    add_summary("audit_state", "NOT RUN", "Required original source files are missing.")
    add_summary("dataset_decision", "NO-GO",
                "Input readiness only; no conclusion about Olist suitability.")
    save_outputs()
    stop(paste0("Missing original Olist CSVs in ", raw_dir, ":\n",
                paste(manifest$source_file[!manifest$exists], collapse = "\n"),
                "\nDownload: https://www.kaggle.com/datasets/olistbr/brazilian-ecommerce",
                "\nManifest saved; no dataset analysis was performed."), call. = FALSE)
  }
  packages <- c("readr", "dplyr", "purrr", "tibble", "lubridate")
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1),
                                      quietly = TRUE)]
  if (length(missing_packages)) abort_audit(paste(
    "Install required R packages first:", paste(missing_packages, collapse = ", ")))
  suppressPackageStartupMessages(library(dplyr))
  add_summary("r_version", R.version.string, "Runtime used for this audit")
  for (pkg in packages) add_detail("environment", "packages", pkg, "version",
                                   as.character(utils::packageVersion(pkg)))
  tables <- list()
  for (i in seq_along(paths)) {
    nm <- names(expected_files)[i]
    # Read as text to preserve IDs, leading postal zeros, and original money text.
    x <- tryCatch(readr::read_csv(paths[i], col_types = readr::cols(
      .default = readr::col_character()), na = c("", "NA"), trim_ws = FALSE,
      name_repair = "minimal", show_col_types = FALSE, progress = FALSE),
      error = function(e) abort_audit(paste("Cannot read", expected_files[i],
                                          conditionMessage(e))))
    tables[[nm]] <- x
    manifest$rows[i] <- nrow(x)
    manifest$columns[i] <- ncol(x)
    manifest$column_names[i] <- paste(names(x), collapse = "|")
    manifest$inferred_types[i] <- paste(names(x), vapply(x, readr::guess_parser,
                                                       character(1)), sep = ":",
                                        collapse = "|")
    manifest$notes[i] <- "Text preserved; types inferred for inspection only"
    missing_cols <- setdiff(required_columns[[nm]], names(x))
    bad_names <- sum(duplicated(names(x))) + sum(names(x) == "")
    parse_issues <- nrow(readr::problems(x))
    add_check(nm, "required_columns", length(missing_cols),
              length(required_columns[[nm]]), if (length(missing_cols)) "FAIL" else "PASS",
              "BLOCKING", "Verify original source/schema", paste(missing_cols, collapse = "|"),
              length(missing_cols) > 0)
    add_check(nm, "duplicate_or_empty_column_names", bad_names, ncol(x),
              if (bad_names) "FAIL" else "PASS", "BLOCKING",
              "Verify original source/schema", blocking = bad_names > 0)
    add_check(nm, "csv_parse_problems", parse_issues, nrow(x),
              if (parse_issues) "FAIL" else "PASS", "BLOCKING",
              "Inspect malformed CSV without repairing raw data", blocking = parse_issues > 0)
    add_check(nm, "empty_table", as.integer(nrow(x) == 0), 1,
              if (nrow(x) == 0) "FAIL" else "PASS", "BLOCKING",
              "Verify original download", blocking = nrow(x) == 0)
  }
  if (any(vapply(quality, function(x) x$blocks_analysis, logical(1)))) {
    abort_audit("Source schema/read checks failed; inspect data_quality.csv.")
  }

  # Missingness is not uniformly an error: comments and status-dependent dates differ.
  for (nm in names(tables)) {
    x <- tables[[nm]]
    for (col in names(x)) {
      n <- sum(is.na(x[[col]]))
      optional <- grepl("review_comment", col)
      conditional <- grepl("approved_at|delivered_", col)
      explanation <- if (optional) "Optional review text" else if (conditional)
        "Status-dependent timestamp; inspect delivered-order coverage separately" else
          "Missing field may limit linkage or analysis; no imputation in Phase 1"
      add_check(nm, paste0("missing:", col), n, nrow(x),
                if (!n) "PASS" else if (optional || conditional) "INFO" else "WARN",
                if (optional || conditional) "LOW" else "MEDIUM",
                "Retain and report metric-specific coverage", explanation)
    }
    add_check(nm, "exact_duplicate_rows", sum(duplicated(x)), nrow(x),
              treatment = "Inspect, do not deduplicate in Phase 1")
  }
  # C-D: enforce parent grain before joins; review multiplicity is only reported.
  keys <- list(orders = "order_id", customers = "customer_id",
               items = c("order_id", "order_item_id"),
               payments = c("order_id", "payment_sequential"),
               products = "product_id", sellers = "seller_id",
               translation = "product_category_name")
  for (nm in names(keys)) {
    x <- tables[[nm]][keys[[nm]]]
    bad <- duplicated(x) | duplicated(x, fromLast = TRUE) |
      !complete.cases(x)
    add_check(nm, "primary_key_contract", sum(bad), nrow(x),
              if (any(bad)) "FAIL" else "PASS", "BLOCKING",
              "Resolve key ambiguity before analytical joins; retain source",
              paste(keys[[nm]], collapse = "+"), any(bad))
  }
  for (pair in list(c("reviews", "review_id"), c("reviews", "order_id"),
                    c("customers", "customer_unique_id"),
                    c("geolocation", "geolocation_zip_code_prefix"))) {
    x <- tables[[pair[1]]][[pair[2]]]
    add_check(pair[1], paste0("repeated:", pair[2]), sum(duplicated(x[!is.na(x)])),
              sum(!is.na(x)), "INFO", "LOW", "Retain; repetition may be structural",
              "Count of occurrences beyond the first, excluding missing keys")
  }
  if (any(vapply(quality, function(x) x$blocks_analysis, logical(1)))) {
    abort_audit("Key contracts failed; stop before joins to avoid inflated evidence.")
  }
  for (rel in list(c("orders", "customer_id", "customers", "customer_id"),
                   c("items", "order_id", "orders", "order_id"),
                   c("items", "product_id", "products", "product_id"),
                   c("items", "seller_id", "sellers", "seller_id"),
                   c("payments", "order_id", "orders", "order_id"),
                   c("reviews", "order_id", "orders", "order_id"),
                   c("products", "product_category_name", "translation", "product_category_name"))) {
    child <- tables[[rel[1]]][[rel[2]]]
    parent <- tables[[rel[3]]][[rel[4]]]
    bad <- is.na(child) | !(child %in% parent)
    add_check(rel[1], paste0("foreign_key:", rel[2], "->", rel[3]), sum(bad),
              length(child), severity = "HIGH",
              treatment = "Retain; unmatched rows cannot support linked analyses",
              notes = "Missing child keys included; percent is of child rows")
  }

  # E-G: status, coverage and date logic establish what can be observed.
  # Dates use UTC only as a neutral arithmetic convention for timezone-naive text.
  # We do not claim the source timestamps were recorded in UTC.
  for (nm in names(tables)) {
    date_cols <- grep("timestamp|_date$|approved_at", names(tables[[nm]]), value = TRUE)
    dates <- list()
    for (col in date_cols) {
      original <- tables[[nm]][[col]]
      parsed <- suppressWarnings(readr::parse_datetime(
        original, format = "%Y-%m-%d %H:%M:%S",
        locale = readr::locale(tz = "UTC")))
      malformed <- !is.na(original) & (is.na(parsed) |
        format(parsed, "%Y-%m-%d %H:%M:%S", tz = "UTC") != original)
      parsed[which(malformed)] <- as.POSIXct(NA, tz = "UTC")
      add_check(nm, paste0("malformed:", col), sum(malformed, na.rm = TRUE),
                length(original), severity = "HIGH",
                treatment = "Retain source; exclude invalid timestamp from date evidence")
      tables[[nm]][[col]] <- parsed
      if (any(!is.na(parsed))) {
        add_detail("dates", nm, col, c("minimum", "maximum", "missing"),
                   c(as.character(min(parsed, na.rm = TRUE)),
                     as.character(max(parsed, na.rm = TRUE)), sum(is.na(parsed))))
        dates[[col]] <- as.numeric(parsed[!is.na(parsed)])
      }
    }
    if (length(dates)) {
      all_dates <- unlist(dates)
      i <- match(nm, names(expected_files))
      manifest$min_date[i] <- as.character(as.POSIXct(min(all_dates), origin = "1970-01-01", tz = "UTC"))
      manifest$max_date[i] <- as.character(as.POSIXct(max(all_dates), origin = "1970-01-01", tz = "UTC"))
    }
  }
  orders <- tables$orders
  customers <- tables$customers
  items <- tables$items
  payments <- tables$payments
  reviews <- tables$reviews
  for (col in c("price", "freight_value")) items[[paste0(col, "_cents")]] <- money_cents(items[[col]])
  payments$value_cents <- money_cents(payments$payment_value)
  for (entry in list(list("items", "price", items$price_cents),
                     list("items", "freight_value", items$freight_value_cents),
                     list("payments", "payment_value", payments$value_cents))) {
    vals <- entry[[3]]
    add_check(entry[[1]], paste0("invalid_or_missing_money:", entry[[2]]),
              sum(is.na(vals)), length(vals), severity = "HIGH",
              treatment = "Flag affected orders as non-comparable; never round silently")
    add_check(entry[[1]], paste0("negative:", entry[[2]]), sum(vals < 0, na.rm = TRUE),
              length(vals), severity = "HIGH")
    add_check(entry[[1]], paste0("zero:", entry[[2]]), sum(vals == 0, na.rm = TRUE),
              length(vals), "INFO", "LOW", "Retain; zero need not mean error")
  }
  profile <- function(values, section, scope, label) {
    x <- values[is.finite(values)]
    if (length(x)) add_detail(section, scope, label,
      c("n", "min", "p25", "median", "p75", "max"),
      c(length(x), as.numeric(quantile(x, c(0, .25, .5, .75, 1)))))
  }
  frequency <- function(values, section, scope, label) {
    if (!length(values)) {
      add_detail(section, scope, "<empty_scope>", paste0(label, ":count"), 0,
                 "No observations; a percentage is undefined")
      return(invisible(NULL))
    }
    counts <- table(ifelse(is.na(values), "<missing>", as.character(values)))
    add_detail(section, scope, names(counts), paste0(label, ":count"), as.numeric(counts))
    add_detail(section, scope, names(counts), paste0(label, ":pct"),
               100 * as.numeric(counts) / length(values))
  }
  for (col in c("order_item_id")) {
    vals <- suppressWarnings(as.numeric(items[[col]]))
    add_check("items", "invalid_item_sequence", sum(is.na(vals) | vals < 1 |
              vals != floor(vals)), nrow(items), severity = "HIGH")
    items$sequence_numeric <- vals
  }
  for (col in c("payment_sequential", "payment_installments")) {
    vals <- suppressWarnings(as.numeric(payments[[col]]))
    bad <- is.na(vals) | vals < if (col == "payment_sequential") 1 else 0
    bad <- bad | vals != floor(vals)
    add_check("payments", paste0("invalid:", col), sum(bad), nrow(payments))
    frequency(vals, "payments", "all", col)
  }
  score <- suppressWarnings(as.numeric(reviews$review_score))
  reviews$usable_score <- !is.na(score) & score %in% 1:5
  add_check("reviews", "invalid_or_missing_score", sum(!reviews$usable_score),
            nrow(reviews), severity = "HIGH")
  frequency(reviews$review_score, "reviews", "all", "score")
  frequency(payments$payment_type, "payments", "all", "type")
  profile(items$price_cents / 100, "items", "all", "price_brl")
  profile(items$freight_value_cents / 100, "items", "all", "freight_brl")
  profile(payments$value_cents / 100, "payments", "all", "payment_brl")
  frequency(orders$order_status, "status", "all_orders", "orders")
  known_status <- c("delivered", "canceled", "unavailable", "shipped",
                    "invoiced", "processing", "approved", "created")
  add_check("orders", "unknown_status", sum(is.na(orders$order_status) |
              !orders$order_status %in% known_status), nrow(orders))

  for (pair in list(c("order_approved_at", "order_purchase_timestamp"),
                    c("order_delivered_carrier_date", "order_purchase_timestamp"),
                    c("order_delivered_customer_date", "order_purchase_timestamp"),
                    c("order_delivered_customer_date", "order_delivered_carrier_date"),
                    c("order_estimated_delivery_date", "order_purchase_timestamp"))) {
    available <- !is.na(orders[[pair[1]]]) & !is.na(orders[[pair[2]]])
    bad <- available & orders[[pair[1]]] < orders[[pair[2]]]
    add_check("orders", paste(pair, collapse = "_before_"), sum(bad),
              sum(available), severity = "HIGH",
              treatment = "Flag timing; exclude affected dates from delivery evidence")
  }
  monthly <- orders %>% filter(!is.na(order_purchase_timestamp)) %>%
    mutate(month = as.Date(format(order_purchase_timestamp, "%Y-%m-01")),
           terminal = order_status %in% c("delivered", "canceled", "unavailable")) %>%
    group_by(month) %>% summarise(orders = n(), terminal_share = mean(terminal),
                                  .groups = "drop")
  monthly_status <- orders %>% filter(!is.na(order_purchase_timestamp)) %>%
    count(month = format(order_purchase_timestamp, "%Y-%m"), order_status)
  if (nrow(monthly_status)) add_detail("monthly_status", "all_orders",
    paste(monthly_status$month, monthly_status$order_status, sep = ":"),
    "orders", monthly_status$n)
  # Include missing calendar months in the volume screen; gaps are not healthy months.
  if (nrow(monthly)) {
    monthly <- tibble::tibble(month = seq(min(monthly$month), max(monthly$month),
                                         by = "month")) %>% left_join(monthly, by = "month")
    monthly$orders[is.na(monthly$orders)] <- 0L
    monthly$terminal_share[is.na(monthly$terminal_share)] <- 0
    monthly$prior_median <- NA_real_
    for (i in seq_len(nrow(monthly))) if (i > 3L)
      monthly$prior_median[i] <- median(monthly$orders[(i - 3L):(i - 1L)])
    observed_end <- as.Date(max(orders$order_purchase_timestamp, na.rm = TRUE))
    monthly$end <- as.Date(lubridate::ceiling_date(monthly$month, "month") - 1)
    monthly$candidate <- !is.na(monthly$prior_median) &
      monthly$prior_median > 0 & monthly$orders >= 100 &
      monthly$orders >= .5 * monthly$prior_median & monthly$terminal_share >= .9 &
      monthly$end < observed_end
    add_detail("window_screen", "all_orders", monthly$month,
               "candidate_month_end", monthly$candidate,
               "Heuristic only; does not approve a cutoff")
    add_detail("window_screen", "all_orders", monthly$month,
               "terminal_share", monthly$terminal_share)
  } else abort_audit("No usable purchase timestamps; customer timing cannot be assessed.")

  # H-J: persistent identity and equal follow-up capacity, not a retention KPI.
  linked <- orders %>% left_join(customers %>% select(customer_id, customer_unique_id),
                                by = "customer_id")
  identities <- n_distinct(customers$customer_unique_id, na.rm = TRUE)
  add_summary("source_customer_identities", identities, "Persistent IDs in customer source")
  add_summary("order_specific_customer_ids", n_distinct(customers$customer_id),
              "Do not use these as persistent customer identities")
  add_check("orders", "missing_linked_identity", sum(is.na(linked$customer_unique_id)),
            nrow(linked), severity = "HIGH")
  repeat_counts <- c()
  for (scope in c("all_observed", "delivered_snapshot")) {
    scoped <- linked %>% filter(!is.na(customer_unique_id))
    if (scope == "delivered_snapshot") scoped <- scoped %>% filter(order_status == "delivered")
    per_customer <- scoped %>% count(customer_unique_id, name = "order_count")
    frequency(per_customer$order_count, "customer_orders", scope, "orders_per_identity")
    bands <- ifelse(per_customer$order_count >= 3, "3+", as.character(per_customer$order_count))
    frequency(bands, "customer_orders", scope, "order_band")
    repeat_counts[scope] <- sum(per_customer$order_count >= 2)
    add_summary(paste0(scope, ":repeat_identities"), repeat_counts[scope],
                "Two or more observed orders; not a final retention rate")
    if (nrow(per_customer)) add_summary(paste0(scope, ":max_orders"),
      max(per_customer$order_count), "Maximum observed orders per linked identity")
    # A missing date could hide the true first/second observed purchase. Exclude
    # that identity from timing only, while retaining its order-count evidence.
    incomplete_ids <- scoped %>% filter(is.na(order_purchase_timestamp)) %>%
      distinct(customer_unique_id)
    add_detail("repeat_timing", scope, "all", "identities_excluded_missing_purchase_date",
               nrow(incomplete_ids), "Retained in order-count distributions")
    history <- scoped %>% anti_join(incomplete_ids, by = "customer_unique_id") %>%
      arrange(customer_unique_id, order_purchase_timestamp, order_id) %>%
      group_by(customer_unique_id) %>%
      summarise(first = first(order_purchase_timestamp),
                second = nth(order_purchase_timestamp, 2), .groups = "drop")
    history$gap_days <- as.numeric(difftime(history$second, history$first, units = "days"))
    profile(history$gap_days, "repeat_timing", scope, "first_to_second_days")
    add_detail("repeat_timing", scope, "all", "same_day_second_orders",
               sum(as.Date(history$first) == as.Date(history$second), na.rm = TRUE))
    add_detail("repeat_timing", scope, "all", "tied_first_second_timestamps",
               sum(history$gap_days == 0, na.rm = TRUE))
    cutoffs <- unique(c(as.character(tail(monthly$end[monthly$candidate], 3)),
                        as.character(observed_end)))
    for (cut in cutoffs) {
      # End of candidate day; final status is still retrospective, not an as-of snapshot.
      endpoint <- as.POSIXct(as.Date(cut) + 1, tz = "UTC") - 1
      for (days in c(30, 60, 90, 180)) {
        eligible <- history$first <= endpoint - days * 86400
        observed_repeat <- !is.na(history$second) & history$second <= endpoint &
          history$gap_days <= days
        label <- paste(cut, days, sep = ":")
        note <- if (cut == as.character(observed_end)) "Observed-end upper bound, NOT approved cutoff" else
          "Candidate month end, NOT approved cutoff"
        add_detail("follow_up", scope, label, "fully_observed_customers", sum(eligible), note)
        add_detail("follow_up", scope, label, "repeaters_among_fully_observed",
                   sum(eligible & observed_repeat), note)
        add_detail("follow_up", scope, label, "observed_repeaters_any_follow_up",
                   sum(observed_repeat), "Descriptive count includes immature histories")
      }
    }
  }

  # K-M: count child rows and distinct categories without assigning review blame.
  item_order <- items %>% group_by(order_id) %>% summarise(
    item_rows = n(), sellers = n_distinct(seller_id, na.rm = TRUE),
    products = n_distinct(product_id, na.rm = TRUE),
    item_cents = sum(price_cents + freight_value_cents),
    valid_amounts = all(!is.na(price_cents) & !is.na(freight_value_cents) &
                         price_cents >= 0 & freight_value_cents >= 0),
    sequence_is_1_to_n = all(!is.na(sequence_numeric)) &&
      identical(sort(sequence_numeric), as.numeric(seq_len(n()))), .groups = "drop")
  profile(item_order$item_rows, "items", "orders_with_items", "item_rows_per_order")
  for (field in c("item_rows", "sellers", "products")) add_detail(
    "order_structure", "orders_with_items", field, c("multiple_count", "denominator", "pct"),
    c(sum(item_order[[field]] > 1), nrow(item_order),
      100 * mean(item_order[[field]] > 1)))
  add_detail("items", "orders_with_items", "sequence", "is_1_to_n",
             sum(item_order$sequence_is_1_to_n), "Sequence is not quantity")
  categorized <- items %>% left_join(tables$products %>% select(product_id, product_category_name),
                                    by = "product_id") %>%
    left_join(tables$translation, by = "product_category_name") %>%
    mutate(category = coalesce(product_category_name, "<unknown>"))
  category_counts <- categorized %>% group_by(category) %>%
    summarise(item_rows = n(), orders = n_distinct(order_id), .groups = "drop")
  add_summary("unique_products", n_distinct(tables$products$product_id),
              "Source product keys; category coverage is measured separately")
  product_translation <- tables$products %>% left_join(tables$translation,
                                                       by = "product_category_name")
  add_detail("categories", "products", "all",
             c("products", "missing_category", "translated_products", "untranslated_products"),
             c(nrow(product_translation), sum(is.na(product_translation$product_category_name)),
               sum(!is.na(product_translation$product_category_name_english)),
               sum(!is.na(product_translation$product_category_name) &
                     is.na(product_translation$product_category_name_english))))
  add_detail("categories", "items", category_counts$category, "item_rows", category_counts$item_rows)
  add_detail("categories", "items", category_counts$category, "distinct_orders", category_counts$orders,
             "Order counts across categories are non-additive")
  add_check("products", "untranslated_categories",
            sum(!is.na(categorized$product_category_name) &
                  is.na(categorized$product_category_name_english)), nrow(categorized),
            treatment = "Retain original category; do not drop item value",
            notes = "Affected rows measured at item grain")
  cat_order <- categorized %>% group_by(order_id) %>%
    summarise(categories = n_distinct(product_category_name, na.rm = TRUE),
              unknown_category = any(is.na(product_category_name)), .groups = "drop")
  add_detail("order_structure", "orders_with_items", "categories",
             c("multiple_count", "denominator", "pct", "unknown_category_orders"),
             c(sum(cat_order$categories > 1), nrow(cat_order),
               100 * mean(cat_order$categories > 1), sum(cat_order$unknown_category)))

  # N-O: reconcile independently aggregated children, preserving missing amounts.
  payment_order <- payments %>% group_by(order_id) %>%
    summarise(payment_rows = n(), payment_cents = sum(value_cents),
              valid_payments = all(!is.na(value_cents) & value_cents >= 0), .groups = "drop")
  profile(payment_order$payment_rows, "payments", "orders_with_payments", "rows_per_order")
  add_detail("payments", "orders_with_payments", "all", "multiple_payment_orders",
             sum(payment_order$payment_rows > 1))
  # Full join retains missing child records as non-comparable rather than zero value.
  reconciled <- full_join(item_order, payment_order, by = "order_id") %>%
    mutate(comparable = !is.na(valid_amounts) & valid_amounts &
             !is.na(valid_payments) & valid_payments,
           difference_cents = payment_cents - item_cents,
           result = case_when(!comparable ~ "non_comparable",
                              difference_cents == 0 ~ "exact",
                              abs(difference_cents) <= 1 ~ "within_1_cent_nonexact",
                              TRUE ~ "material_difference"))
  frequency(reconciled$result, "reconciliation", "union_of_child_order_ids", "result")
  profile(reconciled$difference_cents[reconciled$comparable],
          "reconciliation", "comparable_orders", "payment_minus_items_centavos")
  add_check("payments", "material_reconciliation_difference",
            sum(reconciled$result == "material_difference"), sum(reconciled$comparable),
            severity = "HIGH", treatment = "Investigate; do not force agreement")
  # P-R: retain all review records; measure delivery/review overlap, not inference.
  review_order <- reviews %>% group_by(order_id) %>%
    summarise(review_rows = n(), usable_reviews = sum(usable_score), .groups = "drop")
  order_coverage <- orders %>% left_join(item_order, by = "order_id") %>%
    left_join(payment_order, by = "order_id") %>% left_join(review_order, by = "order_id")
  for (col in c("item_rows", "payment_rows", "review_rows", "usable_reviews"))
    order_coverage[[col]][is.na(order_coverage[[col]])] <- 0L
  for (col in c("item_rows", "payment_rows", "review_rows")) add_check(
    "orders", paste0("without:", col), sum(order_coverage[[col]] == 0),
    nrow(order_coverage), treatment = "Report absence; do not invent child rows")
  review_bands <- ifelse(order_coverage$review_rows > 1, "multiple",
                         as.character(order_coverage$review_rows))
  frequency(review_bands, "reviews", "all_orders", "review_rows_band")
  add_summary("review_records", nrow(reviews), "No review record selected or deduplicated")
  add_summary("unique_review_ids", n_distinct(reviews$review_id, na.rm = TRUE), "Not assumed unique")
  add_summary("orders_with_reviews", sum(order_coverage$review_rows > 0),
              "Matched order coverage; valid-score coverage reported separately")

  delivery <- order_coverage %>% filter(order_status == "delivered") %>%
    mutate(usable_delivery = !is.na(order_delivered_customer_date) &
             !is.na(order_estimated_delivery_date) & !is.na(order_purchase_timestamp) &
             order_delivered_customer_date >= order_purchase_timestamp &
             order_estimated_delivery_date >= order_purchase_timestamp &
             (is.na(order_delivered_carrier_date) |
                (order_delivered_customer_date >= order_delivered_carrier_date &
                   order_delivered_carrier_date >= order_purchase_timestamp)) &
             (is.na(order_approved_at) | order_approved_at >= order_purchase_timestamp),
           delay_days = as.numeric(as.Date(order_delivered_customer_date) -
                                     as.Date(order_estimated_delivery_date)))
  add_check("orders", "delivered_without_usable_dates", sum(!delivery$usable_delivery),
            nrow(delivery), severity = "HIGH",
            treatment = "Retain; omit affected orders from delay comparison")
  usable <- delivery %>% filter(usable_delivery)
  profile(usable$delay_days, "delivery", "usable_delivered", "delay_days")
  add_detail("delivery", "usable_delivered", "all", c("orders", "late", "on_time"),
             c(nrow(usable), sum(usable$delay_days > 0), sum(usable$delay_days <= 0)))
  overlap <- usable %>% filter(usable_reviews > 0)
  add_summary("delivery_review_overlap", nrow(overlap),
              "Usable delivered orders with at least one valid score; no review selection")
  add_detail("delivery_reviews", "usable_delivered", "all",
             c("exactly_one_valid_review", "multiple_valid_reviews", "no_valid_review"),
             c(sum(usable$usable_reviews == 1), sum(usable$usable_reviews > 1),
               sum(usable$usable_reviews == 0)))
  # S: demonstrate actual row multiplication without performing the unsafe join.
  examples <- order_coverage %>%
    mutate(inner_join_rows = item_rows * payment_rows * review_rows,
           left_join_rows = pmax(item_rows, 1) * pmax(payment_rows, 1) * pmax(review_rows, 1)) %>%
    arrange(desc(left_join_rows), order_id) %>% slice_head(n = 5)
  for (col in c("item_rows", "payment_rows", "review_rows",
                "inner_join_rows", "left_join_rows")) add_detail(
    "join_fanout", "example_orders", examples$order_id, col, examples[[col]],
    "Actual source counts; independent order aggregation required")

  commercial <- sum(delivery$item_rows > 0 & delivery$valid_amounts, na.rm = TRUE)
  category_support <- sum(category_counts$category != "<unknown>" & category_counts$orders >= 30)
  decision <- if (commercial < 100) "NO-GO" else "GO WITH LIMITATIONS"
  add_summary("audit_state", "COMPLETED", "Real input files inspected; no cleaning or final model")
  add_summary("dataset_decision", decision, "Screening rules documented in DATASET.md")
  add_summary("total_orders", nrow(orders), "All source statuses")
  add_summary("delivered_orders", nrow(delivery), "Final-snapshot status")
  add_summary("commercial_usable_orders", commercial, "Delivered with valid nonnegative item amounts")
  add_summary("relational_sql", "SUPPORTED", "Required key contracts passed; inspect unmatched references")
  add_summary("repeat_analysis", if (repeat_counts["delivered_snapshot"] >= 30)
    "DESCRIPTIVE ANALYSIS SUPPORTED" else "SPARSE: REDUCE SCOPE",
    "Use persistent identity and equal follow-up; inspect cohort/group sample sizes")
  add_summary("category_analysis", if (category_support >= 2) "SUPPORTED" else "LIMITED",
              paste(category_support, "known categories with at least 30 orders"))
  add_summary("delivery_experience", if (nrow(overlap) >= 100) "SUPPORTED" else "LIMITED",
              "Overlap screen is not a statistical power calculation")
  add_summary("review_usability", "CONDITIONAL",
              "Valid scores are available only for measured coverage; multiplicity needs Phase 2 rules")
  add_summary("observation_window", "NOT APPROVED",
              paste("Candidate ends:", paste(monthly$end[monthly$candidate], collapse = ", "),
                    "; inspect monthly evidence. Observed end", observed_end, "is only an upper bound."))
  q <- do.call(rbind, quality)
  add_summary("quality_priorities", paste(unique(q$check[q$status %in% c("WARN", "FAIL") &
                     q$severity %in% c("HIGH", "BLOCKING")]), collapse = "; "),
              "See affected counts and treatments in data_quality.csv")
  add_summary("structural_not_errors", "Multiple items/payments/reviews and customer-ID repetition",
              "Preserve grain; do not silently deduplicate")
  add_summary("plan_adjustments", "No CLV/RFM claims; fixed follow-up and review-rule review needed",
              "Pool sparse groups; no causation, production metrics or statistical inference in Phase 1")
  if (!identical(unname(tools::md5sum(paths)), manifest$hash)) {
    abort_audit("Raw file hashes changed during audit; outputs cannot be trusted.")
  }
  save_outputs()
  cat("\nDATASET SUITABILITY AUDIT\n")
  print(summary, row.names = FALSE)
  cat("\nRaw files unchanged. Review evidence before approving an observation window or Phase 2.\n")
  invisible(summary)
}

if (!isTRUE(getOption("ecommerce.audit.no_run"))) {
  project <- industry_dir()
  run_audit(file.path(project, "data", "raw"), file.path(project, "outputs"))
}
