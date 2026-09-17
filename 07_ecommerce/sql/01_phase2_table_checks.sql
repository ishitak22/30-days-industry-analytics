-- Phase 2 sanity checks for the processed CSV tables.
-- Intended for DuckDB after 01_build_analytics_tables.R has created data/processed.
-- These checks focus on grain and fan-out risk before deeper business analysis.

-- 1. fact_orders must stay one row per order_id.
select
  count(*) as rows,
  count(distinct order_id) as distinct_orders,
  count(*) - count(distinct order_id) as duplicate_order_rows
from read_csv_auto('07_ecommerce/data/processed/fact_orders.csv');

-- 2. fact_order_items should stay at order_id + order_item_id grain.
select
  count(*) as rows,
  count(distinct order_id || '-' || order_item_id) as distinct_order_items,
  count(*) - count(distinct order_id || '-' || order_item_id) as duplicate_item_rows
from read_csv_auto('07_ecommerce/data/processed/fact_order_items.csv');

-- 3. Monthly GMV should be calculated from fact_orders, not by joining items,
-- payments and reviews together.
select
  purchase_month,
  count(distinct order_id) as orders,
  sum(order_gmv_cents) / 100.0 as order_gmv_brl,
  sum(payment_total_cents) / 100.0 as payment_total_brl
from read_csv_auto('07_ecommerce/data/processed/fact_orders.csv')
group by purchase_month
order by purchase_month;

-- 4. Repeat behaviour should use customer_unique_id, not customer_id.
select
  order_count_observed,
  count(*) as customers
from read_csv_auto('07_ecommerce/data/processed/customer_order_history.csv')
group by order_count_observed
order by order_count_observed;

-- 5. Review multiplicity is visible but does not multiply order value.
select
  review_records,
  count(*) as orders,
  sum(order_gmv_cents) / 100.0 as order_gmv_brl
from read_csv_auto('07_ecommerce/data/processed/fact_orders.csv')
group by review_records
order by review_records;
