# Olist Dataset Suitability Audit

## Identity and access

- Dataset: Brazilian E-Commerce Public Dataset by Olist.
- Provider: Olist and collaborators credited on the original Kaggle page.
- Source: https://www.kaggle.com/datasets/olistbr/brazilian-ecommerce
- Geography and period: Brazil, approximately 2016-2018.
- Source documentation inspected: 2026-09-16. Download date/version: unknown.
- None of the nine required CSVs was present at implementation time.

Download the original source (Kaggle may require sign-in). Extract the nine CSVs
directly into `07_ecommerce/data/raw/` without renaming or editing. Do not use a
cleaned mirror. Record the actual download date/version here when known; file
modification times do not prove provenance. The audit records MD5 fingerprints
to detect changes, not authenticate the publisher. It never downloads data.
The existing ABS workbook is separate Australian context, not a substitute.

## Licence

The source lists **CC BY-NC-SA 4.0**:
https://creativecommons.org/licenses/by-nc-sa/4.0/

Credit Olist, link source/licence, identify modifications and observe the
NonCommercial and ShareAlike conditions for applicable redistributed adaptations.
This includes reviewing Tableau extracts and processed outputs before publishing.
Public availability is not unrestricted commercial permission; a code licence
does not relicense the data. No endorsement is implied. Raw files are ignored by
Git; provide download instructions instead. Suggested attribution: "Based on the
Brazilian E-Commerce Public Dataset by Olist, CC BY-NC-SA 4.0; audit transformations
documented in this project." Retain attribution on public processed outputs.

## Expected files and grains

| Filename | Role and expected grain |
|---|---|
| `olist_customers_dataset.csv` | One order-specific `customer_id`, mapping to persistent identity and destination |
| `olist_orders_dataset.csv` | One `order_id`, status and timestamps |
| `olist_order_items_dataset.csv` | One `(order_id, order_item_id)`, product/seller/price/freight |
| `olist_products_dataset.csv` | One `product_id`, category and attributes |
| `olist_sellers_dataset.csv` | One `seller_id`, location |
| `olist_order_payments_dataset.csv` | One `(order_id, payment_sequential)`, payment component |
| `olist_order_reviews_dataset.csv` | Review record; neither review nor order ID assumed unique |
| `olist_geolocation_dataset.csv` | Geographic observation; postal prefixes repeat |
| `product_category_name_translation.csv` | Portuguese category to English mapping |

These are contracts to test, not verified claims about files not yet acquired.

## Identity and observation

`customer_id` changes per order; `customer_unique_id` links observed purchases.
Using the former would incorrectly make customers look one-off. The audit checks
mapping keys before joining. First observed purchase is not necessarily first-ever
(left truncation). Recent buyers have less follow-up (right truncation).
Separate descriptive all-order and delivered-snapshot scopes are reported.
Counts within 30/60/90/180 days are not final retention KPIs; tied and same-day
purchases are flagged. No subsequent observed purchase does not mean churn.

## Money and join grain

Item values are marketplace transaction value / GMV-like amounts in BRL, not
company revenue, net revenue, profit or margin. Freight is not verified merchant
logistics cost. Item ID is a sequence, not quantity. Instalments do not multiply
payment values. Multiple payment records may be legitimate.

Independently sum payments and item price plus freight to order grain before
comparing. Parse money to whole centavos; flag invalid or more-than-two-decimal
values instead of silently rounding. Orders with missing/invalid amounts are
non-comparable. Differences are not automatically refunds.

Raw items x payments x reviews can multiply rows. Real-order examples quantify
this risk. `SUM(DISTINCT price)` also fails when legitimate items share a price.
Reviews belong to orders, not individual products/sellers. Phase 1 quantifies
multiplicity without choosing a production deduplication rule.

## Outputs and decision rules

- `source_manifest.csv`: existence, size/hash, shape, column names/types and date ranges.
- `data_quality.csv`: checks with status/severity, numerator/denominator and treatment.
- `audit_summary.csv`: concise decision and capability evidence.
- `audit_details.csv`: distributions, monthly coverage, follow-up capacity,
  reconciliation and a small set of order-level fan-out examples.
- `phase2_summary.csv`: whether analytical tables were rebuilt and why.
- `phase2_table_inventory.csv`: processed table names, grains, row counts and paths.

All outputs are generated under `outputs/`; never manually edit them. INFO is a
structural observation; WARN needs qualification; FAIL violates a requirement.
BLOCKING checks stop unsafe analytical joins. Raw observations are never repaired.

Missing inputs produce NOT RUN and NO-GO **for proceeding to Phase 2**, not a
verdict against Olist. With complete inputs, screening thresholds are: 100
delivered orders with valid item amounts, 30 repeating identities, two categories
with 30 orders each, and 100 delivered orders with usable dates and review scores.
These are pragmatic suitability screens, not power calculations. Structural or
commercial blockers yield NO-GO; limited capability yields GO WITH LIMITATIONS.
Historical/truncated data retain limitations even if every screen passes.

## Phase 2 analytical tables

`01_build_analytics_tables.R` builds the first analysis layer after the source
files pass the input gate. It reads raw data as text, parses money to whole
centavos, keeps source IDs unchanged and writes CSVs under `data/processed/`.
The important grain decisions are:

| Table | Planned grain |
|---|---|
| `fact_orders.csv` | One row per `order_id`, with item, payment and review aggregates |
| `fact_order_items.csv` | One row per `(order_id, order_item_id)` |
| `fact_payments.csv` | One row per payment component |
| `fact_reviews.csv` | One row per source review record |
| `dim_customers.csv` | One row per order-specific `customer_id` |
| `dim_products.csv` | One row per `product_id` with translated category |
| `dim_sellers.csv` | One row per `seller_id` |
| `dim_date.csv` | One row per observed calendar date |
| `customer_order_history.csv` | One row per observed `customer_unique_id` |
| `monthly_kpis.csv` | One row per purchase month |
| `category_performance.csv` | One row per translated product category |

`fact_orders.csv` is the safe base for order-level GMV, payments, review and
delivery KPIs. Item, payment and review records are aggregated independently
before joining to order grain, which avoids inflated revenue from raw
items-payments-reviews fan-out.

## Observation-window candidates

Inspect monthly volumes/statuses, not only the maximum timestamp. Candidate month
ends require at least 100 orders, 90% terminal statuses (delivered, canceled,
unavailable), and volume at least 50% of the preceding-three-month median, with
three preceding months available. This is a heuristic, not proof of completeness.
Report follow-up capacity for the last three candidates and the observed end
(labelled upper bound). No reporting cutoff is automatically approved. Final
snapshot statuses cannot reconstruct the status known at earlier month ends.

## Limitations

Historical Brazilian marketplace data does not represent Australia or a person's
entire shopping history. There is no true acquisition source and insufficient
cost, commission, discount and refund information for profitability claims.
Review nonresponse is selective; comments are optional and delivery dates may be
structurally absent for undelivered orders. Payment differences and review
multiplicity require evidence. Sparse repurchasing remains unconfirmed until the
real audit runs. Delivery/review overlap establishes feasibility, not causation.
Review outputs before choosing a window or approving a separate Phase 2.
