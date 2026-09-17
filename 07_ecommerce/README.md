# 07 Ecommerce: Growth Quality

Customer Value, Repeat Purchasing & Experience.

## Current phase

Phase 1 documents and audits dataset readiness. Phase 2 prepares analytical
tables from the unchanged Olist source files when they are available.

The original Olist CSVs were absent at implementation time. Obtain them from the
original source documented in [DATASET.md](DATASET.md) and extract unchanged into
`data/raw/`. The existing ABS workbook is separate Australian context.
`business_questions.md` is an earlier brainstorm, not the current build scope.

## Run

Use R with `readr`, `dplyr`, `purrr`, `tibble` and `lubridate`, familiar
from the existing tidyverse scripts. Missing packages are reported, never
automatically installed. No new dependency manager, database or API is required.

From the repository root:

```sh
Rscript --vanilla 07_ecommerce/scripts/00_dataset_overview.R
```

From the industry folder, use `Rscript --vanilla scripts/00_dataset_overview.R`.
In RStudio with the repository project open:

```r
source("07_ecommerce/scripts/00_dataset_overview.R")
```

Each Phase 1 run replaces the four generated audit CSVs in `outputs/`. Missing files
produce a manifest, quality checks and a NOT RUN summary, then an explicit error.
That guard is not a successful dataset audit. Never manually edit generated CSVs.

Build Phase 2 processed tables:

```sh
Rscript --vanilla 07_ecommerce/scripts/01_build_analytics_tables.R
```

With the real Olist files present, this writes safe-grain tables to
`data/processed/` and summary files to `outputs/`. Without the raw files, it
writes `outputs/phase2_summary.csv` and `outputs/phase2_table_inventory.csv`
showing `NOT RUN`, then stops. That is intentional; no findings are invented.

## Validate

```sh
Rscript --vanilla 07_ecommerce/scripts/validate_audit.R
Rscript --vanilla 07_ecommerce/scripts/validate_phase2_tables.R
```

Validation uses explicitly labelled temporary synthetic test fixtures, never
presented as real Olist observations. They are isolated from `data/raw/` and
removed afterwards. Tests cover missing files, malformed data, grain failures,
date/money parsing, reconciliation, fan-out, immutable inputs and processed-table
grain checks.

## Interpret

Start with `outputs/audit_summary.csv`; supporting checks and distributions are
in `data_quality.csv` and `audit_details.csv`. The source manifest records file
fingerprints and shapes. See DATASET.md for screening thresholds, truncation,
customer identity, GMV terminology, review multiplicity and licensing.

Source credit: Brazilian E-Commerce Public Dataset by Olist, CC BY-NC-SA 4.0.
Commit and push require separate approval. Phase 2 requires a new instruction.
