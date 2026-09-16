# 07 Ecommerce: Growth Quality

Customer Value, Repeat Purchasing & Experience.

## Phase 1 only

This is a dataset suitability audit, not a completed business case study.
The original Olist CSVs were absent at implementation time. Obtain them from
the original source documented in [DATASET.md](DATASET.md) and extract unchanged
into `data/raw/`. The existing ABS workbook is separate Australian context.
`business_questions.md` is an earlier brainstorm, not the Phase 1 scope.

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

Each run replaces the four generated audit CSVs in `outputs/`. Missing files
produce a manifest, quality checks and a NOT RUN summary, then an explicit error.
That guard is not a successful dataset audit. Never manually edit generated CSVs.

## Validate

```sh
Rscript --vanilla 07_ecommerce/scripts/validate_audit.R
```

Validation uses explicitly labelled temporary synthetic test fixtures, never
presented as real Olist observations. They are isolated from `data/raw/` and
removed afterwards. Tests cover missing files, malformed data, grain failures,
date/money parsing, reconciliation, fan-out and immutable inputs.

## Interpret

Start with `outputs/audit_summary.csv`; supporting checks and distributions are
in `data_quality.csv` and `audit_details.csv`. The source manifest records file
fingerprints and shapes. See DATASET.md for screening thresholds, truncation,
customer identity, GMV terminology, review multiplicity and licensing.

Source credit: Brazilian E-Commerce Public Dataset by Olist, CC BY-NC-SA 4.0.
Commit and push require separate approval. Phase 2 requires a new instruction.
