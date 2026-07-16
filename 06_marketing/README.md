# 06 Marketing: Customer Campaign Response Intelligence

## Project Overview

This project uses Databricks to build an end-to-end customer campaign response analytics workflow. The goal is to understand which customer segments respond to marketing campaigns and to create prediction-ready outputs for future campaign targeting.

The project includes:

- GitHub and Databricks Git folder integration
- Unity Catalog schema and managed Volume setup
- Bronze, Silver, and Gold Delta tables
- Python, PySpark, and SQL notebooks
- Customer campaign response classification model
- MLflow experiment tracking
- Prediction Delta table
- Databricks dashboard
- Tableau dashboard workbook

## Business Question

Which customer segments are most likely to respond to a marketing campaign, and how can the business use those patterns to improve future campaign targeting?

## Data Source

The project uses a marketing campaign customer dataset with demographic, purchase, campaign history, and response fields.

Key fields include:

- customer profile: `customer_id`, `education`, `marital_status`, `income`, `customer_age`
- purchase behavior: `total_spend`, `total_purchases`, `numwebvisitsmonth`
- campaign history: `accepted_previous_campaign`, `acceptedcmp1` to `acceptedcmp5`
- target: `response`

## Architecture

```text
Raw CSV file
  -> Unity Catalog Volume
  -> Bronze Delta table
  -> Silver cleaned Delta table
  -> Gold analytics and modelling tables
  -> MLflow model training and evaluation
  -> Prediction table
  -> Databricks dashboard
  -> Tableau dashboard
```

## Databricks Objects

Catalog:

```text
workspace
```

Schema:

```text
workspace.marketing_campaign
```

Volume:

```text
workspace.marketing_campaign.raw_files
```

Main tables:

```text
workspace.marketing_campaign.bronze_marketing_campaign
workspace.marketing_campaign.silver_marketing_campaign
workspace.marketing_campaign.gold_customer_features
workspace.marketing_campaign.gold_campaign_summary
workspace.marketing_campaign.customer_campaign_predictions
workspace.marketing_campaign.tableau_campaign_dashboard
```

## Notebook Run Order

Run the notebooks in this order:

```text
notebooks/00_setup/00_environment_check
notebooks/00_setup/01_unity_catalog_check
notebooks/00_setup/02_volume_setup
notebooks/00_setup/03_raw_file_check
notebooks/01_bronze/01_create_bronze_marketing_campaign
notebooks/01_bronze/02_validate_bronze_marketing_campaign
notebooks/02_silver/01_create_silver_marketing_campaign
notebooks/02_silver/02_validate_silver_marketing_campaign
notebooks/03_gold/01_create_gold_customer_features
notebooks/03_gold/02_create_gold_campaign_summary
sql/01_marketing_campaign_analysis
notebooks/04_ml/01_train_campaign_response_model
notebooks/04_ml/02_evaluate_campaign_response_model
notebooks/04_ml/03_create_campaign_predictions
notebooks/03_gold/03_create_tableau_serving_table
```

## Lakehouse Layers

### Bronze

The Bronze table stores the raw campaign dataset as a Delta table with ingestion metadata.

Table:

```text
bronze_marketing_campaign
```

Purpose:

- preserve raw data
- keep the source structure close to the original file
- provide a reliable starting point for cleaning

### Silver

The Silver table cleans and prepares the data.

Table:

```text
silver_marketing_campaign
```

Main transformations:

- standardized column names
- corrected data types
- handled missing income values
- created customer age and tenure fields
- created total spend and purchase features
- created campaign history features

### Gold

The Gold layer creates analytics-ready and modelling-ready tables.

Tables:

```text
gold_customer_features
gold_campaign_summary
```

Purpose:

- support SQL analysis
- support machine learning
- support dashboard reporting

## SQL Analysis

The SQL notebook explores:

- overall campaign response rate
- response by education
- response by marital status
- response by income band
- response by age band
- impact of previous campaign acceptance
- high-value customer groups

Notebook:

```text
sql/01_marketing_campaign_analysis
```

## Machine Learning

The ML workflow trains a classification model to predict whether a customer will respond to a campaign.

Target:

```text
response
```

Model type:

```text
Logistic Regression baseline
```

Tracked metrics:

- ROC AUC
- accuracy
- F1 score

MLflow experiment:

```text
marketing_campaign_response
```

Prediction output table:

```text
workspace.marketing_campaign.customer_campaign_predictions
```

Prediction fields include:

- `customer_id`
- `actual_response`
- `predicted_response`
- `response_probability`
- `prediction_timestamp`

## Dashboards

### Databricks Dashboard

Dashboard:

```text
Customer Campaign Response Intelligence
```

Purpose:

- internal lakehouse analytics
- quick campaign response exploration
- validation of Gold tables

Main views:

- KPI cards
- response rate by education and income
- customer mix by spend band
- income vs spend by campaign response
- highest probability customers

Dashboard notes:

```text
docs/databricks_dashboard_notes.md
```

### Tableau Dashboard

Workbook:

```text
tableau/Customer Campaign Response Intelligence.twb
```

Purpose:

- final business-facing visualization layer
- polished portfolio dashboard
- customer targeting story

Main views:

- KPI summary
- segment heatmap
- spend-band customer mix
- income vs spend scatter plot
- high-probability customer table

## How To Use This Project In The Future

Use this project as a repeatable campaign analytics workflow.

For a new campaign dataset:

1. Upload the new raw file into the Unity Catalog Volume.
2. Rerun the Bronze notebook to load the raw data.
3. Rerun the Silver notebook to clean and standardize the data.
4. Rerun the Gold notebooks to refresh analytics tables.
5. Rerun the ML notebooks to train, evaluate, and score customers.
6. Rerun the Tableau serving notebook.
7. Refresh the Databricks and Tableau dashboards.

This makes the workflow reusable instead of one-time analysis.

## Why Databricks Instead Of Manual Analysis

Databricks is better than doing this manually in Excel, local Python, or disconnected files because it keeps the full workflow in one governed platform.

Key advantages:

- **Scalable data processing:** PySpark can handle larger datasets than local tools.
- **Reliable table layers:** Bronze, Silver, and Gold Delta tables make the data pipeline organized and repeatable.
- **Governance:** Unity Catalog keeps tables, schemas, and Volumes managed in one place.
- **Version control:** Databricks Git folders connect notebooks to GitHub.
- **Reproducibility:** notebooks can be rerun in order to rebuild the pipeline.
- **ML tracking:** MLflow records model parameters, metrics, and experiment history.
- **Dashboard integration:** Databricks dashboards can be built directly from lakehouse tables.
- **Business visualization:** Tableau can connect to the final serving table for polished reporting.
- **Future orchestration:** Lakeflow Jobs can automate the full workflow.

Manual analysis is useful for quick exploration, but Databricks is better for a project that needs to be repeatable, governed, shareable, and production-style.

## Project Status

Completed:

- Databricks and GitHub integration
- Unity Catalog setup
- Volume setup
- Bronze, Silver, and Gold Delta tables
- SQL analysis
- Databricks dashboard
- Tableau workbook
- ML training and evaluation notebooks
<<<<<<< Updated upstream
- Prediction table