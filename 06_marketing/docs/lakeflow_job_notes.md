# Lakeflow Job Notes

## Job Name

06 Marketing Campaign Response Pipeline

## Purpose

This Lakeflow Job orchestrates the Databricks notebooks for the marketing campaign response project.

## Task Order

1. 01_create_bronze
2. 02_validate_bronze
3. 03_create_silver
4. 04_validate_silver
5. 05_create_gold_features
6. 06_create_gold_summary
7. 07_train_model
8. 08_evaluate_model
9. 09_create_predictions
10. 10_refresh_tableau_serving

## Output Tables

- workspace.marketing_campaign.bronze_marketing_campaign
- workspace.marketing_campaign.silver_marketing_campaign
- workspace.marketing_campaign.gold_customer_features
- workspace.marketing_campaign.gold_campaign_summary
- workspace.marketing_campaign.customer_campaign_predictions
- workspace.marketing_campaign.tableau_campaign_dashboard

## Notes

Setup notebooks are not included in the recurring job because they are used only for environment, catalog, volume, and raw file setup.

The job can be run manually with Run now. It can also be scheduled later if fresh campaign data is added regularly.