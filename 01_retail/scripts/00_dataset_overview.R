# Day 01 - Retail

library(tidyverse)

# 1. Load dataset
retail_sales <- read_csv("data/retail_sales.csv", show_col_types = FALSE)

# 2. Basic structure
cat("DATASET DIMENSIONS\n")
dim(retail_sales)

cat("\nCOLUMN NAMES\n")
names(retail_sales)

cat("\nDATA STRUCTURE\n")
str(retail_sales)

cat("\nSAMPLE ROWS\n")
head(retail_sales, 10)