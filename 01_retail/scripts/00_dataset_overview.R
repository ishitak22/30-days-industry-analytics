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

# 3. Data types
cat("\nCOLUMN TYPES\n")
glimpse(retail_sales)

# 4. Missing values
cat("\nMISSING VALUES BY COLUMN\n")
retail_sales %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# 5. Duplicate rows
cat("\nDUPLICATE ROW COUNT\n")
sum(duplicated(retail_sales))

# 6. Summary statistics
cat("\nSUMMARY STATISTICS\n")
summary(retail_sales)

# 7. Unique values for key categorical columns
cat("\nGENDER VALUES\n")
retail_sales %>%
  count(Gender)

cat("\nPRODUCT CATEGORY VALUES\n")
retail_sales %>%
  count(`Product Category`)