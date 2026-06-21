#' ---
#' title: "Day 02 - Healthcare"
#' output:
#'   pdf_document:
#'     latex_engine: xelatex
#' ---

#+ setup, include=FALSE
library(tidyverse)
library(here)

dataset_path <- here("02_healthcare", "data", "aihw_elective_surgery.csv")

elective_surgery <- read_csv(dataset_path, show_col_types = FALSE)

#' ## Dataset Dimensions

#+ echo=FALSE
dim(elective_surgery)

#' ## Column Names

#+ echo=FALSE
names(elective_surgery)

#' ## Sample Rows

#+ echo=FALSE
head(elective_surgery, 10)

#' ## Missing Values by Column

#+ echo=FALSE
elective_surgery %>%
  summarise(across(everything(), ~ sum(is.na(.))))

#' ## Duplicate Row Count

#+ echo=FALSE
sum(duplicated(elective_surgery))

#' ## Summary Statistics

#+ echo=FALSE
summary(elective_surgery)

#' ## Healthcare Dataset Business Overview

#+ echo=FALSE, results='asis'
cat("
This dataset is an Australian hospital operations dataset from the
Australian Institute of Health and Welfare (AIHW) MyHospitals API.

Each row is a reported elective surgery data point for a specific
reporting unit, reporting period, measure, and reported measure category.

This dataset can support future analysis of surgical access, hospital
activity, waiting times, long waits, treatment timeframes, procedure mix,
reporting-unit comparisons, and changes in elective surgery performance
over time.
")