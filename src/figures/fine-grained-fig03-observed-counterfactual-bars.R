# src/figures/fine-grained-fig03-observed-counterfactual-bars.R
# Produce bar charts showing 2000 observed, 2019 observed, and 2019 and expected 
# (counterfactual) outcomes.
#
# Input: TBD
# Output: TBD

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("readxl")
library("ggplot2")
library("base64enc")
library("sf")
options(scipen = 999)

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation
source("src/utils/plotting-tools.R") # defines `make_observed_cf_barplot()`

# Rounding helpers
round_down_to <- function(x, base) base * floor(x / base)
round_up_to <- function(x, base) base * ceiling(x / base)

# ----- Step 2: Import data ----- #
cf_summaries <- readRDS("throughput/fine-grained-cf-summaries.rds")

#make_observed_cf_barplot(fig06_data |> filter(name == "Number of Persons"), yaxis_override = c(3, 3.5))
