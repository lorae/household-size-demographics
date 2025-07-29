#
# The purpose of this script is to quantify housing shortages according to the results
# of fine-grained-counterfactual-regional

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
load("data/helpers/cpuma-state-cross.rda") # Crosswalks CPUMA0010 to state
load("data/helpers/state-pop-growth.rda") # May be deprecated

# ----- Step 2: Import data ----- #
hhsize_cpuma_summary <- readRDS("throughput/fine-grained-hhsize-diff-cpuma.rds")
headship_cpuma_summary <- readRDS("throughput/fine-grained-headship-diff-cpuma.rds")
cf_summaries <- readRDS("throughput/fine-grained-cf-summaries.rds")

# ----- Housing shortage according to hhsize counterfactual
hhsize_cpuma_summary |> nrow()
hhsize_cpuma_summary |> filter(diff > 0) |> nrow()
693 / 1078
hhsize_cpuma_summary |> filter(diff > 0) |> 
  mutate(x = pop_2019 * diff)  |> pull(x) |> sum()

headship_cpuma_summary |> nrow()
headship_cpuma_summary |> filter(diff < 0) |> nrow()
headship_cpuma_summary |> filter(diff < 0) |> 
  mutate(x = pop_2019 * diff)  |> pull(x) |> sum()
1002 / 1078
