# src/figures/fine-grained-fig03-observed-counterfactual-bars.R
# Produce bar charts showing 2000 observed, 2019 observed, and 2019 and expected 
# (counterfactual) outcomes.
#
# Input: TBD
# Output: TBD

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("stringr")
library("tidyr")
library("ggplot2")
library("patchwork")
library("scales")
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

# ----- Step 3: Make plots ----- #
p <- make_observed_cf_barplot(cf_summaries |> filter(name == "Number of Persons"), yaxis_override = c(3, 3.5))
p

h <- make_observed_cf_barplot(
    cf_summaries |> 
      filter(name == "Headship Rate") |> 
      mutate(
        observed_2000 = 100 * observed_2000,
        observed_2019 = 100 * observed_2019,
        expected_2019 = 100 * expected_2019
      ),
    yaxis_override = c(30, 45),
    label_format = "%.2f%%"
) +
  scale_y_continuous(labels = label_percent(scale = 1)) 
h

fig03 <- p + h
# ----- Step 4: Save plots ----- #
ggsave(
  "output/figures/fine-grained/fig03-observed-expected-barshhsize.png",
  plot = fig03,
  width = 3000, height = 2000, units = "px", dpi = 300
)

