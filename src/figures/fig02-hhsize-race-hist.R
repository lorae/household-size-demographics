# #src/figures/fig02-hhsize-race-hist.R
#
# Produce bar charts showing hhsize by race as a histogram
#
# Input: data/db/ipums.duckdb
# Output: output/figures/fig02-hhsize-race-hist.png
#
# ----- Step 0: Config ----- 
library("ggplot2")

devtools::load_all("../dataduck")
