# #src/figures/fig05-crowding-race-tenure-year-bars.R
#
# Produce bar charts showing people per bedroom (room) by tenure in two panels
# (upper and lower) as well as by race/ethnicity (groups of two bars) and year
# (paired bars)
#
# Input: throughput/kob_output.rds
# Output: output/figures/fig05-crowding-race-tenure-year-bars.png, 
#         output/figures/fig05-appendix-crowding-race-tenure-year-bars.png
#
# TODO: write unit tests for functions

# ----- Step 0: Config ----- 
library("patchwork")
library("ggplot2")

# ----- Step 1: Define functions -----
# Plotting function