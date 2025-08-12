# src/figures/linear-reg/table-A01.R
# Output to CSV for further formatting

library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

source("src/utils/kob_panels.R")
kob_output <- readRDS("throughput/kob_output.rds")

# Create output directory if needed
dir.create("output/tables", showWarnings = FALSE)

# Loop over list elements and write each to a CSV file
for (nm in names(kob_output)) {
  file_path <- file.path("output/tables", paste0("kob_output_", nm, ".csv"))
  write.csv(kob_output[[nm]], file = file_path, row.names = FALSE)
}

agg<-readRDS("throughput/aggregates.rds")
agg_sum <- agg |> mutate(
  diff = mean_2019 - mean_2000
)

sum <- kob_output$p |>
  group_by(variable) |>
  summarize(
    total_c = sum(c),
    total_c_se = sqrt(sum(c^2, na.rm = TRUE)),
    total_e = sum(e),
    total_e_se = sqrt(sum(u^2, na.rm = TRUE)),
    total_u = sum(u),
    total_u_se = sum(u_se)
  )

total_sum <- sum(
  sum(sum$total_c, na.rm = TRUE),
  sum(sum$total_e, na.rm = TRUE),
  sum(sum$total_u, na.rm = TRUE)
)
agg_sum
total_sum