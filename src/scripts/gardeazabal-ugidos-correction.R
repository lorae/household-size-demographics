# src/scripts/gardeazabal-ugidos correction
#
# The purpose of this script is to define a function implementing the G-U 
# correction on a set of KOB results.
# 
# References:
# https://cran.r-project.org/web/packages/oaxaca/vignettes/oaxaca.pdf


# ----- Step 0: Config ----- #
n_strata = 3

library(dplyr)
library(devtools)

# dataduck internal package & helper scripts
# TODO: will someday be added to dataduck
load_all("../dataduck")
source("src/utils/create-benchmark-data.R")
source("src/utils/regression-backends.R")

# ----- Step 1: Load and prepare sample ----- #
create_benchmark_sample(
  year = 2019,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

ipums_2019_sample_tb <- readRDS(glue("cache/benchmark_sample_2019_{n_strata}/tb.rds"))
filtered_tb <- ipums_2019_sample_tb |> filter(GQ %in% c(0, 1, 2))

# ----- Step 2: Run a full KOB analysis ----- #
# Note: standard errors aren't needed here, but until I build the KOB function to
# have optional SEs, I might just keep the SEs in this pipeline even though they don't
# serve a direct purpose

