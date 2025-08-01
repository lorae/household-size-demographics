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
source("src/utils/regression-postprocess-tools.R") # for and gu_adjust

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

# this the input I want to emulate
kob_input <- readRDS("throughput/kob_input.rds") 

formula <- NUMPREC ~ RACE_ETH_bucket

# Test the regression with the default omitted variable (AIAN; first alphabetically)
dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula)

# Now test regression with omitted variable = AAPI
ipums_2019_sample_tb$RACE_ETH_bucket <- 
  relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "AIAN")

dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula)

# Now test regression with omitted variable = Black
ipums_2019_sample_tb$RACE_ETH_bucket <- 
  relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "Black")

dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula)


x <- dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula)
gu_adjust(x)

# Now test regression with omitted variable = AAPI
ipums_2019_sample_tb$RACE_ETH_bucket <- 
  relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "AIAN")

y <- dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula)
gu_adjust(y)

gu_adjust(y, adjust_vars = c("RACE_ETH_bucket", "Peaches"))

# ----- Step 3: multivariate regression
formula_multivar <- NUMPREC ~ RACE_ETH_bucket + AGE_bucket

# Omitted vars: AAPI, 0-4
ipums_2019_sample_tb$RACE_ETH_bucket <- 
  relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "AAPI")
ipums_2019_sample_tb$AGE_bucket <- 
  relevel(factor(ipums_2019_sample_tb$AGE_bucket), ref = "0-4")
a <- dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula_multivar)

gu_adjust(reg_output = a, adjust_vars = c("RACE_ETH_bucket", "AGE_bucket"))

# Omitted vars: White, 45-49
ipums_2019_sample_tb$RACE_ETH_bucket <- 
  relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "White")
ipums_2019_sample_tb$AGE_bucket <- 
  relevel(factor(ipums_2019_sample_tb$AGE_bucket), ref = "45-49")
a <- dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula_multivar)

gu_adjust(reg_output = a, adjust_vars = c("RACE_ETH_bucket", "AGE_bucket"))

# uh oh.... things don't match!
