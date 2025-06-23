#kob/refactor/coefs00_2019.R
# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)

# Read in the pre-subsetted survey
tic("Read 2019 survey design as RDS")
design_2019_survey <- readRDS("kob/throughput/design_2019_survey.rds")
toc()

prop_vars <- c(
  "RACE_ETH_bucket", 
  "AGE_bucket", 
  "EDUC_bucket",
  "INCTOT_cpiu_2010_bucket", 
  "us_born", 
  "tenure", 
  "gender",
  "cpuma"
)

tic("Run model 00")
model00_2019 <- svyglm(NUMPREC ~ -1 + 
                         RACE_ETH_bucket +
                         AGE_bucket +
                         EDUC_bucket +
                         INCTOT_cpiu_2010_bucket +
                         us_born +
                         tenure +
                         gender + 
                         cpuma, design = design_2019_survey)
toc()

# Save results in throughput
tic("Save model00_2019 to kob/throughput")
saveRDS(model00_2019, file = "kob/throughput/model00_2019.rds")
toc()