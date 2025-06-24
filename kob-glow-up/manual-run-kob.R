# kob-glow-up/manual-run-kob.R
# The purpose of this script is to manually run the KOB on a small dataset, comparing
# to the actual KOB, as a proof-of-concept and a stepping stone to modularizing 
# the operation.

library(oaxaca)

# Read in some synthetic data
data <- readRDS("kob/synthetic-data/c-only.rds")

# Run the KOB
# First we need to create the "z" variable according to the `oaxaca()` docs
data <- data |>
  mutate( z = year == 2019) # TRUE when year is 2019, FALSE when year is 2000

# Force EDUC_bucket to use dummy coding (a.k.a. treatment contrasts)
contrasts(data$EDUC_bucket) <- contr.treatment(levels(data$EDUC_bucket))

oaxaca(formula = NUMPREC ~ EDUC_bucket | z, data = data)