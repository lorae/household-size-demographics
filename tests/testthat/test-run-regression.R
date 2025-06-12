# test-run-regression
# ----- Step 0: Workspace setup ----- #

library("testthat")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# ----- Step 1: Create test inputs ----- #

# Read in synthetic data
synth_data <- readRDS("kob/synthetic-data/c-only.rds")

# We'll only experiment with the synthetic data for the year 2000
input <- synth_data |> 
  filter(year == 2000)

# ----- Step 2: Try the function ----- #

model <- lm(NUMPREC ~ HHINCOME_bucket + EDUC_bucket, 
            data = input, weights = PERWT,
            contrasts = list(
              HHINCOME_bucket = "contr.treatment",
              EDUC_bucket = "contr.treatment"
            ))