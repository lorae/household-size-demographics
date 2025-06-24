# ----- STEP 0: Configuration ----- #

set.seed(123)
n <- 5000

educ_levels <- c("less_than_hs", "hs", "some_college", "college_4yr_plus")
inc_levels <- c("less_than_10k", "from_10k_to_100k", "greater_than_100k")

save_path <- "kob/synthetic-data"

# Function to draw positive integers from a normal distribution
draw_positive_ints <- function(n, mean, sd) {
  pmax(1, round(rnorm(n, mean, sd)))
}

# Generalized synthetic data generator
generate_synthetic_data <- function(year, educ_probs, income_probs, numprec_func) {
  tibble(
    year = year,
    EDUC_bucket = sample(educ_levels, size = n, replace = TRUE, prob = educ_probs),
    HHINCOME_bucket = sample(inc_levels, size = n, replace = TRUE, prob = income_probs),
    AGE = sample(1:80, size = n, replace = TRUE),
    PERWT = 1
  ) |>
    mutate(
      EDUC_bucket = factor(EDUC_bucket, levels = educ_levels, ordered = TRUE),
      HHINCOME_bucket = factor(HHINCOME_bucket, levels = inc_levels, ordered = TRUE),
      expected_NUMPREC = numprec_func(as.numeric(EDUC_bucket), as.numeric(HHINCOME_bucket)),
      NUMPREC = draw_positive_ints(n, mean = expected_NUMPREC, sd = 1.5)
    )
}

# ----- STEP 1: Create u-component KOB decomp data frame ----- #
# Via this data generation process, 2000 an 2019 data differ in 
# their correlates (education, income) as well as in their average
# outcome NUMPREC. However, NUMPREC is generated independently and
# depends only on year: It is not generate based on education or
# income. 

u_only_2000 <- generate_synthetic_data(2000, 
                                       educ_probs = c(0.1, 0.3, 0.3, 0.3), 
                                       income_probs = c(0.2, 0.6, 0.2), 
                                       numprec_func = function(...) rep(3, n))
u_only_2019 <- generate_synthetic_data(2019, 
                                       educ_probs = c(0.2, 0.3, 0.3, 0.2), 
                                       income_probs = c(0.3, 0.6, 0.1), 
                                       numprec_func = function(...) rep(4, n))
u_only <- bind_rows(u_only_2000, u_only_2019)
saveRDS(u_only, file = file.path(save_path, "u-only.rds"))

# ----- STEP 2: e-component (endowment-only differences) ----- #
# Via this data generation process, 2000 an 2019 data differ in 
# their correlates (education, income) as well as in their average
# outcome NUMPREC. NUMPREC is generated as a function of education
# and income.

e_func <- function(educ_idx, income_idx) {
  (length(educ_levels) - educ_idx) + (length(inc_levels) - income_idx) + 1
}

e_only_2000 <- generate_synthetic_data(2000, 
                                       educ_probs = c(0.1, 0.3, 0.3, 0.3), 
                                       income_probs = c(0.2, 0.6, 0.2), 
                                       numprec_func = e_func)
e_only_2019 <- generate_synthetic_data(2019, 
                                       educ_probs = c(0.05, 0.2, 0.4, 0.35), 
                                       income_probs = c(0.1, 0.65, 0.25), 
                                       numprec_func = e_func)
e_only <- bind_rows(e_only_2000, e_only_2019)
saveRDS(e_only, file = file.path(save_path, "e-only.rds"))

# ----- STEP 3: c-component (coefficient-only differences) ----- #
# Via this data generation process, 2000 an 2019 data differ do not at all differ
# in their correlates (education, income) but do differ in the marginal effect
# that those correlates have on the measured result (NUMPREC).

c_func_2000 <- function(educ_idx, income_idx) {
  (length(educ_levels)*1.5 - educ_idx*1.5) + (length(inc_levels)*1.5 - income_idx*1.5) + 1
}
c_func_2019 <- function(educ_idx, income_idx) {
  (length(educ_levels) - educ_idx) + (length(inc_levels) - income_idx) + 1
}

c_only_2000 <- generate_synthetic_data(2000, 
                                       educ_probs = c(0.1, 0.3, 0.3, 0.3), 
                                       income_probs = c(0.2, 0.6, 0.2), 
                                       numprec_func = c_func_2000)
c_only_2019 <- generate_synthetic_data(2019, 
                                       educ_probs = c(0.1, 0.3, 0.3, 0.3), 
                                       income_probs = c(0.2, 0.6, 0.2), 
                                       numprec_func = c_func_2019)
c_only <- bind_rows(c_only_2000, c_only_2019)
saveRDS(c_only, file = file.path(save_path, "c-only.rds"))


