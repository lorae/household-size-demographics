# temporal-kob.R
# apply learnings from test-temporal-kob.r to produce the KOB decomposition

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("ggplot2")
library("oaxaca")
library("tibble")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("kob/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

ipums_db <- ipums_db |>
  mutate(
    tenure = ifelse(OWNERSHP == 1, "homeowner", "renter"),
    sex = ifelse(SEX == 1, "male", "female")
  )

# ipums_db <- ipums_db |>
#   mutate(
#     tenure = ifelse(OWNERSHP == 1, as.character("homeowner"), as.character("renter")),
#     sex = ifelse(SEX == 1, as.character("male"), as.character("female"))
#   )
# ipums_db <- ipums_db |>
#   mutate(
#     tenure = sql("CASE WHEN OWNERSHP = 1 THEN 'homeowner' ELSE 'renter' END"),
#     sex = sql("CASE WHEN SEX = 1 THEN 'male' ELSE 'female' END")
#   )



# These two models take about a two minutes to compute. No CPUMAs since they are difficult
# to handle. 8 Gb, whoa!
model_2000 <- lm(data = ipums_db |> filter(YEAR == 2000 & GQ %in% c(0,1,2)),
                 weights = PERWT,
                 formula = NUMPREC ~ RACE_ETH_bucket*us_born + AGE_bucket + sex  +
                   EDUC_bucket + INCTOT_cpiu_2010_bucket + tenure
)

model_2019 <- lm(data = ipums_db |> filter(YEAR == 2019 & GQ %in% c(0,1,2)),
                 weights = PERWT,
                 formula = NUMPREC ~ RACE_ETH_bucket*us_born + AGE_bucket + sex  +
                  EDUC_bucket + INCTOT_cpiu_2010_bucket + tenure
)

coef_df <- full_join(
  enframe(model_2000$coefficients, name = "name", value = "mean_2000"),
  enframe(model_2019$coefficients, name = "name", value = "mean_2019"),
  by = "name"
)

# Known varnames (exact strings)
known_varnames <- c(
  "RACE_ETH_bucket", "AGE_bucket", "sex", "us_born",
  "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "tenure"
)

intercept_row <- coef_df |>
  filter(name %in% c(
    "(Intercept)",
    "RACE_ETH_bucketAIAN:us_bornTRUE",
    "RACE_ETH_bucketBlack:us_bornTRUE",
    "RACE_ETH_bucketHispanic:us_bornTRUE",
    "RACE_ETH_bucketMultiracial:us_bornTRUE",
    "RACE_ETH_bucketOther:us_bornTRUE",
    "RACE_ETH_bucketWhite:us_bornTRUE"
  )) |>
  mutate(varname = NA_character_, value = NA_character_)

non_intercepts <- coef_df |>
  filter(!name %in% c(
    "(Intercept)",
    "RACE_ETH_bucketAIAN:us_bornTRUE",
    "RACE_ETH_bucketBlack:us_bornTRUE",
    "RACE_ETH_bucketHispanic:us_bornTRUE",
    "RACE_ETH_bucketMultiracial:us_bornTRUE",
    "RACE_ETH_bucketOther:us_bornTRUE",
    "RACE_ETH_bucketWhite:us_bornTRUE"
  )) |>
  mutate(
    varname = map_chr(name, function(nm) {
      matched <- keep(known_varnames, function(vn) str_starts(nm, vn))
      if (length(matched) != 1) stop(paste("Could not uniquely match varname for:", nm))
      matched
    }),
    value = str_remove(name, varname)
  )

coef_df <- bind_rows(intercept_row, non_intercepts)



get_weighted_count <- function(varname, value, year, name = NULL) {
  # Return NA for intercept or missing input
  if (!is.null(name) && name == "(Intercept)") {
    return(NA_real_)
  }
  
  # Manual exceptions for RACE_ETH_bucket:us_bornTRUE interaction terms
  if (!is.null(name)) {
    if (name == "RACE_ETH_bucketAIAN:us_bornTRUE") {
      return(ipums_db |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "AIAN", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    } else if (name == "RACE_ETH_bucketBlack:us_bornTRUE") {
      return(ipums_db |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "Black", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    } else if (name == "RACE_ETH_bucketHispanic:us_bornTRUE") {
      return(ipums_db |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "Hispanic", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    } else if (name == "RACE_ETH_bucketMultiracial:us_bornTRUE") {
      return(ipums_db |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "Multiracial", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    } else if (name == "RACE_ETH_bucketOther:us_bornTRUE") {
      return(ipums_db |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "Other", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    } else if (name == "RACE_ETH_bucketWhite:us_bornTRUE") {
      return(ipums_db |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "White", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    }
  }
  
  # Coerce specific label strings to their underlying codes
  if (value == "male" && varname == "sex") {
    value <- 1
    varname <- "SEX"
  } else if (value == "homeowner" && varname == "tenure") {
    value <- 1
    varname <- "OWNERSHP"
  }
  
  # Default case
  ipums_db |>
    filter(YEAR == !!year, GQ %in% c(0, 1, 2)) |>
    filter(!!sym(varname) == !!value) |>
    summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
    collect() |>
    pull(weighted_count)
}


coef <- coef_df |>
  mutate(
    weighted_count_2000 = pmap_dbl(
      list(varname = varname, value = value, name = name),
      ~ get_weighted_count(..1, ..2, 2000, ..3)
    ),
    weighted_count_2019 = pmap_dbl(
      list(varname = varname, value = value, name = name),
      ~ get_weighted_count(..1, ..2, 2019, ..3)
    )
  )


pop_2000 <- ipums_db |> filter(YEAR == 2000, GQ %in% c(0, 1, 2)) |> 
  summarize(weighted_count = sum(PERWT), na.rm = TRUE) |>
  collect() |>
  pull(weighted_count)
  
pop_2019 <- ipums_db |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)) |> 
  summarize(weighted_count = sum(PERWT), na.rm = TRUE) |>
  collect() |>
  pull(weighted_count)

coef <- coef |>
  mutate(
    prop_2000 = weighted_count_2000 / pop_2000,
    prop_2019 = weighted_count_2019 / pop_2019
  )


intercept_2000 <- coef |> filter(name == "(Intercept)") |> pull(mean_2000)
intercept_2019 <- coef |> filter(name == "(Intercept)") |> pull(mean_2019)

coef <- coef |>
  mutate(
    e_component = mean_2019*(prop_2019 - prop_2000),
    c_component = (mean_2019 - mean_2000)*prop_2000
  )

#u <- intercept_2019 - intercept_2000
e <- sum(coef$e_component, na.rm = TRUE)
c <- sum(coef$c_component, na.rm = TRUE)
#u
e
c

sum(e,c) #sum(u,e,c)

lm(data = ipums_db |> filter(YEAR == 2000, GQ %in% c(0, 1, 2)), 
   formula = NUMPREC ~ 1,
   weights = PERWT)$coefficients -> mean_hhsize_2000
lm(data = ipums_db |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)), 
   formula = NUMPREC ~ 1,
   weights = PERWT)$coefficients -> mean_hhsize_2019

mean_hhsize_2019 - mean_hhsize_2000

write.csv(coef, "results/coef2.csv")

# Filter out rows with NA c_component and create label
coef_clean <- coef |>
  filter(!is.na(c_component)) |>
  mutate(label = name)

# Plot
ggplot(coef_clean, aes(x = reorder(label, c_component), y = c_component)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Contribution to Coefficients (c_component)",
    x = NULL,
    y = "c_component"
  ) +
  theme_minimal()

ggplot(coef_clean, aes(x = reorder(label, e_component), y = e_component)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Contribution to Endowments (e_component)",
    x = NULL,
    y = "c_component"
  ) +
  theme_minimal()