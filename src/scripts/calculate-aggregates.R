#src/scripts/calculate-aggregates.R
#
# The purpose of this script is to calculate a select number of aggregate values:
# - NUMPREC: Mean number of housemates a non-group-quartered individual lives with, 
#   2000 and 2019
# - room: Mean number of rooms a non-group-quartered individual has in their home, 
#   2000 and 2019
# - bedroom: Mean number of bedrooms a non-group-quartered individual has in their
#   home, 2000 and 2019
# - persons_per_room: Mean number of persons per room
# - persons_per_bedroom: Mean number of persons per bedroom
#
# These aggregates are needed to validate the KOB analysis.

# ----- Step 0: Config ----- #

# ----- Step 0b: Survey setup for SEs -----
library(survey)

# Load survey designs (one per year)
des2000 <- readRDS("throughput/design_2000_survey.rds")
des2019 <- readRDS("throughput/design_2019_survey.rds")

# Helper: compute (mean, SE) for a variable within a design, with your GQ filter
svy_mean_with_se <- function(design, var, gq_keep = c(0, 1, 2)) {
  # If you truly want *non*-group quarters only, set gq_keep = 0.
  dsub <- subset(design, GQ %in% gq_keep)
  # Build a one-sided formula like ~NUMPREC
  f <- as.formula(paste0("~", var))
  est <- svymean(f, dsub, na.rm = TRUE)
  c(mean = as.numeric(est), se = as.numeric(SE(est)))
}

# List of variables to compute
vars <- c("NUMPREC", "bedroom", "room", "persons_per_bedroom", "persons_per_room")

# Abbreviations for the variables we'll also add to the output for downstream use
abbrev_map <- c(
  NUMPREC = "p",
  bedroom = "b",
  room = "r",
  persons_per_bedroom = "ppbr",
  persons_per_room = "ppr"
)
name_map <- c(
  NUMPREC = "Number of Persons",
  bedroom = "Number of Bedrooms",
  room = "Number of Rooms",
  persons_per_bedroom = "Persons per Bedroom",
  persons_per_room = "Persons per Room"
)

# Compute for all variables for both years
res2000 <- t(vapply(vars, function(v) svy_mean_with_se(des2000, v), numeric(2L)))
res2019 <- t(vapply(vars, function(v) svy_mean_with_se(des2019, v), numeric(2L)))

# Bind into a tibble and join onto your existing 'aggregates'
se_tbl <- tibble(
  variable = vars,
  mean_2000 = res2000[, "mean"],
  mean_2000_se   = res2000[, "se"],
  mean_2019 = res2019[, "mean"],
  mean_2019_se   = res2019[, "se"]
)

# Construct output tibble
aggregates <- tibble(
  variable = vars
) |>
  left_join(se_tbl, by = "variable") |>
  mutate(abbrev_variable = recode(variable, !!!abbrev_map)) |>
  mutate(name = recode(variable, !!!name_map)) |>
  relocate(abbrev_variable, .after = variable) |>
  relocate(name, .before = variable)

# Save
saveRDS(aggregates, "throughput/aggregates.rds")
