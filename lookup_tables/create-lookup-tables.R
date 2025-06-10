# create-lookup-tables.R
#
# This script contains helper code to generate lookup tables for categorizing 
# various demographic variables (e.g., age, income) used in the main workflow.
# These lookup tables are saved as CSV files and can be reused or modified 
# for different bucketing schemes or sensitivity testing.
#
# ALSO NOTE: I've configured these lookup tables to match a range selection that
# is inclusive of lower_bound and exclusive of upper_bound. In other words,
# lower_bound <= x < upper_bound.
#
# TODO: rename from bucket_name to output_val, from lower_bound to input_range_lower,
# from upper_bound to input_range_upper, and from specific_value to input_val. Rearrange
# order as the 3 inputs in the first 3 cols and the output in the fourth column.

# ----- Age lookup tables -----

# Define and populate the age lookup table row-wise
age_buckets00 <- rbind(
  c("r00_49", 0, 50, NA),
  c("r50plus", 50, 200, NA)
) %>%
  as.data.frame() %>%
  setNames(c("bucket_name", "lower_bound", "upper_bound", "specific_value")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(age_buckets00, "age/age_buckets00.csv", row.names = FALSE)

# Define and populate the age lookup table row-wise
age_buckets01 <- rbind(
  c("0-4", 0, 5, NA),
  c("5-9", 5, 10, NA),
  c("10-14", 10, 15, NA),
  c("15-19", 15, 20, NA),
  c("20-24", 20, 25, NA),
  c("25-29", 25, 30, NA),
  c("30-34", 30, 35, NA),
  c("35-39", 35, 40, NA),
  c("40-44", 40, 45, NA),
  c("45-49", 45, 50, NA),
  c("50-54", 50, 55, NA),
  c("55-59", 55, 60, NA),
  c("60-64", 60, 65, NA),
  c("65-69", 65, 70, NA),
  c("70-74", 70, 75, NA),
  c("75-79", 75, 80, NA),
  c("80-84", 80, 85, NA),
  c("85+", 85, Inf, NA)
) %>%
  as.data.frame() %>%
  setNames(c("bucket_name", "lower_bound", "upper_bound", "specific_value")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(age_buckets01, "lookup_tables/age/age_buckets01.csv", row.names = FALSE)

# ----- Household income lookup tables -----

# hhincome_buckets00.csv
hhincome_buckets00 <- rbind(
  c("negative", -Inf, 0, NA),
  c("r000_100k", 0, 100000, NA),
  c("r100kplus", 100000, 9999999, NA),
  c("N/A", NA, NA, 9999999)
) %>%
  as.data.frame() %>%
  setNames(c("bucket_name", "lower_bound", "upper_bound", "specific_value")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(hhincome_buckets00, "hhincome/hhincome_buckets00.csv", row.names = FALSE)

# hhincome_buckets01.csv
hhincome_buckets01 <- rbind(
  c("r000_020k", 0, 20000, NA),
  c("r020k_040k", 20000, 40000, NA),
  c("r040k_060k", 40000, 60000, NA),
  c("r060k_080k", 60000, 80000, NA),
  c("r080k_100k", 80000, 100000, NA),
  c("r100k_150k", 100000, 150000, NA),
  c("r150k_200k", 150000, 200000, NA),
  c("r200k_300k", 200000, 300000, NA),
  c("r300kplus", 300000, Inf, NA)
) %>%
  as.data.frame() %>%
  setNames(c("bucket_name", "lower_bound", "upper_bound", "specific_value")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(hhincome_buckets01, "hhincome/hhincome_buckets01.csv", row.names = FALSE)

# hhincome_buckets03.csv
hhincome_buckets03 <- rbind(
  c("negative", -Inf, 0, NA),
  c("00-30k", 0, 30000, NA),
  c("30-60k", 30000, 60000, NA),
  c("60-100k", 60000, 100000, NA),
  c("100-150k", 100000, 150000, NA),
  c("150-200k", 150000, 200000, NA),
  c("200kplus", 200000, 1000000000000, NA),
  c("N/A", NA, NA, 9999999)
) |>
  as.data.frame() |>
  setNames(c("bucket_name", "lower_bound", "upper_bound", "specific_value")) |> # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(hhincome_buckets03, "lookup_tables/hhincome/hhincome_buckets03.csv", row.names = FALSE)

# ----- Ethnicity (hispanic) lookup tables -----

# hispan_buckets00.csv
hispan_buckets00 <- rbind(
  c(0, "not_hispanic"),
  c(1, "hispanic"),
  c(2, "hispanic"),
  c(3, "hispanic"),
  c(4, "hispanic"),
  c(9, "N/A")
) %>%
  as.data.frame() %>%
  setNames(c("old_val", "new_val")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(hispan_buckets00, "hispan/hispan_buckets00.csv", row.names = FALSE)

# ----- Race lookup tables -----

# race_buckets00.csv
race_buckets00 <- rbind(
  c(1, "white"),
  c(2, "black"),
  c(3, "aian"),
  c(4, "aapi"),
  c(5, "aapi"),
  c(6, "aapi"),
  c(8, "multi"),
  c(9, "multi"),
  c(7, "other")
) %>%
  as.data.frame() %>%
  setNames(c("old_val", "new_val")) %>% # Add column names
  type.convert(as.is = TRUE) # Convert column encoding to character or numeric

write.csv(race_buckets00, "race/race_buckets00.csv", row.names = FALSE)
