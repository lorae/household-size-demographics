# #src/figures/fig02-hhsize-race-hist.R
#
# Produce bar charts showing hhsize by race as a histogram
#
# Input: data/db/ipums.duckdb
# Output: output/figures/fig02-hhsize-race-hist.png
#
# ----- Step 0: Config ----- #
library("ggplot2")
library("duckdb")
library("dplyr")
library("tidyr")
library("glue")
library("patchwork")

devtools::load_all("../dataduck") # The crosstab_count() function used here is defined in dataduck

# ----- Step 1: Define functions ----- #
# Plotting function for one histogram
plot_hhsize_histogram <- function(data = fig02_data,
                                  main_color = "steelblue",
                                  title = NULL,
                                  xtitle = TRUE,
                                  ytitle = TRUE,
                                  ymax = NULL) {
  # Input validation: Only one race group in data
  race_group <- unique(data$RACE_ETH_bucket)
  if (length(race_group) != 1) {
    stop("Data must be filtered to a single RACE_ETH_bucket.")
  }
  
  # Input validation: Frequencies sum to one
  freq_sum <- sum(data$freq)
  if (abs(freq_sum - 1) > 1e-6) {
    stop(glue("Frequencies must sum to one within 1e-6 tolerance. Frequencies sum to {freq_sum}"))
  }
  
  # Input validation: ymax >= largest freq value
  largest_freq <- max(data$freq)
  if (!is.null(ymax) && ymax < largest_freq) {
    stop(glue("Largest frequency provided of {largest_freq} exceeds `ymax` specified of {ymax}. 
              Please increase `ymax` setting."))
  } 
  
  # Build base plot
  p <- ggplot(data, aes(x = factor(NUMPREC), y = freq)) +
    geom_bar(
      stat = "identity",
      width = 1,
      fill = scales::alpha(main_color, 0.5),
      color = main_color,
      size = 0.3
    ) +
    theme_minimal()
  
  # Add y-axis limit if specified
  if (!is.null(ymax)) {
    p <- p + ylim(0, ymax)
  }
  
  # Title
  if (!is.null(title)) {
    p <- p + labs(title = title)
  } else {
    p <- p
  }
  
  # Axis labels
  p <- p + labs(
    x = if (xtitle) "Number of Persons in Household" else NULL,
    y = if (ytitle) "Frequency" else NULL
  )
  
  return(p)
}


# ----- Step 2: Import and wrangle data ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# 2019 estimate of non-group-quarters population in household of X size and Y race/ethnicity
hhsize_race_2019 <- crosstab_count(
  data = ipums_db |> filter(YEAR == 2019 & GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("NUMPREC", "RACE_ETH_bucket"),
  every_combo = TRUE
) |> collect() |> arrange(RACE_ETH_bucket, NUMPREC)

# ----- Step 3: Make plots ----- #
# Choose a max NUMPREC (household size) to display in histogram
topcode_hhsize <- 8

# Topcode the table
fig02_data <- hhsize_race_2019 |>
  mutate(NUMPREC = if_else(NUMPREC >= topcode_hhsize, topcode_hhsize, NUMPREC)) |>
  group_by(RACE_ETH_bucket, NUMPREC) |>
  summarize(
    weighted_count = sum(weighted_count),
    count = sum(count),
    .groups = "drop"
  ) |>
  arrange(RACE_ETH_bucket, NUMPREC) |>
  # Add frequencies for each NUMPREC value within RACE_ETH_bucket subpopulations
  group_by(RACE_ETH_bucket) |>
  mutate(freq = weighted_count / sum(weighted_count)) |>
  ungroup()

# Generate the plots
main_color <- "steelblue"
ymax <- 0.4

black <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "Black"),
  main_color = main_color,
  title = "Black",
  xtitle = FALSE,
  ytitle = TRUE,
  ymax = ymax
)

hispanic <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "Hispanic"),
  main_color = main_color,
  title = "Hispanic",
  xtitle = TRUE,
  ytitle = FALSE,
  ymax = ymax
)

white <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "White"),
  main_color = main_color,
  title = "White",
  xtitle = FALSE,
  ytitle = FALSE,
  ymax = ymax
)

# Combine
fig02 <- black + hispanic + white

# ----- Step 4: Save plots ----- #
ggsave(
  "output/figures/fig02-hhsize-race-hist.png",
  plot = fig02,
  width = 3000, height = 2400, units = "px", dpi = 300
)
