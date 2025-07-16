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
library("grid")

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

# Plotting function for two overlaid histograms
plot_hhsize_histogram_double <- function(
    data,
    per1 = 2000,
    per2 = 2019,
    bar_fills = list(
      per1 = list(color = "skyblue", alpha = 0.4, line_color = "skyblue", line_type = "dashed"),
      per2 = list(color = "forestgreen", alpha = 0.2, line_color = "forestgreen", line_type = "solid")
    ),
    title = NULL,
    xtitle = TRUE,
    ytitle = TRUE,
    ymax = NULL) {
  # Input validation
  race_group <- unique(data$RACE_ETH_bucket)
  if (length(race_group) != 1) {
    stop("Data must be filtered to a single RACE_ETH_bucket.")
  }
  
  years_present <- sort(unique(data$year))
  if (!all(c(per1, per2) %in% years_present)) {
    stop(glue::glue("Data must include both per1 ({per1}) and per2 ({per2}) years. Found: {paste(years_present, collapse=', ')}"))
  }
  
  # If ymax not specified, use max observed value
  max_freq <- max(data$freq)
  if (is.null(ymax)) {
    ymax <- max_freq * 1.05
  }
  
  p <- ggplot(data, aes(x = factor(NUMPREC))) +
    geom_bar(
      data = data |> dplyr::filter(year == per1),
      aes(y = freq),
      stat = "identity",
      fill = scales::alpha(bar_fills$per1$color, bar_fills$per1$alpha),
      color = bar_fills$per1$line_color,
      linetype = bar_fills$per1$line_type,
      size = 0.3,
      width = 0.90
    ) +
    geom_bar(
      data = data |> dplyr::filter(year == per2),
      aes(y = freq),
      stat = "identity",
      fill = scales::alpha(bar_fills$per2$color, bar_fills$per2$alpha),
      color = bar_fills$per2$line_color,
      linetype = bar_fills$per2$line_type,
      size = 0.3,
      width = 0.6
    ) +
    coord_cartesian(ylim = c(0, ymax)) +
    theme_minimal() +
    labs(
      title = title,
      x = if (xtitle) "Number of Persons in Household" else NULL,
      y = if (ytitle) "Frequency" else NULL
    ) +
    theme(
      plot.title = element_text(hjust = 0.5)
    )
  
  return(p)
}

# ----- Step 2: Import and wrangle data ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Estimate of non-group-quarters population in household of X size and Y race/ethnicity
hhsize_race_2019 <- crosstab_count(
  data = ipums_db |> filter(YEAR == 2019 & GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("NUMPREC", "RACE_ETH_bucket"),
  every_combo = TRUE
) |> collect() |> mutate(year = 2019)

hhsize_race_2000 <- crosstab_count(
  data = ipums_db |> filter(YEAR == 2000 & GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("NUMPREC", "RACE_ETH_bucket"),
  every_combo = TRUE
) |> collect() |> mutate(year = 2000)

hhsize_race_year <- bind_rows(hhsize_race_2000, hhsize_race_2019)

# ----- Step 3: Make plots ----- #
# Choose a max NUMPREC (household size) to display in histogram
topcode_hhsize <- 8

# Topcode the table
fig02_data <- hhsize_race_year |>
  mutate(NUMPREC = if_else(NUMPREC >= topcode_hhsize, topcode_hhsize, NUMPREC)) |>
  group_by(RACE_ETH_bucket, NUMPREC, year) |>
  summarize(
    weighted_count = sum(weighted_count),
    count = sum(count),
    .groups = "drop"
  ) |>
  # Add frequencies for each NUMPREC value within RACE_ETH_bucket subpopulations
  group_by(RACE_ETH_bucket, year) |>
  mutate(freq = weighted_count / sum(weighted_count)) |>
  ungroup() |> 
  arrange(year, RACE_ETH_bucket, NUMPREC)

# Generate the plots
color_2019 <- "forestgreen"
alpha_2019 <- 0.4
line_type_2019 <- "solid" 
color_2000 <- "skyblue"
alpha_2000 <- 0.4
line_type_2000 <- "dashed"

ymax <- 0.4

# ... Figure 2 ... (default format)
black <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "Black"),
  title = "Black",
  ymax = ymax,
  bar_fills = list(
    per1 = list(color = color_2000, alpha = alpha_2000, line_color = color_2000, line_type = line_type_2000),
    per2 = list(color = color_2019, alpha = alpha_2019, line_color = color_2019, line_type = line_type_2019)
  ),
)

hispanic <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "Hispanic"),
  title = "Hispanic",
  ymax = ymax,
  bar_fills = list(
    per1 = list(color = color_2000, alpha = alpha_2000, line_color = color_2000, line_type = line_type_2000),
    per2 = list(color = color_2019, alpha = alpha_2019, line_color = color_2019, line_type = line_type_2019)
  ),
)

white <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "White"),
  title = "White",
  ymax = ymax,
  bar_fills = list(
    per1 = list(color = color_2000, alpha = alpha_2000, line_color = color_2000, line_type = line_type_2000),
    per2 = list(color = color_2019, alpha = alpha_2019, line_color = color_2019, line_type = line_type_2019)
  ),
)

# ... Figure 2B ... (alternate format)
# 2019
black_2019 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "Black" & year == 2019),
  main_color = color_2019,
  title = "Black",
  xtitle = FALSE,
  ytitle = TRUE,
  ymax = ymax
)

hispanic_2019 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "Hispanic" & year == 2019),
  main_color = color_2019,
  title = "Hispanic",
  xtitle = FALSE,
  ytitle = FALSE,
  ymax = ymax
)

white_2019 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "White" & year == 2019),
  main_color = color_2019,
  title = "White",
  xtitle = FALSE,
  ytitle = FALSE,
  ymax = ymax
)

# 2000
black_2000 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "Black" & year == 2000),
  main_color = color_2000,
  title = "Black",
  xtitle = FALSE,
  ytitle = TRUE,
  ymax = ymax
)

hispanic_2000 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "Hispanic" & year == 2000),
  main_color = color_2000,
  title = "Hispanic",
  xtitle = TRUE,
  ytitle = FALSE,
  ymax = ymax
)

white_2000 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "White" & year == 2000),
  main_color = color_2000,
  title = "White",
  xtitle = FALSE,
  ytitle = FALSE,
  ymax = ymax
)

# Combine
# Function to create a styled year label
make_year_label <- function(text) {
  ggplot() +
    annotate("text", x = 1, y = 1, label = text, hjust = 1, size = 4.5, fontface = "bold") +
    theme_void() +
    theme(
      plot.margin = margin(5, 10, 5, 10),  # spacing around text
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3)
    ) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
}

# Create year labels
title_2019 <- make_year_label("2019")
title_2000 <- make_year_label("2000")

# Combine all plots
fig02b <- (
  title_2019 / (black_2019 + hispanic_2019 + white_2019) /
    title_2000 / (black_2000 + hispanic_2000 + white_2000)
) + plot_layout(heights = c(0.11, 1, 0.11, 1))


# ----- Step 4: Save plots ----- #
ggsave(
  "output/figures/fig02b-hhsize-race-hist.png",
  plot = fig02b,
  width = 3000, height = 2400, units = "px", dpi = 300
)





# ----------------------




