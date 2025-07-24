# This script contains archived code that may be useful for later analysis
##############################################
#vvvvvvvvvvvvv ARCHIVE vvvvvvvvvvvvvvvvvvvvvv#
##############################################

### FIG 4a: Unexplained diff by cpuma, nationally
# Load shapefiles. Data is unzipped from WHERE? TODO: document
cpuma_sf <- st_read("data/ipums-cpuma0010-sf/ipums_cpuma0010.shp") |>
  filter(!STATEFIP %in% c('60', '64', '66', '68', '69', '70', '72', '78')) |># Remove excluded states, like Puerto Rico
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") |>
  mutate(geometry = st_simplify(geometry, dTolerance =  5000))  # Simplify shapes

# Rotate and move Alaska and Hawaii to fit on map
alaska_cpuma <- transform_state(cpuma_sf, "02", -39, 2.3, c(1000000, -5000000))
hawaii_cpuma <- transform_state(cpuma_sf, "15", -35, 1, c(5200000, -1400000))

# Final map after transforming non-contiguous states
cpuma_sf_final <- cpuma_sf |>
  filter(!STATEFIP %in% c("02", "15")) |>
  bind_rows(alaska_cpuma, hawaii_cpuma)

# Join the state data with household size differences
cpuma_sf_hhsize <- cpuma_sf_final |>
  left_join(hhsize_contributions_state, by = "CPUMA0010") |>
  mutate(hhsize_unexplained = contribution_diff / prop_2019) 

# Choropleth map (color version)
fig04a <- ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = NA, size = 0) +
  geom_sf(data = state_sf_hhsize, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.8, to = 0.6, by = 0.2)
  ) +
  theme_void()
fig04a


# A function that produces a dotplot by state
dotplot_by_state <- function(
    state = "New Jersey",
    data = hhsize_contributions_state, # or bedroom_contributions_state
    x_min = -0.5, # Lowest x-value on dotplot
    x_max = 0.5 # Highest x-value on dotplot
) {
  # Subset the data to just that state
  boxplot_data <- subset(data, State == state)
  
  # Calculate median, weighted median, and weighted mean
  median <- boxplot_data |>
    pull(diff) |> 
    median()
  weighted_median <- rep(boxplot_data$diff, times = boxplot_data$pop_2019) |>
    median()
  weighted_mean <- weighted.mean(boxplot_data$diff, w = boxplot_data$pop_2019)
  
  # Create the horizontal boxplot with overlaid points
  output_plot <- ggplot(boxplot_data, aes(x = diff, y = "")) +
    geom_dotplot(stackdir = "center", dotsize = 0.5, alpha = 0.6, binwidth = 0.02) +
    theme_minimal() +
    labs(title = "",
         x = "",
         y = "") +
    theme_void() +
    geom_vline(xintercept = weighted_mean, linetype = "dotted", color = "red", size = 0.5) +
    geom_vline(xintercept = weighted_median, linetype = "dotted", color = "blue", size = 0.5) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) +
    xlim(x_min, x_max)
  
  
  return(output_plot)
}

# Function to generate base64-encoded ggplot images
dotplot_base64 <- function(
    state, 
    data, 
    x_min = -0.5, # Lowest x-value on dotplot
    x_max = 0.5 # Highest x-value on dotplot
) {
  file_path <- tempfile(fileext = ".png")
  plot <- dotplot_by_state(state, data, x_min, x_max)
  # Save plot as PNG to a temporary file
  ggsave(file_path, plot = plot, width = 5, height = 1, dpi = 100, units = "in")
  # Convert to base64
  base64_img <- base64encode(file_path)
  # Create an HTML img tag with the base64 string
  img_tag <- sprintf('<img src="data:image/png;base64,%s" width="400px" height="80px"/>', base64_img)
  
  return(img_tag)
}

# Add to tables 3.3 and 3.4: "Plot" column which includes base 64 encoded images of dotplots.
# Table 3.3 (Persons per household)
# The warning messages of removal of rows are expected: since our x range is -0.5 to
# 0.5, we exclude two observations falling outside that range. Small sacrifice to make 
# the data easier to view.
# TODO: Add warning showing number of excluded observations based on inputted x_min
# and x_max
# TODO: attach metadata about these x-axis limits to plots themselves and automatically
# read/display in the server rendering of figures
hhsize_state_summary$plot <- sapply(hhsize_state_summary$State, function(state) {
  dotplot_base64(
    state = state, 
    data = hhsize_contributions_state,
    x_min = -0.5,
    x_max = 0.5 
  )
})
# Table 3.4 (Persons per bedroom)
bedroom_state_summary$plot <- sapply(bedroom_state_summary$State, function(state) {
  dotplot_base64(
    state = state, 
    data = bedroom_contributions_state,
    x_min = -0.9,
    x_max = 0.1
  )
})

# ----- Step 4: Merge data for scatterplots ----- #
# Figures 3.1 and 3.2

# Figure 3.1: Scatter household density change with bedroom density change

# Figure 3.2: Scatter household density change with population change
fig3.2_tab <- hhsize_state_summary |> select(-plot) |>
  inner_join(state_pop_growth, by = "State")

# ----- Step 5: Save the results ----- #

# Diff data
save(
  hhsize_contributions_state,
  hhsize_state_summary,
  bedroom_contributions_state,
  bedroom_state_summary,
  list_of_states,
  fig_3.2_tab,
  file = "shiny-app/data/diffs-by-geography.rda"
)
