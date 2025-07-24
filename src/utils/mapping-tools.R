# The purpose of this script is to create a convenient state shapefile throughput
# file that is ready to merge to results and map.
# This script only needs to be run once. Once xxx is saved, it needn't be re-run.
#
# Input:
# - "data/s_05mr24/s_05mr24.shp": Data is unzipped from https://www.weather.gov/gis/USStates
#   This is a NOAA shapefile: the Census shapefiles I downloaded all had strange 
#   shapes because they included water bodies.
#
# Output:
# - throughput/state_shapefiles.rds
# 

# Creates a 2x2 matrix representing the rotation transformation relative to angle
# a
rot <- function(a) {
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}

# Moves a state according to custom specifications
transform_state <- function(
    df, 
    state_fp, 
    rotation_angle, 
    scale_factor, 
    shift_coords
) {
  state <- df %>% filter(STATEFIP == state_fp)
  state_geom <- st_geometry(state)
  state_centroid <- st_centroid(st_union(state_geom))
  rotated_geom <- (state_geom - state_centroid) * rot(rotation_angle * pi / 180) / scale_factor + state_centroid + shift_coords
  state %>% st_set_geometry(rotated_geom) %>% st_set_crs(st_crs(df))
}

# Load shapefiles
state_sf <- st_read("data/s_05mr24/s_05mr24.shp") |>
  rename(
    STATEFIP = FIPS,
    State = NAME,
    state_abbrev = STATE
  ) |> # For consistency with household size data
  filter(!STATEFIP %in% c('60', '64', '66', '68', '69', '70', '72', '78')) |># Remove excluded states, like Puerto Rico
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") |>
  mutate(geometry = st_simplify(geometry, dTolerance =  5000))  # Simplify shapes


# Rotate and move Alaska and Hawaii to fit on map
alaska <- transform_state(state_sf, "02", -39, 2.3, c(1000000, -5000000))
hawaii <- transform_state(state_sf, "15", -35, 1, c(5200000, -1400000))

# Final map after transforming non-contiguous states
state_sf_final <- state_sf %>%
  filter(!STATEFIP %in% c("02", "15")) |>
  bind_rows(alaska, hawaii)

saveRDS(state_sf_final, "throughput/state_shapefiles.rds")