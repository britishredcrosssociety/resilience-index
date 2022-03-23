library(tidyverse)
library(geographr)
library(osmdata)
library(reticulate)
library(sf)

# Bounding box for Scotland
scotland_bb <-
  getbb("Scotland")

# Search for sport centre
sports_centre <-
  opq(scotland_bb, timeout = 1000) %>%
  add_osm_feature("leisure", "sports_centre") %>%
  osmdata_sf()

# Get Local Authorities in Great Britain for the next step
gb_lad <-
  boundaries_lad %>%
  filter(!str_detect(lad_code, "^N"))

# Some sport centre are in Northern Ireland - remove them from the dataset
sco_sports <-
  sports_centre$osm_points[gb_lad, ] %>%
  select(osm_id)

# - Test plot to check sports centre locations -
# gb_lad %>%
#   ggplot() +
#   geom_sf(
#     fill = NA,
#     colour = "black"
#   ) +
#   geom_sf(
#     data = sco_sports,
#     inherit.aes = FALSE
#   )

# Save sport centre for the Python script
write_sf(sco_sports, "data/vulnerability/health-inequalities/scotland/healthy-places/points.geojson")

# ---- Source Python code ----
# Set virtual environment using Conda:
#   - Use the conda-env.txt file to recreate the virtual environment locally
#   - The argument 'required = FALSE' has been set to only suggest to use a
#     Conda virtual env in the first instance. If you choose to not use Conda,
#     the call to source_python() below should still run if you have the correct
#     dependencies installed. See: https://rstudio.github.io/reticulate/reference/use_python.html
use_condaenv(condaenv = "resilience-index", required = FALSE)

# Source python script that computes distance to points
source_python("R/vulnerability/health-inequalities/scotland/healthy-places/distance-to-points.py")

# ---- Resume R Execution ----
# Load the Data Zone travel distances to sport centre
sports_dist <- read_csv("data/vulnerability/health-inequalities/scotland/healthy-places/points-lsoa.csv")

sports_dist <-
  sports_dist %>%
  select(dz_code = lsoa11cd, mean_distance_nearest_three_points) %>%
  mutate(mean_distance_nearest_three_points = mean_distance_nearest_three_points / 1000) %>%
  # convert to km

  # Merge Council Area codes
  left_join(geographr::lookup_dz_iz_lad, by = "dz_code") %>%
  # Take mean distance to a pharmacy for LSOAs within a Local Authority
  group_by(lad_code) %>%
  summarise(sports_centre_distance = mean(mean_distance_nearest_three_points))

write_rds(sports_dist, "data/vulnerability/health-inequalities/scotland/healthy-places/distance-to-sports-centres.rds")

# Don't need these files anymore
file.remove("data/vulnerability/health-inequalities/scotland/healthy-places/points.geojson")
file.remove("data/vulnerability/health-inequalities/scotland/healthy-places/points-lsoa.csv")