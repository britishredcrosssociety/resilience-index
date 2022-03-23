library(tidyverse)
library(geographr)
library(osmdata)
library(reticulate)
library(sf)

# Bounding box for Scotland
scotland_bb <- getbb("Scotland")

# Search for pharmacies
pharmacies <- opq(scotland_bb, timeout = 1000) %>%
  add_osm_feature("amenity", "pharmacy") %>%
  osmdata_sf()

# Get Local Authorities in Great Britain for the next step
gb_lad <- geographr::boundaries_lad %>%
  filter(!str_detect(lad_code, "^N"))

# Some pharmacies are in Northern Ireland - remove them from the dataset
sco_pharmacies <- pharmacies$osm_points[gb_lad, ]

# - Test plot to check pharmacy locations -
# gb_lad %>%
#   ggplot() +
#   geom_sf(
#     fill = NA,
#     colour = "black"
#   ) +
#   geom_sf(
#     data = sco_pharmacies,
#     colour = "red",
#     inherit.aes = FALSE
#   )

# Save pharmacies for the Python script
write_sf(sco_pharmacies, "data/vulnerability/health-inequalities/scotland/healthy-places/points.geojson")

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
# Load the Data Zone travel distances to pharmacies
pharma_dist <- read_csv("data/vulnerability/health-inequalities/scotland/healthy-places/points-lsoa.csv")

pharma_dist_la <-
  pharma_dist %>%
  select(dz_code = lsoa11cd, mean_distance_nearest_three_points) %>%
  # rename(dz_code = lsoa11cd) %>%
  mutate(mean_distance_nearest_three_points = mean_distance_nearest_three_points / 1000) %>%
  # convert to km

  # Merge Council Area codes
  left_join(geographr::lookup_dz_iz_lad, by = "dz_code") %>%
  # Take mean distance to a pharmacy for LSOAs within a Local Authority
  group_by(lad_code) %>%
  summarise(pharmacy_distance = mean(mean_distance_nearest_three_points))

write_rds(pharma_dist_la, "data/vulnerability/health-inequalities/scotland/healthy-places/distance-to-pharmacies.rds")

# Don't need these files anymore
file.remove("data/vulnerability/health-inequalities/scotland/healthy-places/points.geojson")
file.remove("data/vulnerability/health-inequalities/scotland/healthy-places/points-lsoa.csv")