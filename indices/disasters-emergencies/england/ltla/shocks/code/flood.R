# Load packages ----
library(sf)
library(geographr)
library(tidyverse)

source("functions/utils.R")

# Output area boundaries ----
# Output areas (OA) were created for Census data
# Source: https://geoportal.statistics.gov.uk/datasets/a76b2f87057b43d989d8f01733104d62/explore?location=52.950000%2C-2.000000%2C6.87
boundaries_oa <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Output_Areas_December_2011_Boundaries_EW_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

boundaries_oa_eng <- boundaries_oa |>
  filter(str_detect(LAD11CD, "^E")) |>
  rename_with(str_to_lower) |>
  select(objectid, oa11cd, lad11cd, geometry)

# Load England flood maps ----
# Based on code: https://github.com/britishredcrosssociety/resilience-index/blob/main/depreciated/prep%20shocks%20-%20flood%20risks.R
# But used intersecting polygons of flood zone & OA (rather than population weighted centroids)

# Source: https://environment.data.gov.uk/DefraDataDownload/?mapService=EA/FloodMapForPlanningRiversAndSeaFloodZone3&Mode=spatial
# ** You might need to generate a new download URL download .shp file from ^ *
tf <- download_file(
  "https://environment.data.gov.uk/UserDownloads/interactive/f26cae79fd624e59857bacd695c290cd127720/EA_FloodMapForPlanningRiversAndSeaFloodZone3_SHP_Full.zip",
  "zip"
)

tf |>
  unzip(exdir = tempdir())

flood <- read_sf(paste0(tempdir(), "/data/Flood_Map_for_Planning_Rivers_and_Sea_Flood_Zone_3.shp"))

unique(flood$layer)
unique(flood$type)
# There are tidal and fluvial and combined.

# Check coordinate reference systems
st_crs(flood)
st_crs(boundaries_oa)

# Boundaries data
# Transform the OA data from spherical/geographic system (units of degrees of lat/long)
# to projected/planar system (units of meters)
# Flood data is in projected system for UK: https://epsg.io/27700
# More info https://r-spatial.org/r/2020/06/17/s2.html & https://rspatial.org/raster/spatial/6-crs.html

# Transform flood data so same CRS as output area boundaries
oa_transform <- boundaries_oa_eng |>
  st_transform(crs = st_crs(flood))

flood_select <- flood |>
  mutate(flood_zone_id = row_number()) |>
  select(flood_zone_id, layer, type, geometry)

interserctions <- flood_select |>
  st_join(oa_transform, left = TRUE) |>
  relocate(geometry, .after = everything())

flood_risk_oa <- interserctions |>
  st_drop_geometry() |>
  distinct(oa11cd) |>
  mutate(flood_risk = 1)


# OA to LTLA lookup ----
# Output Area to Lower Layer Super Output Area to Middle Layer Super Output Area to Local Authority District (December 2011) Lookup in England and Wales
# Source: https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales
oa_lookup <- read_sf("https://opendata.arcgis.com/datasets/65664b00231444edb3f6f83c9d40591f_0.geojson")

# Source: https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2020-lookup-in-england-and-wales/explore# Source: https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2020-lookup-in-england-and-wales/explore
oa_lookup_select <- oa_lookup |>
  st_drop_geometry() |>
  rename_with(str_to_lower) |>
  select(oa11cd, msoa11cd, lad20cd)

northamptonshire_restructure_2021 <-
  tribble(
    ~pre_lad_name, ~post_ua_name, ~pre_lad_code, ~post_ua_code,
    "Corby", "North Northamptonshire", "E07000150", "E06000061",
    "Daventry", "West Northamptonshire", "E07000151", "E06000062",
    "East Northamptonshire", "North Northamptonshire", "E07000152", "E06000061",
    "Kettering", "North Northamptonshire", "E07000153", "E06000061",
    "Northampton", "West Northamptonshire", "E07000154", "E06000062",
    "South Northamptonshire", "West Northamptonshire", "E07000155", "E06000062",
    "Wellingborough", "North Northamptonshire", "E07000156", "E06000061",
  )

oa_lookup_select_2021 <- oa_lookup_select |>
  left_join(northamptonshire_restructure_2021, by = c("lad20cd" = "pre_lad_code")) |>
  mutate(lad_code = ifelse(!is.na(post_ua_code), post_ua_code, lad20cd)) |>
  select(oa11cd, msoa11cd, lad_code)

# OA population ----
oa_pop <- population_oa |>
  select(oa_code, total_population)

# Join all datasets ----
flood_oa_joined <- boundaries_oa_eng |>
  st_drop_geometry() |>
  left_join(flood_risk_oa, by = "oa11cd") |>
  left_join(oa_pop, by = c("oa11cd" = "oa_code")) |>
  left_join(oa_lookup_select_2021, by = "oa11cd") |>
  mutate(flood_risk = ifelse(is.na(flood_risk), 0, flood_risk))

flood_risk_ltla <- flood_oa_joined |>
  mutate(flood_risk_pop = ifelse(flood_risk == 1, total_population, 0)) |>
  group_by(lad_code) |>
  summarise(prop_pop_flood_risk = sum(flood_risk_pop) / sum(total_population))

# Consider if want to do prop of population at flood risk at LSOA/MSOA level and then use calculate_extent()
# Would this better reflect pockets of highest risk within an LA?

# Save ----
flood_risk_ltla |>
  write_rds("indices/disasters-emergencies/england/ltla/shocks/data/flood.rds")