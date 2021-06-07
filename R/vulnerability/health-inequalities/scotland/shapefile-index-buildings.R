# ---- Load ----
library(tidyverse)
library(geographr)
library(httr)
library(sf)

# Load Scottish Council Area boundaries
lad <-
  boundaries_lad %>%
  filter(str_detect(lad_code, "^S"))

# Load Scottish Health Index
# For testing, make up health index scores
hi <-
  read_csv(
    "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/scotland/index-unweighted.csv"
  )

# Load shapefile of buildings in Scotland
# Source: https://data.humdata.org/dataset/hotosm_gbr_scotland_buildings/resource/aecc7790-f8ef-4468-b4ac-fd34fec281ca
GET(
  "https://export.hotosm.org/downloads/d836366c-7d9b-453c-9e02-8f1e5114ffc6/hotosm_gbr_scotland_buildings_polygons_shp.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = (td <- tempdir()))

scot_buildings <-
  read_sf(
    file.path(
      td,
      "hotosm_gbr_scotland_buildings_polygons.shp"
    )
  )

# Assign health index scores/deciles to each building based on the Council Area it's in
scot_buildings <-
  scot_buildings %>%
  st_join(hi)

# Save
write_sf(
  scot_buildings,
  "data/vulnerability/health-inequalities/scotland/scotland_buildings_with_health_index.shp"
)