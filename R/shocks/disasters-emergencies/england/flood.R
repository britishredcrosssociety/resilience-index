
# TO DO:
# Work on pulling GEOJSON & convert to shapefile directly from site
# More efficient intesection check
# look into crs transformations

library(tidyverse)
library(geographr)
library(sf)

source("R/utils.R")

# Output Areas boundaries -----
# Download data from: https://geoportal.statistics.gov.uk/datasets/a76b2f87057b43d989d8f01733104d62/explore?location=52.950000%2C-2.000000%2C6.87
bondaries_oa <- st_read("data/on-disk/flooding-data/Flood_Map_for_Planning_Rivers_and_Sea_Flood_Zone_3.shp")

# Check crs
st_crs(bondaries_oa)

bondaries_oa_trans <- bondaries_oa |>
  st_transform(crs = 4326)

# ---- Load/process England and Wales flood maps directly from sources ----
# England: Flood Map for Planning (Rivers and Sea) - Flood Zone 3
# Source: https://environment.data.gov.uk/DefraDataDownload/?mapService=EA/FloodMapForPlanningRiversAndSeaFloodZone3&Mode=spatial
# ** You might need to generate a new download URL from ^ *
tf <- download_file("https://environment.data.gov.uk/UserDownloads/interactive/fddd046ad3f848a9971a8d757fafa59856676/EA_FloodMapForPlanningRiversAndSeaFloodZone3_SHP_Full.zip   ",
                    ".zip")

tf |>
  unzip(exdir = tempdir())

list.files(tempdir())

floods_eng <- read_sf(paste0(tempdir(), "/data/Flood_Map_for_Planning_Rivers_and_Sea_Flood_Zone_3.shp"))

# Check crs
st_crs(floods_eng)

# Transform CRS
floods_eng_trans <- floods_eng |>
  st_transform(crs = 4326)

floods_eng |>
  distinct(layer, type)


# Check intersection of points/boundaries with flooding risk areas
sf_use_s2(FALSE) #to look into this further https://r-spatial.github.io/sf/reference/s2.html

flood_risk_oas <- bondaries_oa_trans |>
  st_join(floods_eng_trans)

population_oa |>
  left_join(flood_risk_oas) |>
  left_join(lookup_oa_msoa) |
  left_join(lookup_msoa_lad) |>
  mutate(pop_flood_risk = ifelse(is.na(...), population, 0)) |>
  group_by(lad_code) |>
  summarise(prop_pop_risk_flood = sum(pop_flood_risk) / sum(population))



# Come back to below code to call API to get data as GEOJSON as no url link to download shape file 

# library(geojsonio)
# spdf <- geojson_read("https://opendata.arcgis.com/datasets/a76b2f87057b43d989d8f01733104d62_0.geojson", what = "sp" , parse = TRUE)
# spdf2 <- rgdal::readOGR("https://opendata.arcgis.com/datasets/a76b2f87057b43d989d8f01733104d62_0.geojson")
# spdf3 <- rgdal::readOGR("https://opendata.arcgis.com/datasets/a76b2f87057b43d989d8f01733104d62_0.geojson", layer = "Output_Areas__December_2011__Boundaries_EW_BGC")
# 
# head(spdf)
# spdf$data
# 
# response <- httr::GET("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Output_Areas_December_2011_Boundaries_EW_BGC/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
# content <- httr::content(response, type = "application/json", simplifyVector = TRUE)
# 
# 
# cols <- content$features$attributes
# geog <- content$features$geometry