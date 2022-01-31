library(tidyverse)
library(sf)
library(geographr)
library(readxl)

source("R/utils.R")

lookup <-
  lookup_sa_soa_lgd |>
  select(
    sa_code,
    lad_code = lgd_code
  )

pop_lad <-
  population_lad |>
  select(lad_code, total_population) |>
  filter(str_detect(lad_code, "^N"))

# ---- Load population data for Northern Ireland's Small Areas ----
# Source: https://www.nisra.gov.uk/publications/2018-mid-year-population-estimates-small-areas
tf <-
  download_file(
    "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/SAPE18_SA_Totals.xlsx",
    ".xlsx"
  )

pop_raw <-
  read_excel(
    tf,
    sheet = "Flat"
  )

pop_sa <-
  pop_raw |>
  filter(Year == max(pop_raw$Year)) |>
  select(sa_code = Area_Code, pop_sa = MYE)

# ---- Load Northern Ireland flood risk map area ----
# NI flood map file was created in QGIS and can't be downloaded - so this code must be run
#
# Steps to reproduce this file (with huge thanks to Nick McWilliam from MapAction (@nickmcw)):
# The data are provided as ArcGIS MapServer layers, which means that features are generally not available for local processing, styling etc. If they were FeatureServer layers, we'd be fine. The method shown here is more of a work-around than a good solution. It might be possible to access the data directly (and dynamically) using these tools https://github.com/Bolton-and-Menk-GIS/restapi - but I didn't want to go down this route just now! So, in QGIS......
#
# 1. Connect to service
# Browser panel > ArcGISMapServer > New Connection > enter a name and this URL: https://mapping.dardni.gov.uk/arcgisra/rest/services/FloodMapsNI/SFRA2/MapServer
# In this case, two layers are available, APSFR and TAPSFR
#
# 2. Add layer
# Add the relevant layer to you map. You won't have any control over the styling. Zoom to the extent you need (in this case, full extent) and check that no other layers are visible
#
# 3. Export as georeferenced raster
# Project > Export > Export Map to Image >
# Increase resolution to 300dpi
# Un-check annotations and decorations
# Ensure that world file is checked
# Save > Save as > PNG (don't use a 'lossy' format)
#
# 4. Add georeferenced raster
# Add the new layer to your map. Choose 'Irish Grid' if asked. Check it overlays with the MapServer layer.
# Because of the symbology from the server (with a different coloured outline), the styling needs to be changed:
#   Layer Styling > Singleband Psuedocolour
# Try bands 1/2/3 to find the closest to what you want. In the case of APSFR, I used band 3.
# NOTE: The symbology of TAPSFR is different, requiring (irritatingly) a re-classification before raster-vector conversion.
#
# 5. Raster to vector conversion
# Raster > Conversion > Polygonise
# Choose the band from the previous step, and save
#
# 6. Remove extra polygons
# In edit mode, remove any enclosing polygons and any 'islands' that shouldn't be filled
# It may be necessary to perform a dissolve, if there are edge artefacts

floods_raw <-
  read_sf("data/on-disk/ni-floods/NI small areas in flood risk zones.shp")

flood_areas <-
  floods_raw |>
  st_drop_geometry() |>
  select(sa_code = SA2011)

# ---- Calcualte flood risks ----
# The flood_areas data set provides a list of SA2011 areas that are at risk
# of flooding. Calculate the number of people that live in these areas as a
# proportion of the higher coterminous Local Authority area population.
flood_areas_pop <-
  flood_areas |>
  left_join(pop_sa) |>
  rename(pop_at_risk = pop_sa)

pop_at_risk <-
  lookup |>
  left_join(flood_areas_pop) |>
  replace_na(list(pop_at_risk = 0)) |>
  group_by(lad_code) |>
  summarise(pop_at_risk = sum(pop_at_risk))

proportion_at_risk <-
  pop_at_risk |>
  left_join(pop_lad) |>
  mutate(flood_risk_proportion = pop_at_risk / total_population) |>
  select(lad_code, flood_risk_proportion)

proportion_at_risk |>
  write_rds("data/shocks/disasters-emergencies/northern-ireland/floods.rds")