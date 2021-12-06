library(tidyverse)
library(sf)
library(geographr)
library(readxl)

source("R/utils.R")

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

pop <-
  pop_raw |> 
  filter(Year == max(pop_ni$Year)) |> 
  select(SA2011 = Area_Code, `All Ages` = MYE)

# ---- Load Northern Ireland flood risk map ----
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

floods_transform_crs <-
  st_transform(floods_raw, crs = 4326)

# ---- Keep only OAs that intersect with the flood risk areas ----
areas_at_risk <- 
  floods_transform_crs |> 
  left_join(pop, by = "SA2011") |> 
  rename(OA11CD = SA2011, LSOA11CD = SOA2011) |> 
  st_drop_geometry()