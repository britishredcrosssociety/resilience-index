##
## Flood risks - how many people are at high risk (>1% per year) of flooding?
##
## Note: we currently don't have access to Scottish flood data
##
library(tidyverse)
library(readxl)
library(readODS)
library(httr)
library(sf)
library(rmapshaper)

# ---- Download Output Area population estimates for England/Wales ----
# Currently using mid-2019 estimates
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datalist?sortBy=release_date&query=output%20area&filter=datasets&fromDateDay=&fromDateMonth=&fromDateYear=&toDateDay=&toDateMonth=&toDateYear=&size=10

# ONS publishes output area population estimates in separate files for each region
pop_urls <- c(
  # East Midlands
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesintheeastmidlandsregionofengland%2fmid2019sape22dt10f/sape22dt10fmid2019eastmidlands.zip",
  
  # North West
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthenorthwestregionofengland%2fmid2019sape22dt10b/sape22dt10bmid2019northwest.zip",
  
  # South East
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthesoutheastregionofengland%2fmid2019sape22dt10i/sape22dt10imid2019southeast.zip",
  
  # East
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesintheeastregionofengland%2fmid2019sape22dt10h/sape22dt10hmid2019east.zip",
  
  # West Midlands
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthewestmidlandsregionofengland%2fmid2019sape22dt10e/sape22dt10emid2019westmidlands.zip",
  
  # Wales
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinwales%2fmid2019sape22dt10j/sape22dt10jmid2019wales.zip",
  
  # North East
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthenortheastregionofengland%2fmid2019sape22dt10d/sape22dt10dmid2019northeast.zip",
  
  # South West
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthesouthwestregionofengland%2fmid2019sape22dt10g/sape22dt10gmid2019southwest.zip",
  
  # Yorkshire and The Humber
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesintheyorkshireandthehumberregionofengland%2fmid2019sape22dt10c/sape22dt10cmid2019yorkshireandthehumber.zip",
  
  # London
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthelondonregionofengland%2fmid2019sape22dt10a/sape22dt10amid2019london.zip"
)

# download and upzip population estimates for each region
for (url in pop_urls) {
  GET(url, write_disk(tf <- tempfile(fileext = ".zip")))
  unzip(tf, exdir = "data/raw/population/output areas")
  unlink(tf)
}

# ---- Load population data for England/Wales ----
pop_files <- list.files("data/raw/population/output areas", full.names = T)

for (file in pop_files) {
  
  # if the merged dataset doesn't exist, create it
  if (!exists("pop")){
    pop <- read_excel(file, sheet = "Mid-2019 Persons", skip = 4) %>% 
      select(OA11CD, LSOA11CD, `All Ages`)
    
  } else {
    # if the merged dataset does exist, append to it
    temp_dataset <- read_excel(file, sheet = "Mid-2019 Persons", skip = 4) %>% 
      select(OA11CD, LSOA11CD, `All Ages`)
    
    pop <- rbind(pop, temp_dataset)
    rm(temp_dataset)
  }
}

# ---- Load population data for Northern Ireland's Small Areas ----
# Source: https://www.nisra.gov.uk/publications/2018-mid-year-population-estimates-small-areas
GET("https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/SAPE18_SA_Totals.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

pop_ni <- read_excel(tf, sheet = "Flat")

pop_ni <- pop_ni %>% 
  filter(Year == max(pop_ni$Year)) %>% 
  select(SA2011 = Area_Code, `All Ages` = MYE)

unlink(tf)

# ---- Load output area centroids for England/Wales ----
# Output Areas (December 2011) Population Weighted Centroids
# Source: https://geoportal.statistics.gov.uk/datasets/output-areas-december-2011-population-weighted-centroids-1
oa = read_sf("https://opendata.arcgis.com/datasets/b0c86eaafc5a4f339eb36785628da904_0.geojson")

# ---- Load/process England and Wales flood maps directly from sources ----
# England: Flood Map for Planning (Rivers and Sea) - Flood Zone 3
# Source: https://environment.data.gov.uk/DefraDataDownload/?mapService=EA/FloodMapForPlanningRiversAndSeaFloodZone3&Mode=spatial
# ** You might need to generate a new download URL from ^ **
GET("https://environment.data.gov.uk/UserDownloads/interactive/4101659e47be4da980c5302f0b7f691083468/EA_FloodMapForPlanningRiversAndSeaFloodZone3_SHP_Full.zip",
    write_disk(tf <- tempfile(fileext = ".zip")))
unzip(tf, exdir = "data/raw/floods/england")
unlink(tf)

floods_eng <- read_sf("data/raw/floods/england/data/Flood_Map_for_Planning_Rivers_and_Sea_Flood_Zone_3.shp")

# Transform to web Mercator and simplify
floods_eng <- floods_eng %>% st_transform(crs = 4326)
# floods_eng <- floods_eng %>% ms_simplify(keep = 0.02, keep_shapes = TRUE)  # this causes errors, seemingly because of amount of memory required - don't really need this step anyway

# Wales: Flood Map: Flood Zone 3
# Source: http://lle.gov.wales/catalogue/item/Flood3/?lang=en#downloads-content
# This data is currently unavailable online - load the archived copy
# floods_wal <- read_sf("http://lle.gov.wales/catalogue/item/FloodMapFloodZone3.json")
unzip("data/raw/floods/wales.zip", exdir = "data/raw/floods/wales")
floods_wal <- read_sf("data/raw/floods/wales/NRW_FLOODMAP_FLOOD_ZONE_3_Simplified.shp")

# Transform to web Mercator and simplify
floods_wal <- floods_wal %>% 
  st_transform(crs = 4326) %>% 
  ms_simplify(keep = 0.02, keep_shapes = TRUE)

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
#
unzip("data/raw/floods/ni.zip", exdir = "data/raw/floods/ni")

oa_ni <- read_sf("data/raw/floods/ni/NI small areas in flood risk zones.shp")

oa_ni <- st_transform(oa_ni, crs = 4326)

# ---- Keep only OAs that intersect with the flood risk areas ----
# - England -
flood_oa_eng <- oa %>% 
  st_join(floods_eng) %>% 
  filter(!is.na(type))

# Merge population counts into Output Areas
oa_eng <- flood_oa_eng %>%
  left_join(pop, by = "OA11CD")

# - Wales -
flood_oa_wal <- oa %>% 
  st_join(floods_wal) %>% 
  filter(!is.na(TYPE))

# Merge population counts into Output Areas
oa_wal <- flood_oa_wal %>%
  left_join(pop, by = "OA11CD")

# - NI -
oa_ni <- oa_ni %>% 
  left_join(pop_ni, by = "SA2011") %>% 
  rename(OA11CD = SA2011, LSOA11CD = SOA2011)

# - Combine output areas into single dataframe -
oa_data <- bind_rows(oa_eng, oa_wal, oa_ni) %>%
  st_drop_geometry() %>% 
  select(OA11CD, LSOA11CD, n = `All Ages`)

# write_csv(oa_data, "data/raw/floods/Output Areas in flood risk zones.csv")  # (commented out because we won't need this data to produce combined risks)

# ---- Calculate population counts in LSOAs and MSOAs ----
# - NI population estimates for Super Output Areas
GET("https://www.ninis2.nisra.gov.uk/Download/Population/Population%20Totals%20(statistical%20geographies).ods",
    write_disk(tf <- tempfile(fileext = ".ods")))

pop_ni_lsoa <- read_ods(tf, sheet = "SOA", skip = 3)
names(pop_ni_lsoa)[names(pop_ni_lsoa) == "Persons"] <- as.character(2019:2001)
pop_ni_lsoa <- pop_ni_lsoa %>% select(LSOA11CD = `SOA Code`, `total` = `2019`)
unlink(tf); rm(tf)

# - LSOA -
# Calculate total numbers of people in each LSOA (for proportions in flood areas)
pop_lsoa <- pop %>% 
  group_by(LSOA11CD) %>% 
  summarise(total = sum(`All Ages`)) %>% 
  
  bind_rows(pop_ni_lsoa)

# - MSOA -
# Output Area to Lower Layer Super Output Area to Middle Layer Super Output Area to Local Authority District (December 2011) Lookup in England and Wales
# source: https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales
oa_lookup = read_csv("https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv")

# Calculate total numbers of people in each MSOA (for proportions in flood areas)
pop_msoa <- pop %>% 
  left_join(oa_lookup, by = "OA11CD") %>% 
  group_by(MSOA11CD) %>% 
  summarise(total = sum(`All Ages`))

# ---- Count population at risk by LSOA ----
floods_lsoa <- oa_data %>% 
  group_by(LSOA11CD) %>% 
  summarise(`No. people in flood risk areas` = sum(n)) %>% 
  
  left_join(pop_lsoa, by = "LSOA11CD") %>% 
  mutate(`% people in flood risk areas` = `No. people in flood risk areas` / total) %>% 
  select(-total)
  
# save
write_csv(floods_lsoa, "data/processed/Flood risks - LSOA.csv")

# ---- Count population at risk by MSOA ----
floods_msoa <- oa_data %>% 
  left_join(oa_lookup %>% select(OA11CD, MSOA11CD), by = "OA11CD") %>% 
  
  filter(!startsWith(OA11CD, "N")) %>%   # MSOAs don't exist in NI, so filter them out
  group_by(MSOA11CD) %>% 
  summarise(`No. people in flood risk areas` = sum(n)) %>% 
  
  left_join(pop_msoa, by = "MSOA11CD") %>% 
  mutate(`% people in flood risk areas` = `No. people in flood risk areas` / total) %>% 
  select(-total) %>% 
  
  # tag on NI's LSOAs
  bind_rows(floods_lsoa %>% filter(str_sub(LSOA11CD, 1, 1) == "9"))

# save
write_csv(floods_msoa, "data/processed/Flood risks - MSOA.csv")
