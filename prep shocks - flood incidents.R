##
## Get LSOAs from the flooding and water rescue incidents dataset in England
## Source: https://www.gov.uk/government/statistical-data-sets/fire-statistics-incident-level-datasets
## Data:"https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/861418/flooding-and-water-rescue-incidents-dataset-jan20.ods"
##
library(tidyverse)

flood_incidents <- read_csv("data/raw/floods/flooding-and-water-rescue-incidents-dataset-jan20.csv")

flood_incidents <- flood_incidents %>% 
  filter(LOCATION_TYPE == "Dwellings",
         INCIDENT_TYPE %in% c("Rescue or evacuation from water", "Flooding - Make safe", "Flooding - Other inc Evacuation")) %>% 
  
  # keep only records that included an evacuation, rescue, or fatality/casualty
  filter(EVACUATIONS_ILD != "0" | RESCUES_ILD == "Involved a Rescue" | FATALITY_CASUALTY_ILD == "Fatality/Casualty")

flood_incidents %>% 
  select(LSOA11CD = LSOA_CODE) %>% 
  write_csv("data/processed/flood incidents - lsoa.csv")

# ---- Create list of MSOAs that have experienced flood incidents historically ----
# Output Area to Lower Layer Super Output Area to Middle Layer Super Output Area to Local Authority District (December 2011) Lookup in England and Wales
# source: https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales
lsoa_msoa = read_csv("https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv") %>% 
  select(LSOA11CD, MSOA11CD)

# Count number of incidents
flood_incidents %>% 
  left_join(lsoa_msoa, by = c("LSOA_CODE" = "LSOA11CD")) %>% 
  count(MSOA11CD) %>% 
  write_csv("data/processed/flood incidents - msoa.csv")
