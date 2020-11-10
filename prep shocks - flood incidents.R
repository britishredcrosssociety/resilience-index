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
  write_csv("data/processed/Flood incidents - LSOA.csv")
