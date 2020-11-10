##
## Dwelling fires
##
## Mapping at different geographies
## - England: LSOA 
## - Wales: Fire and Rescue Authority areas
## - Scotland: Intermediate Zones (equivalent of MSOA)
## - NI: ** not available yet **
##
library(tidyverse)
library(readxl)
library(httr)


# ---- Load population estimates ----
# - England/Wales Lower layer Super Output Area population estimates -
# source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
GET("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2019sape22dt2/sape22dt2mid2019lsoasyoaestimatesunformatted.zip",
    write_disk(tf <- tempfile(fileext = ".zip")))
unzip(tf, exdir = "data/raw/population")
unlink(tf); rm(tf)

# Load the data - if updating the file above, you may need to change the filename and the worksheet name
pop_eng_lsoa <- read_excel("data/raw/population/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx", sheet = "Mid-2019 Persons", skip = 4)

# Keep only LSOA-level population totals
pop_eng_lsoa <- pop_eng_lsoa %>% 
  select(LSOA11CD = `LSOA Code`, `No. people` = `All Ages`)


# ---- Load lookup tables ----
# - England and Wales -
# Output Area to Lower Layer Super Output Area to Middle Layer Super Output Area to Local Authority District (December 2011) Lookup in England and Wales
# source: https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales
lsoa_to_msoa <- read_csv("https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv") %>% 
  select(LSOA11CD, MSOA11CD) %>% 
  distinct()


# ---- England ----
# - Incidents by LSOA (Last updated 30 January 2020) -
# Source: "Low level geography dataset" from https://www.gov.uk/government/statistical-data-sets/fire-statistics-incident-level-datasets
# Data: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/858048/low-level-geography-dataset.ods
# This file was manually downloaded and converted to .xlsx because readODS::read_ods() doesn't seem to work with this file
# Also zipping it because otherwise it's too large to upload to GitHub
unzip("data/raw/fires/fires.zip", exdir = "data/raw/fires")

fires_eng_path <- "data/raw/fires/low-level-geography-dataset.xlsx"

# The file contains one worksheet per county - load all into one dataframe
fires_eng_lsoa <- excel_sheets(fires_eng_path) %>% 
  set_names() %>% 
  map_df(~ read_excel(path = fires_eng_path, sheet = .x), .id = "sheet")

# Some of the LSOA_DESCRIPTION columns actually contain LSOA codes - sort this out so all codes are in the same column
fires_eng_lsoa <- fires_eng_lsoa %>% 
  mutate(is_code = str_detect(LSOA_DESCRIPTION, "E[0-9]+")) %>%  # check whether LSOA_DESCRIPTION is actually the LSOA code
  mutate(LSOA11CD = ifelse(is_code, LSOA_DESCRIPTION, LSOA_CODE)) %>% 
  select(-is_code)

eng_dwellings = c("Primary fire - dwelling", "Primary fire - dwelling or other building", "Primary fire - other buildings - Student Hall of Residence",
                  "Primary fire - other buildings - Other Residential Home")

# Count average number of fire incidents in each LSOA over the last three years
fires_eng_lsoa_sum <- fires_eng_lsoa %>% 
  filter(INCIDENT_TYPE %in% eng_dwellings) %>% 
  mutate(Year = as.integer(str_sub(FINANCIAL_YEAR, 1, 4))) %>% 
  
  # summarise for last three years
  filter(Year >= 2016) %>% 
  count(LSOA11CD, Year) %>% 
  
  # calculate three-year average
  group_by(LSOA11CD) %>% 
  summarise(`Number of fires (three-year average)` = mean(n, na.rm = TRUE))

# Calculate per capita rate of fires
fires_eng_lsoa_sum <- fires_eng_lsoa_sum %>% 
  left_join(pop_eng_lsoa, by = "LSOA11CD") %>% 
  mutate(`Proportion of fire incidents per person` = `Number of fires (three-year average)` / `No. people`)

# Aggregate into MSOAs
fires_eng_msoa <- fires_eng_lsoa_sum %>% 
  left_join(lsoa_to_msoa, by = "LSOA11CD") %>% 
  
  # summarise by msoa
  group_by(MSOA11CD) %>% 
  summarise(`Number of fires (three-year average)` = sum(`Number of fires (three-year average)`),
            `No. people` = sum(`No. people`)) %>% 
  
  # recalculate density
  mutate(`Proportion of fire incidents per person` = `Number of fires (three-year average)` / `No. people`) %>% 
  select(-`No. people`)

# Save
# fires_eng_lsoa_sum %>% 
#   write_csv("Fires - LSOA - England.csv"))
# 
# fires_eng_msoa %>% 
#   write_csv(file.path(data.dir.processed, "Fires - MSOA - England.csv"))
