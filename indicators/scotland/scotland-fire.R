library(httr)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)

# Data Zone and Intermediate Zone 2011 Lookups
# source: https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020-data-zone-look-up/
GET(
  "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-data-zone-look-up-file/documents/scottish-index-of-multiple-deprivation-data-zone-look-up/scottish-index-of-multiple-deprivation-data-zone-look-up/govscot%3Adocument/SIMD_2020_Datazone_lookup_tool.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

lookup <-
  read_excel(tf, sheet = "SIMD 2020v2 DZ lookup data") %>%
  select(data_zone = DZ, intermediate_zone = IZcode, lad_code = LAcode) %>%
  distinct()

unlink(tf)
rm(tf)

# Household estimates by data zone (2011)
# source: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings
GET(
  "https://www.nrscotland.gov.uk/files//statistics/household-estimates/small-area/hh-est-by-2011-dz-small-area-14-19.xlsx",
  write_disk(tf <- tempfile())
)

dwellings <-
  read_excel(tf, skip = 2, sheet = "2019") %>%
  drop_na() %>%
  select(
    data_zone = `2011 Data Zone code`,
    total_dwellings = `Total Dwellings`,
    occupied_dwellings = `Occupied Dwellings`
  )

unlink(tf)
rm(tf)

# Incidents by Data Zone
# source: https://www.firescotland.gov.uk/about-us/fire-and-rescue-statistics.aspx
# data: https://www.firescotland.gov.uk/media/1332754/firesbydatazone.csv
fires <- read_csv("https://www.firescotland.gov.uk/media/1332754/firesbydatazone.csv")

# Count fires by Data Zone (~LSOA)
fires_dz <-
  fires %>%
  filter(IncidentType == "Dwelling Fire") %>%
  rename(data_zone = DataZone) %>%
  group_by(data_zone) %>%
  summarise(
    n_fires = n(),
    n_accidental = sum(Accidental),
    n_fatalities = sum(FireFatalities),
    n_casualties = sum(FireCasualties_exc_precautionary_checks)
  ) %>%
  filter(data_zone != ":unknown") %>%
  left_join(dwellings, by = "data_zone") %>%
  mutate(density_fires = n_fires / occupied_dwellings)

# Count fires by Intermediate Zone (MSOA)
fires_iz <- 
  fires_dz %>%
  left_join(lookup, by = "data_zone") %>%
  group_by(intermediate_zone) %>%
  summarise(n_fires = sum(n_fires), n_dwellings = sum(occupied_dwellings)) %>%
  mutate(density_fires = n_fires / n_dwellings)

# Count fires by Council Area (LAD)
fires_lad <-
  fires_dz %>%
  left_join(lookup, by = "data_zone") %>%
  group_by(lad_code) %>%
  summarise(n_fires = sum(n_fires), n_dwellings = sum(occupied_dwellings)) %>%
  mutate(dens_dires = n_fires / n_dwellings)
