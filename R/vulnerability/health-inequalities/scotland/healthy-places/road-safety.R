library(httr)
library(dplyr)
library(readr)
library(readODS)
library(stringr)
library(geographr)
library(sf)

# - Lookup LAD names and LAD Codes-
lookup <-
  boundaries_lad %>%
  as_tibble() %>%
  select(-geometry) %>%
  filter(str_detect(lad_code, "^S"))

# - Road safety data -
# Steps to reproduce the link in the GET reqeust:
#   Step 1: select "Accident"
#   Step 2: select "All(combined)"
#   Step 3: select '2019'
#   Step 4a: select 'Local authorities'
#   Step 4b: select all local authorities in Scotland (32)
#   Step 5: select 'Skip step'
#   Right-click the button that says "ODS (Excel compatible)" and click 'Copy link'

# Source: https://roadtraffic.dft.gov.uk/custom-downloads/road-accidents/reports/d5dc1ae7-dc63-441a-8eac-98e76d241a23
GET(
  "https://dft-statistics.s3.amazonaws.com/road-accidents/reports/d5dc1ae7-dc63-441a-8eac-98e76d241a23.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

road_safety_raw <-
  read_ods(tf, sheet = "LAs - All accidents") %>%
  as_tibble()

# LAD codes appear incorrect so match to lookup
road_safety <-
  road_safety_raw %>%
  select(
    lad_name = `Local authority`,
    accident_count = `All accidents`
  ) %>%
  mutate(
    lad_name = case_when(
      lad_name == "Argyll & Bute" ~ "Argyll and Bute",
      lad_name == "Edinburgh, City of" ~ "City of Edinburgh",
      lad_name == "Glasgow, City of" ~ "Glasgow City",
      lad_name == "Perth & Kinross" ~ "Perth and Kinross",
      lad_name == "Western Isles (Eilean Siar)" ~ "Na h-Eileanan Siar",
      TRUE ~ lad_name
    )
  ) %>%
  left_join(lookup, by = "lad_name") %>%
  relocate(lad_code) %>%
  select(-lad_name)

# - Road traffic volume -
# Retrieve LAD area sizes KM^2
# Source: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-bgc/geoservice?geometry=-81.184%2C46.017%2C76.316%2C63.434
# A custom query URL has been set by selecting only 'lad19cd' & 'st_area(shape)' from
# 'Out Fields' in 'Query' and selecting 'Return Geometry' to 'False' in 'Output Options'
areas <-
  read_sf("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_December_2019_Boundaries_UK_BGC/MapServer/0/query?where=1%3D1&outFields=st_area(shape),lad19cd&returnGeometry=false&outSR=4326&f=json")

areas <-
  areas %>%
  select(
    lad_code = lad19cd,
    area_m_squared = `st_area(shape)`
  ) %>%
  filter(str_detect(lad_code, "^S")) %>%
  mutate(area_km_squared = area_m_squared / 1000^2) %>%
  select(-area_m_squared)

# Retrieve traffic data
# Source: https://roadtraffic.dft.gov.uk/downloads
traffic_raw <-
  read_csv("http://data.dft.gov.uk/road-traffic/local_authority_traffic.csv")

traffic <-
  traffic_raw %>%
  filter(year == "2019") %>%
  select(
    lad_name = local_authority_name,
    vehicle_km_annual = all_motor_vehicles
  ) %>%
  mutate(vehicle_km_billions = vehicle_km_annual / 1e+09) %>%
  select(-vehicle_km_annual) %>%
  mutate(
    lad_name = case_when(
      lad_name == "Dumfries & Galloway" ~ "Dumfries and Galloway",
      lad_name == "" ~ "Na h-Eileanan Siar",
      lad_name == "Argyll & Bute" ~ "Argyll and Bute",
      lad_name == "Perth & Kinross" ~ "Perth and Kinross",
      TRUE ~ lad_name
    )
  )

# Join lookup to traffic data
traffic <-
  lookup %>%
  left_join(traffic, by = "lad_name") %>%
  select(-lad_name)

# Impute missing data
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/methodologies/methodsusedtodevelopthehealthindexforengland2015to2018
# Reason for missing: not specified
# Strategy: match Na h-Eileanan Siar (S12000013) vehicle_km_billions
#           to Shetland Islands (S12000027) and Orkney Islands (S12000023)
#           as they share a similar population size (~25,000), and presumably
#           similar driving patterns
shetland_orkney_traffic <-
  traffic %>%
  filter(
    lad_code == "S12000027" |
      lad_code == "S12000023"
  ) %>%
  pull(vehicle_km_billions) %>%
  mean()

traffic <-
  traffic %>%
  mutate(
    vehicle_km_billions = if_else(
      lad_code == "S12000013",
      shetland_orkney_traffic,
      vehicle_km_billions
    )
  )

# Normalise traffic volume by land area (Km^2)
traffic_volume <-
  traffic %>%
  left_join(areas, by = "lad_code") %>%
  mutate(traffic_volume = vehicle_km_billions / area_km_squared) %>%
  select(lad_code, traffic_volume)

# - Normalise accident count by traffic volume -
road_safety <-
  road_safety %>%
  left_join(traffic_volume, by = "lad_code") %>%
  mutate(road_safety = accident_count / traffic_volume) %>%
  select(lad_code, road_safety)

write_rds(road_safety, "data/vulnerability/health-inequalities/scotland/healthy-places/road-safety.rds")