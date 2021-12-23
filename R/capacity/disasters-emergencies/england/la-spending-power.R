# Load packages ----
library(tidyverse)
library(readxl)
library(geographr)
library(sf)

source("R/utils.R") # for download_file()

# Load data and prep ----
# Source: https://www.gov.uk/government/publications/core-spending-power-provisional-local-government-finance-settlement-2021-to-2022
# (Alternative: https://pldr.org/dataset/20mjj/local-authority-finance-core-spending-power-fin0759)

tf <- download_file(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/945388/Core_Spending_Power_summary_table_2021-22.xlsx",
  "xlsx"
)

raw <- read_excel(tf, sheet = "Per Dwelling")

# CPS = Core Spending Power
cps <- raw |>
  select(lad_code = `...3`, lad_name = `Core Spending Power - Local Authority Summary`, cps_millions = `...8`) |>
  filter(str_detect(lad_code, "^E")) |>
  mutate(cps_millions = ifelse(cps_millions == "NA", NA, cps_millions)) |>
  mutate(cps_millions = as.numeric(cps_millions))

# Confirm no loss of decimal information
print(cps$cps_millions, digits = 17)

# Join on LAD population data ----
# This data uses 2021 LAD boundaries so will use LAD 2021 population figures (geographr currently uses 2019 LADs)

lads_pop <- population_lad |>
  select(lad_code, total_population)

counties_pop <- population_counties_ua |>
  select(county_ua_code, total_population)

# Check any differences in LAD codes between 2 datasets
lads_pop |>
  filter(str_detect(lad_code, "^E")) |>
  anti_join(cps)
# no missings

cps |>
  anti_join(lad_pop) |>
  print(n = Inf)

# Investigate not matches in LAD population data (Fire & Counties) ----

# Many are relating to Fire services
fire_lads <- cps |>
  anti_join(lads_pop, by = "lad_code") |>
  filter(str_detect(lad_name, "Fire"))

# Check for matches at county (i.e. UTLA) level
cps_counties <- cps |>
  anti_join(lads_pop, by = "lad_code") |>
  filter(!str_detect(lad_name, "Fire")) |>
  inner_join(counties_pop, by = c("lad_code" = "county_ua_code")) |>
  rename(county_ua_code = lad_code, county_ua_name = lad_name)


# Investigate not matches in LAD population data (2021 Northampshire LAD restructure) ----
northam_restructure <-
  tribble(
    ~pre_lad_name, ~post_ua_name, ~pre_lad_code, ~post_ua_code,
    "Corby", "North Northamptonshire", "E07000150", "E06000061",
    "Daventry", "West Northamptonshire", "E07000151", "E06000062",
    "East Northamptonshire", "North Northamptonshire", "E07000152", "E06000061",
    "Kettering", "North Northamptonshire", "E07000153", "E06000061",
    "Northampton", "West Northamptonshire", "E07000154", "E06000062",
    "South Northamptonshire", "West Northamptonshire", "E07000155", "E06000062",
    "Wellingborough", "North Northamptonshire", "E07000156", "E06000061",
  )

# CPS data contains old Northam codes since raw dataset also contains 20/21 spending data (but has NA value)
cps |>
  filter(lad_code %in% c(northam_restructure$pre_lad_code, northam_restructure$post_ua_code))


northam_pop <- lads_pop |>
  inner_join(northam_restructure, by = c("lad_code" = "pre_lad_code")) |>
  group_by(post_ua_code, post_ua_name) |>
  summarise(total_population = sum(total_population)) |>
  rename(county_ua_name = post_ua_name, county_ua_code = post_ua_code) |>
  ungroup()

cps_northam <- cps |>
  filter(lad_code %in% c(northam_restructure$post_ua_code)) |>
  rename(county_ua_code = lad_code, county_ua_name = lad_name) |>
  left_join(northam_pop)

# Also remove the county of Northamptonshire (E10000021) as this was removed along with the 7 LADs being restructured
cps_counties_updated <- cps_counties |>
  filter(county_ua_code != "E10000021") |>
  bind_rows(cps_northam)

# Remove the 7 old LADs from LAD level data
cps_updated <- cps |>
  inner_join(lads_pop, by = "lad_code") |>
  filter(!lad_code %in% northam_restructure$pre_lad_code)

# Remaining unmatched is a Combined Authority (don't currently have this lookup in geographr package)
cps |>
  anti_join(lads_pop, by = "lad_code") |>
  anti_join(fire_lads, by = "lad_code") |>
  anti_join(cps_counties_updated, by = c("lad_code" = "county_ua_code"))

# Load in Combined Authority lookup data ----
# Combined Authority lookup source: https://geoportal.statistics.gov.uk/datasets/ons::local-authority-district-to-combined-authority-december-2020-lookup-in-england/about
response <- httr::GET("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD20_CAUTH20_EN_LU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
content <- httr::content(response, type = "application/json", simplifyVector = TRUE)

combined_auth_lookup <- content$features$attributes |>
  select(-column4, -FID)

cps_combined_auth_lookup <- cps |>
  inner_join(combined_auth_lookup, by = c("lad_code" = "CAUTH20CD")) |>
  select(cauth_code = lad_code, cauth_name = lad_name, lad_code = LAD20CD, lad_name = LAD20NM, cps_millions) |>
  left_join(lads_pop, by = "lad_code")

# Disaggregate the UTLA (i.e. counties) and Combined Authority CPS ----

# Assumption: split based on population distribution

cps_combined_auth_split <- cps_combined_auth_lookup |>
  mutate(pop_prop = total_population / sum(total_population)) |>
  mutate(weighted_cps = cps_millions * pop_prop) |>
  select(lad_name, lad_code, cps_millions = weighted_cps)

# Get 2021 LTLA (LAD) to UTLA (county) lookup
response <- httr::GET("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LTLA21_UTLA21_EW_LU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
content <- httr::content(response, type = "application/json", simplifyVector = TRUE)

counties_lads_lookup <- content$features$attributes |>
  select(-FID) |>
  select(lad_code = LTLA21CD, lad_name = LTLA21NM, county_ua_code = UTLA21CD)

# Join LAD to County lookup and the LAD populations
# For the 2 new Northamptonshire counties these have a 1-to-1 mapping with the LAD of the same area so same population applied
cps_counties_split <- cps_counties_updated |>
  rename(county_pop = total_population) |>
  left_join(counties_lads_lookup, by = c("county_ua_code")) |>
  left_join(lads_pop, by = "lad_code") |>
  rename(lad_pop = total_population) |>
  mutate(lad_pop = ifelse(county_ua_code %in% c("E06000061", "E06000062"), county_pop, lad_pop)) |>
  mutate(pop_prop = lad_pop / county_pop) |>
  mutate(weighted_cps = cps_millions * pop_prop) |>
  select(lad_name, lad_code, cps_millions = weighted_cps)

# Combine the LAD level and disaggregated county and combined authorities
cps_combined <- cps_updated |>
  select(-total_population) |>
  bind_rows(cps_combined_auth_split) |>
  bind_rows(cps_counties_split) |>
  group_by(lad_code, lad_name) |>
  summarise(cps_millions = sum(cps_millions))

# TO DO: Fire areas?
# Should UTLA/Combined Auth be included? This is a adjusted version of the data where these are removed?
# https://pldr.org/dataset/20mjj/local-authority-finance-core-spending-power-fin0759

fire_lads
