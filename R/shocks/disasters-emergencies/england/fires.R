# Load packages ----
library(tidyverse)
library(readxl)
library(geographr)
library(sf)
library(httr)

source("R/utils.R") # for download_file()

# Load data and prep ----
# Source: https://www.gov.uk/government/statistical-data-sets/fire-statistics-data-tables#response-times
# FIRE0102: Incidents attended by fire and rescue services in England, by incident type and fire and rescue authority
# Link to dataset (year ending June 2021): https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1032088/fire-statistics-data-tables-fire0102-111121.xlsx

tf <- download_file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1032088/fire-statistics-data-tables-fire0102-111121.xlsx", 
                    "xlsx")

raw <- read_excel(tf, sheet = "Data")

# Assumption to keep only dwelling fires due to the nature of fires responded to by VCS
raw_filtered <- raw |>
  rename_with(tolower, everything()) |>
  filter(financial_year == max(raw$FINANCIAL_YEAR),
         incident_type == "Dwellings") 

fire_incidents <- raw_filtered |>
  group_by(frs_name, frs_code = e_code) |>
  summarise(total_incidents = sum(total_incidents))
# 44 areas

# FRA to LAD lookup data ----
response <- GET("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD21_FRA21_EW_LU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
content <- content(response, type = "application/json", simplifyVector = TRUE)

fra_lad_lookup <- content$features$attributes |>
  select(lad_code = LAD21CD, lad_name = LAD21NM, fra_code = FRA21CD, fra_name = FRA21NM) |>
  filter(!str_detect(fra_name, "Wales"))

# Check coverage of LADs in dataset ----
# 2021 Local Authorities list
# Source: https://geoportal.statistics.gov.uk/datasets/967a3660c4aa49819731ceefe4008d76_0/explore
response <- GET("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LTLA21_UTLA21_EW_LU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
content <- content(response, type = "application/json", simplifyVector = TRUE)

ltla_utla_lookup <- content$features$attributes |>
  select(ltla_code = LTLA21CD, ltla_name = LTLA21NM, utla_code = UTLA21CD, utla_name = UTLA21NM)

ltla_utla_lookup |>
  anti_join(fra_lad_lookup, by = c("ltla_code" = "lad_code")) |> 
  filter(str_detect(ltla_code, "^E")) 

fra_lad_lookup  |>
  anti_join(ltla_utla_lookup, by = c("lad_code" = "ltla_code")) |> 
  filter(str_detect(lad_code, "^E"))

# Check of how many LADs to a FRA ----
fra_lad_lookup |>
  group_by(fra_code) |>
  summarise(count = n()) |>
  summary()

fra_lad_lookup |>
  distinct(fra_name)
# 47 FRAs

# Hampshire and Isle of Wight i.e E31000048, has a trailing whitespace in code which affects the join so removing 
fra_lad_lookup_trim <- fra_lad_lookup |> 
  mutate(fra_code = str_trim(fra_code, side = "both"))

# Check matches between datasets
fra_lad_lookup_trim |>
  anti_join(fire_incidents, by = c("fra_code" = "frs_code")) |>
  distinct(fra_name)

fire_incidents |>
  anti_join(fra_lad_lookup_trim, by = c("frs_code" = "fra_code"))


# Join fire response time data to FRA to LAD lookup ----
# Assumption: split the incidents in a FRA across the LTLA within that LAD weighted by population 
ltla_pop <- population_lad |>
  select(lad_code, total_population)

# Some LADs restructured in 2020 & 2021 - since other aspects of RI use population from geographr package will use these figures 
# for consistency but aggregate the changed authorities 
restructures <-
  tribble(
    ~pre_lad_name, ~post_ua_name, ~pre_lad_code, ~post_ua_code,
    "Aylesbury Vale", "Buckinghamshire", "E07000004", "E06000060",
    "Chiltern", "Buckinghamshire", "E07000005", "E06000060",
    "South Bucks", "Buckinghamshire", "E07000006", "E06000060",
    "Wycombe", "Buckinghamshire", "E07000007", "E06000060",
    "Corby", "North Northamptonshire", "E07000150", "E06000061",
    "Daventry", "West Northamptonshire", "E07000151", "E06000062",
    "East Northamptonshire", "North Northamptonshire", "E07000152", "E06000061",
    "Kettering", "North Northamptonshire", "E07000153", "E06000061",
    "Northampton", "West Northamptonshire", "E07000154", "E06000062",
    "South Northamptonshire", "West Northamptonshire", "E07000155", "E06000062",
    "Wellingborough", "North Northamptonshire", "E07000156", "E06000061",
  )


ltla_pop_restructure <- ltla_pop |> 
  left_join(restructures, by = c("lad_code" = "pre_lad_code")) |>
  mutate(lad_code = ifelse(!is.na(post_ua_code), post_ua_code, lad_code)) |>
  group_by(lad_code) |>
  summarise(total_population = sum(total_population))

# Normalise incidents by population ----
fire_incidents_lad <- fire_incidents |>
  left_join(fra_lad_lookup_trim, by = c("frs_code" = "fra_code")) |>
  left_join(ltla_pop_restructure) |>
  group_by(frs_code) |>
  mutate(prop_fra_pop = total_population/sum(total_population)) |>
  ungroup() |>
  mutate(weighted_incidents = total_incidents * prop_fra_pop) |>
  select(lad_code, weighted_incidents)

# Save data ----
fire_incidents_lad  |>
  write_rds("data/shocks/disasters-emergencies/england/fire.rds")
