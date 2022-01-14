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

tf <- download_file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1032088/fire-statistics-data-tables-fire0102-111121.xlsx", "xlsx")

raw <- read_excel(tf, sheet = "Data")

# TO CHECK: currently keeping all incident types but may filter
raw_filtered <- raw |>
  rename_with(tolower, everything()) |>
  filter(financial_year == max(raw$FINANCIAL_YEAR))

raw_filtered |>
  distinct(incident_type)

fire_incidents <- raw_filtered |>
  group_by(frs_name, frs_code = e_code) |>
  summarise(total_incidents = sum(total_incidents))
# 44 areas

# FRA to LAD lookup data ----
response <- httr::GET("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD21_FRA21_EW_LU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
content <- httr::content(response, type = "application/json", simplifyVector = TRUE)

fra_lad_lookup <- content$features$attributes |>
  select(lad_code = LAD21CD, lad_name = LAD21NM, fra_code = FRA21CD, fra_name = FRA21NM) |>
  filter(!str_detect(fra_name, "Wales"))

# Check of how many LADs to a FRA
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

fire_incidents_lad <- fire_incidents |>
  left_join(fra_lad_lookup_trim, by = c("frs_code" = "fra_code")) |>
  left_join(ltla_pop) |>
  group_by(frs_code) |>
  mutate(prop_fra_pop = total_population/sum(total_population)) |>
  ungroup() |>
  mutate(weighted_incidents = total_incidents * prop_fra_pop) |>
  select(lad_code, weighted_incidents)

# Save data
fire_incidents_lad  |>
  write_rds("data/shocks/disasters-emergencies/england/fire.rds")
