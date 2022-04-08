# Load packages ----
library(tidyverse)
library(readxl)
library(geographr)
library(sf)
library(demographr)


source("R/utils.R") # for download_file()

# Load data and prep ----
# Source: https://www.gov.uk/government/statistics/fire-statistics-incident-level-datasets
# 'Low level geography dataset' file
# This file was manually downloaded and converted to .xlsx because readODS::read_ods() doesn't seem to work with this file
raw <- read_excel("data/on-disk/low-level-geography-dataset-300921.xlsx", sheet = "202021")

# Assumption to keep only dwelling fires due to the nature of fires responded to by VCS
housing_fires_lsoa <- raw |>
  rename_with(tolower, everything()) |>
  filter(incident_type %in% c("Primary fire - dwelling", "Primary fire - dwelling or other building", 
                              "Primary fire - other buildings - Student Hall of Residence",
                              "Primary fire - other buildings - Other Residential Home")) |>
  group_by(lsoa_code) |>
  summarise(count = n())

lookup <- lookup_postcode_oa_lsoa_msoa_lad |>
  distinct(lsoa_code, msoa_code, lad_code) |>
  filter(str_detect(lsoa_code, "^E"))

housing_fires_lad <- housing_fires_lsoa |>
  left_join(lookup, by = "lsoa_code") |>
  group_by(lad_code) |>
  summarise(count = sum(count)) 

# Update to 2021 LAD codes 
lad_lookup_20_21 <- lookup_lad_over_time |>
  distinct(lad_20_code = LAD20CD, lad_21_code = LAD21CD)

housing_fires_lad_21 <- housing_fires_lad |>
  left_join(lad_lookup_20_21, by = c("lad_code" = "lad_20_code")) |>
  group_by(lad_21_code) |>
  summarise(fire_count = sum(count))

# Normalise by population
lad_pop_21 <- population_lad_20_codes_21 |>
  select(lad_21_code, total_population) 
  
housing_fires_lad_normalised <- housing_fires_lad_21  |>
  left_join(lad_pop_21, by = "lad_21_code") |>
  mutate(fire_count_per_pop = fire_count / total_population) |>
  select(lad_code = lad_21_code, fire_count_per_pop)

# Save data ----
housing_fires_lad_normalised  |>
  write_rds("data/shocks/disasters-emergencies/england/fire.rds")
