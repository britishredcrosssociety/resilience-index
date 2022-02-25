# Load packages ----
library(tidyverse)
library(readxl)
library(geographr)
library(sf)


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

# Save data ----
housing_fires_lad  |>
  write_rds("data/shocks/disasters-emergencies/england/fire.rds")
