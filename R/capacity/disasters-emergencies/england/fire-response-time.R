# Load packages ----
library(tidyverse)
library(readxl)
library(geographr)
library(sf)
library(janitor)

source("R/utils.R") # for download_file()

# Load data and prep ----
# Source: https://www.gov.uk/government/statistical-data-sets/fire-statistics-data-tables#response-times
# Link to dataset (year ending June 2021): https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1032104/fire-statistics-data-tables-fire1001-111121.xlsx

tf <- download_file(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1032104/fire-statistics-data-tables-fire1001-111121.xlsx",
  "xlsx"
)

raw <- read_excel(tf, sheet = "Data - annual")

# 'fire_type=Primary' is the summation of the other rows (i.e. sub types such as dwellings etc).
raw_filtered <- raw |>
  janitor::clean_names() |>
  filter(fire_type == "Primary", financial_year == max(raw$FINANCIAL_YEAR))

# Definitions:
# Total response time: time of call to first vehicle to arrive at the incident.
# Call handling: time of call to time station notified
# Crew turnout: from time station notified to first vehicle to leave
# Drive time: from time first vehicle leaves to first vehicle to arrive at the incident

# Consider if want total time or some subset of call handling, turnaround, drive time. Would call handling be outwith the LAs fire service control and be a 999 operator variable?
# Drive time not just reflective of speed but also proximity to fires of fire stations?
# Take total response time for now.

fire_response <- raw_filtered |>
  select(frs = location_category, incidents, total_response_time) |>
  filter(!frs %in% c("England", "Metropolitan", "Non-metropolitan", "Predominantly Rural", "Predominantly Urban", "Significantly Rural"))
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
  ggplot() +
  geom_histogram(aes(x = count))

fra_lad_lookup |>
  distinct(fra_name)
# 47 FRAs

# Check matches between datasets
fra_lad_lookup |>
  anti_join(fire_response, by = c("fra_name" = "frs")) |>
  distinct(fra_name)

fire_response |>
  anti_join(fra_lad_lookup, by = c("frs" = "fra_name"))

# Manual fixes of some names to allow matching
fire_response_updated <- fire_response |>
  mutate(frs = case_when(
    frs == "Devon and Somerset" ~ "Devon & Somerset",
    frs == "Dorset and Wiltshire" ~ "Dorset & Wiltshire",
    frs == "Greater London" ~ "London Fire and Emergency Planning Authority",
    frs == "Isles Of Scilly" ~ "Isles of Scilly",
    frs == "Nottinghamshire" ~ "Nottinghamshire and City of Nottingham",
    frs == "Staffordshire" ~ "Stoke-on-Trent and Staffordshire",
    frs == "Buckinghamshire" ~ "Buckinghamshire & Milton Keynes",
    frs == "Berkshire" ~ "Royal Berkshire",
    frs == "Durham" ~ "County Durham and Darlington",
    frs == "Hereford and Worcester" ~ "Hereford & Worcester",
    TRUE ~ frs
  ))

# Check matches after manual fixes
fire_response_updated |>
  anti_join(fra_lad_lookup, by = c("frs" = "fra_name"))

fra_lad_lookup |>
  anti_join(fire_response_updated, by = c("fra_name" = "frs")) |>
  distinct(fra_name)

# Join fire response time data to FRA to LAD lookup
# Assumption: all LADs within an FRS have same response time as don't currently have data at a lower geography level
fire_response_lad <- fire_response_updated |>
  left_join(fra_lad_lookup, by = c("frs" = "fra_name")) |>
  select(lad_code, total_response_time)

# Save data
fire_response_lad |>
  write_rds("data/capacity/disasters-emergencies/england/fire-response-times.rds")
