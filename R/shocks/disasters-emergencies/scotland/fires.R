# Load packages ----
library(tidyverse)
library(readxl)
library(geographr)
library(sf)
library(demographr)

source("R/utils.R")

# Load data and prep ----
# Data Source: Fire and Rescue Incident Statistics 2020-21 (Scottish Fire and Rescue Service)

tf <- download_file(
  "https://www.firescotland.gov.uk/media/1144334/firesbydatazone.xlsx",
  "xlxs"
)

raw <- read_excel(tf)

reduced <-
  raw |> 
  select(
    fiscal_year = FiscalYear,
    incident_type = IncidentType,
    year = CalendarYear,
    incident_date = IncidentDate,
    lad_code = LACode,
    cat = PropertyCategorySummary
  )

dropped <-
  reduced |>
  filter(incident_type == "Dwelling Fire") |>  
  select(-incident_type, -cat)

fires_id <-
  dropped |> 
  mutate(id = row_number()) |> 
  relocate(id)

fires_id |> 
  filter(year == "2020") |> 
  select(lad_code, id)


















#Keeping only dwelling fires happened in 2020 and 2021
clean <- raw |>
  filter(IncidentType == "Dwelling Fire", FiscalYear == "2020-21") |>
  rename(lad_21_code = LACode)

fire_lads <-
  clean |> 
  distinct(lad_21_code) |>
  pull()

geographr_lads <-
  boundaries_lad_21 |> 
  filter(str_detect(lad_21_code, "^S")) |> 
  pull(lad_21_code)

if(!(setequal(fire_lads, geographr_lads))) {
  stop("LADS don't match")
}


grouped <- clean |>
  group_by(lad_21_code) |>
  summarise(fire_count = n())

codes <-lookup_postcode_oa_11_lsoa_11_msoa_11_lad_20 |>
  select(dz_code = lsoa_11_code, lad_21_code = lad_20_code)

dz_codes_data <- inner_join(grouped, codes, by = "lad_21_code")

pop <- population_dz_20 |>
  filter(sex == "All") |>
  select(dz_code, total_population)

tot <- inner_join(dz_codes_data, pop, by = "dz_code") |>
  group_by(lad_21_code, fire_count) |>
  summarise(total_population = sum(total_population)) |>
  mutate(fire_rate = fire_count/total_population*100000)

#Fire every 100 000 people (normalised)