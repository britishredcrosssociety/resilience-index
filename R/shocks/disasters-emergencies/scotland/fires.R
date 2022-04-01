# Load packages ----
library(tidyverse)
library(readxl)
library(geographr)
library(sf)
library(demographr)

source("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/R/utils.R")

# Load data and prep ----
# Data Source: Fire and Rescue Incident Statistics 2020-21 (Scottish Fire and Rescue Service)

tf <- download_file(
  "https://www.firescotland.gov.uk/media/1144334/firesbydatazone.xlsx",
  "xlxs"
)

raw <- read_excel(tf)

#Keeping only dwelling fires happened in 2020 and 2021
clean <- raw |>
  filter(IncidentType == "Dwelling Fire", FiscalYear == "2020-21") |>
  rename(lad_21_code = LACode)

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