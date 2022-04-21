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

#Keep useful variables only
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

#Filter fires by incident_type and year to include only dwelling fires from 2020
dropped <-
  reduced |>
  filter(incident_type == "Dwelling Fire") |>  
  select(-incident_type, -cat)

fires_id <-
  dropped |> 
  mutate(id = row_number()) |> 
  relocate(id)

fires_id <- fires_id |> 
  filter(year == "2020") |> 
  select(lad_code, id)

fire_lads <-
  fires_id |> 
  distinct(lad_code) |>
  pull()

geographr_lads <-
  boundaries_ltla21 |> 
  select(lad_code = ltla21_code,
         lad_name = ltla21_name) |>
  filter(str_detect(lad_code, "^S")) |> 
  pull(lad_code)

if(!(setequal(fire_lads, geographr_lads))) {
  stop("LADS don't match")
} else {
  "LADS match"
}

# Change lads that do not match using https://findthatpostcode.uk/
differences <- na.omit(setdiff(fire_lads, geographr_lads))
differences

clean <-
  fires_id |>
  mutate(lad_code = case_when(
    lad_code == "S12000015" ~ "S12000047",
    lad_code == "S12000024" ~ "S12000048",
    lad_code == "S12000046" ~ "S12000049",
    lad_code == "S12000044" ~ "S12000050",
    TRUE ~ lad_code))

clean |>
  keep_na()

# Check again
fire_lads <-
  clean |> 
  distinct(lad_code) |>
  pull()

geographr_lads

if(na.omit(!setequal(fire_lads, geographr_lads))) {
  stop("LADS don't match")
} else {
  "LADS match"
}

setdiff(fire_lads, geographr_lads)

# Group by LAD
grouped <- fires_id |>
  group_by(lad_code) |>
  summarise(fire_count = n())

lookup_lad <- lookup_postcode_oa11_lsoa11_msoa11_ltla20 |>
  select(dz_code = lsoa11_code, lad_code = ltla20_code)

pop <- population_dz_20 |>
  filter(sex == "All") |>
  select(dz_code, total_population) |>
  left_join(lookup_lad, by = "dz_code") |>
  group_by(lad_code) |>
  summarise(total_population=sum(total_population))|>
  mutate(lad_code = case_when(
    lad_code ==  "S12000047"~"S12000015",
    lad_code == "S12000048"~"S12000024",
    lad_code ==  "S12000049"~"S12000046",
    lad_code == "S12000050"~"S12000044",
    TRUE ~ lad_code))

fires_lad <- left_join(grouped, pop, by = "lad_code") |>
  mutate(fire_rate = fire_count/total_population*100000) |>
  mutate(rank = rank(fire_rate)) |>
  mutate(quantiles = quantise(rank, 10)) |>
  select(lad_code, quantiles)