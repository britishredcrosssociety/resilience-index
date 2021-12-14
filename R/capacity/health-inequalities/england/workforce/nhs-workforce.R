# Load packages
library(tidyverse)
library(geographr)
library(sf)

source("R/utils.R") # for download_file()
source("R/capacity/health-inequalities/england/trust_types/trust_types.R") # run trust types code to create open_trust_types.feather

# Source https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics/may-2021

# Both FTE and total headcount is available - FTE is likely more representative

tf <- download_file("https://files.digital.nhs.uk/F5/8ADE23/NHS%20Workforce%20Statistics%2C%20May%202021%20England%20and%20Organisation.xlsx", "xlsx")

raw_fte <-
  read_excel(
    tf,
    sheet = "3. NHSE, Org & SG - FTE",
    skip = 6
  )


# Only taking those defined as 'Professionally qualified clinical staff' (includes all HCHS doctors, qualified nurses and health visitors, midwives, qualified scientific, therapeutic and technical staff and qualified ambulance staff).
# So excluding: 'Scientific, therapeutic & technical staff', 'Support to clinical staff' & 'NHS infrastructure support'.

raw_fte_clean <- raw_fte |>
  rename("trust_code" = "...4", "trust_name" = "...3", "staff_fte" = "Professionally qualified clinical staff") |>
  filter(!is.na(trust_code)) %>%
  select(trust_code, trust_name, staff_fte)

# There is rows in the data for CCGs. These make up 0.7% of the total staff in the data and currently don't have a method to map these back to MSOA/LA level. 
raw_fte_clean |>
  mutate(trust_flag = str_detect(trust_name, "NHS Trust|NHS Foundation Trust"))|>
  mutate(ccg_flag = str_detect(trust_name, "CCG")) |>
  group_by(trust_flag, ccg_flag) |>
  summarise(count = n(), total_staff_fte = sum(staff_fte), prop = sum(staff_fte)/sum(raw_fte_clean$staff_fte))

# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")

# Check the matching of cost data & trust table in geographr package --------
open_trusts |>
  anti_join(raw_fte_clean)
# 3 trusts missing from staff data

raw_fte_clean |>
  anti_join(open_trusts)
# mainly CCG (and 2 Trusts) - don't currently have a way to map these to MSOA/LA (as detailed above)

raw_fte_clean |>
  anti_join(open_trust_types) |>
  mutate(ccg_flag = str_detect(trust_name, "CCG")) |>
  group_by(ccg_flag) |>
  summarise(fte_count = sum(staff_fte))

# TO DO: Think about CCG staff

# Join trust to MSOA lookup --------

# Trust to MSOA table only has data for acute trusts
open_trusts |>
  left_join(raw_fte_clean, by = c("trust_code")) |>
  left_join(lookup_trust_msoa) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LSOA and proportion to per capita level

fte_staff_msoa <- open_trusts |>
  left_join(raw_fte_clean, by = c("trust_code")) |>
  inner_join(lookup_trust_msoa)

# Check missings
fte_staff_msoa |>
  distinct(trust_code, `Provider Primary Inspection Category`, staff_fte) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_missing = sum(is.na(staff_fte)) / n())

fte_staff_msoa_weighted <- fte_staff_msoa |>
  mutate(fte_staff_prop = staff_fte * proportion) |>
  group_by(msoa_code) |>
  summarise(fte_staff_per_msoa = sum(fte_staff_prop))

msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)

# Normalise 
fte_staff_msoa_normalised <- fte_staff_msoa_weighted |>
  left_join(msoa_pop) |>
  mutate(fte_staff_rate = fte_staff_per_msoa / total_population * 100) |>
  select(msoa_code, fte_staff_rate, total_population)

fte_staff_lad <- fte_staff_msoa_normalised  |>
  left_join(lookup_msoa_lad) |>
  calculate_extent(
    var = fte_staff_rate,
    higher_level_geography = lad_code,
    population = total_population
  ) 

fte_staff_lad |>
  group_by(extent) |>
  summarise(count = n()/nrow(fte_staff_lad)) |>
  print(n = Inf)
# 39% : extent = 0
# 0.6%: extent = 1

# Save ----
fte_staff_lad |>
  write_rds("data/capacity/health-inequalities/england/nhs_workforce.rds")
