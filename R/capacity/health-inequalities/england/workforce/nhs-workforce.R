# Load packages
library(tidyverse)
library(geographr)
library(sf)
library(readxl)

source("R/utils.R") # for download_file()

# Load data ----

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

fte_clean <- raw_fte |>
  rename("trust_code" = "...4", "trust_name" = "...3", "staff_fte" = "Professionally qualified clinical staff") |>
  filter(!is.na(trust_code)) %>%
  select(trust_code, trust_name, staff_fte)

# There is rows in the data for CCGs. These make up 0.7% of the total staff in the data and currently don't have a method to map these back to MSOA/LA level.
fte_clean |>
  mutate(trust_flag = str_detect(trust_name, "NHS Trust|NHS Foundation Trust")) |>
  mutate(ccg_flag = str_detect(trust_name, "CCG")) |>
  group_by(trust_flag, ccg_flag) |>
  summarise(count = n(), total_staff_fte = sum(staff_fte), prop = sum(staff_fte) / sum(fte_clean$staff_fte))

# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- read_rds("data/open_trust_types.rds")

# Check the matching of indicator data & trust table in geographr package 
open_trusts |>
  anti_join(fte_clean)
# 3 trusts missing from staff data

fte_clean |>
  anti_join(open_trusts)
# 83 are CCG and don't currently have a way to map these to MSOA/LA (as detailed above)

fte_clean |>
  anti_join(open_trusts) |>
  filter(str_detect(trust_name, "(?i)trust"))
# 4 Trusts TAD, TAF, TAH, TAJ (similar to other indicators)

# Join trust to LAD lookup --------

lookup_trust_lad <- read_rds("data/lookup_trust_lad.rds")

lookup_trust_lad <- lookup_trust_lad |>
  select(-lad_prop_by_trust)

# Trust to LAD table only has data for acute trusts
open_trusts |>
  left_join(fte_clean, by = c("trust_code")) |>
  left_join(lookup_trust_lad) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(lad_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to LAD
# For the acute trusts data proportion these to LAD and calculate per capita level
fte_staff_joined <- open_trusts |>
  left_join(fte_clean, by = c("trust_code")) |>
  inner_join(lookup_trust_lad)

# Check missings
fte_staff_joined |>
  distinct(trust_code, primary_category, staff_fte) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_missing = sum(is.na(staff_fte)) / n())
# no missing

fte_staff_lad <- fte_staff_joined |>
  mutate(fte_staff_prop = staff_fte * trust_prop_by_lad) |>
  group_by(lad_code, lad_name) |>
  summarise(fte_staff_per_lad = sum(fte_staff_prop)) |>
  ungroup()

# Checking totals at each stage 
# Will be difference as had to drop staff from non-acute trusts that couldn't map back to LA
sum(fte_clean$staff_fte)
sum(fte_staff_lad$fte_staff_per_lad)


# Normalise for LAD pop ----
lad_pop <- geographr::population_lad |>
  select(lad_code, lad_name, total_population)

fte_staff_lad_normalised <- fte_staff_lad |>
  left_join(lad_pop) |>
  mutate(fte_staff_rate = fte_staff_per_lad / total_population) |>
  select(lad_code, fte_staff_rate)

# Save ----
fte_staff_lad_normalised |>
  write_rds("data/capacity/health-inequalities/england/workforce/nhs-workforce.rds")
