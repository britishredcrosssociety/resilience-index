# Load packages
library(tidyverse)
library(readxl)
library(httr)
library(geographr)
library(sf)

source("R/utils.R") # for download_file()

# Load data ----

# - Night -
tf <- download_file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q4-2020-21-Final-1.xlsx", ".xlsx")

avail_beds_nights_raw <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

# remove first two entries (one is totals, other is blank)
avail_beds_nights_sliced <-
  avail_beds_nights_raw %>%
  slice(-(1:2))

# Select cols
avail_beds_nights_selected <-
  avail_beds_nights_sliced %>%
  select(
    trust_code = `Org Code`,
    avail_night = Total...6,
  )


# - Day -
tf <- download_file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Beds-Open-Day-Only-Web_File-Final-Q4-2020-21-Final-THSDF.xlsx", ".xlsx")

avail_beds_days_raw <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

# remove first two entries (one is totals, other is blank)
avail_beds_days_sliced <-
  avail_beds_days_raw %>%
  slice(-(1:2))

# Select cols
avail_beds_days_selected <-
  avail_beds_days_sliced %>%
  select(
    trust_code = `Org Code`,
    avail_day = Total...6,
  )


# - Join -
avail_beds_joined <-
  avail_beds_nights_selected %>%
  left_join(
    avail_beds_days_selected,
    by = "trust_code"
  )

avail_beds_mean <-
  avail_beds_joined %>%
  rowwise() %>%
  mutate(
    avail_beds = mean(
      c(avail_night, avail_day),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  select(trust_code, avail_beds)

# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- read_rds("data/open_trust_types.rds")

# Check the matching of indicator data & trust table in geographr package
open_trusts |>
  anti_join(avail_beds_mean) |>
  print(n = Inf)
# 26 ambulance or community trusts which may not have beds as service

avail_beds_mean |>
  anti_join(open_trusts) |>
  left_join(points_nhs_trusts, by = c("trust_code" = "nhs_trust_code"))
# 6 missing - 4 of which are TAF, TAJ, TAD, TAH (which appear across other indicators)
# 2 other are old trusts that will have updated codes

# Some of the trusts codes in data are for old trusts which have changed code
# Want to align with the open_trusts file (so only check those returned in the anti_join above)
# Load in trust changes table created in trust_changes.R
trust_changes <- read_rds("data//trust_changes.rds")

old_new_lookup <- avail_beds_mean |>
  anti_join(open_trusts) |>
  rename(old_code = trust_code) |>
  inner_join(trust_changes, by = "old_code") |>
  group_by(new_code) |>
  mutate(new_code_count = n()) |>
  ungroup() |>
  group_by(old_code) |>
  mutate(old_code_count = n()) |>
  ungroup() |>
  mutate(
    split_bed =
      ifelse(old_code_count > 1, avail_beds / old_code_count, avail_beds)
  )

new_trusts <- old_new_lookup |>
  group_by(new_code) |>
  summarise(avail_beds = sum(split_bed)) |>
  rename(trust_code = new_code)

avail_beds_updated <- avail_beds_mean |>
  filter(!trust_code %in% old_new_lookup$old_code) |>
  bind_rows(new_trusts)

# Check duplicates now have updated
avail_beds_updated |>
  group_by(trust_code) |>
  summarise(count = n()) |>
  filter(count > 1)

# Sum any duplicates
avail_beds_updated_combined <- avail_beds_updated |>
  group_by(trust_code) |>
  summarise(avail_beds = sum(avail_beds))

# Check again which trusts are in cost data and not geographr package
avail_beds_updated_combined |>
  anti_join(open_trusts)

# Join trust to LAD lookup --------

lookup_trust_lad <- read_rds("data/lookup_trust_lad.rds")

lookup_trust_lad <- lookup_trust_lad |>
  select(-lad_prop_by_trust)

# Trust to LAD table only has data for acute trusts
open_trusts |>
  left_join(avail_beds_mean) |>
  left_join(lookup_trust_lad) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(lad_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts data proportion these to LAD and calculate per capita level
avail_beds_joined <- open_trusts |>
  left_join(avail_beds_mean) |>
  inner_join(lookup_trust_lad)

# Check missings
avail_beds_joined |>
  distinct(trust_code, primary_category, avail_beds) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_missing = sum(is.na(avail_beds)) / n())
# no missings

avail_beds_lad <- avail_beds_joined |>
  mutate(avail_beds_prop = avail_beds * trust_prop_by_lad) |>
  group_by(lad_code) |>
  summarise(avail_beds_per_lad = sum(avail_beds_prop))

# Check totals
# Will be difference as had to drop staff from non-acute trusts that couldn't map back to LA
sum(avail_beds_lad$avail_beds_per_lad)
sum(avail_beds_updated_combined$avail_beds)


# Normalise for LAD pop ----
lad_pop <- geographr::population_lad |>
  select(lad_code, lad_name, total_population)

avail_beds_msoa_normalised <- avail_beds_lad |>
  left_join(lad_pop) |>
  mutate(avail_beds_rate = avail_beds_per_lad / total_population) |>
  select(lad_code, avail_beds_rate)

# Save ----
avail_beds_msoa_normalised |>
  write_rds("data/capacity/health-inequalities/england/access-availability/bed-availability.rds")