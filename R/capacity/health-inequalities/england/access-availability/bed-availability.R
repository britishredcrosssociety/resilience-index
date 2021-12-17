# Load packages
library(tidyverse)
library(readxl)
library(httr)
library(geographr)

source("R/utils.R") # for download_file()

# Load data ----

# - Night -
tf <- download_file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q4-2020-21-Final-1.xlsx", ".xlsx")

avial_beds_nights_raw <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

# remove first two entries (one is totals, other is blank)
avial_beds_nights_sliced <-
  avial_beds_nights_raw %>%
  slice(-(1:2))

# Select cols
avial_beds_nights_selected <-
  avial_beds_nights_sliced %>%
  select(
    trust_code = `Org Code`,
    avail_night = Total...6,
  )


# - Day -
tf <- download_file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/avial_beds-Open-Day-Only-Web_File-Final-Q4-2020-21-Final-THSDF.xlsx", ".xlsx")

avial_beds_days_raw <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

# remove first two entries (one is totals, other is blank)
avial_beds_days_sliced <-
  avial_beds_days_raw %>%
  slice(-(1:2))

# Select cols
avial_beds_days_selected <-
  avial_beds_days_sliced %>%
  select(
    trust_code = `Org Code`,
    avail_day = Total...6,
  )


# - Join -
avial_beds_joined <-
  avial_beds_nights_selected %>%
  left_join(
    avial_beds_days_selected,
    by = "trust_code"
  )

avial_beds_mean <-
  avial_beds_joined %>%
  rowwise() %>%
  mutate(
    avial_beds = mean(
      c(avail_night, avail_day),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  select(trust_code, avial_beds)

# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")

# Check the matching of indicator data & trust table in geographr package 
open_trusts |>
  anti_join(avial_beds_mean) |>
  print(n = Inf)
# 26 ambulance or community trusts which may not have beds as service

avial_beds_mean |>
  anti_join(open_trusts) |>
  left_join(geographr::points_nhs_trusts, by = c("trust_code" = "nhs_trust_code"))
# 6 missing - 2 of which are closed, 4 remaining TAF, TAJ, TAD, TAH

# Join trust to LAD lookup --------

lookup_trust_lad <- read_feather("R/capacity/health-inequalities/england/trust_types/lookup_trust_lad.feather")

# Trust to LAD table only has data for acute trusts
open_trusts |>
  left_join(avial_beds_mean) |>
  left_join(lookup_trust_lad) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(lad_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts data proportion these to LAD and calculate per capita level
avial_beds_joined <- open_trusts |>
  left_join(avial_beds_mean) |>
  inner_join(lookup_trust_lad)

# Check missings
avial_beds_joined |>
  distinct(trust_code, `Provider Primary Inspection Category`, avial_beds) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_missing = sum(is.na(avial_beds)) / n())

avial_beds_lad <- avial_beds_joined |>
  mutate(avial_beds_prop = avial_beds * trust_prop_by_lad) |>
  group_by(lad_code) |>
  summarise(avial_beds_per_lad = sum(avial_beds_prop))

# Check totals
# Will be difference as had to drop staff from non-acute trusts that couldn't map back to LA
sum(avial_beds_lad$avial_beds_per_lad)
sum(avial_beds_mean$avial_beds)


# Normalise for LAD pop ----
lad_pop <- geographr::population_lad |>
  select(lad_code, lad_name, total_population)

avial_beds_msoa_normalised <- avial_beds_lad |>
  left_join(lad_pop) |>
  mutate(avial_beds_rate = avial_beds_per_lad / total_population * 100) |>
  select(lad_code, avial_beds_rate)

# Save ----
avial_beds_msoa_normalised |>
  write_rds("data/capacity/health-inequalities/england/bed-availability.rds")
