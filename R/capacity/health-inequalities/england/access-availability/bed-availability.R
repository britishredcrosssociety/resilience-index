# Load packages
library(tidyverse)
library(readxl)
library(httr)
library(geographr)

source("R/utils.R")
source("R/capacity/health-inequalities/england/trust_types/trust_types.R") # run trust types code to create open_trust_types.feather

# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")


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
    available_night = Total...6,
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
    available_day = Total...6,
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
    avial_beds_available = mean(
      c(available_night, available_day),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  select(trust_code, avial_beds_available)

# Join trust to MSOA lookup --------

# Trust to MSOA table only has data for acute trusts
open_trusts |>
  left_join(avial_beds_mean) |>
  left_join(lookup_trust_msoa) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LSOA and proportion to per capita level

avial_beds_weights <- open_trusts |>
  left_join(avial_beds_mean) |>
  inner_join(lookup_trust_msoa) |>
  mutate(avial_beds_prop = avial_beds_available * proportion)

# Check missings
avial_beds_weights |>
  distinct(trust_code, `Provider Primary Inspection Category`, avial_beds_available) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_missing = sum(is.na(avial_beds_available)) / n())

avial_beds_msoa <- avial_beds_weights |>
  group_by(msoa_code) |>
  summarise(avial_beds_per_msoa = sum(avial_beds_available))

# Check distributions
summary(avial_beds_msoa)
summary(avial_beds_mean)

msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)


# Normalise
avial_beds_msoa_normalised <- avial_beds_msoa |>
  left_join(msoa_pop) |>
  mutate(avial_beds_rate = avial_beds_per_msoa / total_population * 100) |>
  select(msoa_code, avial_beds_rate, total_population)


avial_beds_lad <- avial_beds_msoa_normalised |>
  left_join(lookup_msoa_lad) |>
  calculate_extent(
    var = avial_beds_rate,
    higher_level_geography = lad_code,
    population = total_population
  )

avial_beds_lad |>
  group_by(extent) |>
  summarise(count = n() / nrow(avial_beds_lad)) |>
  print(n = Inf)
# 27% : extent = 0
# 0%: extent = 1

# Save ----
avial_beds_lad |>
  write_rds("data/capacity/health-inequalities/england/avial_avial_beds-availability.rds")
