library(tidyverse)
library(readxl)
library(covid19.nhs.data)
library(httr)
library(geographr)

source("R/utils.R")
source("R/capacity/health-inequalities/england/trust_types/trust_types.R") # run trust types code to create open_trust_types.feather

# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")



# - Night -
tf <- download_file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Beds-Open-Overnight-Web_File-Final-Q4-2020-21-Final-THSDF.xlsx", ".xlsx")

beds_nights_raw <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

# remove first two entries (one is totals, other is blank)
beds_nights_sliced <-
  beds_nights_raw %>%
  slice(-(1:2))

# Select cols
beds_nights_selected <-
  beds_nights_sliced %>%
  select(
    trust_code = `Org Code`,
    percentage_occupied_night = Total...18,
  )

# Replace '-' character with NA
beds_nights_na <-
  beds_nights_selected %>%
  mutate(
    percentage_occupied_night = str_replace_all(
      percentage_occupied_night,
      "-",
      NA_character_
    )
  )

# Change col to double
beds_nights_formatted <-
  beds_nights_na %>%
  mutate(
    percentage_occupied_night = as.double(percentage_occupied_night)
  )

# - Day -
tf <- download_file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Beds-Open-Day-Only-Web_File-Final-Q4-2020-21-Final-THSDF.xlsx", ".xlsx")

beds_days_raw <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

# remove first two entries (one is totals, other is blank)
beds_days_sliced <-
  beds_days_raw %>%
  slice(-(1:2))

# Select cols
beds_days_selected <-
  beds_days_sliced %>%
  select(
    trust_code = `Org Code`,
    percentage_occupied_day = Total...18,
  )

# Replace '-' character with NA
beds_days_na <-
  beds_days_selected %>%
  mutate(
    percentage_occupied_day = str_replace_all(
      percentage_occupied_day,
      "-",
      NA_character_
    )
  )

# Change cols to double
beds_days_formatted <-
  beds_days_na %>%
  mutate(
    percentage_occupied_day = as.double(percentage_occupied_day)
  )

# - Join -
beds_joined <-
  beds_nights_formatted %>%
  left_join(
    beds_days_formatted,
    by = "trust_code"
  )

beds_mean <-
  beds_joined %>%
  rowwise() %>%
  mutate(
    beds_occupied = mean(
      c(percentage_occupied_night, percentage_occupied_day),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  select(trust_code, beds_occupied) %>%
  drop_na()


# Join trust to MSOA lookup --------

# Trust to MSOA table only has data for acute trusts
open_trusts |>
  left_join(beds_mean) |>
  left_join(lookup_trust_msoa) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LSOA and proportion to per capita level

beds_weights <- open_trusts |>
  left_join(beds_mean) |>
  inner_join(lookup_trust_msoa) |>
  mutate(beds_prop = beds_occupied * proportion) 

beds_msoa <- beds_weights |>
  group_by(msoa_code) |>
  summarise(beds_per_msoa = sum(beds_prop))

# Check distributions
summary(beds_msoa)
summary(beds_mean)

msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)

beds_lad <- beds_msoa  |>
  left_join(lookup_msoa_lad) |>
  left_join(msoa_pop) |>
  calculate_extent(
    var = beds_per_msoa,
    higher_level_geography = lad_code,
    population = total_population
  ) 

# TO DO: Ask if proportion of available better measure than the number of available beds proportioned to population
# since could be a high prop but a low number
