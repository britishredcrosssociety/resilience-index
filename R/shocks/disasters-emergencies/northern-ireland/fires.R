library(tidyverse)
library(readxl)
library(geographr)
library(sf)

source("R/utils.R")

pop <-
  population_soa |>
  filter(sex == "All") |>
  select(
    soa_name,
    soa_code,
    total_population
  )

lookup <-
  lookup_sa_soa_lgd |>
  select(
    soa_code,
    lgd_code
  ) |>
  distinct()

fires_20 <-
  read_excel(
    "data/on-disk/ni-fires.xlsx",
    sheet = "Major Dwellings Fires 2020",
    range = "A2:B510"
  ) |>
  rename(
    soa_name = `Super Output Area`,
    num_fires_20 = `Number of Major Fires - Dwelling`
  )

fires_21 <-
  read_excel(
    "data/on-disk/ni-fires.xlsx",
    sheet = "Major Dwellings Fires 2021",
    range = "A2:B391"
  ) |>
  rename(
    soa_name = `Super Output Area`,
    num_fires_21 = `Number of Major Fires - Dwelling`
  )

fires_total <-
  fires_20 |>
  full_join(fires_21) |>
  rowwise() |>
  mutate(
    total_fires = sum(
      c_across(starts_with("num_")),
      na.rm = TRUE
    )
  ) |>
  select(
    soa_name,
    total_fires
  ) |>
  ungroup()

fires_joined <-
  fires_total |>
  left_join(pop) |>
  left_join(lookup) |>
  rename(lad_code = lgd_code)

fires_extent <-
  fires_joined |>
  calculate_extent_depreciated(
    var = total_fires,
    higher_level_geography = lad_code,
    population = total_population
  ) |>
  rename(fires_extent = extent)

fires_extent |>
  write_rds("data/shocks/disasters-emergencies/northern-ireland/fires.rds")