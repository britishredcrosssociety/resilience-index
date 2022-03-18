# ---- Load ----
library(tidyverse)
library(sf)
library(geographr)

source("R/utils.R")

raw <-
  read_sf(
    "data/on-disk/heat-hazard-raw/DataZone_Scotland_Heat_Hazard_v1.shp"
  )

# ---- Prep ----
dz_pop <-
  population_dz |>
  filter(sex == "All") |>
  select(dz_code, total_population)

heat_hazard_raw <-
  raw |>
  st_drop_geometry() |>
  select(
    dz_code = DataZone,
    mean_temp = mean_std_t
  )

lookup_dz_lad <-
  lookup_dz_iz_lad |>
  distinct(dz_code, lad_code)

# ---- Join ----
heat_hazard_raw_joined <-
  heat_hazard_raw |>
  left_join(lookup_dz_lad) |>
  relocate(lad_code, .after = dz_code) |>
  left_join(dz_pop) |>
  select(-dz_code)

# ---- Compute extent scores ----
extent <-
  heat_hazard_raw_joined |>
  calculate_extent_depreciated(
    var = mean_temp,
    higher_level_geography = lad_code,
    population = total_population
  )

# ---- Normalise, rank, & quantise ----
heat_hazard_quantiles <-
  extent |>
  normalise_indicators() |>
  mutate(rank = rank(extent)) |>
  mutate(quantiles = quantise(rank, 5)) |>
  select(lad_code, heat_hazard_quintiles = quantiles)

# ---- Save ----
heat_hazard_quantiles |>
  write_rds("data/vulnerability/disasters-emergencies/scotland/heat-hazard.rds")