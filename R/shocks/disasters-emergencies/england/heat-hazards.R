# ---- Load ----
library(tidyverse)
library(sf)
library(geographr)
library(demographr)

source("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/R/utils.R")

raw <-
  read_sf("data/on-disk/heat-hazard-raw/england/LSOA_England_Heat_Hazard_v1.shp")

# ---- Prep ----
lsoa_pop <-
  population_lsoa |>
  select(lsoa_code, total_population)

heat_hazard_raw <-
  raw |>
  st_drop_geometry() |>
  select(
    lsoa_code = LSOA11CD,
    mean_temp = mean_std_t
  ) |>
  filter(str_detect(lsoa_code, "^E"))

lookup_lsoa_lad <-
  lookup_lsoa_msoa |>
  select(ends_with("code")) |>
  filter(str_detect(lsoa_code, "^E")) |>
  left_join(lookup_msoa_lad_21, by = c("msoa_code" = "msoa_11_code")) |>
  select(lsoa_code, lad_code = lad_21_code)

# ---- Join ----
heat_hazard_raw_joined <-
  heat_hazard_raw |>
  left_join(lookup_lsoa_lad) |>
  relocate(lad_code, .after = lsoa_code) |>
  left_join(lsoa_pop) |>
  select(-lsoa_code)

# ---- Compute extent scores ----
extent <-
  heat_hazard_raw_joined |>
  calculate_extent(
    var = mean_temp,
    higher_level_geography = lad_code,
    population = total_population,
    weight_high_scores = TRUE
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
  write_rds("data/shocks/disasters-emergencies/england/heat-hazard.rds")
