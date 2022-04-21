# ---- Load ----
library(tidyverse)
library(sf)
library(geographr)
library(demographr)

source("R/utils.R")

raw <- read_sf("data/on-disk/DataZone_Scotland_Heat_Hazard_v1.shp")

raw %>%
  filter(!str_detect(DataZone, "^S"))

# ---- Prep ----
lsoa_pop <- population_dz_20 %>%
  filter(sex == "All") %>%
  select(dz_code, total_population)

heat_hazard_raw <-
  raw |>
  st_drop_geometry() |>
  select(
    dz_code = DataZone,
    mean_temp = mean_std_t
  ) |>
  filter(str_detect(dz_code, "^S"))

# ---- Join ----
heat_hazard_raw_joined <- left_join(heat_hazard_raw, lsoa_pop)

# ---- Compute extent scores ----
calculate_extent <-
  function(data,
           var,
           higher_level_geography,
           population,
           weight_high_scores = TRUE) {
    data <-
      data |>
      mutate(percentile = ntile({{ var }}, 100))
    
    if (weight_high_scores) {
      data <-
        data |>
        mutate(percentile = invert_this(percentile))
    }
    
    data <-
      data |>
      mutate(
        extent = case_when(
          percentile <= 10 ~ {{ population }},
          percentile == 11 ~ {{ population }} * 0.95,
          percentile > 11 & percentile <= 30 ~ {{ population }} * (0.95 - ((0.9 / 19) * (percentile - 11))),
          TRUE ~ 0
        )
      ) |>
      group_by({{ higher_level_geography }}) |>
      summarise(extent = sum(extent) / sum({{ population }}))
    
    if (!weight_high_scores) {
      data <-
        data |>
        mutate(extent = extent * -1)
    }
    
    return(data)
  }


extent <-
  heat_hazard_raw_joined |>
  calculate_extent(
    var = mean_temp,
    higher_level_geography = dz_code,
    population = total_population,
    weight_high_scores = TRUE
  )

# ---- Normalise, rank, & quantise ----
heat_hazard_quantiles <-
  extent |>
  normalise_indicators() |>
  mutate(rank = rank(extent)) |>
  mutate(quantiles = quantise(rank, 5)) |>
  select(dz_code, heat_hazard_quintiles = quantiles)

# ---- Save ----
heat_hazard_quantiles |>
  write_rds("data/shocks/disasters-emergencies/scotland/heat-hazard.rds")
