library(geographr)
library(sf)
library(tidyverse)
library(arrow)

# 2019 MSOA pop
msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)

lookup_trust_msoa_full <- geographr::lookup_trust_msoa |>
  left_join(msoa_pop) |>
  rename(msoa_pop = total_population) |>
  mutate(msoa_catchment_for_trust = proportion * msoa_pop) |>
  group_by(trust_code) |>
  mutate(trust_catchment = sum(msoa_catchment_for_trust)) |>
  ungroup() |>
  rename(msoa_prop_by_trust = proportion) |>
  mutate(trust_prop_by_msoa = msoa_catchment_for_trust/trust_catchment)
    
# Note that not all MSOAs have a sum of 100% when total the column msoa_prop_by_trust. 
# Some patients for a given MSOA which can't be attributed to a Trust?
# The raw patient numbers for each MSOA/Trust combo is available in the workings of geographr package

lookup_trust_lad <- lookup_trust_msoa_full |>
  left_join(lookup_msoa_lad) |>
  group_by(trust_code, lad_code, lad_name) |>
  summarise(lad_catchment_for_trust = sum(msoa_catchment_for_trust)) |>
  ungroup() |>
  group_by(trust_code) |>
  mutate(trust_catchment = sum(lad_catchment_for_trust)) |> 
  mutate(trust_prop_by_lad = lad_catchment_for_trust/trust_catchment) |>
  ungroup() |>
  group_by(lad_code) |>
  mutate(lad_pop = sum(lad_catchment_for_trust)) |> 
  mutate(lad_prop_by_trust = lad_catchment_for_trust/lad_pop) |>
  ungroup() |>
  select(lad_code, lad_name, trust_code, trust_prop_by_lad, lad_prop_by_trust)

write_feather(lookup_trust_lad, "R/capacity/health-inequalities/england/trust_types/lookup_trust_lad.feather")
