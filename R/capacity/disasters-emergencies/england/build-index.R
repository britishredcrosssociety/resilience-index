# ---- Load libraries and Functions ----
library(tidyverse)
library(demographr)

source("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/R/utils.R")

# ---- Load indicators ----
indicators <-
  load_indicators(
    path = "data/capacity/disasters-emergencies/england",
    key = "lad_code"
  )

# Check entry for all 2021 LAD codes
lads_21 <- lookup_lad_counties_21 |>
  filter(str_detect(lad_21_code, "^E")) |>
  distinct(lad_code = lad_21_code) 

lads_21 |>
  anti_join(indicators, by = "lad_code")

indicators |>
  anti_join(lads_21, by = "lad_code")

indicators |>
  dplyr::filter(if_any(everything(), ~is.na(.x))) 
# City of London & Isle of Scilly are both missing civic assests & engagement
# Isle of Scilly also missing station response time
# Assumption: drop these LADs due to too many missing variables. 

indicators_complete <- indicators |>
  drop_na()

# Align direction so that high score = low capacity 
indicators_aligned <- indicators_complete |>
  mutate(cps_millions = cps_millions * -1)

# ---- Build Index ----

# Check distribution
indicators_aligned |>
  normalise_indicators() |> 
  summary()

de <-
  indicators_aligned |>
  normalise_indicators() |>
  calculate_domain_scores(domain_name = "de") 

# Inverting ranks and deciles so that higher scores equal higher capacity
de_invert <- de |>
  mutate(de_domain_rank = inverse_rank(de_domain_rank),
         de_domain_quantiles = invert_this(de_domain_quantiles))


# Check removing fire response time since at FRA level ----
de_no_fire <-
  indicators_aligned |>
  select(-station_response_time) |>
  normalise_indicators() |>
  calculate_domain_scores(domain_name = "de") |>
  select(lad_code, de_domain_rank_no_fire = de_domain_rank, de_domain_quantiles_no_fire = de_domain_quantiles)

de_no_fire_change <- de |>
  select(-de_domain_score) |> 
  left_join(de_no_fire, by = "lad_code") |>
  mutate(rank_change = de_domain_rank_no_fire - de_domain_rank,
         quantile_change = de_domain_quantiles_no_fire - de_domain_quantiles) |> 
  select(lad_code, rank_change, quantile_change)

de_no_fire_change |> 
  summary()

de_no_fire_change |> 
  group_by(quantile_change) |>
  summarise(prop_of_lads = n()/nrow(de_no_fire_change))

de_no_fire_change |> 
  group_by(rank_change) |>
  summarise(prop_of_lads = n()/nrow(de_no_fire_change))

indicators_aligned |> 
  select(-lad_code) |>
  ggpairs()

indicators_aligned |>
  normalise_indicators() |>
  select(-lad_code) |>
  ggpairs()



