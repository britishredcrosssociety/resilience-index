# Fire station response time is not included for now (17/03) as don't have LTLA level data 
# (only FRA level). Include again when get this from Gov. 

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

# Check entry for all 2019 LAD codes
# lads_19 <- lookup_lad_counties_19 |>
#   filter(str_detect(lad_19_code, "^E")) |>
#   distinct(lad_code = lad_19_code)
# 
# lads_19 |>
#   anti_join(indicators, by = "lad_code")
# 
# indicators |>
#   anti_join(lads_19, by = "lad_code")
# 
# indicators |>
#   dplyr::filter(if_any(everything(), ~is.na(.x)))
# # City of London & Isle of Scilly are both missing civic assests & engagement
# # Assumption: drop these LADs due to too many missing variables. 

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

# Inverting ranks and deciles so that higher scores = higher capacity
de_invert <- de |>
  mutate(de_domain_rank = inverse_rank(de_domain_rank),
         de_domain_quantiles = invert_this(de_domain_quantiles))

# Save 
de_invert |>
  write_csv("data/capacity/disasters-emergencies/england/de-index.csv")

# Compare to 2021 LAD calcs ----
de_2021 <- read_csv("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/96183f7994c36f875318cffcd1847245663e29e8/data/capacity/disasters-emergencies/england/de-index.csv")

de_2021 <- de_2021 |>
  rename_with(~paste0(., "_21"), where(is.numeric))

de_2019 <- de_invert |>
  rename_with(~paste0(., "_19"), where(is.numeric))

# Only compare LADs in both years
de_compare <- de_2019 |> 
  inner_join(de_2021, by = "lad_code") |>
  select(lad_code, de_domain_rank_19, de_domain_quantiles_19, de_domain_rank_21, de_domain_quantiles_21) |>
  mutate(rank_change = de_domain_rank_21 - de_domain_rank_19,
         quintiles_change = de_domain_quantiles_21 - de_domain_quantiles_19)

de_compare |> 
  group_by(rank_change) |>
  summarise(count = n(), 
            prop = n() / nrow(de_compare))

de_compare |> 
  group_by(quintiles_change) |>
  summarise(count = n(), 
            prop = n() / nrow(de_compare))

de_2019 |> 
  anti_join(de_2021, by = "lad_code")

de_2021 |> 
  anti_join(de_2019, by = "lad_code")

# Build index for VCSEP Risk Indicator Tool (Quintiles not Deciles) -----
de_quint <- indicators_aligned |>
  normalise_indicators() |>
  calculate_domain_scores(domain_name = "de", num_quantiles = 5) 

# Inverting ranks and deciles so that higher scores = higher capacity
de_quint_invert <- de_quint |>
  mutate(de_domain_rank = inverse_rank(de_domain_rank),
         de_domain_quantiles = invert_this(de_domain_quantiles))

# Save 
de_quint_invert |>
  select(lad_code, de_domain_quantiles) |>
  write_csv("data/capacity/disasters-emergencies/england/de-index-quint.csv")




