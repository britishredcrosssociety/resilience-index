# Fire station response time is not included for now (17/03) as don't have LTLA level data 
# (only FRA level). Include again when get this from Gov. 
# ---- Load libraries and Functions ----
library(tidyverse)
library(demographr)

source("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/R/utils.R")

# ---- Load indicators ----
indicators <-
  load_indicators(
    path = "data/capacity/disasters-emergencies/scotland",
    key = "lad_code"
  )

# # Check entry for all 2020 LAD codes
lads_20 <- lookup_postcode_oa11_lsoa11_msoa11_ltla20 |>
   filter(str_detect(ltla20_code, "^S")) |>
   distinct(lad_code = ltla20_code) 

lads_20 |>
  anti_join(indicators, by = "lad_code")

indicators |>
  anti_join(lads_20, by = "lad_code")

# Check for NAs
indicators |>
  dplyr::filter(if_any(everything(), ~is.na(.x))) 


# Align direction so that high score = low capacity 
indicators_aligned <- indicators |>
  mutate(cap_exp_person = cap_exp_person * -1)

# Build Index ----

# Check distribution
indicators_aligned |>
  normalise_indicators() |> 
  summary()

de <-
  indicators_aligned |>
  normalise_indicators() |>
  calculate_domain_scores(domain_name = "de") 

cor(de[,-1])

pca_rec <- recipe(~., data = de[, -1]) %>%
  update_role(name, category, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

pca_prep

# Inverting ranks and deciles so that higher scores = higher capacity
de_invert <- de |>
  mutate(de_domain_rank = inverse_rank(de_domain_rank),
         de_domain_quantiles = invert_this(de_domain_quantiles))

# Save index
de_invert |>
  write_csv("data/capacity/disasters-emergencies/england/de-index.csv")