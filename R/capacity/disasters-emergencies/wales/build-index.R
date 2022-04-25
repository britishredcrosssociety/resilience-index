# ---- Load libraries and Functions ----
library(tidyverse)
source("R/utils.R")

# ---- Load indicators ----
indicators <-
  load_indicators(
    path = "data/capacity/disasters-emergencies/wales",
    key = "lad_code"
  )

# Align direction so that high score = low capacity
indicators_aligned <-
  indicators |>
  mutate(la_spending_power = la_spending_power * -1)

# ---- Build Index ----
de <-
  indicators_aligned |>
  normalise_indicators() |>
  calculate_domain_scores(domain_name = "de")

# Inverting ranks and deciles so that higher scores = higher capacity
de_invert <-
  de |>
  mutate(
    de_domain_rank = inverse_rank(de_domain_rank),
    de_domain_quantiles = invert_this(de_domain_quantiles)
  )

# Save index
de_invert |>
  write_csv("data/capacity/disasters-emergencies/wales/de-index.csv")