# ---- Load libs & funs ----
library(tidyverse)

source("functions/utils.R")

# ---- Individual Vulnerability Scores ----
# Load and join indicators
vuln_ind_raw <-
  load_indicators(
    "indices/disasters-emergencies/england/ltla/vulnerability-individual/data",
    key = "lad_code"
  )

# Align indicators
vuln_ind_aligned <-
  vuln_ind_raw |>
  mutate(digital_extent = digital_extent * -1)

# Normalise indicators
vuln_ind_normalised <-
  vuln_ind_aligned |>
  normalise_indicators(ignore_nas = TRUE)

# Compute domain scores
vuln_ind_domain <-
  vuln_ind_normalised |>
  calculate_domain_scores(
    domain_name = "individual",
    ignore_nas = TRUE
  )

# ---- Community Vulnerability Scores ----
# Load and join indicators
vuln_com_raw <-
  load_indicators(
    "indices/disasters-emergencies/england/ltla/vulnerability-community/data",
    key = "lad_code"
  )

# Align indicators
vuln_com_aligned <-
  vuln_com_raw |>
  mutate(cps_millions = cps_millions * -1)

# Normalise indicators
vuln_com_normalised <-
  vuln_com_aligned |>
  normalise_indicators(ignore_nas = TRUE)

# Compute domain scores
vuln_com_domain <-
  vuln_com_normalised |>
  calculate_domain_scores(
    domain_name = "community",
    ignore_nas = TRUE
  )

# ---- Compute Overall Vulnerability Score ----
# Join domains
vuln_overall_joined <-
  vuln_ind_domain |>
  left_join(vuln_com_domain)

# Normalise ranks to between 0-1
vuln_overall_normalised <-
  vuln_overall_joined |>
  select(
    lad_code,
    ends_with("_rank")
  ) |>
  mutate(
    across(
      ends_with("rank"),
      scale_ranks
    )
  )

# Transform normalised ranks to exponential distribution
vuln_overall_transformed <-
  vuln_overall_normalised |>
  mutate(
    across(
      ends_with("_rank"),
      exp_transform
    )
  )

# Sum transformed indicators with equal weighting
vuln_overall_summed <-
  vuln_overall_transformed |>
  rowwise(lad_code) |>
  summarise(
    overall_score = sum(
      c_across(
        ends_with("_rank")
      )
    )
  ) |>
  ungroup()

# Rank scores
vuln_overall_ranked <-
  vuln_overall_summed |>
  mutate(
    overall_rank = rank(overall_score)
  )

# Quantise scores
vuln_overall_quantised <-
  vuln_overall_ranked |>
  mutate(
    overall_quantiles = quantise(
      overall_rank,
      num_quantiles = 5
    )
  )

# ---- Join All Vulnerability Scores ----
vuln_all <-
  vuln_overall_quantised |>
  left_join(vuln_ind_domain) |>
  left_join(vuln_com_domain)

# ---- Shocks ----
# Load
shocks_raw <-
  load_indicators(
    "indices/disasters-emergencies/england/ltla/shocks/data",
    key = "lad_code"
  )

# Rank and quantise fires and floods
shocks <-
  shocks_raw |>
  mutate(
    fire_rank = rank_na_first(fire_count_per_pop),
    flood_rank = prop_pop_flood_risk
  ) |>
  select(
    -fire_count_per_pop,
    -prop_pop_flood_risk
  ) |>
  mutate(
    fire_quantiles = quantise(fire_rank, 5),
    flood_quantiles = quantise(flood_rank, 5)
  ) |>
  select(
    lad_code,
    heat_hazard_quantiles = heat_hazard_quintiles,
    fire_quantiles,
    flood_quantiles
  )

# ---- Join All Scores ----
all_scores <-
  vuln_all |>
  left_join(shocks)

write_csv(
  all_scores,
  "indices/disasters-emergencies/england/ltla/de-england-ltla-index.csv"
)