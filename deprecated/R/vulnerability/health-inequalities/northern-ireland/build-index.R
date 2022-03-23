# ---- Method ----
# The build method is adapated from the methods used to develop the Health Index
# for England. As the devolved nations versions of the Health Index are not
# measured across time, several aspects of the construction differ, and mirror
# those found in the Indices of Multiple Deprivation.

# Technical docs:
#   - https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/methodologies/methodsusedtodevelopthehealthindexforengland2015to2018#overview-of-the-methods-used-to-create-the-health-index
#   - https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833951/IoD2019_Technical_Report.pdf

# Steps:
#   1. Scale (align) indicators so that higher value = worse health to align
#      with the other resilience indices (higher = worse).
#   2. Missing step: Apply functional transformations (e.g. log, square) to
#      address skewness in the distributions.
#   3. Normalise to mean of 0 and SD +-1 and then apply MFLA.
#   4. Optional step: Weight the indicators within domains: apply MFLA.
#   5. Calculate domain scores: add together normalised indicator scores
#      (weighted or unweighted) and rank and qunatise.
#   6. Combine domains with equal weighting to produce composite score: rank
#      and quantise output.

# ---- Load libraries and Functions ----
library(tidyverse)
source("R/utils.R")

# ---- Build Healthy Lives Domain ----
# Load indicators
healthy_lives_indicators <-
  load_indicators(
    path = "data/vulnerability/health-inequalities/northern-ireland/healthy-lives",
    key = "lad_code"
  )

# 1. Scale (align) indicators - Higher value = worse health.
healthy_lives_scaled <-
  healthy_lives_indicators |>
  mutate(
    gcse_qualifications_percent = gcse_qualifications_percent * -1
  )

# 3. Weight the indicators within the domain
healthy_lives_weighted <-
  healthy_lives_scaled |>
  normalise_indicators()

# 5. Calculate domain scores
healthy_lives_scores <-
  healthy_lives_weighted |>
  calculate_domain_scores(
    domain_name = "healthy_lives",
    num_quantiles = 10
  )

# ---- Build Healthy People Domain ----
# Load indicators
healthy_people_indicators <-
  load_indicators(
    path = "data/vulnerability/health-inequalities/northern-ireland/healthy-people",
    key = "lad_code"
  )

# 1. Scale (align) indicators - Higher value = worse health.
healthy_people_scaled <-
  healthy_people_indicators |>
  mutate(
    happiness_score_out_of_10 = happiness_score_out_of_10 * -1,
    healthy_life_expectancy = healthy_life_expectancy * -1,
    life_satisfaction_score_out_of_10 = life_satisfaction_score_out_of_10 * -1,
    life_worthwhileness_score_out_of_10 = life_satisfaction_score_out_of_10 * -1
  )

# 3. Weight the indicators within the domain
healthy_people_weighted <-
  healthy_people_scaled |>
  normalise_indicators()

# 5. Calculate domain scores
healthy_people_scores <-
  healthy_people_weighted |>
  calculate_domain_scores(
    domain_name = "healthy_people",
    num_quantiles = 10
  )

# ---- Build Healthy Places Domain ----
# Load indicators
healthy_places_indicators <-
  load_indicators(
    path = "data/vulnerability/health-inequalities/northern-ireland/healthy-places",
    key = "lad_code"
  )

# 1. Scale (align) indicators - Higher value = worse health.
healthy_places_scaled <-
  healthy_places_indicators

# 3. Weight the indicators within the domain
healthy_places_weighted <-
  healthy_places_scaled |>
  normalise_indicators()

# 5. Calculate domain scores
healthy_places_scores <-
  healthy_places_weighted |>
  calculate_domain_scores(
    domain_name = "healthy_places",
    num_quantiles = 10
  )

# ---- Combine Domains ----
# 6. Combine domains with equal weighting to produce composite score
health_inequalities_scores <-
  healthy_lives_scores |>
  left_join(
    healthy_people_scores,
    by = "lad_code"
  ) |>
  left_join(
    healthy_places_scores,
    by = "lad_code"
  ) |>
  select(
    lad_code,
    ends_with("domain_score")
  ) |>
  calculate_composite_score(
    index_name = "health_inequalities",
    num_quantiles = 10
  )

# ---- Create Index ----
# Save only quantiles
health_inequalities_index <-
  healthy_lives_scores |>
  left_join(
    healthy_people_scores,
    by = "lad_code"
  ) |>
  left_join(
    healthy_places_scores,
    by = "lad_code"
  ) |>
  left_join(
    health_inequalities_scores,
    by = "lad_code"
  ) |>
  select(lad_code, ends_with("quantiles"))

write_csv(
  health_inequalities_index,
  "data/vulnerability/health-inequalities/northern-ireland/index-unweighted.csv"
)

# Save a version with all underlying non-normalised indicators
health_inequalities_index_raw_indicators <-
  healthy_lives_indicators |>
  left_join(
    healthy_lives_scores,
    by = "lad_code"
  ) |>
  left_join(
    healthy_people_indicators,
    by = "lad_code"
  ) |>
  left_join(
    healthy_people_scores,
    by = "lad_code"
  ) |>
  left_join(
    healthy_places_indicators,
    by = "lad_code"
  ) |>
  left_join(
    healthy_places_scores,
    by = "lad_code"
  ) |>
  left_join(
    health_inequalities_scores,
    by = "lad_code"
  )

write_csv(
  health_inequalities_index_raw_indicators,
  "data/vulnerability/health-inequalities/northern-ireland/index-unweighted-all-indicators.csv"
)

# Save a version with normalised indicator scores
health_inequalities_index_normalised_indicators <-
  healthy_lives_weighted |>
  left_join(
    healthy_lives_scores,
    by = "lad_code"
  ) |>
  left_join(
    healthy_people_weighted,
    by = "lad_code"
  ) |>
  left_join(
    healthy_people_scores,
    by = "lad_code"
  ) |>
  left_join(
    healthy_places_weighted,
    by = "lad_code"
  ) |>
  left_join(
    healthy_places_scores,
    by = "lad_code"
  ) |>
  left_join(
    health_inequalities_scores,
    by = "lad_code"
  )

write_csv(
  health_inequalities_index_normalised_indicators,
  "data/vulnerability/health-inequalities/northern-ireland/index-unweighted-normalised-indicators.csv"
)