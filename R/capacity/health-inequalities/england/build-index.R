# ---- Method ----
# The build method is adapted from the methods used to develop the Health Index
# for England. As the devolved nations versions of the Health Index are not
# measured across time, several aspects of the construction differ, and mirror
# those found in the Indices of Multiple Deprivation.

# Technical docs:
#   - https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/methodologies/methodsusedtodevelopthehealthindexforengland2015to2018#overview-of-the-methods-used-to-create-the-health-index
#   - https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833951/IoD2019_Technical_Report.pdf

# Steps:
#   1. Scale (align) indicators so that higher value = higher capacity.
#   2. Missing step: Apply functional transformations (e.g. log, square) to
#      address skewness in the distributions.
#   3. Normalise to mean of 0 and SD +-1.
#   4. Optional step: Weight the indicators within domains: apply MFLA.
#   5. Calculate domain scores: add together normalised indicator scores
#      (weighted or unweighted) and rank and qunatise.
#   6. Combine domains with equal weighting to produce composite score: rank
#      and quantise output.









# ******************************************************************************
# TODO: Before the index can be built, the scaling of indicators needs
#       establishing. Currently some indicators use `calculate_extent()`, and 
#       set 1 to equal pockets of low capacity, meaning they would need scaling
#       in the build script by multiplying by negative one. Does this make
#       sense, and if not, where should the inversion/scaling take place?
# ******************************************************************************

# # ---- Load libraries and Functions ----
# library(tidyverse)
# source("R/utils.R")

# # ---- Build Access & Availability Domain ----
# # Load indicators
# access_availability_indicators <-
#   load_indicators(
#     path = "data/capacity/health-inequalities/england/access-availability",
#     key = "lad_code"
#   )

# # 1. Scale (align) indicators - Higher value = Higher capacity.
# access_availability_scaled <-
#   access_availability_indicators |>
#   mutate(
#     ae_over_4_hours_wait_rate = ae_over_4_hours_wait_rate * -1,
#     waiting_over_13_weeks_rate = waiting_over_13_weeks_rate * -1
#   )

# # 3. Weight the indicators within the domain
# access_availability_weighted <-
#   access_availability_scaled |>
#   normalise_indicators()

# # 5. Calculate domain scores
# access_availability_scores <-
#   access_availability_weighted |>
#   calculate_domain_scores(
#     domain_name = "access_availability",
#     num_quantiles = 5
#   )

# # ---- Build Workforce Domain ----
# # Load indicators
# workforce_indicators <-
#   load_indicators(
#     path = "data/capacity/health-inequalities/england/workforce/",
#     key = "lad_code"
#   )

# # 1. Scale (align) indicators - Higher value = Higher capacity.
# workforce_scaled <-
#   workforce_indicators |>
#   mutate(
#     vacancy = vacancy * -1
#   )

# # 3. Weight the indicators within the domain
# workforce_weighted <-
#   workforce_scaled |>
#   normalise_indicators()

# # 5. Calculate domain scores
# workforce_scores <-
#   workforce_weighted |>
#   calculate_domain_scores(
#     domain_name = "workforce",
#     num_quantiles = 10
#   )

# # # ---- Build Quality Domain ----
# # Load indicators
# quality_indicators <-
#   load_indicators(
#     path = "data/capacity/health-inequalities/england/quality/",
#     key = "lad_code"
#   )

# # 1. Scale (align) indicators - Higher value = Higher capacity.
# quality_scaled <-
#   quality_indicators |>
#   mutate(
# # Add indicators here that need scaling
#   )

# # 3. Weight the indicators within the domain
# quality_weighted <-
#   quality_scaled |>
#   normalise_indicators()

# # 5. Calculate domain scores
# quality_scores <-
#   quality_weighted |>
#   calculate_domain_scores(
#     domain_name = "quality",
#     num_quantiles = 10
#   )

# # ---- Combine Domains ----
# # 6. Combine domains with equal weighting to produce composite score
# capacity_domain_scores <-
#   access_availability_scores |>
#   left_join(
#     workforce_scores,
#     by = "lad_code"
#   ) |>
#   left_join(
#     quality_scores,
#     by = "lad_code"
#   ) |>
#   select(
#     lad_code,
#     ends_with("domain_score")
#   ) |> 
#   calculate_composite_score(
#     index_name = "health_inequalities",
#     num_quantiles = 10
#   )

# write_csv(
#   capacity_domain_score,
#   "data/vulnerability/health-inequalities/england/index-unweighted.csv"
# )