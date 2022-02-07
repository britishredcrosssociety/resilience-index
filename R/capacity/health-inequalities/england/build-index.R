# ---- Method ----
# The build method is adapated from the methods used to develop the Health Index
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

# ---- Load libraries and Functions ----
library(tidyverse)
source("R/utils.R")

# ---- Build Access & Availability Domain ----
# Load indicators
access_availability_indicators <-
  load_indicators(
    path = "data/capacity/health-inequalities/england/access-availability",
    key = "lad_code"
  )

# 1. Scale (align) indicators - Higher value = Higher capacity.
access_availability_scaled <-
  access_availability_indicators |>
  mutate(
    ae_over_4_hours_wait_rate = ae_over_4_hours_wait_rate * -1,
    waiting_over_13_weeks_rate = waiting_over_13_weeks_rate * -1
  )

# How should be the missing data in the 2021 Local Authority areas be handled?
access_availability_indicators |>
  keep_na() |>
  View()















# # ---- TO BE EDITED: ----
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
#     path = "data/capacity/health-inequalities/northern-ireland/workforce",
#     key = "trust_code"
#   )

# # 1. Scale (align) indicators - Higher value = Higher capacity.
# workforce_scaled <-
#   workforce_indicators |>
#   mutate(
#     cancelled_operations_per_1000 = cancelled_operations_per_1000 * -1
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
#     num_quantiles = 5
#   )

# # ---- Build Quality Domain ----
# # Load indicators
# quality_indicators <-
#   load_indicators(
#     path = "data/capacity/health-inequalities/northern-ireland/quality",
#     key = "trust_code"
#   )

# # 1. Scale (align) indicators - Higher value = Higher capacity.
# # Nothin to algin

# # 3. Weight the indicators within the domain
# quality_weighted <-
#   quality_indicators |>
#   normalise_indicators()

# # 5. Calculate domain scores
# quality_scores <-
#   quality_weighted |>
#   calculate_domain_scores(
#     domain_name = "quality",
#     num_quantiles = 5
#   )

# # ---- Save Domain Quantiles ----
# capacity_domain_scores <-
#   access_availability_scores |>
#   left_join(
#     workforce_scores,
#     by = "trust_code"
#   ) |>
#   left_join(
#     quality_scores,
#     by = "trust_code"
#   ) |>
#   select(
#     trust_code,
#     ends_with("_quantiles")
#   )

# capacity_domain_scores |>
#   write_csv("data/capacity/health-inequalities/northern-ireland/capacity-domain-scores.csv")

# # ---- Combine Domains ----
# # 6. Combine domains with equal weighting to produce composite score
# # capacity_scores <-
# #   access_availability_scores |>
# #   left_join(
# #     workforce_scores,
# #     by = "trust_code"
# #   ) |>
# #   left_join(
# #     quality_scores,
# #     by = "trust_code"
# #   ) |>
# #   select(
# #     trust_code,
# #     ends_with("domain_score")
# #   ) |>
# #   rowwise(!where(is.numeric)) |>
# #   summarise(composite_score = sum(c_across(where(is.numeric)))) |>
# #   ungroup() |>
# #   mutate(composite_rank = rank(composite_score)) |>
# #   mutate(composite_quantiles = quantise(composite_rank, 5)) |>
# #   rename_with(
# #     ~ str_c("hi_capacity", .x, sep = "_"),
# #     where(is.numeric)
# #   )

# # TODO: The calculate_composite_scores() function has had to be adpated to get
# # ditinct ranks, but this has places a much higher emphasis on the access and
# # availability domain which has much higher normalised domain scores. What can
# # be done about this?