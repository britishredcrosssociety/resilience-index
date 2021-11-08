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

# ---- Build Access & Availability Domain ----
# Load indicators
access_avilability_indicators <-
  load_indicators(
    path = "data/capacity/health-inequalities/northern-ireland/access-availability",
    key = "trust_code"
  )

# 1. Scale (align) indicators - Higher value = Higher capacity.
access_availability_scaled <-
  access_avilability_indicators %>%
  mutate(
    ambulance_response_time_seconds = ambulance_response_time_seconds * -1,
    outpatient_waiting_more_52_weeks_percent = outpatient_waiting_more_52_weeks_percent * -1
  )

# 3. Weight the indicators within the domain
access_availability_weighted <-
  access_availability_scaled %>%
  normalise_indicators()

# 5. Calculate domain scores
access_avialbility_scores <-
  access_availability_weighted %>%
  calculate_domain_scores(
    domain_name = "access_availability",
    num_quantiles = 5
  )