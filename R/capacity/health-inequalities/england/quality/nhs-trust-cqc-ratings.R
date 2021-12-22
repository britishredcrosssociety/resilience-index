# Load packages
library(tidyverse)
library(readODS)
library(geographr)
library(readxl)
library(sf)

source("R/utils.R") # for download_file() 

# Source page: https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory
# Info on how is rated https://www.cqc.org.uk/what-we-do/how-we-do-our-job/how-we-rate-trusts-their-use-resources
# Download the data
tf <- download_file("https://www.cqc.org.uk/sites/default/files/01_December_2021_Latest_ratings.ods", "ods")

raw_providers <-
  read_ods(
    tf,
    sheet = "Providers"
  )

# Check social care orgs -----

raw_providers |>
  filter(`Provider Type` == "Social Care Org")
# Only 1 provider in data


# NHS trusts CQC ratings -----

# 1. Filter only `Provider Type` for only NHS Trusts
# 2. Filter only the overall ratings (as also provides broken down ratings - more info here: https://www.cqc.org.uk/guidance-providers/nhs-trusts/your-ratings-nhs-trusts)
# 3. Filter only the "Overall" category for `Service / Population Group` (there is many breakdown categories e.g. Critical care, Community health services for adults, Urgent care centre)
#     it's the  equivalent to filtering `Report type` == "Provider" (the breakdown categories are `Report type` == "CoreService")
cqc_nhs_trusts_overall <- raw_providers |>
  filter(`Provider Type` == "NHS Healthcare Organisation") |>
  filter(Domain == "Overall") |>
  filter(`Service / Population Group` == "Overall") |>
  select(trust_code = `Provider ID`, latest_rating = `Latest Rating`) 

# Check distinct mapping i.e. only 1 score for 1 provider due to comment in the rating documentation stating:
# 'In some cases, a location or provider may have had more than one rating published on the same date, and in this case both ratings are shown in this file. It will be necessary to check on the CQC website to see which is the current rating for these organisations.'
cqc_nhs_trusts_overall |>
  group_by(trust_code) |>
  summarise(rating_count = n()) |>
  filter(rating_count > 1)
# Check ok

# Check spread of scores
cqc_nhs_trusts_overall |>
  group_by(latest_rating) |>
  summarise(prop = round(n() / nrow(cqc_nhs_trusts_overall), 2))


# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")


# Check the matching of CQC scores & trust table in geographr package 

# Check of any trusts that are in the CQC ratings but not in open trusts
cqc_nhs_trusts_overall |>
  anti_join(open_trusts)
# missing 4 trusts - "TAD" "TAF" "TAH" "TAJ" (similar to hospital maint. backlog)

# Check if any trusts in CQC scores not in open trusts
open_trusts |>
  anti_join(cqc_nhs_trusts_overall) 
# 5 Trusts missing R0D, RQF, RT4, RW6, RYT (similar to hospital maint. backlog)


# Trust to MSOA lookup ----

# Trust to MSOA table only has data for acute trusts
open_trusts |>
  left_join(cqc_nhs_trusts_overall) |>
  left_join(lookup_trust_msoa) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LSOA and proportion to per capita level
rating_joined <- open_trusts |>
  left_join(cqc_nhs_trusts_overall) |>
  inner_join(lookup_trust_msoa)

# Check missings
rating_joined |>
  distinct(trust_code, `Provider Primary Inspection Category`, latest_rating) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_missing = sum(is.na(latest_rating)) / n())

# Convert ratings numeric and the weight by the proportions of each MSOA pop come from each Trust
rating_msoa <- rating_joined |>
  mutate(rating_numeric = recode(latest_rating, "Outstanding" = 5, "Good" = 4, "Inadequate" = 2, "Requires improvement" = 1, .default = NA_real_)) |>
  mutate(rating_msoa_weighted = proportion * rating_numeric) |>
  group_by(msoa_code) |>
  summarise(weighted_rating = sum(rating_msoa_weighted))

# Check distribtions
summary(rating_msoa$weighted_rating)

cqc_nhs_trusts_overall |>
  mutate(rating_numeric = recode(latest_rating, "Outstanding" = 5, "Good" = 4, "Inadequate" = 2, "Requires improvement" = 1, .default = NA_real_)) |>
  select(rating_numeric) |>
  summary()

# Aggregate from MSOA to LA ----

msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)

rating_lad <- rating_msoa |>
  select(msoa_code, weighted_rating) |>
  left_join(lookup_msoa_lad) |>
  left_join(msoa_pop) |>
  calculate_extent(
    var = weighted_rating,
    higher_level_geography = lad_code,
    population = total_population,
    invert_percentiles = FALSE # lower score is worse outcome
  )

rating_lad |>
  group_by(extent) |>
  summarise(count = n()/nrow(rating_lad)) |>
  print(n = Inf)
# 58% : extent = 0
# 2%: extent = 1

# Save ----
rating_lad |>
  write_rds("data/capacity/health-inequalities/england/nhs-trust-cqc-rating.rds")

