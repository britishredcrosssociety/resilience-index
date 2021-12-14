# Load packages
library(tidyverse)
library(readODS)
library(geographr)
library(readxl)
library(sf)

source("R/utils.R") # for download_file() & calculate_extent()
source("R/capacity/health-inequalities/england/trust_types/trust_types.R") # run trust types code

# Source page: https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory
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
  select(`Provider ID`, `Latest Rating`)

# Check distinct mapping i.e. only 1 score for 1 provider due to comment in the rating documentation stating:
# 'In some cases, a location or provider may have had more than one rating published on the same date, and in this case both ratings are shown in this file. It will be necessary to check on the CQC website to see which is the current rating for these organisations.'
cqc_nhs_trusts_overall |>
  group_by(`Provider ID`) |>
  summarise(rating_count = n()) |>
  filter(rating_count > 1)
# Check ok

# Check spread of scores
cqc_nhs_trusts_overall |>
  group_by(`Latest Rating`) |>
  summarise(prop = round(n() / nrow(cqc_nhs_trusts_overall), 2))


# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")


# Check the matching of CQC scores & trust table in geographr package --------

# Join CQC scores onto the open trusts table and check for any missing
trusts_missing_cqc_score <- open_trusts |>
  anti_join(cqc_nhs_trusts_overall, by = c("trust_code" = "Provider ID")) |>
  pull(trust_code)

# Check those with no score
points_nhs_trusts |>
  as_tibble() |>
  filter(nhs_trust_code %in% trusts_missing_cqc_score)
# 5 Trusts with no score - R0D, RQF, RT4, RW6, RYT

# Check of any trusts that are in the CQC ratings but not in open trusts
cqc_score_trusts_not_matched <- cqc_nhs_trusts_overall |>
  anti_join(open_trusts, by = c("Provider ID" = "trust_code")) |>
  pull(`Provider ID`)

cqc_score_trusts_not_matched
# missing 4 trusts ("TAD" "TAF" "TAH" "TAJ")

# Check not in full trusts table (i.e. maybe closed)
points_nhs_trusts |>
  as_tibble() |>
  filter(nhs_trust_code %in% cqc_score_trusts_not_matched)


# Trust to MSOA lookup ----

# Trust to MSOA table only has data for acute trusts
open_trusts |>
  left_join(cqc_nhs_trusts_overall, by = c("trust_code" = "Provider ID")) |>
  left_join(lookup_trust_msoa, by = "trust_code") |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LSOA and proportion to per capita level

rating_msoa <- open_trusts |>
  left_join(cqc_nhs_trusts_overall, by = c("trust_code" = "Provider ID")) |>
  inner_join(lookup_trust_msoa, by = "trust_code")

# Check missings
rating_msoa |>
  distinct(trust_code, `Provider Primary Inspection Category`, `Latest Rating`) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_missing = sum(is.na(`Latest Rating`)) / n())

# TO DO: Look into different method of weighting to more heavily weight poorer performing
# (So have code to amend later will convert ordinal to numeric and average)

rating_msoa_numeric <- rating_msoa |>
  mutate(rating_numeric = recode(`Latest Rating`, "Outstanding" = 5, "Good" = 4, "Inadequate" = 2, "Requires improvement" = 1, .default = NA_real_)) |>
  group_by(msoa_code) |>
  summarise(avg_rating = mean(rating_numeric, na.rm = T), num_trusts = n(), prop_missing = sum(is.na(rating_numeric)) / n())

# Check if any MSOA have a high prop of missing as may be better to keep as NA rather than take single value as the average
rating_msoa_numeric |>
  arrange(desc(prop_missing))

msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)

rating_lad <- rating_msoa_numeric |>
  select(msoa_code, avg_rating) |>
  left_join(lookup_msoa_lad) |>
  left_join(msoa_pop) |>
  calculate_extent(
    var = avg_rating,
    higher_level_geography = lad_code,
    population = total_population
  )

rating_lad |>
  group_by(extent) |>
  summarise(count = n()/nrow(deaths_lad))

# TO DO: Look into different method of weighting to more heavily weight poorer performing
