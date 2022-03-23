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
tf <- download_file("https://www.cqc.org.uk/sites/default/files/01_February_2022_Latest_ratings.ods", "ods")

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
open_trusts <- read_rds("data/open_trust_types.rds")
trust_changes <- read_rds("data/trust_changes.rds")

# Check the matching of CQC scores & trust table in geographr package

# Check of any trusts that are in the CQC ratings but not in open trusts
cqc_nhs_trusts_overall |>
  anti_join(open_trusts)
# missing 4 trusts - "TAD" "TAF" "TAH" "TAJ" (similar to hospital maint. backlog)
# These are in the CQC rating data as 'Mental health - community & residential - NHS'.
# Not found in the PHE data that allows mapping from Trust to MSOA so would not be able to proportion back to LA.

# Check if any trusts in open trusts and not in CQC ratings
missing_trusts <- open_trusts |>
  anti_join(cqc_nhs_trusts_overall)
# 5 Trusts missing R0D, RQF, RT4, RW6, RYT (some same as those missing in hospital maint. backlog)

trust_changes |>
  filter(new_code %in% missing_trusts$trust_code | old_code %in% missing_trusts$trust_code) |>
  arrange(desc(date))
# RYT, RT4 and RQF are not found in the data used to map from Trusts to LA so ignore for the current Trust to LA mapping.
# RW6 has changed to RM3 & R0A (will deal with this at next stage)
# RD3 & RDZ have changed to R0D (but do not have ratings for RD3 or RDZ either)


# Trust to MSOA lookup ----

# Trust to MSOA table only has data for acute trusts
open_trusts |>
  left_join(cqc_nhs_trusts_overall) |>
  left_join(lookup_trust_msoa) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LSOA and proportion to per capita level
rating_joined <- open_trusts |>
  left_join(cqc_nhs_trusts_overall) |>
  inner_join(lookup_trust_msoa)

# RW6 split into RM3 and R0A (which were already established Trusts, not newly created)
# Assume that is a 50:50 split (as don't have data on the split)
rm3_rating <- rating_joined |>
  filter(trust_code == "RM3") |>
  distinct(latest_rating) |>
  pull()

r0a_rating <- rating_joined |>
  filter(trust_code == "R0A") |>
  distinct(latest_rating) |>
  pull()

rm3_add <- rating_joined |>
  filter(trust_code == "RW6") |>
  mutate(proportion = proportion * 0.5) |>
  mutate(trust_code = "RM3") |>
  mutate(latest_rating = rm3_rating)

r0a_add <- rating_joined |>
  filter(trust_code == "RW6") |>
  mutate(proportion = proportion * 0.5) |>
  mutate(trust_code = "R0A") |>
  mutate(latest_rating = r0a_rating)

rating_joined_updated <- rating_joined |>
  filter(trust_code != "RW6") |>
  bind_rows(rm3_add) |>
  bind_rows(r0a_add) |>
  group_by(trust_code, primary_category, latest_rating, msoa_code) |>
  summarise(proportion = sum(proportion)) |>
  ungroup()

# Check missings
rating_joined_updated |>
  distinct(trust_code, primary_category, latest_rating) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_missing = sum(is.na(latest_rating)) / n())

rating_joined_updated |>
  filter(is.na(latest_rating)) |>
  distinct(trust_code)

# R0D is still missing so re-proportion to deal with this
rating_joined_reprop <- rating_joined_updated |>
  filter(!is.na(latest_rating)) |>
  group_by(msoa_code) |>
  mutate(denominator_msoa = sum(proportion)) |>
  mutate(reweighted_proportion = proportion / denominator_msoa)

# Convert ratings numeric and the weight by the proportions of each MSOA pop come from each Trust
rating_msoa <- rating_joined_reprop |>
  mutate(rating_numeric = recode(latest_rating, "Outstanding" = 5, "Good" = 4, "Inadequate" = 2, "Requires improvement" = 1, .default = NA_real_)) |>
  mutate(rating_msoa_weighted = reweighted_proportion * rating_numeric) |>
  group_by(msoa_code) |>
  summarise(weighted_rating = sum(rating_msoa_weighted))

# Check distributions
summary(rating_msoa$weighted_rating)

cqc_nhs_trusts_overall |>
  mutate(rating_numeric = recode(latest_rating, "Outstanding" = 5, "Good" = 4, "Inadequate" = 2, "Requires improvement" = 1, .default = NA_real_)) |>
  select(rating_numeric) |>
  summary()

# Aggregate from MSOA to LA ----
msoa_pop <- population_msoa |>
  select(msoa_code, total_population)

if (
  anti_join(
    filter(lookup_msoa_lad, str_detect(lad_code, "^E")),
    lookup_lad_over_time,
    by = c("lad_code" = "LAD21CD")
  ) |>
    pull(lad_code) |>
    length() != 0
) {
  stop("Lad codes need changing to 2021 - check if 2019 or 2020")
}

rating_lad_lookup <- rating_msoa |>
  select(msoa_code, weighted_rating) |>
  left_join(lookup_msoa_lad) |>
  left_join(lookup_lad_over_time, by = c("lad_code" = "LAD19CD")) |>
  select(msoa_code, weighted_rating, msoa_code, lad_code = LAD21CD)


rating_lad <- rating_lad_lookup |>
  left_join(msoa_pop) |>
  calculate_extent_depreciated(
    var = weighted_rating,
    higher_level_geography = lad_code,
    population = total_population,
    invert_percentiles = FALSE # lower score is worse outcome
  ) |>
  rename(cqc_ratings = extent)

rating_lad |>
  group_by(cqc_ratings) |>
  summarise(count = n() / nrow(rating_lad)) |>
  filter(cqc_ratings %in% c(0, 1))
# 57% : extent = 0
# 3%: extent = 1

# Save ----
rating_lad |>
  write_rds("data/capacity/health-inequalities/england/quality/nhs-trust-cqc-rating.rds")