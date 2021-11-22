# Load packages
library(tidyverse)
library(readODS)
library(geographr)
library(readxl)
library(sf)

source("R/utils.R") # for download_file()

# Source page: https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory 
# Download the data
tf <- download_file("https://www.cqc.org.uk/sites/default/files/01_November_2021_Latest_ratings.ods", "ods")

raw_providers <-
  read_ods(
    tf,
    sheet = "Providers"
  )

# Commented out reading in locations and takes very long time
# raw_locations <-
#   read_ods(
#     tf,
#     sheet = "Locations"
#   )


# Investigate the "Provider Type" & "Provider Primary Inspection Category"
raw_providers |>
  distinct(`Provider Type`, `Provider Primary Inspection Category`) |>
  arrange(`Provider Type`)
# To investigate these different categories


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

# NHS TRUST table in geographr package -----

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

# Check the matching of CQC scores & trust table in geographr package --------

# Join CQC scores onto the open trusts table and check for any missing
trusts_missing_cqc_score <- open_trusts |>
  anti_join(cqc_nhs_trusts_overall, by = c("trust_code" = "Provider ID")) |>
  pull(trust_code)

# Check those with no score
points_nhs_trusts |>
  as_tibble() |>
  filter(nhs_trust_code %in% trusts_missing_cqc_score)

# Check of any trusts that are in the CQC ratings but not in open trusts
cqc_score_trusts_not_matched <- cqc_nhs_trusts_overall |>
  anti_join(open_trusts, by = c("Provider ID" = "trust_code")) |>
  pull(`Provider ID`)

# Check not in full trusts table (i.e. maybe closed)
points_nhs_trusts |>
  as_tibble() |>
  filter(nhs_trust_code %in% cqc_score_trusts_not_matched)

# Can't find landing page of the dataset but found through searching missing trust codes
# https://www.england.nhs.uk/wp-content/uploads/2014/11/nhs-non-nhs-ods-codes.xlsx
tf <- download_file("https://www.england.nhs.uk/wp-content/uploads/2014/11/nhs-non-nhs-ods-codes.xlsx", "xlsx")

raw_non_nhs_codes <-
  read_excel(
    tf
  )

raw_non_nhs_codes |>
  filter(Code %in% cqc_score_trusts_not_matched)

# Check deactivated locations data --------

tf <- download_file("https://www.cqc.org.uk/sites/default/files/01_November_2021_Deactivated_locations.ods", "ods")

raw_deactivated <-
  read_ods(
    tf,
    sheet = "Deactivated_Locations"
  )


# Social care orgs -----

cqc_social_overall <- raw_providers |>
  filter(`Provider Type` == "Social Care Org")
# Only 1
