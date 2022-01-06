# This script joins the trust type (from the CQC rating data) onto the open trust data is geographr package  

# Load packages
library(geographr)
library(tidyverse)

# Downloading CQC rating data as has information on what is the primary type of care trust provides ---
# Landing page of data: https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory
tf <- download_file("https://www.cqc.org.uk/sites/default/files/01_December_2021_Latest_ratings.ods", "ods")

raw_providers <-
  read_ods(
    tf,
    sheet = "Providers",
  )

providers <- raw_providers |>
  distinct(`Provider ID`, `Provider Primary Inspection Category`) 

# Joining onto open trusts geographr data
open_trusts <-
  geographr::points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

open_trust_types <- open_trusts |>
  left_join(providers, by = c("trust_code" = "Provider ID")) |>
  rename(primary_category = `Provider Primary Inspection Category`)

arrow::write_feather(open_trust_types, "R/capacity/health-inequalities/england/trust_calculations/open_trust_types.feather")
