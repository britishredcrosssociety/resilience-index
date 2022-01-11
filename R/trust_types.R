# This script joins the trust type (from the CQC rating data) onto the open trust data in the geographr package  

# Load libraries 
library(geographr)
library(sf)
library(tidyverse)
library(readODS)

source("R/utils.R") # for download_file()

# Downloading CQC rating data as has information on what is the primary type of care trust provides 
# Landing page of data: https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory
tf <- download_file("https://www.cqc.org.uk/sites/default/files/04_January_2022_Latest_ratings.ods", "ods")

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

write_rds(open_trust_types, "data/open_trust_types.RDS")
