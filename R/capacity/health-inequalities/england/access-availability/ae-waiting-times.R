# Load libs
library(tidyverse)
library(httr)
library(readxl)
library(sf)
library(geographr)

source("R/utils.R") # for download_file() & calculate_extent()
source("R/capacity/health-inequalities/england/trust_types/trust_types.R") # run trust types code to create open_trust_types.feather

# Load raw data
tf <- download_file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/June-2021-AE-by-provider-j47iI.xls", ".xls")

raw <-
  read_excel(
    tf,
    sheet = "Provider Level Data",
    skip = 15
  )

# remove first two entries (one is totals, other is blank)
ae_sliced <-
  raw |>
  slice(-(1:2))

# Remove empty rows at the end of the spreadsheet
ae_remove_empty <-
  ae_sliced |>
  drop_na()

# Keep vars of interest
ae_vars <-
  ae_remove_empty |>
  select(
    trust_code = Code,
    `% Total <= 4 hours` = `Percentage in 4 hours or less (all)`
  )

# Replace '-' character with NA
ae_replace <-
  ae_vars |>
  mutate(
    across(
      .cols = !c(trust_code),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
ae_double <-
  ae_replace |>
  mutate(
    across(
      .cols = !c(trust_code),
      as.double
    )
  )


# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")

# Check if any open trusts missing from ae data
open_trusts |>
  anti_join(ae_double) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n())
# Data is for all A&E types, including minor injury units and walk-in centers, so not all non acute trusts will have have these services and therefore not have data
# on this. Lookup data is not available to map these non acute trusts back to MSOA so will be getting dropped at the next stage.

# Check if any trusts in the A&E data not in the open_trusts dataset
raw |>
  anti_join(open_trusts, by = c("Code" = "trust_code"))
# 55 entries but all not NHS Trusts
# States 'Data are shown at provider organisation level, from NHS Trusts, NHS Foundation Trusts and Independent Sector Organisations' https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/


# Join trust to MSOA lookup --------

# Trust to MSOA table only has data for acute trusts
open_trusts |>
  left_join(ae_double) |>
  left_join(lookup_trust_msoa) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
ae_waiting_trusts <- open_trusts |>
  left_join(ae_double) |>
  inner_join(lookup_trust_msoa)

ae_waiting_trusts |>
  distinct(trust_code, `Provider Primary Inspection Category`, `% Total <= 4 hours`) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_missing = sum(is.na(`% Total <= 4 hours`)) / n())

# Some of the open trusts don't have the A&E waiting data 
missing_ae_waiting <- ae_waiting_trusts |>
  filter(is.na(`% Total <= 4 hours`)) |>
  pull(trust_code)

geographr::points_nhs_trusts |>
  filter(nhs_trust_code %in% missing_ae_waiting) |>
  arrange(nhs_trust_name) |>
  print(n = Inf)

# Notes state that 14 Trusts are not required to report on the number of attendances over 4 hours from May 2019
# Notes here: https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/12/Statistical-commentary-November-2021-jf8.pdf

not_reporting_trusts <- c(
  "BEDFORDSHIRE HOSPITALS NHS FOUNDATION TRUST",
  "CAMBRIDGE UNIVERSITY HOSPITALS NHS FOUNDATION TRUST",
  "CHELSEA AND WESTMINSTER HOSPITAL NHS FOUNDATION TRUST",
  "FRIMLEY HEALTH NHS FOUNDATION TRUST",
  "IMPERIAL COLLEGE HEALTHCARE NHS TRUST",
  "KETTERING GENERAL HOSPITAL NHS FOUNDATION TRUST",
  "MID YORKSHIRE HOSPITALS NHS TRUST",
  "NORTH TEES AND HARTLEPOOL NHS FOUNDATION TRUST",
  "NOTTINGHAM UNIVERSITY HOSPITALS NHS TRUST",
  "PORTSMOUTH HOSPITALS UNIVERSITY NATIONAL HEALTH SERVICE TRUST",
  "THE ROTHERHAM NHS FOUNDATION TRUST",
  "UNIVERSITY HOSPITALS DORSET NHS FOUNDATION TRUST",
  "UNIVERSITY HOSPITALS PLYMOUTH NHS TRUST",
  "WEST SUFFOLK NHS FOUNDATION TRUST"
)

geographr::points_nhs_trusts |>
  filter(nhs_trust_code %in% missing_ae_waiting) |>
  filter(!nhs_trust_name %in% not_reporting_trusts) |>
  arrange(nhs_trust_name) |>
  left_join(open_trusts, by = c("nhs_trust_code" = "trust_code")) |>
  print(n = Inf)
# Remaining 10 are specialist Trusts and so perhaps do not have A&E services

# So have 14 cases where have A&E but don't have data and 10 cases where no data since potentially no A&E

# Re-proportion for the trusts with no data
# TO DO: get this checked
ae_waiting_trusts_reprop <- ae_waiting_trusts |>
  filter(!is.na(`% Total <= 4 hours`)) |>
  group_by(msoa_code) |>
  mutate(denominator_msoa = sum(proportion)) |>
  mutate(reweighted_proportion = proportion / denominator_msoa) |>
  ungroup()

ae_waiting_scores_msoa <- ae_waiting_trusts_reprop |>
  mutate(ae_waiting_prop = `% Total <= 4 hours` * reweighted_proportion) |>
  group_by(msoa_code) |>
  summarise(avg_ae_waiting_msoa = sum(ae_waiting_prop))

# Check distributions
summary(ae_waiting_scores_msoa)
summary(ae_double)

msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)

ae_waiting_lad <- ae_waiting_scores_msoa |>
  left_join(lookup_msoa_lad) |>
  left_join(msoa_pop) |>
  calculate_extent(
    var = avg_ae_waiting_msoa,
    higher_level_geography = lad_code,
    population = total_population
  )
