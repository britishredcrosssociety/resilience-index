# Load libs
library(tidyverse)
library(httr)
library(readxl)
library(sf)
library(geographr)

source("R/utils.R") # for download_file() 

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
    ae_over_4_hours_wait = "Total Attendances > 4 hours",
    ae_total_wait = "Total attendances"
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
  anti_join(open_trusts, by = c("Code" = "trust_code")) |>
  pull(Name)
# 88 entries but all not NHS Trusts
# States 'Data are shown at provider organisation level, from NHS Trusts, NHS Foundation Trusts and Independent Sector Organisations' https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/


# Join trust to LAD lookup --------

lookup_trust_lad <- read_feather("R/capacity/health-inequalities/england/trust_types/lookup_trust_lad.feather")

# Trust to LAD table only has data for acute trusts
open_trusts |>
  left_join(ae_double) |>
  left_join(lookup_trust_lad) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(lad_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts data proportion these to LAD and calculate per capita level
ae_wait_joined <- open_trusts |>
  left_join(ae_double) |>
  inner_join(lookup_trust_lad)

# Check missings
ae_wait_joined |>
  distinct(trust_code, `Provider Primary Inspection Category`, ae_over_4_hours_wait) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_missing = sum(is.na(ae_over_4_hours_wait)) / n())

# Some of the open trusts don't have the A&E waiting data 
ae_wait_joined |>
  filter(is.na(ae_over_4_hours_wait)) |>
  distinct(trust_code) |>
  left_join(geographr::points_nhs_trusts, by = c("trust_code" = "nhs_trust_code"))

# Notes state that 14 Trusts are not required to report on the number of attendances over 4 hours from May 2019
# Notes here: https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/12/Statistical-commentary-November-2021-jf8.pdf
# TO DO: Try to find this data
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

ae_wait_joined |>
  filter(is.na(ae_over_4_hours_wait)) |>
  distinct(trust_code) |>
  left_join(geographr::points_nhs_trusts, by = c("trust_code" = "nhs_trust_code")) |>
  filter(!nhs_trust_name %in% not_reporting_trusts) |>
  left_join(open_trusts) 
# Remaining 5 are specialist Trusts and so perhaps do not have A&E services

# So have 14 cases where have A&E but don't have data and 10 cases where no data since potentially no A&E


ae_wait_lad <- ae_wait_joined |>
  filter(!is.na(ae_over_4_hours_wait)) |>
  mutate(ae_over_4_hours_wait_prop = ae_over_4_hours_wait * trust_prop_by_lad,
         ae_total_wait_prop = ae_total_wait * trust_prop_by_lad) |>
  group_by(lad_code) |>
  summarise(ae_over_4_hours_wait_per_lad = sum(ae_over_4_hours_wait_prop),
            ae_total_wait_per_lad = sum(ae_total_wait_prop))

# Check totals
# Will be difference as had to drop staff from non-acute trusts that couldn't map back to LA
ae_wait_lad |>
  summarise(sum(ae_over_4_hours_wait_per_lad), sum(ae_total_wait_per_lad))

ae_double |>
  summarise(sum(ae_over_4_hours_wait, na.rm = T), sum(ae_total_wait))


# Normalise  ----
# Testing normalising by both LAD population and also attributed total waiting
lad_pop <- geographr::population_lad |>
  select(lad_code, lad_name, total_population)

ae_wait_normalised <- ae_wait_lad |>
  left_join(lad_pop) |>
  mutate(ae_over_4_hours_wait_per_capita = ae_over_4_hours_wait_per_lad / total_population * 100,
         ae_over_4_hours_wait_rate = ae_over_4_hours_wait_per_lad / ae_total_wait_per_lad * 100)
  

# Save ----
#ae_wait_normalised |>
#  write_rds("data/capacity/health-inequalities/england/ae-waiting-times.rds")
