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
open_trusts <- read_rds("data/open_trust_types.rds")

# Check if any open trusts missing from ae data
open_trusts |>
  anti_join(ae_double) |>
  group_by(primary_category) |>
  summarise(count = n())
# Data is for all A&E types, including minor injury units and walk-in centers, so not all non acute trusts will have have these services and therefore not have data
# on this. Lookup data is not available to map these non acute trusts back to MSOA so will be getting dropped at the next stage.

# Check if any trusts in the A&E data not in the open_trusts dataset
raw |>
  anti_join(open_trusts, by = c("Code" = "trust_code")) |>
  pull(Name)
# 88 entries but all not NHS Trusts
# States 'Data are shown at provider organisation level, from NHS Trusts, NHS Foundation Trusts and Independent Sector Organisations' https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/

raw |>
  anti_join(open_trusts, by = c("Code" = "trust_code")) |>
  filter(str_detect(Name, "(?i)trust")) |>
  pull(Name)
# No NHS or NHS Foundation Trusts

# Deal with 14 trusts that don't report number waiting over 4 hours ----

# Notes state that 14 Trusts are not required to report on the number of attendances over 4 hours from May 2019
# Bu they do report the total number of patients waiting
# Notes here (page 3): https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/12/Statistical-commentary-November-2021-jf8.pdf
not_reporting_trusts <- tibble(
  name = c(
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
) |>
  left_join(points_nhs_trusts, by = c("name" = "nhs_trust_name")) |>
  rename(trust_name = name, trust_code = nhs_trust_code)


# Aim to get the 14 missing trusts data but current approach is to impute a value for these 14.
# Approach: load the last reported value (April 2019), calculate the % waited over 4 hours
# Check the median % change of waits between April 2019 and now and apply for the missing trust data
tf <- download_file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/08/Monthly-AE-April-2019-revised-220720-632O5.xls", "xls")

raw_april_2019 <-
  read_excel(
    tf,
    sheet = "Provider Level Data",
    skip = 15
  )

ae_april_2019 <- raw_april_2019 |>
  slice(-(1:2)) |>
  drop_na() |>
  select(
    trust_code = Code,
    trust_name = Name,
    ae_over_4_hours_wait = "Total Attendances > 4 hours",
    ae_total_wait = "Total attendances"
  ) |>
  mutate(
    across(
      .cols = !c(trust_code, trust_name),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  ) |>
  mutate(
    across(
      .cols = !c(trust_code, trust_name),
      as.double
    )
  )


ae_april_2019_prop <- ae_april_2019 |>
  mutate(old_wait_prop = ae_over_4_hours_wait / ae_total_wait) |>
  select(trust_code, old_wait_prop)

change_in_wait_prop <- ae_double |>
  inner_join(open_trusts) |>
  mutate(latest_wait_prop = ae_over_4_hours_wait / ae_total_wait) |>
  select(trust_code, latest_wait_prop) |>
  left_join(ae_april_2019_prop) |>
  mutate(change = latest_wait_prop - old_wait_prop) |>
  drop_na()

change_in_wait_prop |>
  ggplot(aes(x = change)) +
  geom_boxplot()

mising_trusts_estimates <- not_reporting_trusts |>
  left_join(ae_april_2019, by = "trust_code") |>
  mutate(old_wait_prop = ae_over_4_hours_wait / ae_total_wait) |>
  mutate(updated_prop = old_wait_prop + median(change_in_wait_prop$change, na.rm = T)) |>
  select(trust_code, updated_prop)
# Have checked trust changes and can't find the previous trust codes for R0D (i.e. RD3 & RDZ) in the 2019 data
# Will impute using the median waiting prop for all of the data

ae_double_updated <- ae_double |>
  left_join(mising_trusts_estimates) |>
  mutate(ae_over_4_hours_wait = ifelse(!is.na(updated_prop), ae_total_wait * updated_prop, ae_over_4_hours_wait)) |>
  mutate(over_4_hours_wait_prop = ae_over_4_hours_wait / ae_total_wait) |>
  mutate(ae_over_4_hours_wait = ifelse(trust_code == "R0D", ae_total_wait * median(over_4_hours_wait_prop, na.rm = T), ae_over_4_hours_wait)) |>
  select(-updated_prop, -over_4_hours_wait_prop)


# Join trust to LAD lookup --------

lookup_trust_lad <- read_rds("data/lookup_trust_lad.rds")

# Trust to LAD table only has data for acute trusts
open_trusts |>
  left_join(ae_double_updated) |>
  left_join(lookup_trust_lad) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(lad_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts data proportion these to LAD and normalise by total A&E count
ae_wait_joined <- open_trusts |>
  left_join(ae_double_updated) |>
  inner_join(lookup_trust_lad)

# Check missings
ae_wait_joined |>
  distinct(trust_code, primary_category, ae_over_4_hours_wait) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_missing = sum(is.na(ae_over_4_hours_wait)) / n())
# 5 trusts with no A&E data

# Some of the open trusts don't have the A&E waiting data
ae_wait_joined |>
  filter(is.na(ae_over_4_hours_wait)) |>
  distinct(trust_code, primary_category, ae_over_4_hours_wait, ae_total_wait) |>
  left_join(points_nhs_trusts, by = c("trust_code" = "nhs_trust_code"))
# Are specialist and may not have an A&E facility

# Check highest % of a LAD missing info due to these Trusts with no A&E data
ae_wait_joined |>
  mutate(prop_missing = is.na(ae_over_4_hours_wait) * trust_prop_by_lad) |>
  group_by(lad_code) |>
  summarise(prop_missing = sum(prop_missing)) |>
  arrange(desc(prop_missing))
# 7 LADs between 10 - 50% missing, rest below 10%

# need to re-weight the trust_prop_to_lad due to Trusts which don't have A&E
ae_wait_lad <- ae_wait_joined |>
  filter(!is.na(ae_over_4_hours_wait)) |>
  group_by(lad_code) |>
  mutate(trust_prop_by_lad_reweight = trust_prop_by_lad / sum(trust_prop_by_lad)) |>
  ungroup() |>
  mutate(
    ae_over_4_hours_wait_prop = ae_over_4_hours_wait * trust_prop_by_lad_reweight,
    ae_total_wait_prop = ae_total_wait * trust_prop_by_lad_reweight
  ) |>
  group_by(lad_code) |>
  summarise(
    ae_over_4_hours_wait_per_lad = sum(ae_over_4_hours_wait_prop),
    ae_total_wait_per_lad = sum(ae_total_wait_prop)
  )

# Check totals
# Will be difference as had to drop staff from non-acute trusts that couldn't map back to LA
ae_wait_lad |>
  summarise(sum(ae_over_4_hours_wait_per_lad), sum(ae_total_wait_per_lad))

ae_double |>
  summarise(sum(ae_over_4_hours_wait, na.rm = T), sum(ae_total_wait))


# Normalise  ----
# Normalising by proportioned total in A&E per LAD
ae_wait_normalised <- ae_wait_lad |>
  mutate(
    ae_over_4_hours_wait_rate = ae_over_4_hours_wait_per_lad / ae_total_wait_per_lad
  ) |>
  select(lad_code, ae_over_4_hours_wait_rate)

# Check distributions
summary(ae_wait_normalised$ae_over_4_hours_wait_rate)

ae_double |>
  mutate(ae_over_4_hours_wait_rate = ae_over_4_hours_wait / ae_total_wait) |>
  pull(ae_over_4_hours_wait_rate) |>
  summary()

# Save ----
ae_wait_normalised |>
  write_rds("data/capacity/health-inequalities/england/access-availability/ae-waiting-times.rds")
