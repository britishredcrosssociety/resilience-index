# Note: once decided approach for A&E waiting times repeat here.

# Load libs
library(tidyverse)
library(httr)
library(readxl)
library(sf)
library(geographr)

source("R/utils.R") #for download_file() 

# Load data ----

# Data from https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2021-22/
tf <- download_file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/Monthly-Diagnostics-Web-File-Provider-May-2021_84CSC.xls",".xls")

raw <-
  read_excel(
    tf,
    sheet = "Provider",
    skip = 13
  )

# Remove first two rows (summary & blank)
diagnostics_sliced <-
  raw |>
  slice(-(1:2))

# Select cols
diagnostics_vars <-
  diagnostics_sliced |>
  select(
    trust_code = `Provider Code`,
    `Waiting 13+ weeks` = `Number waiting 13+ Weeks`
  )

# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")

# Check those in the waiting times data and not in the trusts data 
raw |>
  anti_join(open_trusts, by = c("Provider Code" = "trust_code")) |>
  select(`Regional Team Name`, `Provider Name`) |>
  print(n = Inf)
# There are many 243 missing in open trusts but notes say 'Data are shown at provider organisation level, from NHS Trusts, NHS Foundation Trusts and Independent Sector Providers'
# So from names look to be independent providers (e.g. Nuffield, Spire etc)
# Note here: https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/

open_trusts |>
  anti_join(diagnostics_vars) |>
  print(n = Inf)
# 51 missing that are ambulance and community trusts
# In note (https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/08/DM01-FAQs-v-3.0.pdf) says
# 'All trusts that provide any of the diagnostic tests that are on the monthly template should complete a return' - so these trusts may not provide these tests. 

# Join trust to LAD lookup --------

lookup_trust_lad <- read_feather("R/capacity/health-inequalities/england/trust_types/lookup_trust_lad.feather")


# Trust to LAD table only has data for acute trusts
open_trusts |>
  left_join(diagnostics_vars) |>
  left_join(lookup_trust_lad) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(lad_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts data proportion these to LAD and calculate per capita level
diagnostics_vars_joined <- open_trusts |>
  left_join(diagnostics_vars) |>
  inner_join(lookup_trust_lad)

# Check missings
diagnostics_vars_joined |>
  distinct(trust_code, `Provider Primary Inspection Category`, `Waiting 13+ weeks`) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_missing = sum(is.na(`Waiting 13+ weeks`)) / n())

diagnostics_vars_lad <- diagnostics_vars_joined |>
  mutate(waiting_over_13_weeks_prop = `Waiting 13+ weeks` * trust_prop_by_lad) |>
  group_by(lad_code) |>
  summarise(waiting_over_13_weeks_per_lad = sum(waiting_over_13_weeks_prop))

# Check totals
# Will be difference as had to drop staff from non-acute trusts that couldn't map back to LA
sum(diagnostics_vars_lad$waiting_over_13_weeks_per_lad)
sum(diagnostics_vars$`Waiting 13+ weeks`)


# Normalise for LAD pop ----
lad_pop <- geographr::population_lad |>
  select(lad_code, lad_name, total_population)

diagnostics_vars_normalised <- diagnostics_vars_lad |>
  left_join(lad_pop) |>
  mutate(waiting_over_13_weeks_rate = waiting_over_13_weeks_per_lad / total_population * 100) |>
  select(lad_code, waiting_over_13_weeks_rate)

# Save ----
diagnostics_vars_normalised |>
  write_rds("data/capacity/health-inequalities/england/waiting-lists.rds")

