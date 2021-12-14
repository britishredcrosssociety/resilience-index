# Load libs
library(tidyverse)
library(httr)
library(readxl)
library(sf)
library(geographr)

source("R/utils.R") #for download_file() & calculate_extent()
source("R/capacity/health-inequalities/england/trust_types/trust_types.R") # run trust types code to create open_trust_types.feather

# Load raw data
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
    `Trust Code` = `Provider Code`,
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
# So from names look to be indpendent providers (e.g. Nuffield, Spire etc)
# Note here: https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/

# Filter to only open trusts
diagnostic_open <-
  open_trusts |>
  left_join(
    diagnostics_vars,
    by = c("trust_code" = "Trust Code")
  ) 

# Join trust to MSOA lookup --------

# Trust to MSOA table only has data for acute trusts
diagnostic_open |>
  left_join(lookup_trust_msoa) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LSOA and proportion to per capita level

diagnostic_msoa <- diagnostic_open |>
  inner_join(lookup_trust_msoa)

# Check missings
diagnostic_msoa |>
  distinct(trust_code, `Provider Primary Inspection Category`, `Waiting 13+ weeks`) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_missing = sum(is.na(`Waiting 13+ weeks`)) / n())

# Calculate MSOA proportions
diagnostic_msoa_weighted <- diagnostic_msoa |>
  mutate(waiting_prop = `Waiting 13+ weeks` * proportion) |>
  group_by(msoa_code) |>
  summarise(waiting_per_msoa = sum(waiting_prop))

# Check distributions
summary(diagnostic_msoa |> distinct(trust_code, `Waiting 13+ weeks`))
summary(diagnostic_msoa_weighted)

msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)

# Normalise by population
diagnostic_msoa_normalised <- diagnostic_msoa_weighted |>
  left_join(msoa_pop) |>
  mutate(diagnostic_rate = waiting_per_msoa / total_population * 100) |>
  select(msoa_code, diagnostic_rate, total_population)

# Aggregate to LAD
diagnostic_lad <-
  diagnostic_msoa_normalised |>
  left_join(lookup_msoa_lad) |>
  calculate_extent(
    var = diagnostic_rate,
    higher_level_geography = lad_code,
    population = total_population
  )

diagnostic_lad |>
  group_by(extent) |>
  summarise(count = n()/nrow(deaths_lad))

# Save ----
diagnostic_lad |>
  write_rds("data/capacity/health-inequalities/england/waiting_lists.rds")

