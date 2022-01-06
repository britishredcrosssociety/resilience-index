# Load packages
library(tidyverse)
library(geographr)
library(readxl)
library(sf)

source("R/utils.R") # for download_file() & calculate_extent()

# NHS trust deaths associated with hospitalisation -----
# IMPORTANT NOTE: This data does not include COVID 'activity' i.e. stays and deaths

# Source: https://digital.nhs.uk/data-and-information/publications/statistical/shmi
# Download the data
tf <- download_file("https://files.digital.nhs.uk/A6/0F708F/SHMI%20data%20files%2C%20Jul20-Jun21.zip", "zip")

unzip(tf, exdir = tempdir())

deaths_raw <-
  read_excel(
    list.files(
      tempdir(),
      pattern = "*SHMI data at trust level, Jul20-Jun21 \\(xls\\)",
      recursive = T,
      full.names = TRUE
    ),
    sheet = "Data",
    skip = 10
  )

deaths_columns <- deaths_raw %>%
  select(trust_code = `Provider code`, `Provider name`, `SHMI value`, `SHMI banding`, `Number of spells`, `Observed deaths`, `Expected deaths`)
# 122 trusts

# There is data for only 122 trusts (but there is over 200 open trusts) - this is because data is only available for non-specialist acute trusts (see below)
# Also have done a check of this at end of script (under 'Extra checks' section) by loading in trust inspection categories from CQC data to confirm.

# Source of below quote on coverage of data: https://files.digital.nhs.uk/2C/498A3E/SHMI%20background%20quality%20report%2C%20Jul20-Jun21.pdf
# The SHMI methodology has been designed for non-specialist acute trusts. Specialist trusts,
# mental health trusts, community trusts and independent sector providers are excluded from
# the SHMI because there are important differences in the case-mix of patients treated there
# compared to non-specialist acute trusts and the SHMI has not been designed for these types
# of trusts.


# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_calculations/open_trust_types.feather")


# Check the matching of deaths data & trust table in geographr package --------

# Have established not all trusts have available death data (i.e. non-specialist acute trusts).
open_trusts |>
  anti_join(deaths_columns)

# Check death data trusts not missing in open trusts list from geographr
deaths_columns |>
  anti_join(open_trusts)
# all matched

# From https://digital.nhs.uk/data-and-information/publications/statistical/shmi/2021-11:
# The SHMI is the ratio between the actual number of patients who die following hospitalisation at the trust and the number
# that would be expected to die on the basis of average England figures, given the characteristics of the patients treated there.
# It covers patients admitted to hospitals in England who died either while in hospital or within 30 days of being discharged.
# Deaths related to COVID-19 are excluded from the SHMI.

# Trust to MSOA (then to LA) lookup ----

open_trusts |>
  left_join(deaths_columns) |>
  left_join(lookup_trust_msoa) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LSOA and proportion to per capita level

deaths_full <- open_trusts |>
  left_join(deaths_columns) |>
  inner_join(lookup_trust_msoa) |>
  select(trust_code, primary_category, `Provider name`, `SHMI value`, msoa_code, proportion)

# Check missings
deaths_full |>
  distinct(trust_code, primary_category, `SHMI value`) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_missing = sum(is.na(`SHMI value`)) / n())

# Only have death data on acute non specialist trusts (i.e. those in the deaths dataset) so re-proportion the splits and
# calculate the proportion  of acute non-specialist patients for a msoa that come from each particular acute non specialist trusts

# Re-proportion for the trusts with no data
deaths_full_reprop <- deaths_full |>
  filter(!is.na(`SHMI value`)) |>
  group_by(msoa_code) |>
  mutate(denominator_msoa = sum(proportion)) |>
  mutate(reweighted_proportion = proportion / denominator_msoa) |>
  mutate(weighted_shmi = reweighted_proportion * `SHMI value`)


# Aggregate up to MSOA level
deaths_msoa <- deaths_full_reprop |>
  group_by(msoa_code) |>
  summarise(shmi_averaged = sum(weighted_shmi, na.rm = T))

# Check distributions
summary(deaths_columns$`SHMI value`)
summary(deaths_msoa)

# Get MSOA pop
msoa_pop <-
  population_msoa |>
  select(msoa_code, total_population)

# Join on MSOA to LAD look up & aggreagte up to LAD
deaths_lad <- deaths_msoa |>
  left_join(lookup_msoa_lad, by = "msoa_code") |>
  left_join(msoa_pop, by = "msoa_code") |>
  calculate_extent(
    var = shmi_averaged,
    higher_level_geography = lad_code,
    population = total_population
  )

deaths_lad |>
  group_by(extent) |>
  summarise(count = n() / nrow(deaths_lad))
# 63% : extent = 0
# 5%: extent = 1

# Save ----
deaths_lad |>
  write_rds("data/capacity/health-inequalities/england/deaths-associated-with-hospitalisation.rds")
