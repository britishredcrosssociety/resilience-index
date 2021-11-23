# Load packages
library(tidyverse)
library(geographr)
library(readxl)
library(sf)

source("R/utils.R") # for download_file() & calculate_extent()

# NHS trust deaths associated with hospitalisation -----
# IMPORTANT NOTE: This data does not include COVID 'activity' i.e. stays and deaths

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
  select(`Provider code`, `Provider name`, `SHMI value`, `SHMI banding`, `Number of spells`, `Observed deaths`, `Expected deaths`)
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

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

# Check the matching of deaths data & trust table in geographr package --------

# Have established not all trusts have available death data (i.e. non-specialist acute trusts).
open_trusts |>
  anti_join(deaths_columns, by = c("trust_code" = "Provider code"))

# Check death data trusts not missing in open trusts list from geographr
deaths_columns |>
  anti_join(open_trusts, by = c("Provider code" = "trust_code"))
# all matched

# From https://digital.nhs.uk/data-and-information/publications/statistical/shmi/2021-11:
# The SHMI is the ratio between the actual number of patients who die following hospitalisation at the trust and the number
# that would be expected to die on the basis of average England figures, given the characteristics of the patients treated there.
# It covers patients admitted to hospitals in England who died either while in hospital or within 30 days of being discharged.
# Deaths related to COVID-19 are excluded from the SHMI.

# Trust to MSOA (then to LA) lookup ----
# Think about effect of not all trusts data being available
msoa_shmi <- open_trusts |>
  left_join(deaths_columns, by = c("trust_code" = "Provider code")) |>
  left_join(lookup_trust_msoa, by = "trust_code") |>
  mutate(weighted = `SHMI value` * proportion)

# Since don't have all trust types in the lookup_trust_msoa table workaround is to get absolute numbers for the patients from a msoa attending
# a trust for only non-specialist acute trusts and calculate proportions that way? Need to chat over with team if this makes sense.

# Below code taken and amended from https://github.com/britishredcrosssociety/geographr/blob/main/data-raw/lookup_trust_msoa.R

# Download file
tf <- download_file("https://app.box.com/index.php?rm=box_download_shared_file&shared_name=qh8gzpzeo1firv1ezfxx2e6c4tgtrudl&file_id=f_877983829510", ".xlsx")

# All admissions
catchment_populations <-
  read_excel(tf, sheet = "All Admissions")

# Only most recent (2019) rows
catchment_populations_columns <- catchment_populations |>
  filter(CatchmentYear == 2019) |>
  select(msoa, TrustCode, patients) |>
  rename(msoa_code = msoa)

# Only return acute non specialist trusts (i.e. those in the deaths dataset) and calculate the prop of acute non specialist patients for a msoa
# that come from each particular acute non specialist trusts
acute_nonspec_trust_msoa_trust_lookup <- catchment_populations_columns |>
  filter(TrustCode %in% deaths_columns$`Provider code`) |>
  group_by(msoa_code) |>
  mutate(acute_nonspec_trust_total_patients = sum(patients)) |>
  mutate(acute_nonspec_trust_prop = patients / acute_nonspec_trust_total_patients)

# Join the proportions for weighting onto the death data and weight the shmi values
msoa_shmi_acute_nonspec_prop_only <- open_trusts |>
  left_join(deaths_columns, by = c("trust_code" = "Provider code")) |>
  left_join(acute_nonspec_trust_msoa_trust_lookup, by = c("trust_code" = "TrustCode")) |>
  mutate(weighted = `SHMI value` * acute_nonspec_trust_prop)

# Aggregate up to MSOA level
msoa_shmi_acute_nonspec_prop_only_weighted <- msoa_shmi_acute_nonspec_prop_only |>
  group_by(msoa_code) |>
  mutate(shmi_averaged = sum(weighted, na.rm = T)) |>
  ungroup() |>
  select(msoa_code, shmi_averaged) |>
  distinct()

# Get MSOA pop
msoa_pop <-
  population_msoa |>
  select(msoa_code, total_population)

# Join on MSOA to LAD look up
shmi_acute_nonspec_prop_only_lad <-
  msoa_shmi_acute_nonspec_prop_only_weighted |>
  left_join(lookup_msoa_lad, by = "msoa_code") |>
  left_join(msoa_pop, by = "msoa_code")

# Aggregate to LAD ----
shmi_extent_lad <-
  shmi_acute_nonspec_prop_only_lad |>
  calculate_extent(
    var = shmi_averaged,
    higher_level_geography = lad_code,
    population = total_population
  )

# 63% : extent = 0
# 5%: extent = 1

###################################################

## Extra checks  ----
# Downloading CQC rating data as has information on what is the primary type of care trust provides ---
# This is used to check against the trusts with no death data
tf <- download_file("https://www.cqc.org.uk/sites/default/files/01_November_2021_Latest_ratings.ods", "ods")

raw_providers <-
  read_ods(
    tf,
    sheet = "Providers",
  )

trust_categories <- raw_providers |>
  select(`Provider ID`, `Provider Name`, `Provider Type`, `Provider Primary Inspection Category`) |>
  distinct()

combined_table <- open_trusts |>
  left_join(trust_categories, by = c("trust_code" = "Provider ID")) |>
  left_join(deaths_columns, by = c("trust_code" = "Provider code"))

# Check missing death data for each care category
combined_table |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), count_death_data = sum(!is.na(`SHMI value`)))
