# Load packages
library(tidyverse)
library(geographr)
library(sf)
library(readODS)

source("R/utils.R") # for download_file()

# Source of data: https://digital.nhs.uk/data-and-information/publications/statistical/estates-returns-information-collection/england-2019-20

# There was trust level data for 'Investment to reduce backlog maintenance (£)' but investment != cost and so have used the site level data for
# 'Cost to eradicate high risk backlog (£)' and aggregated the cost up to trust level
# In case needed in future the trust level investment data can be found at https://files.digital.nhs.uk/84/07227E/ERIC%20-%20201920%20-%20TrustData.csv

# Load data -----

# Download site level data
tf <- download_file("https://files.digital.nhs.uk/11/BC1043/ERIC%20-%20201920%20-%20SiteData%20-%20v2.csv", "csv")

raw_site <-
  read_csv(
    tf,
    locale = locale(encoding = "latin1") # to deal with special characters in column names
  )

site_maintance_columns <- c("Cost to eradicate high risk backlog (£)", "Cost to eradicate significant risk backlog (£)", "Cost to eradicate moderate risk backlog (£)", "Cost to eradicate low risk backlog (£)")

site_columns <- raw_site |>
  select("Trust Code", "Trust Name", "Site Code", "Site Name", "Site Type", all_of(site_maintance_columns)) |>
  mutate_at(site_maintance_columns, ~ ifelse(.x == "Not Applicable", NA, .x)) |>
  mutate_at(site_maintance_columns, ~ as.numeric(str_remove_all(.x, ",")))

# Aggregate the cost across sites up to trust level & only want to look at the cost for high risk back log
# From data definition (https://files.digital.nhs.uk/7B/0FF3E8/ERIC%20-%20201920%20-%20Data%20Definitions.xlsx):
# High risk is where repairs/replacement must be addressed with urgent priority in order to prevent catastrophic failure,
# major disruption to clinical services or deficiencies in safety liable to cause serious injury and/or prosecution
trust_maint_cost <- site_columns |>
  group_by(`Trust Code`) |>
  summarise_if(is.numeric, ~ sum(.x, na.rm = TRUE)) |>
  select(trust_code = `Trust Code`, cost = `Cost to eradicate high risk backlog (£)`)


# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- read_rds("data/open_trust_types.rds")


# Check which trusts are in cost data and not geographr package
trust_maint_cost |>
  anti_join(open_trusts)

# Some of the trusts codes in data are for old trusts which have changed code
# Want to align with the open_trusts file (so only check those returned in the anti_join above)
# Load in trust changes table created in trust_changes.R
trust_changes <- read_rds("data/trust_changes.rds")

old_new_lookup <- trust_maint_cost |>
  anti_join(open_trusts) |>
  rename(old_code = trust_code) |>
  inner_join(trust_changes, by = "old_code") |>
  group_by(new_code) |>
  mutate(new_code_count = n()) |>
  ungroup() |>
  group_by(old_code) |>
  mutate(old_code_count = n()) |>
  ungroup() |>
  mutate(
    split_cost =
      ifelse(old_code_count > 1, cost / old_code_count, cost)
  )

new_trusts <- old_new_lookup |>
  group_by(new_code) |>
  summarise(cost = sum(split_cost)) |>
  rename(trust_code = new_code)

trust_main_cost_updated <- trust_maint_cost |>
  filter(!trust_code %in% old_new_lookup$old_code) |>
  bind_rows(new_trusts)

# Check duplicates now have updated
trust_main_cost_updated |>
  group_by(trust_code) |>
  summarise(count = n()) |>
  filter(count > 1)

# Sum any duplicates
trust_main_cost_updated_combined <- trust_main_cost_updated |>
  group_by(trust_code) |>
  summarise(cost = sum(cost))

# Check again which trusts are in cost data and not geographr package
trust_main_cost_updated_combined |>
  anti_join(open_trusts)
# 4 - TAD, TAF, TAH, TAJ (similar to CQC rating)
# These are in the CQC rating data as 'Mental health - community & residential - NHS'.
# Not found in the PHE data that allows mapping from Trust to MSOA so would not be able to proportion back to LA.

# In quality report (https://files.digital.nhs.uk/4E/5A51F9/ERIC-201920%20-%20Data%20Quality%20Report%20v5.pdf) says 'All 224 trusts required to complete an ERIC return in 2019/20 did so.'
# And there are 224 trusts in the raw data
missing_trusts <- open_trusts |>
  anti_join(trust_main_cost_updated_combined) |>
  inner_join(geographr::points_nhs_trusts, by = c("trust_code" = "nhs_trust_code"))
# 3 trusts missing remaining: RQF, RT4, RYT (similar to CQC rating)

trust_changes |>
  filter(new_code %in% missing_trusts$trust_code | old_code %in% missing_trusts$trust_code)
# Are not recent trust changes
# These Trusts are not found in the data used to map from Trusts to LA so ignore for the current Trust to LA mapping.

# Join trust to LAD lookup --------

lookup_trust_lad <- read_rds("data/lookup_trust_lad.rds")

lookup_trust_lad <- lookup_trust_lad |>
  select(-lad_prop_by_trust)

# Trust to LAD table only has data for acute trusts
open_trusts |>
  left_join(trust_main_cost_updated_combined) |>
  left_join(lookup_trust_lad) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(lad_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts data proportion these to LAD and calculate per capita level
trust_maint_cost_joined <- open_trusts |>
  left_join(trust_main_cost_updated_combined) |>
  inner_join(lookup_trust_lad)

# Check missings
trust_maint_cost_joined |>
  distinct(trust_code, primary_category, cost) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_missing = sum(is.na(cost)) / n())
# no missing

maint_cost_lad <- trust_maint_cost_joined |>
  mutate(maint_cost_prop = cost * trust_prop_by_lad) |>
  group_by(lad_code) |>
  summarise(maint_cost_per_lad = sum(maint_cost_prop))


# Check totals
# Will be difference as had to drop staff from non-acute trusts that couldn't map back to LA
sum(maint_cost_lad$maint_cost_per_lad)
sum(trust_main_cost_updated_combined$cost)

# Load in population figures
lad_pop <- population_lad |>
  select(lad_code, lad_name, total_population) |>
  filter(str_detect(lad_code, "^E"))

# Check lad codes are 2021 for both indicator and population data ----
if (
  anti_join(
    maint_cost_lad,
    lookup_lad_over_time,
    by = c("lad_code" = "LAD21CD")
  ) |>
    pull(lad_code) |>
    length() != 0
) {
  stop("Lad codes need changing to 2021 - check if 2019 or 2020")
}

if (
  anti_join(
    lad_pop,
    lookup_lad_over_time,
    by = c("lad_code" = "LAD21CD")
  ) |>
    pull(lad_code) |>
    length() != 0
) {
  stop("Lad codes need changing to 2021 - check if 2019 or 2020")
}

# Update indicator from 2019 to 2020 and population from 2020 to 2021
# Aggregation only of LADs between 2019 to 2021
maint_cost_lad_update <- maint_cost_lad |>
  left_join(lookup_lad_over_time, by = c("lad_code" = "LAD19CD")) |>
  group_by(LAD21CD) |>
  summarise(across(where(is.numeric), sum))

# Distinct table foe 2020 - 2021 due to E06000060
lookup_lad_over_time_2020 <- lookup_lad_over_time |>
  distinct(LAD20CD, LAD21CD)

lad_pop_update <- lad_pop |>
  left_join(lookup_lad_over_time_2020, by = c("lad_code" = "LAD20CD")) |>
  group_by(LAD21CD) |>
  summarise(across(where(is.numeric), sum))

# Normalise for LAD pop ----
maint_cost_msoa_normalised <- maint_cost_lad_update |>
  left_join(lad_pop_update, by = "LAD21CD") |>
  mutate(maint_cost_rate = maint_cost_per_lad / total_population) |>
  select(lad_code = LAD21CD, maint_cost_rate)

# Save ----
maint_cost_msoa_normalised |>
  read_rds("data/capacity/health-inequalities/england/quality/hospital-maintanance-backlog-cost.rds")