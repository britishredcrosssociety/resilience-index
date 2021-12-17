# Load packages
library(tidyverse)
library(geographr)
library(sf)
library(readODS)

source("R/utils.R") # for download_file() 

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
  select(trust_code = `Trust Code`, `Cost to eradicate high risk backlog (£)`)


# NHS Trust table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")


# Check the matching of indicator data & trust table in geographr package

# Check which trusts are in geographr package and not cost data 
# In quality report (https://files.digital.nhs.uk/4E/5A51F9/ERIC-201920%20-%20Data%20Quality%20Report%20v5.pdf) says 'All 224 trusts required to complete an ERIC return in 2019/20 did so.'
# And there are 224 trusts in the raw data
open_trusts |>
  anti_join(trust_maint_cost)
# 4 trusts missing: R0D, RQF, RT4, RYT
# Checked the https://github.com/britishredcrosssociety/geographr/blob/main/data-raw/lookup_trust_msoa.R for these
# R0D used to be RD3 - which is in the cost data
# RQF?
# RT4 used to be RKV, RP8, RQE, RRC - which aren't in the cost data
# RYT?

# Check which trusts are in cost data and not geographr package 
trust_maint_cost |>
  anti_join(open_trusts) |>
  left_join(geographr::points_nhs_trusts, by = c("trust_code" = "nhs_trust_code"))
# 5 are closed ones and then 10 remaining - TAD, TAF, TAH, TAJ, RA3, RBA, RC1, RDD, RQ6, RQ8

# Join trust to LAD lookup --------

lookup_trust_lad <- read_feather("R/capacity/health-inequalities/england/trust_types/lookup_trust_lad.feather")

# Trust to LAD table only has data for acute trusts
open_trusts |>
  left_join(trust_maint_cost) |>
  left_join(lookup_trust_lad) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(lad_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts data proportion these to LAD and calculate per capita level
trust_maint_cost_joined <- open_trusts |>
  left_join(trust_maint_cost) |>
  inner_join(lookup_trust_lad)

# Check missings
trust_maint_cost_joined |>
  distinct(trust_code, `Provider Primary Inspection Category`, `Cost to eradicate high risk backlog (£)`) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_missing = sum(is.na(`Cost to eradicate high risk backlog (£)`)) / n())

# Drop the trust R0D with missing cost data
trust_maint_cost_joined_drop_na <- trust_maint_cost_joined |>
  filter(!is.na(`Cost to eradicate high risk backlog (£)`))

maint_cost_lad <- trust_maint_cost_joined_drop_na |>
  mutate(maint_cost_prop = `Cost to eradicate high risk backlog (£)` * trust_prop_by_lad) |>
  group_by(lad_code) |>
  summarise(maint_cost_per_lad = sum(maint_cost_prop))


# Check totals
# Will be difference as had to drop staff from non-acute trusts that couldn't map back to LA
sum(maint_cost_lad$maint_cost_per_lad)
sum(trust_maint_cost$`Cost to eradicate high risk backlog (£)`)


# Normalise for LAD pop ----
lad_pop <- geographr::population_lad |>
  select(lad_code, lad_name, total_population)

maint_cost_msoa_normalised <- maint_cost_lad |>
  left_join(lad_pop) |>
  mutate(maint_cost_rate = maint_cost_per_lad / total_population * 100) |>
  select(lad_code, maint_cost_rate)


# Save ----
maint_cost_msoa_normalised  |>
  write_rds("data/capacity/health-inequalities/england/hospital-maintanance-backlog-cost.rds")
