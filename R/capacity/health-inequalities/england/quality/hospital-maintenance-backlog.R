# Load packages
library(tidyverse)
library(geographr)
library(sf)

source("R/utils.R") # for download_file()


# There was trust level data for 'Investment to reduce backlog maintenance (£)' but investment != cost and so have used the site level data for 
# 'Cost to eradicate high risk backlog (£)' and aggregated the cost up to trust level 
# In case needed in future the trust level investment data can be found at https://files.digital.nhs.uk/84/07227E/ERIC%20-%20201920%20-%20TrustData.csv

# NHS site level data on maintenance backlog -----

# Download site level data
tf <- download_file("https://files.digital.nhs.uk/11/BC1043/ERIC%20-%20201920%20-%20SiteData%20-%20v2.csv", "csv")

raw_site <-
  read_csv(
    tf,
    locale = locale(encoding = "latin1") # to deal with special characters in column names
  )

site_maintance_columns <- c("Cost to eradicate high risk backlog (£)", "Cost to eradicate significant risk backlog (£)", "Cost to eradicate moderate risk backlog (£)", "Cost to eradicate low risk backlog (£)")

site_columns <- raw_site |>
  select("Trust Code", "Trust Name", "Site Code", "Site Name", "Site Type", site_maintance_columns) |>
  mutate_at(site_maintance_columns, ~ ifelse(.x == "Not Applicable", NA, .x)) |>
  mutate_at(site_maintance_columns, ~ as.numeric(str_remove_all(.x, ",")))

# Aggregate the cost across sites up to trust level
site_agg <- site_columns |>
  group_by(`Trust Code`) |>
  summarise_if(is.numeric, ~ sum(.x, na.rm = TRUE))

# Only want to look at the cost for high risk back log
# From data definition (https://files.digital.nhs.uk/7B/0FF3E8/ERIC%20-%20201920%20-%20Data%20Definitions.xlsx):
# High risk is where repairs/replacement must be addressed with urgent priority in order to prevent catastrophic failure,
# major disruption to clinical services or deficiencies in safety liable to cause serious injury and/or prosecution
site_agg_columns <- site_agg |>
  select(`Trust Code`, `Cost to eradicate high risk backlog (£)`)


# NHS TRUST table in geographr package -----

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

# Check the matching of cost data & trust table in geographr package --------

# Check which trusts are in geographr package and not cost data --

# In quality report (https://files.digital.nhs.uk/4E/5A51F9/ERIC-201920%20-%20Data%20Quality%20Report%20v5.pdf) says 'All 224 trusts required to complete an ERIC return in 2019/20 did so.'
# And there are 224 trusts in the raw data

high_risk_cost_open <- open_trusts |>
  left_join(site_agg_columns, by = c("trust_code" = "Trust Code"))

trusts_missing_maint_backlog <- high_risk_cost_open |>
  keep_na() |>
  pull(trust_code)

points_nhs_trusts |>
  as_tibble() |>
  filter(nhs_trust_code %in% trusts_missing_maint_backlog)
# 4 trusts missing 

# Checked the https://github.com/britishredcrosssociety/geographr/blob/main/data-raw/lookup_trust_msoa.R for these
# R0D used to be RD3 - which is in the cost data
# RQF?
# RT4 used to be RKV, RP8, RQE, RRC - which aren't in the cost data
# RYT?     

# Check which trusts are in cost data and not geographr package --
trusts_missing_geographr <- site_agg_columns |>
  anti_join(open_trusts, by = c("Trust Code" = "trust_code")) 

trusts_open_closed <- points_nhs_trusts |>
  as_tibble() 

trusts_missing_geographr |>
  left_join(trusts_open_closed,  by = c("Trust Code" = "nhs_trust_code"))
# 9 are closed ones and then 4 remaining - TAD, TAF, TAH, TAJ

# Join trust to MSOA lookup --------

lookup_trust_msoa 

high_risk_cost_open |>
  left_join()

# TODO:
# Trusts need to be matched to MSOA's, and then aggregated up to LTLA.
# As detailed in the 'access-availability/ae-waiting-times.R' script not all trusts are matching the
# lookup table. This needs fixing before aggregation can happen.
