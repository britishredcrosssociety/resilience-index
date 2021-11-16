# Load libs
library(tidyverse)
library(geographr)
library(sf)

source("R/utils.R") # for download_file()

# NHS trust level & site level data on maintenance backlog -----

# Download trust level data
tf <- download_file("https://files.digital.nhs.uk/84/07227E/ERIC%20-%20201920%20-%20TrustData.csv", "csv")

raw_trust <-
  read_csv(
    tf,
    locale = locale(encoding = "latin1") # to deal with special characters in column names
  )

trust_columns <- raw_trust |>
  select("Trust Code", "Trust Name", "Trust Type", "Investment to reduce backlog maintenance (£)")

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

site_agg <- site_columns |>
  group_by(`Trust Code`) |>
  summarise_if(is.numeric, ~ sum(.x, na.rm = TRUE))

site_agg_columns <- site_agg |>
  select(`Trust Code`, `Cost to eradicate high risk backlog (£)`)

# Only want to look at the cost for high risk back log
# From data definition (https://files.digital.nhs.uk/7B/0FF3E8/ERIC%20-%20201920%20-%20Data%20Definitions.xlsx):
# High risk is where repairs/replacement must be addressed with urgent priority in order to prevent catastrophic failure,
# major disruption to clinical services or deficiencies in safety liable to cause serious injury and/or prosecution

# Check to see how total cost at trust level compares to investment -----
site_agg_total <- site_agg |>
  rowwise(`Trust Code`) |>
  mutate(total_cost = rowSums(across(where(is.numeric))))

combined_maint_backlog_data <- trust_columns |>
  left_join(site_agg_total, by = "Trust Code")
# cost != investment at trust level


# NHS TRUST table in geographr package -----

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

# Check the matching of cost/investment data & trust table in geographr package --------

high_risk_cost_open <- open_trusts |>
  left_join(site_agg_columns, by = c("trust_code" = "Trust Code"))

trusts_missing_maint_backlog <- high_risk_cost_open |>
  keep_na() |>
  pull(trust_code)

trusts_missing_maint_backlog
# 4 trusts missing : "R0D" "RQF" "RT4" "RYT"
