# Load libs
library(tidyverse)
library(httr)
library(readxl)
library(sf)
library(geographr)

source("R/utils.R")

msoa_pop <-
  population_msoa |>
  select(msoa_code, total_population)

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    `Trust Code` = nhs_trust_code,
    `Trust Name` = nhs_trust_name
  ) |>
  mutate(
    `Trust Name` = str_to_title(`Trust Name`),
    `Trust Name` = str_replace(`Trust Name`, "Nhs", "NHS")
  )

# Load raw data
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/Monthly-Diagnostics-Web-File-Provider-May-2021_84CSC.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

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

# Filter to only open trusts
# diagnostic_wait_times <-
diagnostic_open_trusts <-
  open_trusts |>
  left_join(
    diagnostics_vars,
    by = "Trust Code"
  ) |>
  select(
    trust_code = `Trust Code`,
    waiting_13_weeks = `Waiting 13+ weeks`
  )

# Remove NA values
diagnostic_drop_na <-
  diagnostic_open_trusts |>
  drop_na()

# Aggregate to MSOA

# Notice, that many trusts don't have lookup data available. This is because
# the catchment population data used in the trust to msoa lookup does not
# include these trusts. This is presumably because the catchment data (2018) is
# out of date with the Trust data. This needs verifying somehow. Note: the
# lookup script in geographr does attempt to update codes where a change has
# been made.
# not_available <-
#   diagnostic_drop_na |>
#   left_join(lookup_trust_msoa) |>
#   keep_na() |>
#   pull(trust_code)  |>
#   unique()

# points_nhs_trusts |>
#   filter(nhs_trust_code %in% not_avilable) |>
#   ggplot() +
#   geom_sf(data = boundaries_lad) +
#   geom_sf(size = 3) +
#   theme_minimal()

# Calculate MSOA proportions
diagnostic_msoa <-
  diagnostic_drop_na |>
  left_join(lookup_trust_msoa) |>
  drop_na() |> # Because the Trust lookup table is currently incomplete
  mutate(weighted = waiting_13_weeks * proportion) |>
  group_by(msoa_code) |>
  mutate(waiting_diagnostic = sum(weighted)) |>
  ungroup() |>
  select(msoa_code, waiting_diagnostic) |>
  distinct()

# Normalise by population
diagnostic_msoa_normalised <-
  diagnostic_msoa |>
  left_join(msoa_pop) |>
  mutate(diagnostic_rate = waiting_diagnostic / total_population * 100) |>
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

# TODO:
# - The missing Trusts need amending otherwise the data is likely going to be
#   skewed/inaccurate