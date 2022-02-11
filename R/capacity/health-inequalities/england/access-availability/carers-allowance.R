# ---- Load ----
library(tidyverse)
library(sf)
library(geographr)

# LAD population data ----
lad_pop <-
  population_lad |>
  select(lad_code, total_population) |>
  filter(str_detect(lad_code, "^E"))

if(
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

# Distinct table for 2020 - 2021 due to E06000060
lookup_lad_over_time_2020 <- lookup_lad_over_time |>
  distinct(LAD20CD, LAD21CD, LAD21NM)

lad_pop_update <- lad_pop |>
  left_join(lookup_lad_over_time_2020, by = c("lad_code" = "LAD20CD")) |>
  group_by(LAD21CD, LAD21NM) |>
  summarise(across(where(is.numeric), sum)) |>
  ungroup()


# Carers allowance data ----
# Data must be extracted from Stat-Xplore (https://stat-xplore.dwp.gov.uk/webapi/jsf/login.xhtml). 
# It is stored locally on disk and appended to .gitignore.

# Table is 'Carer's Allowance: Entitled Cases' rather than 'Carer's Allowance: Cases in Payment'
# Entitled cases show both the number of people in receipt of an 
# allowance AND those with entitlement where the payment has been suspended, for example if they are in hospital

# Data date May 2021

raw <-
  read_csv(
    "data/on-disk/england-carers-allowance-raw.csv",
    skip = 9
  )

# Clean ----
# Remove empty rows & columns & rows with text at end
raw_england <-
  raw |>
  select(lad_name = Quarter, ca_count = `May-21`) |>
  drop_na() |>
  filter(str_detect(ca_count, "[0-9]")) |>
  mutate(count = as.double(ca_count))

# Check non matches ----
lad_pop_update |>
  anti_join(raw_england, by = c("LAD21NM" = "lad_name"))

wales_lads <- population_lad |>
  filter(str_detect(lad_code, "^W")) |>
  select(lad_name)
      
raw_england |>
  anti_join(lad_pop_update, by = c("lad_name" = "LAD21NM")) |>
  print(n = Inf)
# Scotland/Wales or other

# Join pop to counts ----
joined <- lad_pop_update |>
  left_join(raw_england, by = c("LAD21NM" = "lad_name"))

joined |>
  filter(is.na(ca_count))

joined |>
  filter(is.na(total_population))

# ---- Calculate rate ----
carers_allowance <-
  joined |>
  mutate(carers_allowance_rate = count / total_population) |>
  select(lad_code = LAD21CD, carers_allowance_rate)

carers_allowance |>
  summary()

# ---- Save ----
carers_allowance |>
  write_rds("data/capacity/health-inequalities/england/access-availability/carers-allowance.rds")
