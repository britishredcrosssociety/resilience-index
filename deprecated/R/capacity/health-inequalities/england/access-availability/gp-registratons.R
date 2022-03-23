# ---- Load libs ----
library(tidyverse)
library(httr)
library(readxl)
library(geographr)
library(sf)

source("R/utils.R")

# ---- Load data ----
lad_pop <-
  population_lad |>
  select(lad_code, pop = total_population) |>
  filter(str_detect(lad_code, "^E"))

tf <- download_file("https://files.digital.nhs.uk/74/93D6A1/gp-reg-pat-prac-lsoa-male-female-october-21.zip", ".zip")

unzip(tf, exdir = tempdir())

raw <-
  read_csv(
    list.files(
      tempdir(),
      pattern = "gp-reg-pat-prac-lsoa-all.csv",
      full.names = TRUE
    )
  )

raw_reduced <-
  raw |>
  select(
    lsoa_code = LSOA_CODE,
    count = NUMBER_OF_PATIENTS
  )

lsoa_summaries <-
  raw_reduced |>
  group_by(lsoa_code) |>
  summarise(count = sum(count))

# Remove non-English LSOA's
lsoa_registrations <-
  lsoa_summaries |>
  filter(str_detect(lsoa_code, "^E"))

msoa_registrations <-
  lsoa_registrations |>
  left_join(lookup_lsoa_msoa) |>
  group_by(msoa_code) |>
  summarise(count = sum(count))

lad_registrations <-
  msoa_registrations |>
  left_join(lookup_msoa_lad) |>
  group_by(lad_code) |>
  summarise(count = sum(count))

# Check lad codes are 2021 for both indicator and population data ----
if(
  anti_join(
    lad_registrations,
    lookup_lad_over_time,
    by = c("lad_code" = "LAD21CD")
  ) |>
  pull(lad_code) |>
  length() != 0
) {
  stop("Lad codes need changing to 2021 - check if 2019 or 2020")
}

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

# Update indicator from 2019 to 2020 and population from 2020 to 2021
# Aggregation only of LADs between 2019 to 2021
lad_registrations_update <- lad_registrations |>
  left_join(lookup_lad_over_time, by = c("lad_code" = "LAD19CD")) |>
  group_by(LAD21CD) |>
  summarise(across(where(is.numeric), sum))

# Distinct table for 2020 - 2021 due to E06000060
lookup_lad_over_time_2020 <- lookup_lad_over_time |>
  distinct(LAD20CD, LAD21CD)

lad_pop_update <- lad_pop |>
  left_join(lookup_lad_over_time_2020, by = c("lad_code" = "LAD20CD")) |>
  group_by(LAD21CD) |>
  summarise(across(where(is.numeric), sum))

gp_registrations <-
  lad_registrations_update |>
  left_join(lad_pop_update) |>
  mutate(perc_registered_gp = count / pop) |>
  select(lad_code = LAD21CD, perc_registered_gp)

summary(gp_registrations)

# Save ----
gp_registrations |>
  write_rds("data/capacity/health-inequalities/england/access-availability/gp-registrations.rds")
