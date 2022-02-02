# ---- Load libs ----
library(tidyverse)
library(httr)
library(readxl)
library(geographr)
library(sf)

source("R/utils.R")

# ---- Load data ----
pop_lad <-
  population_lad |>
  select(lad_code, pop = total_population)

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

gp_registrations <-
  lad_registrations |>
  left_join(pop_lad) |>
  rename(pop = pop) |>
  mutate(perc_registered_gp = count / pop)

# Check missings
gp_registrations |>
  filter(is.na(count) | is.na(pop))
# 4 LADs with missing population data

# Deal with LAD restructures -----

# Some LADs restructured in 2020 & 2021 - since GP data and population data is at different years may need to use lookups (depending on what point in time data collected as so which LADs used)
buckinghamshire_restructure_2020 <-
  tribble(
    ~pre_lad_name, ~post_ua_name, ~pre_lad_code, ~post_ua_code,
    "Aylesbury Vale", "Buckinghamshire", "E07000004", "E06000060",
    "Chiltern", "Buckinghamshire", "E07000005", "E06000060",
    "South Bucks", "Buckinghamshire", "E07000006", "E06000060",
    "Wycombe", "Buckinghamshire", "E07000007", "E06000060"
  )

northamptonshire_restructure_2021 <-
  tribble(
    ~pre_lad_name, ~post_ua_name, ~pre_lad_code, ~post_ua_code,
    "Corby", "North Northamptonshire", "E07000150", "E06000061",
    "Daventry", "West Northamptonshire", "E07000151", "E06000062",
    "East Northamptonshire", "North Northamptonshire", "E07000152", "E06000061",
    "Kettering", "North Northamptonshire", "E07000153", "E06000061",
    "Northampton", "West Northamptonshire", "E07000154", "E06000062",
    "South Northamptonshire", "West Northamptonshire", "E07000155", "E06000062",
    "Wellingborough", "North Northamptonshire", "E07000156", "E06000061",
  )

gp_registrations_buck <- gp_registrations |>
  filter(is.na(pop)) |>
  left_join(buckinghamshire_restructure_2020, by = c("lad_code" = "pre_lad_code")) |>
  group_by(post_ua_code) |>
  summarise(count = sum(count)) |>
  rename(lad_code = post_ua_code) |>
  left_join(pop_lad) |>
  mutate(perc_registered_gp = count / pop)

gp_registrations_clean <- gp_registrations |>
  filter(!is.na(pop)) |>
  bind_rows(gp_registrations_buck)

# Check distribution & total
summary(gp_registrations_clean$perc_registered_gp)

gp_registrations_clean |>
  summarise(total_gp_reg = sum(count), total_pop = sum(pop))

# Total population data for England is less than the GP registrations
# May need to assume that the over registrations (perhaps by people that have left country or died) is consistent across all LADs (as don't have info to test this) and use as relative comparison between LADs
# ONS did some analysis in 2016 on the difference between GP regs and pop but found mix of over and under differences https://commonslibrary.parliament.uk/population-estimates-gp-registers-why-the-difference/
# Commented out code at end of script tests if same issue with 2020 ONS population data and found still get majoritivly over 100%

# Current assumption: over registration rate equal across LADs (would need data on this otherwise)

# Save ----
gp_registrations_clean |>
  select(lad_code, perc_registered_gp) |>
  write_rds("data/capacity/health-inequalities/england/access-availability/gp-registrations.rds")