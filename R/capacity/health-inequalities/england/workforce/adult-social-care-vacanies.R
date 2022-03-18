# ---- Load libraries ----
library(tidyverse)
library(readxl)
library(httr)
library(geographr)
library(sf)

# ---- Load data ----
source("R/utils.R")

lad_lookup <-
  boundaries_lad |>
  st_drop_geometry()

lookup_counties_lad <-
  lookup_counties_ua_lad |>
  select(-lad_name, -county_ua_code)

# 2019 LAD populations
pop_lad <- geographr::population_lad |>
  select(
    lad_code,
    total_pop = total_population
  )

# Source page https://www.skillsforcare.org.uk/adult-social-care-workforce-data/Workforce-intelligence/publications/local-information/Local-authority-comparison.aspx
tf <- download_file(
  "https://www.skillsforcare.org.uk/adult-social-care-workforce-data/Workforce-intelligence/documents/Raw-data/LA-data-download.xlsx",
  ".xlsx"
)

raw <-
  read_excel(
    tf,
    sheet = "LA level data"
  )

# ---- Clean data ----
# Select cols
social <-
  raw |>
  select(
    lad_name = `CSSR (LA area)`,
    sector = Sector,
    service = Service,
    job_role = `Job role`,
    turnover = `Turnover rate`,
    vacancy = `Vacancy rate`,
    sickness_days_mean = `Sickness - Mean (divide by Employees)`,
    zero_hour_yes = `Zero Hour Contracts - Yes`
  )

# Select all services and sectors
all_services_sectors <-
  social |>
  filter(sector == "All sectors" &
    service == "All services")

# Select only job roles which provide direct care
direct_care_roles <-
  all_services_sectors |>
  filter(
    job_role == "Individual role - Care worker" |
      job_role == "Job role group - Direct care" |
      job_role == "Individual role - Occupational therapist" |
      job_role == "Individual role - Registered nurse" |
      job_role == "Individual role - Senior care worker" |
      job_role == "Individual role - Social worker"
  )

# Replace suppressed data (where there isn't enough jobs) with zero
# (see Notes tab in raw data) and convert to class double for computation
replace_suppressed <-
  direct_care_roles |>
  mutate(
    across(turnover:zero_hour_yes, ~ str_replace_all(., "\\*", "0")),
    across(turnover:zero_hour_yes, as.double)
  )

# Calculate mean values per LA
lad_means <-
  replace_suppressed |>
  group_by(lad_name) |>
  summarise(
    across(
      turnover:zero_hour_yes, mean
    )
  )

# Only take vacancy as proxy for inverse of the total workforce
lad_vacancy <- lad_means |>
  select(lad_name, vacancy)

# ---- Match LAD Codes ----
# The Local Authorities are a mixture of UTLA's & LTLA's
# - Match LTLA's -
match_ltlas <-
  lad_vacancy |>
  inner_join(lad_lookup) |>
  relocate(lad_code)

# - Match counties, weighting figures by population -
# Assumption: apply the vacancy value for the county to all LADs within the county

# Match counties
match_county <-
  lad_vacancy |>
  anti_join(lad_lookup) |>
  inner_join(
    lookup_counties_lad,
    by = c("lad_name" = "county_ua_name")
  ) |>
  relocate(lad_code)


# - Manually match anything that didn't match to an LA or county -
match_remainder <-
  lad_vacancy |>
  anti_join(lad_lookup) |>
  anti_join(
    lookup_counties_lad,
    by = c("lad_name" = "county_ua_name")
  ) |>
  mutate(
    lad_code = case_when(
      lad_name == "Redcar & Cleveland" ~ "E06000003",
      lad_name == "Stockton on Tees" ~ "E06000004",
      lad_name == "Durham" ~ "E06000047",
      lad_name == "Kingston upon Hull" ~ "E06000010",
      lad_name == "St Helens" ~ "E08000013",
      lad_name == "Stoke on Trent" ~ "E06000021",
      lad_name == "Herefordshire" ~ "E06000019",
      lad_name == "Telford & Wrekin" ~ "E06000020",
      lad_name == "Windsor & Maidenhead" ~ "E06000040",
      lad_name == "Southend on Sea" ~ "E06000033",
      lad_name == "Hammersmith & Fulham" ~ "E09000013",
      lad_name == "Kensington & Chelsea" ~ "E09000020",
      lad_name == "Barking & Dagenham" ~ "E09000002",
      lad_name == "Bournemouth Christchurch and Poole" ~ "E06000058",
      lad_name == "Brighton & Hove" ~ "E06000043",
      lad_name == "Cornwall and Isles of Scilly" ~ "E06000052/E06000053",
      lad_name == "Bristol" ~ "E06000023",
      lad_name == "Cheshire West & Chester" ~ "E06000050"
    )
  ) |>
  # separate Cornwall and Isles of Scilly
  separate_rows(lad_code, sep = "/") |>
  relocate(lad_code)

# Join all LAD19CD
match_all <-
  bind_rows(
    match_ltlas,
    match_county,
    match_remainder
  )

# Check any duplicates/missings
boundaries_lad |>
  filter(str_detect(lad_code, "^E")) |>
  anti_join(match_all, by = "lad_code")

match_all |>
  group_by(lad_code) |>
  summarise(count = n()) |>
  filter(count > 1)

# Check lad codes are 2021 ----
if(
  anti_join(
    match_all,
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
# Since don't have raw counts average across the 2019 LTLAs to the new 2021 LTLAs
match_all_update <- match_all |>
  left_join(lookup_lad_over_time, by = c("lad_code" = "LAD19CD")) |>
  group_by(LAD21CD) |>
  summarise(across(where(is.numeric), mean)) |>
  select(lad_code = LAD21CD, vacancy)

# Save ----
match_all_update |>
  write_rds("data/capacity/health-inequalities/england/workforce/adult-social-care-vacancies.rds")
