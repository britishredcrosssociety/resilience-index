# Load packages ----
library(tidyverse)
library(readxl)
library(geographr)
library(httr)

# Load data and prep ----
# Data saved: https://brcsbrms.sharepoint.com/sites/StrategicInsightandForesight/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStrategicInsightandForesight%2FShared%20Documents%2FData%2FFire%20response%20times&viewid=d6ca3ec2%2D0b57%2D4ed5%2D8934%2Da4981af10332
# Source: email request to FireStatistics@homeoffice.gov.uk

# Request times for dwelling fires only due to the nature of fires responded to by VCS
# 'Dwellings' is the sum of 'House/bungalow', 'Flats' & 'Other Dwellings'
raw <- read_excel("data/on-disk/fire-response-times/response times data - by LA for dwelling fires 2020 to 2021 fy.xlsx",
  sheet = "RAW DATA"
)

raw_filtered <- raw |>
  rename_with(tolower, everything()) |>
  drop_na(incident_type)

# Definitions:
# Total response time: time of call to first vehicle to arrive at the incident.
# Call handling: time of call to time station notified
# Crew turnout: from time station notified to first vehicle to leave
# Drive time: from time first vehicle leaves to first vehicle to arrive at the incident

# Assume call handling be outwith the LAs fire service control and be a 999 operator variable. So will not include this.
# Drive time arguably not just reflective of speed but also proximity to fires of fire stations?
fire_response <- raw_filtered |>
  mutate(station_response_time = total_response_time - call_handling_time) |>
  select(local_authority, station_response_time)

fire_response_stats <- fire_response |>
  group_by(local_authority) |>
  summarise(
    mean = mean(station_response_time),
    median = median(station_response_time),
    sd = sd(station_response_time),
    count = n()
  )

fire_response_median <- fire_response_stats |>
  select(local_authority, median_response_time = median)

# Join on the LTLA codes -------------

ltla_2021 <- lookup_lad_21_counties_ua_21 |>
  distinct(lad_21_code, lad_21_name)

ltla_21_matches <- fire_response_median |>
  inner_join(ltla_2021, by = c("local_authority" = "lad_21_name")) |>
  rename(lad_21_name = "local_authority")

# Any remaining unmatched
ltla_21_remaining <- fire_response_median |>
  anti_join(ltla_2021, by = c("local_authority" = "lad_21_name"))

# To save to send to Fire Stats email
ltla_21_remaining |>
  write_csv("~/Documents/fire-response-times-ltlas-remaining.csv")

# Check if any unmatches are 2019 LTLA codes -----
ltla_19_matches <- lookup_lad_lad |>
  inner_join(ltla_21_remaining, by = c("lad_19_name" = "local_authority")) |>
  select(lad_19_name, lad_21_name, lad_21_code)

# For those with 2019 codes take median response times across the 2019 LTLAs within the 2021 LTAs
ltla_19_matches_times <- fire_response |>
  inner_join(ltla_19_matches, by = c("local_authority" = "lad_19_name")) |>
  group_by(lad_21_name, lad_21_code) |>
  summarise(median_response_time = median(station_response_time)) |>
  ungroup()

# Any remaining unmatched
ltla_19_remaining <- ltla_21_remaining |>
  anti_join(ltla_19_matches, by = c("local_authority" = "lad_19_name"))


# Matches to Local Authorities that were pre 2019 ------------

# Local Authority code changes (not covered in the lookup_lad_lad table in geographr package)
# Source: https://geoportal.statistics.gov.uk/datasets/code-history-database-december-2021-for-the-united-kingdom/about
query_url <- "https://www.arcgis.com/sharing/rest/content/items/a8807f0914234a52b00edecdc34428fa/data"

GET(
  query_url,
  write_disk(
    zip_folder <- tempfile(fileext = ".zip")
  )
)

unzip(zip_folder, exdir = tempdir())

all_changes <-
  read_csv(
    file.path(
      tempdir(),
      "Changes.csv"
    )
  )

# Clean up columns
all_changes <-
  all_changes |>
  distinct(
    GEOGCD,
    GEOGNM,
    GEOGCD_P,
    GEOGNM_P,
    YEAR
  ) |>
  mutate(year_prev = YEAR - 1) |>
  rename(
    code_new = GEOGCD,
    name_new = GEOGNM,
    code_prev = GEOGCD_P,
    name_prev = GEOGNM_P,
    year_new = YEAR
  )

# Only keep changes that occurred within between 2018/19
# These are not covered in the 'lookup_lad_lad' table in geographr
changes_2019 <- all_changes |>
  filter(year_new == 2019)

# Need to filter out changes to 'Bournemouth, Christchurch and Poole' registration authorites
# Only want LTLA changes
ltla_18_matches <- ltla_19_remaining |>
  inner_join(changes_2019, by = c("local_authority" = "name_prev")) |>
  select(lad_18_name = local_authority, lad_19_name = name_new, lad_19_code = code_new) |>
  filter(str_detect(lad_19_code, "^E0"))

# For those with new 2019 codes take median response times across the 2019 LTLAs within the 2021 LTAs
# These codes don't change between 2019 and 2021 so rename at end to align with other datasets
ltla_18_matches_times <- fire_response |>
  inner_join(ltla_18_matches, by = c("local_authority" = "lad_18_name")) |>
  group_by(lad_19_name, lad_19_code) |>
  summarise(median_response_time = median(station_response_time)) |>
  ungroup() |>
  rename(lad_21_name = lad_19_name, lad_21_code = lad_19_code)


# Any remaining unmatched
ltla_18_remaining <- ltla_19_remaining |>
  anti_join(ltla_18_matches, by = c("local_authority" = "lad_18_name"))

# Shepway was renamed to 'Folkestone and Hythe' (E07000112) in 2018 ----
# Check only Shepway in data and not also an entry for 'Folkestone and Hythe'
fire_response_median |>
  filter(local_authority %in% c("Shepway", "Folkestone and Hythe"))

shepway_time <- fire_response_median |>
  filter(local_authority == "Shepway") |>
  pull(median_response_time)

folkestone <- ltla_2021 |>
  filter(lad_21_name == "Folkestone and Hythe") |>
  mutate(median_response_time = shepway_time)

# Combined all the matches -----
combined <- ltla_21_matches |>
  bind_rows(ltla_19_matches_times) |>
  bind_rows(ltla_18_matches_times) |>
  bind_rows(folkestone) |>
  ungroup() 

# Checks on combined
combined |>
  group_by(lad_21_name) |>
  mutate(count = n()) |>
  filter(count > 1)

# Check all 2021 LTLA codes are included
ltla_2021 |> 
  filter(str_detect(lad_21_code, "^E")) |>
  anti_join(combined, by = "lad_21_code")
# Isle of Scilly missing (may be included in Cornwall local authority data or could be missinf)

# Save data ----
combined |>
  select(lad_code = lad_21_code, median_response_time) |>
  write_rds("data/capacity/disasters-emergencies/england/fire-response-times.rds")


