library(tidyverse)
library(readxl)
library(covid19.nhs.data)
library(httr)
library(geographr)

source("R/utils.R")

# Load trust to LAD mappings from covid19.nhs.data package
# Trust map to LADs in a many-to-one fashion, with the proportion denoted by
# the p_trust column. See: https://epiforecasts.io/covid19.nhs.data/articles/mapping_summary.html
trust_lad <-
  trust_ltla_mapping %>%
  ungroup() %>% 
  rename(lad_code = geo_code) %>%
  select(-p_geo)

# Source:
# - https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/

# Date: Jan-March 2021

# Note: In general hospitals will experience capacity pressures at lower overall
# occupancy rates than would previously have been the case.

# - Night -
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Beds-Open-Overnight-Web_File-Final-Q4-2020-21-Final-THSDF.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

beds_nights_raw <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

# remove first two entries (one is totals, other is blank)
beds_nights_sliced <-
  beds_nights_raw %>%
  slice(-(1:2))

# Select cols
beds_nights_selected <-
  beds_nights_sliced %>%
  select(
    trust_code = `Org Code`,
    percentage_occupied_night = Total...18,
  )

# Replace '-' character with NA
beds_nights_na <-
  beds_nights_selected %>%
  mutate(
    percentage_occupied_night = str_replace_all(
      percentage_occupied_night,
      "-",
      NA_character_
    )
  )

# Change col to double
beds_nights_formatted <-
  beds_nights_na %>%
  mutate(
    percentage_occupied_night = as.double(percentage_occupied_night)
  )

# - Day -
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Beds-Open-Day-Only-Web_File-Final-Q4-2020-21-Final-THSDF.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

beds_days_raw <- read_excel(tf, sheet = "NHS Trust by Sector", skip = 14)

# remove first two entries (one is totals, other is blank)
beds_days_sliced <-
  beds_days_raw %>%
  slice(-(1:2))

# Select cols
beds_days_selected <-
  beds_days_sliced %>%
  select(
    trust_code = `Org Code`,
    percentage_occupied_day = Total...18,
  )

# Replace '-' character with NA
beds_days_na <-
  beds_days_selected %>%
  mutate(
    percentage_occupied_day = str_replace_all(
      percentage_occupied_day,
      "-",
      NA_character_
    )
  )

# Change cols to double
beds_days_formatted <-
  beds_days_na %>%
  mutate(
    percentage_occupied_day = as.double(percentage_occupied_day)
  )

# - Join -
beds_joined <-
  beds_nights_formatted %>% 
  left_join(
    beds_days_formatted,
    by = "trust_code"
  )

beds_mean <-
  beds_joined %>% 
  rowwise() %>% 
  mutate(
    beds_occupied = mean(
      c(percentage_occupied_night, percentage_occupied_day),
      na.rm = TRUE
      )
  ) %>% 
  ungroup() %>% 
  select(trust_code, beds_occupied)

# TODO:
# - Calculate LAD bed occupancy scores
# - Revisit the Trust-LTLA mapping. The epiforecasts method is both (i) missing
#   lots of trusts (is this because it only uses acute trusts?) that contain bed
#   occupancy data and (ii) is probably not entirely appropriate to use as the 
#   proportion estimates are based off covid admissions, which likely differ
#   from non-covid admissions. Potential solution: https://github.com/VictimOfMaths/COVID-19/issues/7

# Calculate the proportion of beds that are expected to be 
# available for any given LAD. This should be equivilant to the mean of the
# beds occupied at the Trust level, weighted by the proportion of that Trust
# serving the LAD (i.e., p_trust)
trust_lad %>% 
  left_join(beds_mean, by = "trust_code") %>% 
  mutate(
    p_beds_occupied = p_trust * beds_occupied
  )