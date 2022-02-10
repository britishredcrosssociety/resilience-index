#  Load packages
library(httr)
library(tidyverse)
library(geographr)
source("R/utils.R")

# ---- Load data ----
# Source: https://register-of-charities.charitycommission.gov.uk/register/full-register-download

# Chairty list
GET(
  "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)
unzip(tf, exdir = tempdir())

charity_list_raw <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity.txt",
      full.names = TRUE
    )
  )

# Charity areas of operation
GET(
  "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity_area_of_operation.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)
unzip(tf, exdir = tempdir())

charity_areas_raw <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity_area_of_operation.txt",
      full.names = TRUE
    )
  )

# Clean chairty list
charity_list_active <-
  charity_list_raw %>%
  filter(
    charity_registration_status == "Registered" &
    charity_insolvent == FALSE &
    charity_in_administration == FALSE
  ) %>%
  select(
      organisation_number = "organisation_number",
      latest_income = "latest_income",
      latest_expenditure = "latest_expenditure"
  )

# Clean charity area,
# Only select Wales LA,
# The result shows that one organisation could has multiple geo_area_description
charity_areas_clean  <-
  charity_areas_raw %>%
  filter(
    geographic_area_type == "Local Authority",
    welsh_ind == TRUE
  ) %>%
  select(
    organisation_number,
    geographic_area_type,
    lad_name = "geographic_area_description"
  ) %>%
  inner_join(charity_list_active, by = "organisation_number") 

# Though one orgnisation had multiple geo_area_description, their latest income are the same
# Question here is how to distinguish charity income in different LA?
charity_areas_clean %>% 
  filter(latest_income > 0)


# Welsh Local Authorities
charity_income_lad <-
  charity_areas_clean %>%
  mutate(
    lad_name = case_when(
      lad_name == "City Of Swansea" ~ "Swansea",
      lad_name == "Rhondda Cynon Taff" ~ "Rhondda Cynon Taf",
      lad_name == "Newport City" ~ "Newport",
      TRUE ~ lad_name
    ),
    lad_name = tolower(lad_name)
  ) %>%
  left_join(
    lookup_counties_ua_lad %>%
      mutate(lad_name = tolower(lad_name)),
    by = "lad_name"
  ) %>%
  mutate(
    latest_income = if_else(is.na(latest_income), 0, latest_income) # replace NA by 0
  ) %>%
  group_by(lad_code) %>%
  summarise(across(latest_income, sum)) # aggregate by la

write_rds(
  charity_income_lad,
  "data/capacity/disasters-emergencies/wales/charity-lad.rds"
)
