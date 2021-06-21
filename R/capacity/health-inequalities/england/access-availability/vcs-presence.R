# ---- Load libs ----
library(tidyverse)
library(httr)
library(geographr)
library(sf)
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

# Chairty returns
GET(
  "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity_annual_return_history.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

charity_returns_raw <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity_annual_return_history.txt",
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

# Chairty classification
GET(
  "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity_classification.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

charity_classification_raw <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity_classification.txt",
      full.names = TRUE
    )
  )

# ---- Clean data ----
# Chairty list
charity_list_cols <-
  charity_list_raw %>%
  select(
    registered_charity_number,
    charity_name,
    charity_registration_status,
    charity_insolvent,
    charity_in_administration
  )

# Keep only registered charities that are not insolvent or in administation
charities_active <-
  charity_list_cols %>%
  filter(charity_registration_status == "Registered") %>%
  filter(charity_insolvent == FALSE) %>%
  filter(charity_in_administration == FALSE) %>%
  select(
    registered_charity_number,
    charity_name
  )

# Charity returns
charity_returns_cols <-
  charity_returns_raw %>%
  select(
    registered_charity_number,
    total_gross_income
  )

# Calculate annual mean income
charity_returns_mean <-
  charity_returns_cols %>%
  group_by(registered_charity_number) %>%
  summarise(mean_annual_income = mean(total_gross_income, na.rm = TRUE))

# Charity areas of operation
charity_areas <-
  charity_areas_raw %>%
  select(
    registered_charity_number,
    geographic_area_type,
    geographic_area_description
  )

# Charity classification
charity_classification <-
  charity_classification_raw %>%
  select(
    registered_charity_number,
    classification_type,
    classification_description
  )

# ---- Join data ----
charities_joined <-
  charities_active %>%
  left_join(
    charity_returns_mean,
    by = "registered_charity_number"
  ) %>%
  left_join(
    charity_areas,
    by = "registered_charity_number"
  ) %>%
  left_join(
    charity_classification,
    by = "registered_charity_number"
  )

# ---- Keep health/social VCS orgs only ----
# Remove classifications that are obviously not related
charities_health <-
  charities_joined %>%
  filter(
    classification_description == "Accommodation/housing" |
      classification_description == "Children/young People" |
      classification_description == "Disability" |
      classification_description == "Economic/community Development/employment" |
      classification_description == "Education/training" |
      classification_description == "Elderly/old People" |
      classification_description == "People With Disabilities" |
      classification_description == "Sponsors Or Undertakes Research" |
      classification_description == "The Advancement Of Health Or Saving Of Lives" |
      classification_description == "The General Public/mankind" |
      classification_description == "The Prevention Or Relief Of Poverty"
  )

# ---- Assign geographies ----
# Assign Local Authorities to the geography_area columns:
#   - for for geographic_area_type == "Country", remove all as presence will
#     be indistinguishable for any UK orgs.
#   - for geographic_area_type == "Region", remove all regions, as they do not
#     provide any granular detail as they are at the devolved nation or London
#     level.
#   - for for geographic_area_type == NA, remove all as information is needed.
#   - for geographic_area_type == "Local Authortiy", just keep the
#     corresponding geographic_area_description and then match to the ONS
#     region. Note the areas are UTLA's.

# - Country, Region, & NA -
# Remove country, region, and NA level data
# i.e. keep only Local Authority level data
charities_local_authorities_not_matched <-
  charities_health %>%
  filter(geographic_area_type == "Local Authority")

# - Local Authorities -
# Create lists of UTLA's (2019) to compare against
utla_list <-
  boundaries_counties_ua %>%
  filter(str_detect(county_ua_code, "^E")) %>%
  mutate(county_ua_name = str_to_lower(county_ua_name)) %>%
  arrange(county_ua_name) %>%
  pull(county_ua_name)

# Find Local Authority names not matched in UTLA list
charities_local_authorities_not_matched %>%
  mutate(geographic_area_description = str_to_lower(geographic_area_description)) %>%
  select(geographic_area_description) %>%
  filter(!(geographic_area_description %in% utla_list)) %>%
  distinct() %>%
  print(n = Inf)

# Match UTLA names and keep on English UTLA's
charities_local_authorities <-
  charities_local_authorities_not_matched %>%
  mutate(geographic_area_description = str_to_lower(geographic_area_description)) %>%
  mutate(
    geographic_area_description = case_when(
      geographic_area_description == "bristol city" ~ "bristol, city of",
      geographic_area_description == "birmingham city" ~ "birmingham",
      geographic_area_description == "plymouth city" ~ "plymouth",
      geographic_area_description == "peterborough city" ~ "peterborough",
      geographic_area_description == "leeds city" ~ "leeds",
      geographic_area_description == "leicester city" ~ "leicester",
      geographic_area_description == "city of westminster" ~ "westminster",
      geographic_area_description == "herefordshire" ~ "herefordshire, county of",
      geographic_area_description == "liverpool city" ~ "liverpool",
      geographic_area_description == "nottingham city" ~ "nottingham",
      geographic_area_description == "city of york" ~ "york",
      geographic_area_description == "cheshire west & chester" ~ "cheshire west and chester",
      geographic_area_description == "sheffield city" ~ "sheffield",
      geographic_area_description == "salford city" ~ "salford",
      geographic_area_description == "durham" ~ "county durham",
      geographic_area_description == "city of wakefield" ~ "wakefield",
      geographic_area_description == "derby city" ~ "derby",
      geographic_area_description == "manchester city" ~ "manchester",
      geographic_area_description == "southampton city" ~ "southampton",
      geographic_area_description == "bradford city" ~ "bradford",
      geographic_area_description == "coventry city" ~ "coventry",
      geographic_area_description == "telford & wrekin" ~ "telford and wrekin",
      geographic_area_description == "st helens" ~ "st. helens",
      geographic_area_description == "kingston upon hull city" ~ "kingston upon hull, city of",
      geographic_area_description == "newcastle upon tyne city" ~ "newcastle upon tyne",
      geographic_area_description == "poole" ~ "bournemouth, christchurch and poole",
      geographic_area_description == "bournemouth" ~ "bournemouth, christchurch and poole",
      geographic_area_description == "stoke-on-trent city" ~ "stoke-on-trent",
      geographic_area_description == "portsmouth city" ~ "portsmouth",
      TRUE ~ geographic_area_description
    )
  ) %>%
  filter(geographic_area_description %in% utla_list)

# TODO:
# 1. keep only one record of each unique chairty
# 2. Calculate presence (no. of orgs multipled by mean annual income?)
# 3. Disaggregate to LTLA?