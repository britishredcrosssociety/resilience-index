# ---- Load libs ----
library(tidyverse)
library(httr)
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

# TODO:
# 1. Filter the joined charity list to onl health/social VCS orgs
# 2. Assign Local authority coverage to the geography_area columns:
#    - for geographic_area_type == "Local Authortiy", just keep the
#      corresponding geographic_area_description.
#    - for for geographic_area_type == "Country", remove all as presence will
#      be indistinguishable for any UK orgs. 
#    - for for geographic_area_type == "Region", decide if the region can be
#      assigned to multiple Local Authortities.