#  Load packages
library(tidyverse)
library(geographr)
library(DataExplorer)
source("R/utils.R")

# ---- Load data ----
# Source: https://register-of-charities.charitycommission.gov.uk/register/full-register-download # nolint

# Chairty list
file_path <- download_file(url = "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity.zip", file_extension = ".zip") # nolint
unzip(file_path, exdir = tempdir())

charity_list_raw <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity.txt",
      full.names = TRUE
    )
  )

# Charity areas of operation
file_path <- download_file(url = "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity_area_of_operation.zip", file_extension = ".zip") # nolint
unzip(file_path, exdir = tempdir())

charity_areas_raw <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity_area_of_operation.txt",
      full.names = TRUE
    )
  )

# Charity classification
file_path <- download_file("https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity_classification.zip", ".zip") # nolint
unzip(file_path, exdir = tempdir())

charity_classification_raw <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity_classification.txt",
      full.names = TRUE
    )
  )

# charity_classification_raw %>%
#   distinct(classification_description) %>%
#   print(n = 34)

# Clean chairty list
charity_list_active <-
  charity_list_raw %>%
  filter(
    charity_registration_status == "Registered" &
    charity_insolvent == FALSE &
    charity_in_administration == FALSE
  ) %>%
  select(
      organisation_number = "organisation_number"
      # latest_income = "latest_income",
      # latest_expenditure = "latest_expenditure"
  )

# Clean charity area
# The result shows that one organisation could has multiple geo_area_description
charity_areas_clean  <-
  charity_areas_raw %>%
  # Only select Wales LA
  filter(
    geographic_area_type == "Local Authority" &
    welsh_ind == TRUE
  ) %>%
  select(
    organisation_number,
    geographic_area_type,
    lad_name = "geographic_area_description"
  ) %>%
  inner_join(charity_list_active, by = "organisation_number")


charity_classification_clean <-
  charity_classification_raw %>%
  select(
    organisation_number,
    classification_type,
    classification_description
  ) %>%
  inner_join(charity_areas_clean, by = "organisation_number") %>%

  # Mannualy choose
  filter(
    classification_description == "The Prevention Or Relief Of Poverty" |
    classification_description == "People With Disabilities" |
    classification_description == "Provides Other Finance" |
    classification_description == "The Advancement Of Health Or Saving Of Lives" |
    classification_description == "Religious Activities" |
    classification_description == "Acts As An Umbrella Or Resource Body" |
    classification_description == "Recreation" |
    classification_description == "Overseas Aid/famine Relief" |
    classification_description == "Armed Forces/emergency Service Efficiency"
  )


### ABANDONED
# To avoid skewness, try log transformation
# charity_areas_log <-
#   charity_areas_clean %>%
#   mutate(
#     latest_income = log(latest_income + 1),  # +1 to avoid -inf
#     latest_income = replace_na(latest_income, 0),  # replace NA with 0 or mean?
#     latest_expenditure = log(latest_expenditure + 1),  # +1 to avoid -inf
#     latest_expenditure = replace_na(latest_expenditure, 0)  # replace NA with 0 or mean?
#   )

# Welsh Local Authorities
charity_lad <-
  charity_classification_clean %>%
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
  count(lad_code)

write_rds(
  charity_lad,
  "data/capacity/disasters-emergencies/wales/charity-lad.rds"
)
