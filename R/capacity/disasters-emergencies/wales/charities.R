#  Load packages
library(httr)
library(tidyverse)
library(geographr)
library(DataExplorer)
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
# ??? Need to filter charity type?
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
      latest_expenditure = "latest_expenditure",
      charity_type = "charity_type"
  )


# # Data exlporation
# plot_intro(charity_list_active)
# # 11% missing on latest_income and expenditure
# plot_missing(charity_list_active)
# # histogram chart
# # !!! Few "BIG" charities have extremely large value, most are small, use log transform
# plot_histogram(charity_list_active)

# Clean charity area
# The result shows that one organisation could has multiple geo_area_description
charity_areas_clean  <-
  charity_areas_raw %>%
  # Only select Wales LA
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

# To avoid skewness, try log transformation
charity_areas_log <-
  charity_areas_clean %>%
  mutate(
    latest_income = log(latest_income + 1),  # +1 to avoid -inf
    latest_income = replace_na(latest_income, 0),  # replace NA with 0
    latest_expenditure = log(latest_expenditure + 1),  # +1 to avoid -inf
    latest_expenditure = replace_na(latest_expenditure, 0)  # replace NA with 0
  )


# plot_histogram(charity_areas_log)  # looks normal
# # 0.01% missisng on charity type，
# # 1.57% missisng on income and expenditure
# plot_missing(charity_areas_log)


# Though one orgnisation works in multiple lad, their latest income are the same
# !!! Question here is how to distinguish charity income in different LA?
charity_areas_log %>%
  filter(latest_income > 0)


# Welsh Local Authorities
charity_income_lad <-
  charity_areas_log %>%
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
  group_by(lad_code) %>%
  summarise(across(latest_income, sum)) %>% # aggregate by LA

  # For all ranks: 1 is most deprived
  # mutate(rank = rank(latest_income)) %>%
  mutate(
    rank = rank(latest_income),
    deciles = quantise(rank, num_quantiles = 10)
  ) %>%
  select(-latest_income, -rank)

# Right now, only aggregate total income by LA,
# Need to consider how to rank
# By average? By population?
write_rds(
  charity_income_lad,
  "data/capacity/disasters-emergencies/wales/charity-lad.rds"
)
