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

# Clean chairty list
charity_list_clean <-
  charity_list_raw %>%
  filter(
    charity_registration_status == "Registered" &
    charity_insolvent == FALSE &
    charity_in_administration == FALSE &
    !is.na(charity_contact_postcode)
  ) %>%
  select(
      org_num = "organisation_number",
      registed_num = "registered_charity_number",
      charity_name = "charity_name",
      status = "charity_registration_status",
      postcode = "charity_contact_postcode"
  ) %>%
  as_tibble()

# Remove the whitespace in postcode in order to join lookup_postcode_lad
charity_list_clean$postcode  <-
  str_replace_all(charity_list_clean$postcode, " ", "")

# Join charity list with lookup_postcode_lad, filter Wales
# then count charity numbers group by lad_code
charity_lad <-
  charity_list_clean %>%
  left_join(lookup_postcode_lad, by = "postcode") %>%
  filter(str_detect(lad_code, "^W")) %>%
  count(lad_code) %>%
  mutate(
    deciles =
      quantise(
        n,
        num_quantiles = 10,
        highest_quantile_worst = FALSE
      )
  )

  
write_rds(
  charity_lad,
  "data/capacity/disasters-emergencies/wales/charity-lad.rds"
)
