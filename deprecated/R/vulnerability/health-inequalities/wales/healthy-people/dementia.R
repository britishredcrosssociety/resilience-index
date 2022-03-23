# ---- Load ----
library(tidyverse)
library(geographr)
library(sf)

source("R/utils.R")

wales_lookup <-
  boundaries_lad %>%
  as_tibble() %>%
  select(starts_with("lad")) %>%
  filter_codes(lad_code, "^W")

# ---- Extract and clean ----
raw <-
  read_csv("https://www.healthmapswales.wales.nhs.uk/IAS/data/csv?viewId=214&geoId=108&subsetId=&viewer=CSV")

dementia_unmatched <-
  raw %>%
  select(
    lad_name = Name,
    dementia_death_rate_per_100000 = `Dementia: Death Rates (Age-Standardised) per 100K pop(2017)`
  )

dementia <-
  dementia_unmatched %>%
  mutate(
    lad_name = if_else(
      lad_name == "The Vale of Glamorgan",
      "Vale of Glamorgan",
      lad_name
    )
  ) %>%
  right_join(wales_lookup) %>%
  relocate(lad_code) %>%
  select(-lad_name)

write_rds(dementia, "data/vulnerability/health-inequalities/wales/healthy-people/dementia.rds")