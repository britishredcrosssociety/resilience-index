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
  read_csv("https://www.healthmapswales.wales.nhs.uk/IAS/data/csv?viewId=142&geoId=108&subsetId=&viewer=CSV")

copd_unmatched <-
  raw %>%
  select(
    lad_name = Name,
    copd_rate_per_100000 = `Chronic Obstructive Pulmonary Disease (COPD): Emergency Admission Rates (Age-Standardised) per 100K pop(FY 17/18)`
  )

copd <-
  copd_unmatched %>%
  filter(lad_name != "Wales") %>%
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

write_rds(copd, "data/vulnerability/health-inequalities/wales/healthy-people/copd.rds")