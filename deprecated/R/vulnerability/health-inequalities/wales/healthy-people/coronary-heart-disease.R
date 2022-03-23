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
  read_csv("https://www.healthmapswales.wales.nhs.uk/IAS/data/csv?viewId=224&geoId=108&subsetId=&viewer=CSV")

chd_unmatched <-
  raw %>%
  select(
    lad_name = Name,
    chd_mortality_rate_per_10000 = `ACS_4 Rate of mortality from coronary heart disease (CHD) in people aged under 75 years (age-standardised per 10,000 population) (2015-17)`
  )

chd <-
  chd_unmatched %>%
  filter(lad_name != "Wales") %>%
  mutate(
    lad_name = if_else(
      lad_name == "The Vale of Glamorgan",
      "Vale of Glamorgan",
      lad_name
    )
  ) %>%
  left_join(wales_lookup) %>%
  relocate(lad_code) %>%
  select(-lad_name)

write_rds(chd, "data/vulnerability/health-inequalities/wales/healthy-people/coronary-heart-disease.rds")