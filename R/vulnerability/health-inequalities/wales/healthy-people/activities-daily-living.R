# ---- Load ----
library(tidyverse)
library(httr)
library(statswalesr)
library(geographr)
library(sf)

source("R/utils.R")

wales_codes <-
  boundaries_lad %>%
  as_tibble() %>%
  select(starts_with("lad")) %>%
  filter_codes(lad_code, "W") %>%
  pull(lad_code)

# ---- Extract data ----
raw <-
  statswales_get_dataset("hlth5052")

adl_unmatched <-
  raw %>%
  as_tibble() %>%
  select(
    lad_code = Area_Code,
    variable = Variable_ItemName_ENG,
    value = Data,
    measure = Measure_ItemName_ENG,
    standardisation = Standardisation_ItemName_ENG,
    year = Year_ItemName_ENG
  ) %>%
  filter(variable == "Limited at all by longstanding illness") %>%
  filter(year == "2018-19 & 2019-20") %>%
  filter(measure == "Percentage of adults (16)") %>%
  filter(standardisation == "Age standardised") %>%
  select(lad_code, limited_adl_percentage = value)

adl <-
  adl_unmatched %>%
  filter(lad_code %in% wales_codes)

write_rds(adl, "data/vulnerability/health-inequalities/wales/healthy-people/activities-daily-living.rds")