# ---- Load ----
library(tidyverse)
library(httr)
library(statswalesr)
library(geographr)
library(sf)

source("R/utils.R")

wales_lookup <-
  boundaries_lad %>%
  as_tibble() %>%
  select(starts_with("lad")) %>%
  filter_codes(lad_code, "^W")

# ---- Extract data ----
raw <-
  statswales_get_dataset("wimd1911")

cancer_incidence <-
  raw %>%
  as_tibble() %>%
  select(
    lad_code = LocalAuthority_Code,
    variable = Indicator_ItemName_ENG,
    value = Data
  ) %>%
  filter(variable == "Cancer incidence (rate per 100,000)") %>%
  right_join(wales_lookup) %>%
  select(lad_code, cancer_incidence_per_100000 = value)

write_rds(cancer_incidence, "data/vulnerability/health-inequalities/wales/healthy-people/cancer-incidence.rds")