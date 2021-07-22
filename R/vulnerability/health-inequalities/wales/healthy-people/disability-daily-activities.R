# ---- Load ----
library(readr)
library(httr)
library(dplyr)
library(geographr)
library(sf)

source("R/utils.R")

wales_codes <-
  boundaries_lad %>%
  as_tibble() %>%
  select(starts_with("lad")) %>%
  filter_codes(lad_code, "W") %>%
  pull(lad_code)

# ---- Retrieve data ----
# The NOMIS API query creator was used to generate the url in the GET request:
# Source: https://www.nomisweb.co.uk/datasets/apsnew
# Data Set: Annual Population Survery
# Indicator: % aged 16-64 who are EA core or work-limiting disabled
raw <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1946157401,1946157395,1946157400,1946157397,1946157392,1946157390,1946157385...1946157387,1946157384,1946157383,1946157399,1946157403,1946157394,1946157404,1946157391,1946157389,1946157398,1946157393,1946157402,1946157396,1946157388,2013265930&date=latest&variable=1733&measures=20599,21001,21002,21003&select=date_name,geography_name,geography_code,variable_name,measures_name,obs_value,obs_status_name")

# Keep vars of interest
disability_raw <-
  raw %>%
  select(
    lad_code = GEOGRAPHY_CODE,
    measures_name = MEASURES_NAME,
    disability_daily_activities_percent = OBS_VALUE
  ) %>%
  filter(measures_name == "Variable") %>%
  select(-measures_name)

# Remove Wales summary
disability <-
  disability_raw %>%
  filter(lad_code != "W92000004")

# Check LAD codes match
if (!setequal(disability$lad_code, wales_codes)) {
  stop("LAD codes do not match")
}

write_rds(disability, "data/vulnerability/health-inequalities/wales/healthy-people/disability-daily-activities.rds")