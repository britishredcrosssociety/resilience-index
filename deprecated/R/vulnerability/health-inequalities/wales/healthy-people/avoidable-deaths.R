# ---- Load libraries ----
library(tidyverse)
library(httr)
library(readxl)
library(geographr)
library(sf)

source("R/utils.R")

wales_lookup <-
  boundaries_lad %>%
  as_tibble() %>%
  select(starts_with("lad")) %>%
  filter_codes(lad_code, "^W")

# ---- Retrieve data ----
GET(
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2favoidablemortalitybylocalauthorityinenglandandwales%2f2019/avoidablemortalitybylocalauthority2019.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

raw <-
  read_excel(
    tf,
    sheet = "Table 1 ",
    range = "B5:BS435"
  )

avoidable_deaths <-
  raw %>%
  select(
    lad_code = `...1`,
    avoidable_deaths_per_100000 = `Rate per 100,000 population...67`
  ) %>%
  right_join(wales_lookup) %>%
  select(-lad_name)

write_rds(avoidable_deaths, "data/vulnerability/health-inequalities/wales/healthy-people/avoidable-deaths.rds")