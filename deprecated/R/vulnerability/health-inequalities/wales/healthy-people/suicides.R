# ---- Load ----
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
  "https://www2.nphs.wales.nhs.uk/PubHObservatoryProjDocs.nsf/3653c00e7bb6259d80256f27004900db/920b258a5a6102fc802581a1003c196d/$FILE/PHOFDataDownload2017_v1.xlsx",
  write_disk(tf <- tempfile(fileext = ".xslx"))
)

raw <-
  read_excel(
    tf,
    sheet = "Wales, HB & LA most recent data"
  )

suicides <-
  raw %>%
  filter(Title == "Suicides, 2014 to 2018") %>%
  select(
    lad_name = Area,
    suicides_per_100000 = `Area Value`
  ) %>%
  mutate(suicides_per_100000 = as.double(suicides_per_100000)) %>%
  right_join(wales_lookup) %>%
  select(lad_code, suicides_per_100000)

write_rds(suicides, "data/vulnerability/health-inequalities/wales/healthy-people/suicides.rds")