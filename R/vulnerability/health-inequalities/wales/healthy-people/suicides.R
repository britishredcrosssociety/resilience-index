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

hip_fractures <-
  raw %>%
  filter(Title == "Hip fractures among older people, 2018/19") %>%
  select(
    lad_name = Area,
    hip_fractures_per_100000 = `Area Value`
  ) %>%
  mutate(hip_fractures_per_100000 = as.double(hip_fractures_per_100000)) %>%
  right_join(wales_lookup) %>%
  select(lad_code, hip_fractures_per_100000)

write_rds(hip_fractures, "data/vulnerability/health-inequalities/wales/healthy-people/frailty.rds")