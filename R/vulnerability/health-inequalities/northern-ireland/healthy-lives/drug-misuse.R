# ---- Load ----
library(tidyverse)
library(httr)
library(readxl)
library(geographr)
library(sf)

source("R/utils.R")

ni_lookup <-
  boundaries_lad |>
  st_drop_geometry() |>
  filter_codes(lad_code, "^N") |>
  select(lad_name, lad_code)

# ---- Load and clean drug deaths data ----
GET(
  "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Drug-related%20and%20drug-misuse%20deaths%2C%202009-2019.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

drug_raw <-
  read_excel(
    tf,
    sheet = "Table 9",
    range = "A3:K14"
  )

drug_deaths <-
  drug_raw |>
  select(
    lad_name = `Local Government District`,
    drug_related_deaths_per_100000 = `2019 Rate`
  )

# Match LAD names to geographr and join to LAD codes
drug_deaths_codes <-
  drug_deaths |>
  mutate(
    lad_name = str_replace_all(lad_name, "&", "and")
  ) |>
  left_join(ni_lookup) |>
  select(lad_code, drug_related_deaths_per_100000)

# Save
write_rds(drug_deaths_codes, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/drug-misuse.rds")