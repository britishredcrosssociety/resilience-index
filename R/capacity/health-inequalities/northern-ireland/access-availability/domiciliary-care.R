library(tidyverse)
library(geographr)
library(readxl)

source("R/utils.R")

lookup <-
  population_trusts_ni |>
  select(starts_with("trust_"))

raw_file <-
  download_file(
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/dcs-adults-ni-tables-20_0.XLSX",
    ".xslx"
  )

raw <-
  read_excel(
    raw_file,
    sheet = "Table9",
    skip = 2
  )

dom_care_select <-
  raw |>
  select(
    trust_name = `HSC Trust`,
    domiciliary_care_per_10000 = `...12`
  ) |>
  slice(2:6)

dom_care <-
  dom_care_select |>
  left_join(lookup) |>
  select(trust_code, domiciliary_care_per_10000)

dom_care |>
  write_rds("data/capacity/health-inequalities/northern-ireland/access-availability/domiciliary-care.rds")