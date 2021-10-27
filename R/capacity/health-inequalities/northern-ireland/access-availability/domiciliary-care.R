library(tidyverse)
library(geographr)
library(readxl)

source("R/utils.R")

raw_file <-
  download_file(
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/dcs-adults-ni-tables-20_0.XLSX",
    ".xslx"
  )

raw <-
  read_excel(
    raw_file,
    sheet = "Table2",
    skip = 2
  )

dom_care <-
  raw |>
  select(
    trust_code = `HSC Trust`,
    domiciliary_care_hours_per_client = `...10`
  ) |>
  slice(2:6)

dom_care |>
  write_rds("data/capacity/health-inequalities/northern-ireland/domiciliary-care.rds")