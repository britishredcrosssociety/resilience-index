library(tidyverse)
library(readxl)
library(geographr)

source("R/utils.R")

lookup <-
  population_trusts_ni |>
  select(starts_with("trust_"))

raw_file <-
  download_file(
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/qof-achievement-points-payments-summary-2019-20_0.xlsx",
    ".xlsx"
  )

raw <-
  read_excel(
    raw_file,
    sheet = "QOF achievement summary",
    range = "A7:F12"
  )

cqc_ratings <-
  raw |>
  rename(trust_name = LCG1) |>
  left_join(lookup) |>
  select(
    trust_code,
    cqc_rating = `% Points achieved`
  )

cqc_ratings |>
  write_rds("data/capacity/health-inequalities/northern-ireland/quality/cqc-ratings.rds")