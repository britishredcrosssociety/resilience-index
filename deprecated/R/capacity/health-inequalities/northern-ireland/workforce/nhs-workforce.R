library(tidyverse)
library(readxl)
library(geographr)

pop <-
  population_trusts_ni |>
  select(
    starts_with("trust_"),
    total_population
  )

source("R/utils.R")

raw_file <-
  download_file(
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/hscwb-key-facts-tables-june-2021.xlsx",
    ".xlsx"
  )

raw <-
  read_excel(
    raw_file,
    sheet = "Tables",
    range = "A22:S27"
  )

nhs_selected <-
  raw |>
  select(
    trust_name = "...1",
    starts_with("WTE")
  )

nhs_totals <-
  nhs_selected |>
  rowwise(trust_name) |>
  summarise(
    total_workforce = sum(c_across(starts_with("WTE")))
  ) |>
  ungroup()

nhs_workforce <-
  nhs_totals |>
  mutate(
    trust_name = str_remove_all(
      trust_name, " HSC Trust$"
    )
  ) |>
  left_join(pop) |>
  mutate(
    nhs_workforce_per_1000 = total_workforce / total_population * 1000
  ) |>
  select(
    trust_code,
    nhs_workforce_per_1000
  )

nhs_workforce |>
  write_rds("data/capacity/health-inequalities/northern-ireland/workforce/nhs-workforce.rds")