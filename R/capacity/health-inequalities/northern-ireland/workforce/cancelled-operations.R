library(tidyverse)
library(readxl)
library(geographr)

source("R/utils.R")

pop <-
  population_trusts_ni |>
  select(
    starts_with("trust_"),
    total_population
  )

raw_file <-
  download_file(
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-outpatient-tables-20-21.xlsx",
    ".xslx"
  )

raw_1 <-
  read_excel(
    raw_file,
    sheet = "2c",
    range = "A4:I10"
  ) |>
  slice(-1)

raw_2 <-
  read_excel(
    raw_file,
    sheet = "2c",
    range = "A16:I22"
  ) |>
  slice(-1)

raw <-
  raw_1 |>
  left_join(raw_2)

cancelled_count <-
  raw |>
  select(
    -`Patient treated elsewhere`,
    -`Administrative error by hospital / GP`,
    -`Total appointments cancelled by either patient or hospital`
  ) |>
  rowwise() |>
  mutate(total_cancelled = sum(c_across(!`HSC Trust`))) |>
  ungroup() |>
  select(
    trust_name = `HSC Trust`,
    num_cancelled_operations = total_cancelled
  ) |>
  mutate(
    trust_name = str_remove_all(trust_name, " HSCT")
  )

cancelled_operations <-
  cancelled_count |>
  left_join(pop) |>
  mutate(
    cancelled_operations_per_1000 = num_cancelled_operations / total_population * 1000
  ) |>
  select(
    trust_code,
    cancelled_operations_per_1000
  )

cancelled_operations |>
  write_rds("data/capacity/health-inequalities/northern-ireland/workforce/cancelled-operations.rds")