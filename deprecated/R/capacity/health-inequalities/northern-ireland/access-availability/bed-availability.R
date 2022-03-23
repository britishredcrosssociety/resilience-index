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
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-inpatient-day-case-tables-20-21_0.xlsx",
    ".xslx"
  )

raw <-
  read_excel(
    raw_file,
    sheet = "2a",
    skip = 6
  )

beds_unweighted <-
  raw |>
  select(
    trust_name = `Hospital/\r\nHSC Trust`,
    total_beds_available = `Average\r\nAvailable\r\nBeds`
  ) |>
  filter(
    trust_name == "Belfast HSCT" |
      trust_name == "Northern HSCT" |
      trust_name == "South Eastern HSCT" |
      trust_name == "Southern HSCT" |
      trust_name == "Western HSCT"
  ) |>
  mutate(total_beds_available = as.double(total_beds_available)) |>
  mutate(trust_name = str_remove_all(trust_name, " HSCT"))

beds_available <-
  beds_unweighted |>
  left_join(pop) |>
  mutate(beds_per_1000 = total_beds_available / total_population * 1000) |>
  select(
    trust_code,
    beds_per_1000
  )

beds_available |>
  write_rds("data/capacity/health-inequalities/northern-ireland/access-availability/bed-availability.rds")