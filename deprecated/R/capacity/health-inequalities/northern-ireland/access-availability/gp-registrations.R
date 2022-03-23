library(tidyverse)
library(readODS)
library(geographr)

source("R/utils.R")

pop <-
  population_trusts_ni |>
  select(
    trust_code,
    total_population
  )

raw_file <-
  download_file(
    "https://www.ninis2.nisra.gov.uk/Download/Health%20and%20Social%20Care/GPs,%20Practices%20and%20Registered%20Patients%20(administrative%20geographies).ods",
    ".ods"
  )

raw <-
  read_ods(
    raw_file,
    sheet = "HSCT",
    range = "B4:D9"
  )

gps_unweighted <-
  raw |>
  select(
    trust_code = `HSCT Code`,
    gp_count = `GPs`
  )

gp_registrations <-
  gps_unweighted |>
  left_join(pop) |>
  mutate(
    gps_per_1000 = gp_count / total_population * 1000
  ) |>
  select(trust_code, gps_per_1000)

gp_registrations |>
  write_rds("data/capacity/health-inequalities/northern-ireland/access-availability/gp-registrations.rds")