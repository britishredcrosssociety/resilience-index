library(tidyverse)
library(geographr)
library(sf)
library(readxl)

source("R/utils.R")

raw_file <-
  download_file(
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/cc-adults-ni-tables-19-20.XLSX",
    ".xlsx"
  )

raw <-
  read_excel(
    raw_file,
    sheet = "Table 16",
    skip = 3
  )

care_home_beds_unweighted <-
  raw |>
  slice(2:6) |>
  rename(trust_name = `HSC Trust`) |>
  select(
    trust_name,
    bed_count = `...7`
  ) |>
  mutate(bed_count = as.double(bed_count))

care_home_beds <-
  care_home_beds_unweighted |>
  left_join(population_trusts_ni) |>
  mutate(care_home_beds_per_1000 = bed_count / population * 1000) |>
  select(trust_code, care_home_beds_per_1000)

care_home_beds |>
  write_rds("data/capacity/health-inequalities/northern-ireland/care-home-beds.rds")