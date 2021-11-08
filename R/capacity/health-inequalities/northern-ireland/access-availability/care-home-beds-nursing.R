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
    sheet = "Table 17",
    skip = 3
  )

care_home_beds_nursing_unweighted <-
  raw |>
  slice(2:6) |>
  rename(trust_name = `HSC Trust`) |>
  select(
    trust_name,
    bed_count = `...7`
  ) |>
  mutate(bed_count = as.double(bed_count))

# Normalise per 1,000 population over 65
pop_over_65 <-
  population_trusts_ni |>
  select(starts_with("trust_"), `65`:`90`) |>
  pivot_longer(
    cols = !starts_with("trust_"),
    names_to = "age",
    values_to = "count"
  ) |>
  group_by(trust_code, trust_name) |>
  summarise(pop_over_65 = sum(count)) |>
  ungroup()

care_home_beds_nursing <-
  care_home_beds_nursing_unweighted |>
  left_join(pop_over_65) |>
  mutate(care_home_beds_nursing_per_1000 = bed_count / pop_over_65 * 1000) |>
  select(trust_code, care_home_beds_nursing_per_1000)

care_home_beds_nursing |>
  write_rds("data/capacity/health-inequalities/northern-ireland/access-availability/care-home-beds-nursing.rds")