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

# ---- Load Latest 3 year birth & death rates ----
GET(
  "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/RG_2017.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

raw_infant_deaths_2017 <-
  read_excel(
    list.files(
      tempdir(),
      pattern = "Stillbirths_InfantDeaths_2017.xls",
      full.names = TRUE
    ),
    sheet = "Table 4.4b",
    range = "A4:C53"
  )

infant_deaths_2017 <-
  raw_infant_deaths_2017 |>
  slice(-c(1:6)) |>
  drop_na() |>
  select(
    lad_name = `...1`,
    infant_mortality_count = `All infant`
  ) |>
  mutate(lad_name = str_replace_all(lad_name, "&", "and")) |>
  left_join(ni_lookup) |>
  select(lad_code, infant_mortality_count) |>
  mutate(year = "2017")

raw_live_births_2017 <-
  read_excel(
    list.files(
      tempdir(),
      pattern = "Births_2017.xls",
      full.names = TRUE
    ),
    sheet = "Table 3.7b",
    range = "A4:C52"
  )

live_births_2017 <-
  raw_live_births_2017 |>
  slice(-c(1:5)) |>
  drop_na() |>
  select(
    lad_name = Area,
    live_birth_count = `All births`
  ) |>
  mutate(lad_name = str_replace_all(lad_name, "&", "and")) |>
  left_join(ni_lookup) |>
  select(lad_code, live_birth_count) |>
  mutate(year = "2017")

GET(
  "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/RG_2018.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

raw_infant_deaths_2018 <-
  read_excel(
    list.files(
      tempdir(),
      pattern = "Stillbirths_Infant_Deaths_Tables_2018.xlsx",
      full.names = TRUE
    ),
    sheet = "Table 4.4b",
    range = "A4:C53"
  )

infant_deaths_2018 <-
  raw_infant_deaths_2018 |>
  slice(-c(1:6)) |>
  drop_na() |>
  select(
    lad_name = `...1`,
    infant_mortality_count = `All infant`
  ) |>
  mutate(lad_name = str_replace_all(lad_name, "&", "and")) |>
  left_join(ni_lookup) |>
  select(lad_code, infant_mortality_count) |>
  mutate(year = "2018")

raw_live_births_2018 <-
  read_excel(
    list.files(
      tempdir(),
      pattern = "Births_Tables_2018.xlsx",
      full.names = TRUE
    ),
    sheet = "Table 3.7b",
    range = "A3:C51"
  )

live_births_2018 <-
  raw_live_births_2018 |>
  slice(-c(1:5)) |>
  drop_na() |>
  select(
    lad_name = Area,
    live_birth_count = `All births`
  ) |>
  mutate(lad_name = str_replace_all(lad_name, "&", "and")) |>
  left_join(ni_lookup) |>
  select(lad_code, live_birth_count) |>
  mutate(year = "2018")

GET(
  "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Stillbirths_InfantDeaths_Tables_2019.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

raw_infant_deaths_2019 <-
  read_excel(
    tf,
    sheet = "Table 4.4b",
    range = "A4:C53"
  )

infant_deaths_2019 <-
  raw_infant_deaths_2019 |>
  slice(-c(1:6)) |>
  drop_na() |>
  select(
    lad_name = `2019`,
    infant_mortality_count = `All infant`
  ) |>
  mutate(lad_name = str_replace_all(lad_name, "&", "and")) |>
  left_join(ni_lookup) |>
  select(lad_code, infant_mortality_count) |>
  mutate(year = "2019")

GET(
  "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Births_Tables_2019.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

raw_live_births_2019 <-
  read_excel(
    tf,
    sheet = "Table 3.7b",
    range = "A3:C51"
  )

live_births_2019 <-
  raw_live_births_2019 |>
  slice(-c(1:5)) |>
  drop_na() |>
  select(
    lad_name = Area,
    live_birth_count = `All births`
  ) |>
  mutate(lad_name = str_replace_all(lad_name, "&", "and")) |>
  left_join(ni_lookup) |>
  select(lad_code, live_birth_count) |>
  mutate(year = "2019")

# ---- Join and calculate mean rate ----
all_births <-
  bind_rows(
    live_births_2017,
    live_births_2018,
    live_births_2019
  )

all_deaths <-
  bind_rows(
    infant_deaths_2017,
    infant_deaths_2018,
    infant_deaths_2019
  ) |>
  mutate(infant_mortality_count = as.double(infant_mortality_count))

joined <-
  all_births |>
  left_join(
    all_deaths,
    by = c("lad_code", "year")
  )

infant_mortality <-
  joined |>
  mutate(
    infant_mortality_per_1000 = (infant_mortality_count / live_birth_count) * 1000
  ) |>
  group_by(lad_code) |>
  summarise(infant_mortality_per_1000 = mean(infant_mortality_per_1000))

# Save
write_rds(infant_mortality, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/infant-mortality.rds")