# ---- Load ----
library(tidyverse)
library(geographr)
library(sf)

pop <-
  population_lad |>
  filter(str_detect(lad_code, "^N")) |>
  select(starts_with("lad_"), total_population)

# Data is unavailable for download copy/paste
raw <-
  tribble(
    ~lad_name, ~Income, ~Expenditure, ~Surplus,
    "Antrim and Newtownabbey", 64.7, 69.2, -4.5,
    "Ards and North Down", 61.7, 81.6, -19.9,
    "Armagh City, Banbridge and Craigavon", 91, 105, -14,
    "Belfast", 221.2, 239.3, -18.1,
    "Causeway Coast and Glens", 65.9, 78.1, -12.1,
    "Derry City and Strabane", 86.8, 88.5, -1.7,
    "Fermanagh and Omagh", 53.8, 58.3, -4.5,
    "Lisburn and Castlereagh", 67.9, 76.5, -8.5,
    "Mid and East Antrim", 68.8, 82.3, -13.6,
    "Mid Ulster", 62.4, 58.5, 3.9,
    "Newry, Mourne and Down", 72.7, 85.8, -13.1,
  )

surplus <-
  raw |>
  left_join(pop) |>
  mutate(
    surplus_per_1000 = Surplus / total_population * 1000
  ) |>
  select(
    lad_code, surplus_per_1000
  )

surplus |>
  write_rds("data/capacity/disasters-emergencies/northern-ireland/spending-power.rds")