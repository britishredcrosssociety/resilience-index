# ---- Load ----
library(tidyverse)
library(statswalesr)
library(geographr)
source("R/utils.R")

# Download data
# Source: https://statswales.gov.wales/Catalogue/Local-Government/Finance/Revenue/Budgets/budgetedrevenueexpenditure-by-authority-service
raw <-
  statswales_get_dataset("lgfs0030") |>
  as_tibble()

# ---- Clean ----
expenditure <-
  raw |>
  filter(
    Year_ItemName_ENG == "2021-22",
    str_detect(Measure_ItemName_ENG, pattern = "per head"),
    str_detect(Authority_AltCode1, "^W060000")
  ) |>
  select(
    lad_code = Authority_AltCode1,
    variable = Description_ItemName_ENG,
    value = Data
  ) |>
  group_by(lad_code) |>
  summarise(la_spending_power = sum(value))

write_rds(
  expenditure,
  "data/capacity/disasters-emergencies/wales/la-spending-power.rds"
)