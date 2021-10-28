library(tidyverse)
library(lubridate)
library(geographr)

lookup <-
  population_trusts_ni |>
  select(starts_with("trust_"))

raw <-
  read_csv(
    "https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-niwts-tables-outpatients-q1-21-22_0.csv",
    col_types = cols(
      .default = col_character()
    )
  )

outpatient_vars <-
  raw |>
  select(-`...25`, -`...26`) |>
  rename(trust_name = `HSC Trust`)

# Remove commas from the data columns
outpatient_commas <-
  outpatient_vars |>
  mutate(
    across(
      `0 - 6 weeks`:`Total Waiting`,
      ~ as.numeric(str_remove(.x, ","))
    )
  )

outpatient_date <-
  outpatient_commas |>
  mutate(date = dmy(`Quarter Ending`)) |>
  relocate(date) |>
  filter(date == max(date, na.rm = TRUE))

outpatient_summaries <-
  outpatient_date |>
  group_by(date, trust_name, Specialty) |>
  summarise(
    `Total waiting > 18 weeks` = sum(`>18-52 weeks`, na.rm = TRUE) + sum(`>52 weeks`, na.rm = TRUE),
    `Total waiting > 52 weeks` = sum(`>52 weeks`, na.rm = TRUE),
    `Total waiting` = sum(`Total Waiting`, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(date, trust_name) |>
  summarise(
    `Outpatient: Total waiting > 18 weeks` = sum(`Total waiting > 18 weeks`, na.rm = TRUE),
    `Outpatient: Total waiting > 52 weeks` = sum(`Total waiting > 52 weeks`, na.rm = TRUE),
    `Outpatient: Total waiting` = sum(`Total waiting`, na.rm = TRUE)
  ) |>
  ungroup()

waiting_lists <-
  outpatient_summaries |>
  mutate(
    `Outpatient: % waiting > 18 weeks` = `Outpatient: Total waiting > 18 weeks` / `Outpatient: Total waiting`,
    `Outpatient: % waiting > 52 weeks` = `Outpatient: Total waiting > 52 weeks` / `Outpatient: Total waiting`
  ) |>
  select(-date) |>
  filter(trust_name != "Day Case Procedure Centre") |>
  left_join(lookup) |>
  select(
    trust_code,
    outpatient_waiting_more_52_weeks_percent = `Outpatient: % waiting > 52 weeks`
  )

waiting_lists |>
  write_rds("data/capacity/health-inequalities/northern-ireland/waiting-lists.rds")