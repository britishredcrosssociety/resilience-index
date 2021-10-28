library(tidyverse)
library(geographr)

lookup <-
  population_trusts_ni |>
  select(starts_with("trust_"))

raw <-
  read_csv(
    "data/on-disk/ni-ae-wait-times-raw.csv"
  )

ae <-
  raw |>
  filter(
    Date == max(Date) &
      Type == "Type 1"
  ) |>
  select(
    trust_name = Trust,
    ae_percent_under_4h = `Percent Under 4 Hours`
  ) |>
  mutate(ae_percent_under_4h = str_remove(ae_percent_under_4h, "%")) |>
  mutate(ae_percent_under_4h = as.numeric(ae_percent_under_4h)) |>
  group_by(trust_name) |>
  filter(ae_percent_under_4h == min(ae_percent_under_4h)) |>
  ungroup() |>
  left_join(lookup) |>
  select(trust_code, ae_percent_under_4h)

ae |>
  write_rds("data/capacity/health-inequalities/northern-ireland/ae-waiting-times.rds")