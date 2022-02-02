# ---- Load ----
library(tidyverse)
library(sf)
library(geographr)

# lookup
lookup <-
  boundaries_lad |>
  st_drop_geometry()

pop <-
  population_lad |>
  select(lad_code, total_population)

# Data must be extracted from Stat-Xplore. It is stored locally on disk and
# appended to .gitignore.
raw <-
  read_csv(
    "data/on-disk/england-carers-allowance-raw.csv",
    skip = 9
  )

# ---- Clean ----
# Remove empty rows & columns
raw_england <-
  raw |>
  select(lad_name = Quarter, count = `Feb-21`) |>
  slice(c(2:310))

# Change back to 2019 LAD areas for consistency with the rest of the RI
buckinghamshire <-
  raw_england |>
  filter(lad_name == "Buckinghamshire") |>
  pull(count)

north_northamptonshire <-
  raw_england |>
  filter(lad_name == "North Northamptonshire") |>
  pull(count)

west_northamptonshire <-
  raw_england |>
  filter(lad_name == "West Northamptonshire") |>
  pull(count)

missing_lads <-
  tribble(
    ~lad_name, ~count,
    "Aylesbury Vale", buckinghamshire,
    "Chiltern", buckinghamshire,
    "South Bucks", buckinghamshire,
    "Wycombe", buckinghamshire,
    "Corby", north_northamptonshire,
    "East Northamptonshire", north_northamptonshire,
    "Kettering", north_northamptonshire,
    "Wellingborough", north_northamptonshire,
    "Daventry", west_northamptonshire,
    "Northampton", west_northamptonshire,
    "South Northamptonshire", west_northamptonshire
  )

all_lads <-
  raw_england |>
  filter(
    lad_name != "Buckinghamshire",
    lad_name != "North Northamptonshire",
    lad_name != "West Northamptonshire"
  ) |>
  bind_rows(missing_lads)

# Join LAD codes and pop counts
joined <-
  all_lads |>
  left_join(lookup) |>
  left_join(pop)

# Population counts are missing for 2019 LAD areas that became Buckinghamshire
buckinghamshire_pop <-
  pop |>
  filter(lad_code == "E06000060") |>
  pull(total_population)

joined_buck_pop <-
  joined |>
  replace_na(
    list(
      total_population = buckinghamshire_pop
    )
  )

# The population counts for the 2019 LAD areas that became North
# Northamptonshire and West Northamptonshire need adjusting to match their
# respective totals (because the Carers allowance count is also a summary)
adjusted_pop <-
  joined_buck_pop |>
  mutate(
    grouping_var = case_when(
      lad_name == "Corby" ~ "north_northamptonshire",
      lad_name == "East Northamptonshire" ~ "north_northamptonshire",
      lad_name == "Kettering" ~ "north_northamptonshire",
      lad_name == "Wellingborough" ~ "north_northamptonshire",
      lad_name == "Daventry" ~ "west_northamptonshire",
      lad_name == "Northampton" ~ "west_northamptonshire",
      lad_name == "South Northamptonshire" ~ "west_northamptonshire",
      TRUE ~ lad_name
    )
  ) |>
  group_by(grouping_var) |>
  mutate(total_population = sum(total_population)) |>
  ungroup() |>
  select(-grouping_var)

# Drop Isles of Scilly as no data present
dropped <-
  adjusted_pop |>
  filter(lad_name != "Isles of Scilly")

# Convert to Numeric
converted <-
  dropped |>
  mutate(count = as.double(count))

# ---- Calculate rate ----
carers_allowance <-
  converted |>
  mutate(carers_allowance_rate = count / total_population * 100) |>
  select(lad_code, carers_allowance_rate)

# ---- Save ----
carers_allowance |>
  write_rds("data/capacity/health-inequalities/england/access-availability/carers-allowance.rds")