library(tidyverse)
library(geographr)

# Lookup
lookup <-
  boundaries_lad |>
  select(-geometry) |>
  st_drop_geometry() |>
  filter(str_detect(lad_code, "^N"))

# Source: https://uk-air.defra.gov.uk/data/pcm-data
air_pollution_raw <-
  read_csv(
    "https://uk-air.defra.gov.uk/datastore/pcm/popwmpm252019byUKlocalauthority.csv",
    skip = 2
  )

# Use the anthropogenic component for health burden calculations
air_pollution <-
  air_pollution_raw |>
  select(
    lad_name = `Local Authority`,
    air_pollution_weighted = `PM2.5 2019 (anthropogenic)`
  )

# Join
air_pollution <-
  lookup |>
  left_join(air_pollution, by = "lad_name") |>
  select(-lad_name)

write_rds(air_pollution, "data/vulnerability/health-inequalities/northern-ireland/healthy-places/air-pollution.rds")