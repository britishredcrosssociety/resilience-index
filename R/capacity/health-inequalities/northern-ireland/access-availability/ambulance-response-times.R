library(tidyverse)
library(readODS)
library(lubridate)

source("R/utils.R")

raw_file <-
  download_file(
    "https://www.ninis2.nisra.gov.uk/Download/Health%20and%20Social%20Care/Ambulance%20Response%20Times%20(administrative%20geographies).ods",
    ".ods"
  )

raw <-
  read_ods(
    raw_file,
    sheet = "HSCT",
    range = "B4:C9"
  ) |>
  as_tibble()

ambulance <-
  raw |>
  select(
    trust_code = `HSCT Code`,
    ambulance_response_time = `Median Response Time (Minutes/Seconds)`
  ) |>
  mutate(ambulance_response_time_seconds = period_to_seconds(hms(ambulance_response_time))) |>
  select(-ambulance_response_time)

ambulance |>
  write_rds("data/capacity/health-inequalities/northern-ireland/ambulance-response-times.rds")