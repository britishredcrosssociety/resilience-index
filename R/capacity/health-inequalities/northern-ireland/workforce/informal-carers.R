library(tidyverse)
library(readODS)

source("R/utils.R")

raw_file <-
  download_file(
    "https://www.ninis2.nisra.gov.uk/Download/Health%20and%20Social%20Care/Caring%20Responsibilities%20-%20Health%20Survey%20(administrative%20geographies).ods",
    ".ods"
  )

raw <-
  read_ods(
    raw_file,
    sheet = "HSCT",
    range = "B4:C9"
  ) |>
  as_tibble()

informal_carers <-
  raw |>
  rename(
    trust_code = `HSCT Code`,
    caring_responsibilites_percent = `Persons with caring responsibilities (%)`
  )

informal_carers |>
  write_rds("data/capacity/health-inequalities/northern-ireland/informal-carers.rds")