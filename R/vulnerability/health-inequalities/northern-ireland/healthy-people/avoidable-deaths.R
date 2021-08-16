library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Health%20and%20Social%20Care/Standardised%20Death%20Rate%20-%20Avoidable%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "B4:E15"
  )

avoidable_deaths <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    avoidable_death_rate_per_100000 = `Standardised Death Rate - Avoidable (per 100,000): All`
  )

write_rds(avoidable_deaths, "data/vulnerability/health-inequalities/northern-ireland/healthy-people/avoidable-deaths.rds")