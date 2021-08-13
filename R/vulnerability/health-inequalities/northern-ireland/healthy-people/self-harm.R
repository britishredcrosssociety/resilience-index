library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Health%20and%20Social%20Care/Standardised%20Admission%20Rate%20for%20Self%20Harm%20Admissions%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "B4:E15"
  )

self_harm <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    self_harm_admissions_per_100000 = `Standardised Admission Rate`
  )

write_rds(self_harm, "data/vulnerability/health-inequalities/northern-ireland/healthy-people/self-harm.rds")