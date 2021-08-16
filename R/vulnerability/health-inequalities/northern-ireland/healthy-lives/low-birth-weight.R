library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Health%20and%20Social%20Care/Low%20Birth%20Weight%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "B4:C15"
  )

birth_weight <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    low_birth_weight_rate = `Live Births Under 2,500g (%)`
  )

# Save
write_rds(birth_weight, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/birth-weight.rds")