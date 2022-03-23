library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Children%20Education%20and%20Skills/School%20Leavers%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "A4:S15"
  )

raw_tibble <-
  raw |>
  as_tibble()

young_people <-
  raw_tibble |>
  select(
    lad_code = `LGD2014 Code`,
    young_people_unemployed_unknown_percent = `Destination: Unemployed/Unknown (%)`
  )

# Save
write_rds(young_people, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/young-peoples-training.rds")