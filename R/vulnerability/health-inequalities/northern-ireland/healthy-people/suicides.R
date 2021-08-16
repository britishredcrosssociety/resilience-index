library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Population/Deaths%20by%20Cause%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "B4:M15"
  )

suicide <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    suicide_percentage_of_all_deaths = `Deaths from suicide and undetermined intent (%)`
  )

write_rds(suicide, "data/vulnerability/health-inequalities/northern-ireland/healthy-people/suicide.rds")