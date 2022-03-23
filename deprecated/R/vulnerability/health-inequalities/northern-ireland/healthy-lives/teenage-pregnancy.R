library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Population/Births%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "B4:I15"
  )

teenage_pregnancies <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    teenage_pregnancies_per_1000 = `Birth rate to teenage mothers per 1,000 female population aged 13-19 years`
  )

# Save
write_rds(teenage_pregnancies, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/teenage-pregnancies.rds")