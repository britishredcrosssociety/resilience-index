library(tidyverse)
library(readODS)
library(httr)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Agriculture%20and%20Environment/Noise%20Complaints%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "A4:E15"
  )

noise <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    neighbourhood_noise_complaints_per_1000 = `Rate of Noise Complaints per 1,000 population`
  )

write_rds(noise, "data/vulnerability/health-inequalities/northern-ireland/healthy-places/neighbourhood-noise.rds")