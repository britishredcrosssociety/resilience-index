library(tidyverse)
library(httr)
library(readODS)
library(geographr)
library(sf)

pop_ni <-
  population_lad |>
  filter(str_detect(lad_code, "^N")) |>
  select(lad_code, total_population)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Agriculture%20and%20Environment/Noise%20Complaints%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "B4:AR15"
  )

noise_raw <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    complaints = `Noise Complaints: Transport`
  )

noise <-
  noise_raw |>
  left_join(pop_ni) |>
  mutate(transport_noise_complaints_per_1000 = (complaints / total_population) * 1000) |>
  select(lad_code, transport_noise_complaints_per_1000)

write_rds(noise, "data/vulnerability/health-inequalities/northern-ireland/healthy-places/transport-noise.rds")