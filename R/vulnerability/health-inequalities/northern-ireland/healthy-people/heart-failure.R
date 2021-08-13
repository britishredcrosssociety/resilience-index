library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Health%20and%20Social%20Care/Disease%20Prevalence%20(Quality%20Outcomes%20Framework)%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "B4:AL15"
  )

hf <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    heart_failure_1_per_1000_patients = `Heart Failure 1 Register: Raw Prevalence per 1,000 patients2`,
    heart_failure_3_per_1000_patients = `Heart Failure 3 Register: Raw Prevalence per 1,000 patients2`
  ) |>
  rowwise() |>
  mutate(
    heart_failure_per_1000_patients = heart_failure_1_per_1000_patients + heart_failure_3_per_1000_patients
  ) |>
  ungroup() |>
  select(
    lad_code, heart_failure_per_1000_patients
  )

write_rds(hf, "data/vulnerability/health-inequalities/northern-ireland/healthy-people/heart-failure.rds")