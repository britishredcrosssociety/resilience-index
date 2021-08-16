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

gcse <-
  raw_tibble |>
  select(
    lad_code = `LGD2014 Code`,
    gcse_qualifications_percent = `Achieved At Least 5 GCSE's grades A*-C (or equiv) inc. GCSE English and GCSE Maths (%)`
  )

# Save
write_rds(gcse, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/gcse-achievement.rds")