library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Deprivation/Northern%20Ireland%20Multiple%20Deprivation%20Measure%202017%20-%20Indicators%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "B4:AM15"
  )

crime <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    `Rate of Violence (including sexual offences), robbery and public order\n(per 1,000 population)`,
    `Rate of Burglary\n(per 1,000 population)`,
    `Rate of Theft\n(per 1,000 population)`,
    `Rate of Criminal Damage and Arson\n(per 1,000 population)`
  ) |>
  rowwise() |>
  mutate(
    personal_crime_per_1000 = sum(c_across(!lad_code))
  ) |>
  ungroup() |>
  select(lad_code, personal_crime_per_1000)

write_rds(crime, "data/vulnerability/health-inequalities/northern-ireland/healthy-places/personal-crime.rds")