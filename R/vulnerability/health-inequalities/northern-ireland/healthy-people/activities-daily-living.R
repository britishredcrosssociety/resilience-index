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

adl <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    limited_adl_standardised_ratio = `Standardised ratio of people with a long-term health problem or disability\n(Excluding Mental Health problems)\n(NI = 100)`
  )

write_rds(adl, "data/vulnerability/health-inequalities/northern-ireland/healthy-people/activities-daily-living.rds")