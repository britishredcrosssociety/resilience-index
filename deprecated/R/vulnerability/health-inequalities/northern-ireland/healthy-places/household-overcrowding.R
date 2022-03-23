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

overcrowding <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    overcrowding_rate = `Rate of Household overcrowding\n(%)`
  )

write_rds(overcrowding, "data/vulnerability/health-inequalities/northern-ireland/healthy-places/household-overcrowding.rds")