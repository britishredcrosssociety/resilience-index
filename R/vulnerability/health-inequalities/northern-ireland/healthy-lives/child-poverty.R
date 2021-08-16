library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Making%20Life%20Better/Northern%20Ireland%20Multiple%20Deprivation%20Measure%202017%20-%20Indicators%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "A4:AM15"
  )

child_poverty <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    child_poverty_percent = `Proportion of the population aged 15 and under living in households whose equivalised income is below 60 per cent of the NI median\n(%)`
  )

# Save
write_rds(child_poverty, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/child-poverty.rds")