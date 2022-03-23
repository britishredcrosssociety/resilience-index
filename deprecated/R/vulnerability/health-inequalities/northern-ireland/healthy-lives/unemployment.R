library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Labour%20Market/Claimant%20Count%20Annual%20Averages%20-%20Experimental%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "B4:D15"
  )

unemployment <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    unemployment_rate = `Claimant Count Annual Averages (%)`
  )

# Save
write_rds(unemployment, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/unemployment.rds")