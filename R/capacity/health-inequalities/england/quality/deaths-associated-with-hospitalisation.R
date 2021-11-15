# Load libs
library(tidyverse)
library(geographr)
library(readxl)
library(sf)

source("R/utils.R") # for download_file()

# NHS trust deaths associated with hospitalisation -----

# Download the data
tf <- download_file("https://files.digital.nhs.uk/A6/0F708F/SHMI%20data%20files%2C%20Jul20-Jun21.zip", "zip")

unzip(tf, exdir = tempdir())

deaths_raw <-
  read_excel(
    list.files(
      tempdir(),
      pattern = "*SHMI data at trust level, Jul20-Jun21 \\(xls\\)",
      recursive = T,
      full.names = TRUE
    ),
    sheet = "Data",
    skip = 10
  )

deaths_columns <- deaths_raw %>%
  select(`Provider code`, `Provider name`, `SHMI value`, `SHMI banding`, `Number of spells`, `Observed deaths`, `Expected deaths`)
# 122 trusts

# NHS TRUST table in geographr package -----

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

# Check the matching of deaths data & trust table in geographr package --------
open_trusts |>
  anti_join(deaths_columns, by = c("trust_code" = "Provider code"))
# 93 trusts no deaths data
