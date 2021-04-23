library(httr)
library(readxl)
library(dplyr)
library(geographr)
library(stringr)
library(readr)

# LAD code lookup
lookup <-
  boundaries_lad %>%
  select(lad_name, lad_code) %>%
  as_tibble() %>%
  filter(str_detect(lad_code, "^S"))

# Source: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/avoidable-mortality
GET(
  "https://www.nrscotland.gov.uk/files//statistics/avoidable-mortality/2019/avoid-mortality-19-all-tabs.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

raw_avoiable_deaths <-
  read_excel(
    tf,
    sheet = "Table 5",
    range = "B10:C41",
    col_names = c("lad_name", "avoidable_deaths_per_100000")
  )

# Match to LAD codes
avoidable_deaths <-
  raw_avoiable_deaths %>%
  left_join(lookup, by = "lad_name") %>%
  select(lad_code, avoidable_deaths_per_100000)

write_rds(avoidable_deaths, "data/vulnerability/health-inequalities/scotland/healthy-people/avoidable-deaths.rds")