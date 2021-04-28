library(httr)
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(geographr)

# Lookup
lookup <-
  boundaries_lad %>%
  select(-geometry) %>%
  as_tibble() %>%
  filter(str_detect(lad_code, "^S"))

# Source: https://www.gov.scot/publications/recorded-crime-scotland-2019-2020/
GET(
  "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/09/recorded-crime-scotland-2019-2020/documents/recorded-crime-2019-20-bulletin-tables-final/recorded-crime-2019-20-bulletin-tables-final/govscot%3Adocument/recorded-crime-2019-20-bulletin-tables-final.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

crime_raw <-
  read_excel(
    tf,
    sheet = "Table 9",
    range = "A4:L36"
  )

crime <-
  crime_raw %>%
  select(
    lad_name = `Local authority area`,
    crimes_per_10000 = `Total crimes and offences`
  ) %>%
  mutate(
    lad_name = case_when(
      lad_name == "Argyll & Bute" ~ "Argyll and Bute",
      lad_name == "Dumfries & Galloway" ~ "Dumfries and Galloway",
      lad_name == "Edinburgh, City of" ~ "City of Edinburgh",
      lad_name == "Perth & Kinross" ~ "Perth and Kinross",
      TRUE ~ lad_name
    )
  )

crime <-
  crime %>%
  left_join(lookup, by = "lad_name") %>%
  relocate(lad_code) %>%
  select(-lad_name)

write_rds(crime, "data/vulnerability/health-inequalities/scotland/healthy-places/personal-crime.rds")