library(httr)
library(readxl)
library(dplyr)
library(readr)
library(stringr)
library(geographr)

# Lookup HSCP to LAD (contains 2011 and 2019 LAD codes):
# Source: # https://www.opendata.nhs.scot/km/dataset/geography-codes-and-labels/resource/967937c4-8d67-4f39-974f-fd58c4acfda5
lookup <- read_csv("https://www.opendata.nhs.scot/km/datastore/dump/967937c4-8d67-4f39-974f-fd58c4acfda5?bom=True")

# Keep only 2019 LAD codes
lookup <-
  lookup %>%
  select(
    lad_code = CA,
    hscp_name = HSCPName
  ) %>%
  filter(lad_code %in% boundaries_lad$lad_code) %>%
  distinct(lad_code, hscp_name, .keep_all = TRUE)

# Heart Failure
GET(
  "https://beta.isdscotland.org/media/3764/disease-prevalence.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

hf_raw <- read_excel(tf, sheet = "Data")

hf <-
  hf_raw %>%
  filter(Year == "2018-19 Financial Year") %>%
  filter(`Area Type` == "HSCP") %>%
  select(
    hscp_name = `GPPractice / Area`,
    heart_failure_percent = `Heart Failure Rate`
  ) %>%
  mutate(heart_failure_percent = heart_failure_percent / 100)

# Clean up HSCP names to match lookup table
hf <-
  hf %>%
  mutate(hscp_name = str_remove_all(hscp_name, "HSCP")) %>%
  mutate(hscp_name = str_squish(hscp_name)) %>%
  mutate(hscp_name = str_replace_all(hscp_name, "&", "and")) %>%
  mutate(hscp_name = if_else(hscp_name == "City of Edinburgh", "Edinburgh", hscp_name)) %>%
  mutate(hscp_name = if_else(hscp_name == "Comhairle nan Eilean Siar", "Western Isles", hscp_name))

# Join to lookup
hf <-
  hf %>%
  left_join(lookup, by = "hscp_name") %>%
  select(lad_code, heart_failure_percent)

write_rds(hf, "data/vulnerability/health-inequalities/scotland/healthy-people/heart-failure.rds")