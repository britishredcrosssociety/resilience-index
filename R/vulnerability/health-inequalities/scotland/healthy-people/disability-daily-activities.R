library(readr)
library(httr)
library(dplyr)

# The NOMIS API query creator was used to generate the url in the GET request:
# Source: https://www.nomisweb.co.uk/datasets/apsnew
# Data Set: Annual Population Survery
# Indicator: % aged 16-64 who are EA core or work-limiting disabled
disability_raw <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1946157405...1946157408,1946157416,1946157409...1946157415,1946157418...1946157424,1946157417,1946157425...1946157436,2013265931&date=latest&variable=1733&measures=20599,21001,21002,21003&select=date_name,geography_name,geography_code,variable_name,measures_name,obs_value,obs_status_name")

# Keep vars of interest
disability <-
  disability_raw %>%
  select(
    lad_code = GEOGRAPHY_CODE,
    measures_name = MEASURES_NAME,
    disability_daily_activities_percent = OBS_VALUE
  ) %>%
  filter(measures_name == "Variable") %>%
  select(-measures_name)

# Remove Scotland Aggregate and convert to decimal
disability <-
  disability %>%
  filter(lad_code != "S92000003") %>%
  mutate(disability_daily_activities_percent = disability_daily_activities_percent / 100)

# Match 2019 LAD codes
# https://www.opendata.nhs.scot/dataset/geography-codes-and-labels/resource/967937c4-8d67-4f39-974f-fd58c4acfda5
# Look at the 'CADateArchived' column to view changes
disability <-
  disability %>%
  mutate(
    lad_code = case_when(
      lad_code == "S12000015" ~ "S12000047",
      lad_code == "S12000024" ~ "S12000048",
      lad_code == "S12000046" ~ "S12000049",
      lad_code == "S12000044" ~ "S12000050",
      TRUE ~ lad_code
    )
  )

write_rds(disability, "data/vulnerability/health-inequalities/scotland/healthy-people/disability-daily-activities.rds")