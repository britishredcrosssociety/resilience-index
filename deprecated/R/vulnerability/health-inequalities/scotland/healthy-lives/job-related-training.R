library(readr)
library(readxl)
library(httr)
library(dplyr)
library(tidyr)
library(geographr)

# Load population estimates
# source: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2019
GET(
  "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-19/mid-year-pop-est-19-data.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

pop_raw <- read_excel(tf, sheet = "Table 2", range = "A4:C38")

# Calculate population estimates
pop <-
  pop_raw %>%
  slice(-(1:2)) %>%
  select(lad_code = `Area code1`, pop_count = `All Ages`)

# The NOMIS API query creator was used to generate the url in the GET request:
# Source: https://www.nomisweb.co.uk/datasets/apsnew
# Data Set: Annual Population Survery
# Indicator: T22a (Job related training (SIC 2007)
training_raw <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_17_1.data.csv?geography=1946157405...1946157408,1946157416,1946157409...1946157415,1946157418...1946157424,1946157417,1946157425...1946157436,2013265931&date=latest&cell=404693507,404694275&measures=20100,20701&select=date_name,geography_name,geography_code,cell_name,measures_name,obs_value,obs_status_name")

# Keep vars of interest
training <-
  training_raw %>%
  select(
    lad_name = GEOGRAPHY_NAME,
    lad_code = GEOGRAPHY_CODE,
    measures_name = MEASURES_NAME,
    count = OBS_VALUE
  ) %>%
  filter(measures_name == "Value") %>%
  select(-measures_name)

# Remove Scotland Aggregate and add counts for part-time and full-time employment
training <-
  training %>%
  filter(lad_code != "S92000003") %>%
  group_by(lad_code) %>%
  summarise(count = sum(count))

# Match LAD codes to population counts
# https://www.opendata.nhs.scot/dataset/geography-codes-and-labels/resource/967937c4-8d67-4f39-974f-fd58c4acfda5
# Look at the 'CADateArchived' column to view changes
training <-
  training %>%
  mutate(
    lad_code = case_when(
      lad_code == "S12000015" ~ "S12000047",
      lad_code == "S12000024" ~ "S12000048",
      lad_code == "S12000044" ~ "S12000050",
      lad_code == "S12000046" ~ "S12000049",
      TRUE ~ lad_code
    )
  )

# Join population counts and calculate relative rate
training <-
  training %>%
  left_join(pop, by = "lad_code") %>%
  mutate(job_related_training_percent = count / pop_count) %>%
  select(-count, -pop_count)

# Impute missing data
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/methodologies/methodsusedtodevelopthehealthindexforengland2015to2018
# Reason for missing: small sample size
# Strategy: replace with mean for that LAD's HB
missing_hb_code <-
  lookup_hb_lad %>%
  filter(lad_code == "S12000005") %>%
  pull(hb_code)

matching_lad_codes <-
  lookup_hb_lad %>%
  filter(hb_code == missing_hb_code) %>%
  filter(lad_code != "S12000005") %>%
  pull(lad_code)

missing_hb_score <-
  training %>%
  filter(lad_code %in% matching_lad_codes) %>%
  pull(job_related_training_percent) %>%
  mean()

training <-
  training %>%
  mutate(
    job_related_training_percent = if_else(
      is.na(job_related_training_percent),
      missing_hb_score,
      job_related_training_percent
    )
  )

write_rds(training, "data/vulnerability/health-inequalities/scotland/healthy-lives/job-related-training.rds")