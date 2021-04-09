##
## Calculate average fire and rescue response times
## - stats are given for Fire & Rescue Authorities, so just use that as the number for each Local Authority within every FRA
##
## Source: FIRE1001: Average response times by location and fire and rescue authority/geographical category, England
## - https://www.gov.uk/government/statistical-data-sets/fire-statistics-data-tables#response-times
##
library(tidyverse)
library(readxl)
library(httr)

# ---- Load data ----
GET("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/857926/fire-statistics-data-tables-fire1001-jan2020.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

fra_stats <- read_excel(tf, sheet = "Data incl. H&S")  # all data including "heat and smoke damage only" incidents

unlink(tf); rm(tf)

# Local Authority District to Fire and Rescue Authority (December 2019) Lookup in England and Wales
# Source: https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-fire-and-rescue-authority-december-2019-lookup-in-england-and-wales
lad_fra <- read_csv("https://opendata.arcgis.com/datasets/cdfde6c07a2145e6a275a41ed9d7a906_0.csv")

# ---- Prep data ----
fra_stats_sum <- fra_stats %>% 
  filter(FIRE_TYPE == "Dwellings" & GEOGRAPHICAL_CATEGORY != "England") %>% 
  mutate(Year = as.integer(str_sub(FINANCIAL_YEAR, 1, 4))) %>% 
  select(Year, FRA19NM = GEOGRAPHICAL_CATEGORY, TOTAL_RESPONSE_TIME) %>% 
  
  group_by(FRA19NM) %>% 
  summarise(`Fire and Rescue response time (three-year average)` = mean(TOTAL_RESPONSE_TIME, na.rm = TRUE)) %>% 
  
  # manual lookups
  mutate(FRA19NM = case_when(
    FRA19NM == "Berkshire" ~ "Royal Berkshire",
    FRA19NM == "Buckinghamshire" ~ "Buckinghamshire & Milton Keynes",
    FRA19NM == "Devon and Somerset" ~ "Devon & Somerset",
    FRA19NM == "Durham" ~ "County Durham and Darlington",
    FRA19NM == "Hereford and Worcester" ~ "Hereford & Worcester",
    FRA19NM == "Isles Of Scilly" ~ "Isles of Scilly",
    FRA19NM == "Greater London" ~ "London Fire and Emergency Planning Authority",
    FRA19NM == "Nottinghamshire" ~ "Nottinghamshire and City of Nottingham",
    FRA19NM == "Staffordshire" ~ "Stoke-on-Trent and Staffordshire",
    FRA19NM == "Isle Of Wight" ~ "Isle of Wight",
    FRA19NM == "Dorset and Wiltshire" ~ "Dorset & Wiltshire",
    TRUE ~ FRA19NM
  ))


lad_stats <- lad_fra %>% 
  filter(str_sub(LAD19CD, 1, 1) == "E") %>% 
  left_join(fra_stats_sum, by = "FRA19NM") %>% 
  select(-FID)

write_csv(lad_stats, "data/processed/fire and rescue response times.csv")
