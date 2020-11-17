##
## Number of VCS organisations per 1,000 people in LAs
##
## Source: NCVO Civil Society Almanac 2020
## - https://almanac.fc.production.ncvocloud.net/about/almanac-data-tables/
##
library(tidyverse)
library(readxl)
library(httr)

# ---- Load data ----
GET("https://almanac.fc.production.ncvocloud.net/documents/51/uk_civil_society_almanac_2020_data_tables.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))
vcs_stats <- read_excel(tf, sheet = "A5", skip = 5)  # Number of organisations and income by Local Authority 
unlink(tf); rm(tf)

# LA populations
pop <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/population%20estimates%20msoa11%20lad17%20lad19%20tacticall%20cell.csv")

# ---- Calculate density of charities ----
pop_la <- pop %>% 
  select(LAD19CD, pop_lad19) %>% 
  distinct()

lad17_19 <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/LAD%202017%20to%20LAD%202019%20codes.csv")

vcs_stats_sum <- vcs_stats %>% 
  # The data is for LA 2017 codes - summarise into LA 2019 codes first
  left_join(lad17_19, by = c("LA code" = "LAD17CD")) %>% 
  
  group_by(LAD19CD) %>% 
  summarise(`Number of charities` = sum(`General charities`)) %>% 
  ungroup() %>% 
  
  # Get population estimates for LA 2019 areas
  left_join(pop_la, by = "LAD19CD") %>% 
  
  mutate(`Charity density (per 1,000 people)` = (`Number of charities` / pop_lad19) * 1000) %>% 
  select(-pop_lad19)

write_csv(vcs_stats_sum, "data/processed/charity density.csv")
