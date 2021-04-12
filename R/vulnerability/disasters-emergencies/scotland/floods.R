library(readr)
library(dplyr)
library(httr)
library(readxl)

floods_raw <- read_csv("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdwellings-in-flood-risk-areas")

floods <- 
  floods_raw %>% 
  group_by(FeatureCode) %>% 
  summarise(percentage_dwellings = sum(Value)) %>% 
  rename(data_zone = FeatureCode)

# The data_zone codes are now archived.
# https://statistics.gov.scot/atlas/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fid%2Fstatistical-geography%2FS01000001&inactive=false

# Should they be matched to the updated codes. Or, is the 2006 data redundant now?
