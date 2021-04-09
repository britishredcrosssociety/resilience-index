##
## Calculate average ambulance response times
## - these are for Ambulance Trusts - need to find a way to link them to Local Authorities
## - Trusts seems to have defined boundaries (https://aace.org.uk/uk-ambulance-service/map-of-nhs-ambulance-services/) but I can't find a shapefile for this...
##
## Source: https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/
##
library(tidyverse)
library(lubridate)

# Load LA to health region lookup
# Source: https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-public-health-england-centre-to-public-health-england-region-december-2019-lookup-in-england/data
lad_region <- read_csv("https://opendata.arcgis.com/datasets/4da177ab2ab34edaba9d2696c3a6da64_0.csv")

# Load AmbSYS August 2017 to October 2020
ambo_stats <- read_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/AmbSYS-to-20201031.csv")

ambo_stats_sum <- ambo_stats %>% 
  filter(`Org Code` != "Eng") %>% 
  select(Year, Month, `Org Code`, `Org Name`, `Mean response time (seconds)` = A25) %>% 
  
  # data wrangling
  mutate(`Mean response time (seconds)` = as.integer(`Mean response time (seconds)`),
         Date = dmy(paste("1", Month, Year, sep = "/"))) %>% 
  
  # ggplot(aes(x = Date, y = `Mean response time (seconds)`)) +
  # geom_line(aes(colour = `Org Code`), show.legend = FALSE)

  # keep only NHS Regions' data from the last three years
  filter(Year >= 2018 & str_sub(`Org Code`, 1, 1) == "Y") %>% 

  # calculate average over past three years
  group_by(`Org Code`, `Org Name`) %>% 
  summarise(`Mean response time (seconds)` = mean(`Mean response time (seconds)`, na.rm = TRUE))
  
