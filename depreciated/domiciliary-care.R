# Two data sources to try:
# 1. DToC - can time series data at LAD level determine if there is enough
#    annual stability to warrant using 2019 data.
# 2. Skills for Care - this data is at UTLA so isn't appropriate. Can they be
#    contacted and a custom data set built?

# DToC
# Pull month/year files and build time series. Assess stability over time.
# https://www.england.nhs.uk/statistics/statistical-work-areas/delayed-transfers-of-care/

# # Skills for Care
# library(tidyverse)
# library(httr)
# library(readxl)
# 
# GET(
#   "https://www.skillsforcare.org.uk/adult-social-care-workforce-data/Workforce-intelligence/documents/Raw-data/LA-data-download-2019-20.xlsx",
#   write_disk(tf <- tempfile(fileext = ".xlsx"))
# )
# 
# domiciliary_raw <- 
#   read_excel(tf, sheet = "LA level data")



