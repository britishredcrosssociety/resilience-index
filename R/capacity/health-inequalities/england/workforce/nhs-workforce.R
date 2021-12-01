# TODO:
# 1. Inspect different data sets and choose most appropriate
# 2. Make sure locum/agency spend is separated and not captured in the data to
#    differentiate the two types of care.

# Load packages
library(tidyverse)
library(geographr)
library(sf)

source("R/utils.R") # for download_file()

# Source https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics/may-2021

# Both FTE and total headcount is available - to decide which most appropriate. 

tf <- download_file("https://files.digital.nhs.uk/F5/8ADE23/NHS%20Workforce%20Statistics%2C%20May%202021%20England%20and%20Organisation.xlsx", "xlsx")

raw_headcount <-
  read_excel(
    tf,
    sheet = "2. NHSE, Org & SG - HC",
    skip = 6
  )

raw_fte <-
  read_excel(
    tf,
    sheet = "3. NHSE, Org & SG - FTE",
    skip = 6
  )
 
