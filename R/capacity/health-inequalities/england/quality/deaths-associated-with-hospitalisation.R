# Load libs
library(tidyverse)
library(geographr)
library(readxl)
library(sf)

source("R/utils.R") # for download_file()

# NHS trust deaths associated with hospitalisation -----
# IMPORTANT NOTE: This data does not include COVID 'activity' i.e. stays and deaths

# Download the data
tf <- download_file("https://files.digital.nhs.uk/A6/0F708F/SHMI%20data%20files%2C%20Jul20-Jun21.zip", "zip")

unzip(tf, exdir = tempdir())

deaths_raw <-
  read_excel(
    list.files(
      tempdir(),
      pattern = "*SHMI data at trust level, Jul20-Jun21 \\(xls\\)",
      recursive = T,
      full.names = TRUE
    ),
    sheet = "Data",
    skip = 10
  )

deaths_columns <- deaths_raw %>%
  select(`Provider code`, `Provider name`, `SHMI value`, `SHMI banding`, `Number of spells`, `Observed deaths`, `Expected deaths`)
# 122 trusts

# Source of below quote on coverage of data: https://files.digital.nhs.uk/2C/498A3E/SHMI%20background%20quality%20report%2C%20Jul20-Jun21.pdf
# The SHMI methodology has been designed for non-specialist acute trusts. Specialist trusts,
# mental health trusts, community trusts and independent sector providers are excluded from
# the SHMI because there are important differences in the case-mix of patients treated there
# compared to non-specialist acute trusts and the SHMI has not been designed for these types
# of trusts. 
  
# NHS TRUST table in geographr package -----

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

# Check the matching of deaths data & trust table in geographr package --------
open_trusts |>
  anti_join(deaths_columns, by = c("trust_code" = "Provider code"))
# 93 trusts no deaths data

# TO DO - make sure 122 in open trusts

# Downloading CQC rating data as has information on what is the primary type of care trust provides
# Used to check against the trusts with no death data
tf <- download_file("https://www.cqc.org.uk/sites/default/files/01_November_2021_Latest_ratings.ods", "ods")

raw_providers <-
  read_ods(
    tf,
    sheet = "Providers",
  )

trust_categories <- raw_providers |>
  select(`Provider ID`, `Provider Name`, `Provider Type`, `Provider Primary Inspection Category`) |>
  distinct()
  
combined_table <- open_trusts |>
  left_join(trust_categories, by = c("trust_code" = "Provider ID")) |>
  left_join(deaths_columns, by = c("trust_code" = "Provider code"))

# Check missing death data for each care category 
combined_table |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), count_missing_death_data = sum(is.na(`SHMI value`)))
  
  
  
  