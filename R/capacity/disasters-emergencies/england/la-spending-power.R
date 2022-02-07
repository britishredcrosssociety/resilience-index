# Load packages ----
library(tidyverse)
library(readxl)
library(geographr)
library(sf)

source("R/utils.R") # for download_file()

# Load data and prep ----
# Source: https://www.gov.uk/government/publications/core-spending-power-provisional-local-government-finance-settlement-2021-to-2022
# (Alternative: https://pldr.org/dataset/20mjj/local-authority-finance-core-spending-power-fin0759)

tf <- download_file(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/945388/Core_Spending_Power_summary_table_2021-22.xlsx",
  "xlsx"
)

raw <- read_excel(tf, sheet = "Per Dwelling")

# CPS = Core Spending Power
cps <- raw |>
  select(lad_code = `...3`, lad_name = `Core Spending Power - Local Authority Summary`, cps_millions = `...8`) |>
  filter(str_detect(lad_code, "^E")) |>
  mutate(cps_millions = ifelse(cps_millions == "NA", NA, cps_millions)) |>
  mutate(cps_millions = as.numeric(cps_millions)) |>
  drop_na(cps_millions)

# Confirm no loss of decimal information
print(cps$cps_millions, digits = 17)

# Join on LTLA population data ----
# This data uses 2021 LTLA boundaries so will use LTLA 2021 population figures (geographr currently uses 2019 LTLAs)

ltla_pop <- population_lad |>
  select(lad_code, total_population)

utla_pop <- population_counties_ua |>
  select(county_ua_code, total_population)

# Check any differences in LAD codes between 2 datasets
ltla_pop |>
  filter(str_detect(lad_code, "^E")) |>
  anti_join(cps)
# no missings

cps |>
  anti_join(ltla_pop) |>
  print(n = Inf)
# 58 not matches 

# Investigate not matches in LAD population data (2021 Northampshire LAD restructure) ----
northam_restructure <-
  tribble(
    ~pre_lad_name, ~post_ua_name, ~pre_lad_code, ~post_ua_code,
    "Corby", "North Northamptonshire", "E07000150", "E06000061",
    "Daventry", "West Northamptonshire", "E07000151", "E06000062",
    "East Northamptonshire", "North Northamptonshire", "E07000152", "E06000061",
    "Kettering", "North Northamptonshire", "E07000153", "E06000061",
    "Northampton", "West Northamptonshire", "E07000154", "E06000062",
    "South Northamptonshire", "West Northamptonshire", "E07000155", "E06000062",
    "Wellingborough", "North Northamptonshire", "E07000156", "E06000061",
  )

ltla_pop_2021 <- ltla_pop |> 
  left_join(northam_restructure, by = c("lad_code" = "pre_lad_code")) |>
  mutate(lad_code = ifelse(!is.na(post_ua_code), post_ua_code, lad_code)) |>
  group_by(lad_code) |>
  summarise(total_population = total_population)

# Investigate not matches in LTLA population data (Fire & UTLA) ----

# Many are actually Fire & Rescue Authorities
fire_res_auth <- cps |>
  anti_join(ltla_pop, by = "lad_code") |>
  filter(str_detect(lad_name, "Fire")) |>
  rename(far_auth_code = lad_code, far_auth_name = lad_name)
# 30 FRS 

# Check not matches after 2021 restructure & fire authorities accounted
not_matched <- cps |>
  anti_join(ltla_pop_2021) |>
  anti_join(fire_res_auth, by = c("lad_code" = "far_auth_code"))

# Check for matches at county (i.e. UTLA) level -----
cps_utla <- not_matched |>
  inner_join(utla_pop, by = c("lad_code" = "county_ua_code")) |>
  rename(county_ua_code = lad_code, county_ua_name = lad_name)
# 25 UTLAs


# Are these in addition to LTLAs within these counties?
cps_utla |>
  left_join(lookup_counties_ua_lad,  by = "county_ua_code") |>
  select(lad_code) |>
  left_join(cps, by = "lad_code") 
# All the ltlas that make up the utlas are already in the cps data
# This utla level data is in addition to the ltla data

# Checking which funds available to LTLAs vs. UTLAs vs Combined Auth

tf <- download_file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/945389/Core_Spending_Power_supporting_table_2021-22.xlsx",
                    "xlsx")

raw_detail <- read_excel(tf, sheet = "2021-22", skip = 4)

cps_detail <- raw_detail |>
  select(-c("...1", "...2", "...4", "...5")) |>
  rename(geog_code = "...3", geog_name = `Local Authority`) |>
  filter(!is.na(geog_code))

utla_detail <- cps_detail |>
  inner_join(cps_utla, by = c("geog_code" = "county_ua_code"))

# View UTLA (E10000003) and LTLA within that UTLA (E07000008 - E07000012)
cambridge_check <- cps_detail |>
  filter(geog_code %in% c("E10000003", "E07000008", "E07000009", "E07000010", "E07000011", "E07000012")) |>
  arrange(geog_code)


##############################################################################################################################



# Remaining unmatched is a Combined Authority (don't currently have this lookup in geographr package)
cps |>
  anti_join(ltla_pop, by = "lad_code") |>
  anti_join(fire_res_auth, by = c("lad_code" = "far_auth_code")) |>
  anti_join(cps_utla_updated, by = c("lad_code" = "county_ua_code"))

# Load in Combined Authority lookup data ----
# Combined Authority lookup source: https://geoportal.statistics.gov.uk/datasets/ons::local-authority-district-to-combined-authority-december-2020-lookup-in-england/about
response <- httr::GET("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD20_CAUTH20_EN_LU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
content <- httr::content(response, type = "application/json", simplifyVector = TRUE)

combined_auth_lookup <- content$features$attributes |>
  select(-column4, -FID)

cps_combined_auth_lookup <- cps |>
  inner_join(combined_auth_lookup, by = c("lad_code" = "CAUTH20CD")) |>
  select(cauth_code = lad_code, cauth_name = lad_name, lad_code = LAD20CD, lad_name = LAD20NM, cps_millions) |>
  left_join(ltla_pop, by = "lad_code")

# Disaggregate the UTLA (i.e. counties) and Combined Authority CPS ----

# Assumption: split based on population distribution

cps_combined_auth_split <- cps_combined_auth_lookup |>
  mutate(pop_prop = total_population / sum(total_population)) |>
  mutate(weighted_cps = cps_millions * pop_prop) |>
  select(lad_name, lad_code, cps_millions = weighted_cps)

# Get 2021 LTLA (LAD) to UTLA (county) lookup
response <- httr::GET("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LTLA21_UTLA21_EW_LU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
content <- httr::content(response, type = "application/json", simplifyVector = TRUE)

counties_lads_lookup <- content$features$attributes |>
  select(-FID) |>
  select(lad_code = LTLA21CD, lad_name = LTLA21NM, county_ua_code = UTLA21CD)


# Join LAD to County lookup and the LAD populations
# For the 2 new Northamptonshire counties these have a 1-to-1 mapping with the LTLa of the same area so same population applied
cps_utla_split <- cps_utla_updated |>
  rename(county_pop = total_population) |>
  left_join(counties_lads_lookup, by = c("county_ua_code")) |>
  left_join(ltla_pop, by = "lad_code") |>
  rename(lad_pop = total_population) |>
  mutate(lad_pop = ifelse(county_ua_code %in% c("E06000061", "E06000062"), county_pop, lad_pop)) |>
  mutate(pop_prop = lad_pop / county_pop) |>
  mutate(weighted_cps = cps_millions * pop_prop) |>
  select(lad_name, lad_code, cps_millions = weighted_cps)

# Check if any LTLAs appear in more than 1 of the datasets 
cps_combined_auth_split |>
  inner_join(cps_updated, by = "lad_code") 
# All those in the Combined Auth dataset already in the LTLA dataset i.e. it is additional funding
# 10 LTLAs from 1 Combined Auth

cps_counties_split |>
  inner_join(cps_updated, by = "lad_code") 
# All those in the UTLA dataset already in the LTLA dataset i.e. it is additional funding
# 171 LTLAs from 25 UTLAs

cps_combined_auth_split |>
  inner_join(cps_counties_split, by = "lad_code") 
# No LTLAs both in Combined Auth and UTLA datasets

cps_updated |>
  select(-total_population) |>
  bind_rows(cps_combined_auth_split) |>
  bind_rows(cps_counties_split) |>
  group_by(lad_code) |>
  mutate(count_lad_code = n()) |>
  filter(count_lad_code > 1)

# FRA to LAD lookup data ----
response <- httr::GET("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD21_FRA21_EW_LU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
content <- httr::content(response, type = "application/json", simplifyVector = TRUE)

fra_lad_lookup <- content$features$attributes |>
  select(lad_code = LAD21CD, lad_name = LAD21NM, fra_code = FRA21CD, fra_name = FRA21NM) |>
  filter(!str_detect(fra_name, "Wales"))

fra_lad_lookup |>
  distinct(fra_code, fra_name) |>
  anti_join(fire_res_auth, by = c("fra_code" = "far_auth_code"))
# Not all Fire Authorities are in the Spending data 

fire_res_auth_ltla <- fire_res_auth |>
 left_join(fra_lad_lookup, by = c("far_auth_code" = "fra_code")) |>
  left_join(ltla_pop, by = c("lad_code")) |>
  group_by(far_auth_code) |>
  mutate(pop_weight = total_population / sum(total_population)) |>
  ungroup() |>
  mutate(weighted_cps = cps_millions * pop_weight) |>
  select(lad_name, lad_code, cps_millions = weighted_cps)
  
  # Combine the LAD level and disaggregated county and combined authorities and fire & rescue
cps_combined <- cps_updated |>
  select(-total_population) |>
  bind_rows(cps_combined_auth_split) |>
  bind_rows(cps_counties_split) |>
  bind_rows(fire_res_auth_ltla) |>
  group_by(lad_code, lad_name) |>
  summarise(cps_millions = sum(cps_millions))

# To discuss:
# Many of the LTLAs within the ULTAs and Combined Auth level already appear at LTLA in the data
# However since some LTLAs & UTLAs have a 1-2-1 mapping how to distinguish which money was given at LTLA level and which at UTLA level (as may be combined in at LTLAs with 1-2-1 mapping)
# Some analysis below to see if can distinuigh which funds given at what level
# Not all the Fire & Rescue Authorities across England appear in the data, only a subset

# Should UTLA/Combined Auth be included? This is a adjusted version of the data where these are removed maybe?
# https://pldr.org/dataset/20mjj/local-authority-finance-core-spending-power-fin0759





