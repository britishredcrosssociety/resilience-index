library(tidyverse)
library(geographr)

source("R/utils.R") # for download_file() & calculate_extent()

# Based on code https://github.com/britishredcrosssociety/covid-19-vulnerability/blob/master/prep%20CACI%20vulnerability%20data.r

# Data can't be shared publicly
vuln <- read_csv("data/on-disk/CACI/CCI_British Red Cross - UK Postcodes with Vunerability Indicators.csv")

# Digital vulnerability is combination of:
# fixed internet speed + online purchasing + online finance + mobile phone + internet user + confused by computer

# Lower decile values correspond to more vulnerable population
digital_vuln <- vuln |>
  select(postcode = Postcode, digital_vuln_decile = "Digital_combined_-_Decile") |>
  mutate(postcode = str_remove(postcode, " "))

# Postcode to MSOA lookup https://www.arcgis.com/home/item.html?id=6a46e14a6c2441e3ab08c7b277335558
tf <- download_file("https://www.arcgis.com/sharing/rest/content/items/6a46e14a6c2441e3ab08c7b277335558/data", "zip")

tf |>
  unzip(exdir = tempdir())

postcode_lookup <- read_csv(paste0(tempdir(), "/PCD_OA_LSOA_MSOA_LAD_FEB20_UK_LU.csv"))

postcode_lookup_eng <- postcode_lookup |>
  filter(str_detect(ladcd, "^E")) |>
  select(postcode = pcd7, lsoa_code = lsoa11cd, msoa_code = msoa11cd, lad_code = ladcd) |>
  mutate(postcode = str_remove(postcode, " "))

digital_msoa <- digital_vuln |>
  inner_join(postcode_lookup_eng, by = "postcode") |>
  group_by(msoa_code) |>
  summarise(median_decile = median(internet_user_decile))


msoa_pop <- population_msoa |>
  select(msoa_code, total_population)

digital_extent <- digital_msoa |>
  left_join(lookup_msoa_lad, by = "msoa_code") |>
  left_join(msoa_pop, by = "msoa_code") |>
  calculate_extent(
    var = median_decile,
    higher_level_geography = lad_code,
    population = total_population,
    invert_percentiles = TRUE #  TRUE when a higher variable score equates to a worse outcome
  ) 

digital_extent  |>
  group_by(extent) |>
  summarise(prop = n()/nrow(internet_extent)) |>
  print(n = Inf)
# 0: 30%
# 1: 4%

# To discuss approach with Mike. 