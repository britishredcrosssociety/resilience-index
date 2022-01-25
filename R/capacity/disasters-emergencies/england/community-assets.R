library(tidyverse)
library(readxl)

source("R/utils.R") # for download_file() & calculate_extent()

# OSCI Community Needs Index data
osci <- read_excel("data/on-disk/OSCI/community-needs-index/Community Needs Index domain scores.xlsx")

civic_assests <- osci |>
  select(ward_code = "Ward Code", lad_name = "LA Name", lad_code = "LA code",  civic_assests_score = "Civic Assets score")

# Take median score across the wards within a local authority
civic_assests_lad <- civic_assests |>
  group_by(lad_code) |>
  summarise(median_civic_assests_score = median(civic_assests_score))

# Save data 
civic_assests_lad |> 
  write_rds("data/capacity/disasters-emergencies/england/community-assets.rds")


# Alternative approach: to use calculate_extent with ward population ----

# Ward population counts
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimatesexperimental
tf <- download_file("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fwardlevelmidyearpopulationestimatesexperimental%2fmid2020sape23dt8a/sape23dt8amid2020ward2020on2021lasyoaestimatesunformattedcorrection.xlsx",
  ".xlsx")

raw <- read_excel(tf, 
                       sheet = "Mid-2020 Persons",
                       skip = 3)

ward_pop <- raw |>
  select(ward_code = "Ward Code 1", population = "All Ages")


raw <- read_excel("~/Downloads/SAPE22DT8a-mid-2019-ward-2019-on-2019 and 2020-LA-syoa-estimates-unformatted.xlsx", 
                  sheet = "Mid-2019 Persons",
                  skip = 3)

ward_pop_2019 <- raw |>
  select(ward_code = "Ward Code 1", population = "All Ages")

# Check wards with no pop info
civic_assests |>
  left_join(ward_pop_2019, by = "ward_code") |>
  filter(is.na(population)) 
# ~2k

# ~2k also seem to be missing from geographr::lookup_lsoa_ward
civic_assests |>
  left_join(geographr::lookup_lsoa_ward, by = "ward_code") |>
  filter(is.na(ward_name)) 

# geographr data from https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-2011-to-ward-2019-lookup-in-england-and-wales
civic_assests |>
  left_join(geographr::lookup_lsoa_ward, by = "ward_code") |>
  filter(is.na(ward_name)) |>
  distinct(lad_code, lad_name)

geographr::lookup_lsoa_ward |>
  left_join(ward_pop_2019, by = "ward_code") |>
  filter(is.na(ward_name)) 

response <- httr::GET("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD20_LAD20_CTY20_OTH_UK_LU_v2/FeatureServer/0/query?where=1%3D1&outFields=WD20CD,WD20NM,LAD20CD,LAD20NM&outSR=4326&f=json")
content <- content(response, type = "application/json", simplifyVector = TRUE)

jsonlite::fromJSON(content(response, type = "application/json"), flatten = TRUE)
content <- httr::content(response, type = "application/json")

ward_ltla_lookup <- content$features$attributes |>
    select(ward_code = WD20CD, ward_name = WD20NM, lad_code = LAD20CD, lad_name = LAD20NM)

test <- read_csv("/data/Ward_to_Local_Authority_District_to_County_to_Region_to_Country_(December_2019)_Lookup_in_United_Kingdom.csv")

test <- test  |>
  select(ward_code = WD19CD, ward_name = WD19NM, lad_code = LAD19CD, lad_name = LAD19NM) |>
  filter(str_detect(lad_code, "^E"))

  test |>
    left_join(geographr::lookup_lsoa_ward, by = "ward_code") |>
  filter(is.na(lsoa_name)) |>
    arrange(lad_name) |> print(n = Inf)

  civic_assests |>
    left_join(test, by = "ward_code") |>
    filter(is.na(ward_name)) 
  
  test|>
    left_join(civic_assests , by = "ward_code") |>
    filter(is.na(lad_name.y)) |>
    arrange(lad_name.x)
  