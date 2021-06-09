library(dplyr)
library(tidyr)
library(httr)
library(readODS)
library(geographr)
library(sf)
library(stringr)

source("R/utils.R")

# Source: https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory
# Care directory with filters
GET(
  "https://www.cqc.org.uk/sites/default/files/1%20June%202021%20HSCA%20Active%20Locations.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

care_homes_raw <- read_ods(tf, sheet = 2)
care_homes_raw <- readxl::read_excel("/Users/mike/Downloads/cqc.xlsx", sheet = 2)

# Remove vars
care_homes <-
  care_homes_raw %>%
  select(
    care_home = `Care home?`,
    dormant = `Dormant (Y/N)`,
    num_beds = `Care homes beds`,
    postcode = `Location Postal Code`,
    nursing = `Service type - Care home service with nursing`
  )

# Clean postcodes
care_homes_postcodes <-
  care_homes %>%
  mutate(postcode = str_to_upper(postcode)) %>%
  mutate(postcode = str_remove_all(postcode, " "))

# Lookup postcode to LAD
care_homes_joined <-
  care_homes_postcodes %>%
  left_join(
    lookup_postcode_lad,
    by = "postcode"
  )

# Count beds per LAD
care_home_beds <-
  care_homes_joined %>%
  filter(care_home == "Y") %>%
  filter(dormant == "N") %>%
  group_by(lad_code) %>%
  mutate(num_beds = as.double(num_beds)) %>%
  summarise(num_beds = sum(num_beds, na.rm = TRUE))

# Normalise per 1,000 population above 65
population_lad %>% 
  select(lad_code, `65`:`90+`) %>% 
  pivot_longer(cols = !lad_code, names_to = "age", values_to = "count") %>% 
  group_by(lad_code) %>% 
  summarise(pop_over_65 = )

# Save
care_home_beds %>%
  write_csv("")


care_home_beds %>% filter(str_detect(lad_code, "E06000062"))
