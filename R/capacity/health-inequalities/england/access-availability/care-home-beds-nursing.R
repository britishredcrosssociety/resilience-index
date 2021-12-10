# Care home data is at postcode level and was felt couldn't assume people would attend a care home within their local authority (LA) 
# just because they live in that LA. If can find data on mapping care home attendees proportions to LA will revisit (with dataset coded below).


# library(dplyr)
# library(tidyr)
# library(httr)
# library(readODS)
# library(geographr)
# library(sf)
# library(stringr)
# 
# source("R/utils.R")
# 
# # Source: https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory
# # Care directory with filters
# GET(
#   "https://www.cqc.org.uk/sites/default/files/1%20June%202021%20HSCA%20Active%20Locations.ods",
#   write_disk(tf <- tempfile(fileext = ".ods"))
# )
# 
# care_homes_raw <- read_ods(tf, sheet = 2)
# 
# # Remove vars
# care_homes <-
#   care_homes_raw %>%
#   select(
#     care_home = `Care home?`,
#     dormant = `Dormant (Y/N)`,
#     num_beds = `Care homes beds`,
#     postcode = `Location Postal Code`,
#     nursing = `Service type - Care home service with nursing`
#   )
# 
# # Clean postcodes
# care_homes_postcodes <-
#   care_homes %>%
#   mutate(postcode = str_to_upper(postcode)) %>%
#   mutate(postcode = str_remove_all(postcode, " "))
# 
# # Lookup postcode to LAD
# care_homes_joined <-
#   care_homes_postcodes %>%
#   left_join(
#     lookup_postcode_lad,
#     by = "postcode"
#   )
# 
# # Filter to homes with nursing
# care_homes_nursing <-
#   care_homes_joined %>%
#   filter(nursing == "Y")
# 
# # Count beds per LAD
# care_home_nursing_beds <-
#   care_homes_nursing %>%
#   filter(care_home == "Y") %>%
#   filter(dormant == "N") %>%
#   group_by(lad_code) %>%
#   mutate(num_beds = as.double(num_beds)) %>%
#   summarise(num_beds = sum(num_beds, na.rm = TRUE))
# 
# # Drop care home beds which couldn't be matched (less than 0.09%)
# care_home_nursing_beds <-
#   care_home_nursing_beds %>%
#   drop_na()
# 
# # Normalise per 1,000 population above 65
# pop_over_65 <-
#   population_lad %>%
#   select(lad_code, `65`:`90+`) %>%
#   pivot_longer(cols = !lad_code, names_to = "age", values_to = "count") %>%
#   group_by(lad_code) %>%
#   summarise(pop_over_65 = sum(count)) %>%
#   mutate(pop_over_65_per_1000 = pop_over_65 / 1000)
# 
# care_home_nursing_beds_normalised <-
#   care_home_nursing_beds %>%
#   left_join(pop_over_65, by = "lad_code") %>%
#   mutate(nursing_beds_per_1000_over_65 = num_beds / pop_over_65_per_1000) %>%
#   select(lad_code, nursing_beds_per_1000_over_65)
# 
# # Save
# care_home_nursing_beds_normalised %>%
#   write_rds("data/capacity/health-inequalities/england/care-home-beds-nursing.rds")
# 
# # The LADs "E09000001" (City of London) and "E06000053" (Isles of Scilly) do not
# # appear to have any nursing beds. How should this "missing" data be handled?