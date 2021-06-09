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

# Remove vars
domiciliary <-
  care_homes_raw %>%
  select(
    rating = `Location Latest Overall Rating`,
    provider = `Provider Name`,
    care_home = `Care home?`,
    num_beds = `Care homes beds`,
    domiciliary = `Service type - Domiciliary care service`,
    dormant = `Dormant (Y/N)`,
    postcode = `Location Postal Code`
  )

# Clean postcodes
domiciliary_postcodes <-
  domiciliary %>%
  mutate(postcode = str_to_upper(postcode)) %>%
  mutate(postcode = str_remove_all(postcode, " "))

# Lookup postcode to LAD
domiciliary_joined <-
  domiciliary_postcodes %>%
  left_join(
    lookup_postcode_lad,
    by = "postcode"
  )

# Keep only domiciliary homes that are not dormant
domiciliary_homes <-
  domiciliary_joined %>% 
  filter(dormant == "N") %>% 
  filter(domiciliary == "Y")

# Count no. services registered with CQC
# Keep only English LAD's
eng_lads <-
  population_lad %>% 
  select(lad_code) %>% 
  filter(str_detect(lad_code, "^E")) %>% 
  pull(lad_code)

domiciliary_count <-
  domiciliary_homes %>% 
  count(lad_code) %>% 
  filter(lad_code %in% eng_lads) %>% 
  rename(domiciliary_care_count = n)

# Save
domiciliary_count %>%
  write_rds("data/capacity/health-inequalities/england/domiciliary-count.rds")

# Does the number of domiciliary care orgs need normalising to account for the
# fact that larger areas are more likely to have more providers? Test this
# assumption?

population_lad %>% 
  select(lad_code, total_population) %>% 
  filter(str_detect(lad_code, "^E")) %>% 
  left_join(domiciliary_count) %>%
  mutate(domiciliary_care_count_normalised = domiciliary_care_count / total_population)  %>% 
  ggplot(aes(x = total_population, y = domiciliary_care_count_normalised)) + 
  geom_point() +
  geom_smooth() +
  theme_minimal()

population_lad %>% 
  select(lad_code, total_population) %>% 
  filter(str_detect(lad_code, "^E")) %>% 
  left_join(domiciliary_count) %>%
  ggplot(aes(x = total_population, y = domiciliary_care_count)) + 
  geom_point() +
  geom_smooth() +
  theme_minimal()

population_lad %>%
  select(lad_code, `65`:`90+`) %>%
  pivot_longer(cols = !lad_code, names_to = "age", values_to = "count") %>%
  group_by(lad_code) %>%
  summarise(pop_over_65 = sum(count))  %>% 
  filter(str_detect(lad_code, "^E")) %>% 
  left_join(domiciliary_count) %>%
  mutate(domiciliary_care_count_normalised = domiciliary_care_count / pop_over_65)  %>% 
  ggplot(aes(x = pop_over_65, y = domiciliary_care_count_normalised)) + 
  geom_point() +
  geom_smooth() +
  theme_minimal()

# TODO:
# 1. Assess if ratings differ by location/provider and if that can be used as
# proxy
# 2. Assess annual stability of DToC. Can 2020 data be used?
# 3. https://www.skillsforcare.org.uk/adult-social-care-workforce-data/Workforce-intelligence/publications/local-information/Local-authority-comparison.aspx