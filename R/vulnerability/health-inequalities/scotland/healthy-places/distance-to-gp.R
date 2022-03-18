library(httr)
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(geographr)

source("R/utils.R")

# Lookup
lookup <-
  lookup_hb_lad %>%
  select(lad_name, lad_code)

# Source: https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020v2-indicator-data/
GET(
  "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-indicator-data/documents/simd_2020_indicators/simd_2020_indicators/govscot%3Adocument/SIMD%2B2020v2%2B-%2Bindicators.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

gp_raw <-
  read_excel(tf, sheet = "Data")

# Data is at the Data Zone level (DZ)
gp_dz <-
  gp_raw %>%
  select(
    lad_name = Council_area,
    gp_distance = drive_GP,
    pop_count = Total_population
  ) %>%
  mutate(
    lad_name = if_else(
      lad_name == "Na h-Eileanan an Iar",
      "Na h-Eileanan Siar",
      lad_name
    )
  ) %>%
  left_join(lookup, by = "lad_name") %>%
  relocate(lad_code) %>%
  select(-lad_name)

gp <-
  gp_dz %>%
  calculate_extent_depreciated(
    var = gp_distance,
    higher_level_geography = lad_code,
    population = pop_count
  ) %>%
  rename(gp_distance_extent = extent)

write_rds(gp, "data/vulnerability/health-inequalities/scotland/healthy-places/distance-to-gp.rds")