# load packages
library(tidyverse)
library(readxl)
library(geographr)
library(demographr)
source("R/utils.R")

# OSCI Community Needs Index data ----
# Prepare the Community Needs Index, showing the Index and its domains for all
# Wards, along with which are the left-behind areas
# More info: https://ocsi.uk/2019/10/21/community-needs-index-measuring-social-and-cultural-factors/
# The data isn't public but OCSI can provide it to social purpose organisations
# - email them for info

# Higher score and therefore lower rank is the LACK of assets so HIGH SCORE/LOW
# RANK = LOW CAPABILITIES
# Civic assets + Engagement + Connectness = Community Need score (high score =
# high need)

# Data is at 2017 ward level
raw <-
  read_excel(
    "data/on-disk/OCSI/community-needs-index/WCNI and domains - 2021.xlsx"
  )

civic_assets_vars <-
  raw |>
  select(
    msoa11_code = `MSOA code`,
    civic_assets_score = Civic_Assets_Domain
  )

civic_assets_joined <-
  population_msoa_20_codes_11 |>
  select(msoa11_code = msoa_11_code, pop = total_population) |>
  right_join(civic_assets_vars) |>
  left_join(lookup_msoa11_ltla21) |>
  select(-ends_with("_name"))

civic_assets_ltla <-
  civic_assets_joined |>
  calculate_extent(
    var = civic_assets_score,
    higher_level_geography = ltla21_code,
    population = pop,
    weight_high_scores = TRUE
  ) |>
  rename(
    lad_code = ltla21_code,
    community_assets = extent
  )

write_rds(
  civic_assets_ltla,
  "data/capacity/disasters-emergencies/wales/community-assets.rds"
)