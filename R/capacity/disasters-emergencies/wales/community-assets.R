# load packages
library(tidyverse)
library(readxl)
library(geographr)

community_assets_raw <- read_excel("data/on-disk/community-needs-index/community-needs-index/WCNI and domains - 2021.xlsx")

# The bigger number of 'Civic_Assets_Domain', the more deprive is
community_assets_lad <-
  community_assets_raw %>%
  select(
    msoa_code = "MSOA code",
    Civic_Assets_Domain = "Civic_Assets_Domain"
  ) %>%
  left_join(lookup_msoa_lad, by = "msoa_code") %>%
  left_join(population_msoa, by = "msoa_code") %>%

  calculate_extent(
    var = Civic_Assets_Domain,
    higher_level_geography = lad_code,
    population = total_population,
    weight_high_scores = TRUE
  )


write_rds(
  community_assets_lad,
  "data/capacity/disasters-emergencies/wales/community_assets_lad.rds"
)
