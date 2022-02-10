# load packages
library(tidyverse)
library(readxl)

community_assets_raw <- read_excel("data/on-disk/community-needs-index/community-needs-index/WCNI and domains - 2021.xlsx")

community_assets_lad <-
    community_assets_raw %>%
    select(
        msoa_code = "MSOA code",
        Civic_Assets_Domain = "Civic_Assets_Domain"
    ) %>%
    left_join(lookup_msoa_lad, by = "msoa_code") %>%
    group_by(lad_code) %>%
    summarise(mean(Civic_Assets_Domain)) # aggregation method need to consider

write_rds(
  community_assets_lad,
  "data/capacity/disasters-emergencies/wales/community_assets_lad.rds"
)