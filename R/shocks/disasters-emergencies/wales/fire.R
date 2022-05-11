# ---- Load libs ----
library(tidyverse)
library(statswalesr)
library(demographr)

# ---- Source data ----
# Download data
# Source: https://statswales.gov.wales/Catalogue/Community-Safety-and-Social-Inclusion/Community-Safety/Fire-Incidents/Fires-and-False-Alarms/fires-by-localauthority-financialyear-motive
raw <-
    statswales_get_dataset("CSAF0066") |>
    as_tibble()

# ---- Clean ----
# Filter to latest fires from all motives
num_fires <-
    raw |>
    filter(
        Year_ItemName_ENG == "2020-21",
        str_detect(LocalAuthority_AltCode1, "^W06"),
        Typeoffires_ItemName_ENG != "Chimney Fires",
        Typeoffires_ItemName_ENG != "All Fires",
        Motive_ItemName_ENG == "All Motives",
        Grasslandmarker_ItemName_ENG == "Not grassland, woodland or crop fires"
    ) |>
    select(
        lad_code = LocalAuthority_AltCode1,
        value = Data,
        Typeoffires_ItemName_ENG,
        Motive_ItemName_ENG,
        Grasslandmarker_ItemName_ENG
    ) |>
    group_by(lad_code) |>
    summarise(num_fires = sum(value))

num_fires_normalised <-
    num_fires |>
    left_join(population_lad_20_codes_21, by = c("lad_code" = "lad_21_code")) |>
    mutate(num_fires_per_1000 = num_fires / total_population * 1000) |>
    select(lad_code, num_fires_per_1000)

# ---- Save ----
write_rds(
    num_fires_normalised,
    "data/shocks/disasters-emergencies/wales/fire.rds"
)