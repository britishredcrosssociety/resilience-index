# ---- Load libs ----
library(tidyverse)
library(statswalesr)
library(demographr)

# ---- Load data ----
# Download data
# Properties at risk of flooding
# Source: https://statswales.gov.wales/Catalogue/Environment-and-Countryside/Flooding/environment-and-countryside-state-of-the-environment-our-local-environment-properties-at-risk-of-flooding
raw <-
    statswales_get_dataset("ENVI0211") |>
    as_tibble()

# ---- Clean ----
# Calculate the number of properties at risk of any type of flooding, by any
# level of risk

# ---- Variable Guide: --------------------------------------------------------
# FloodingType_Code: River, Surface, Tidal
# Risk_ItemName_ENG: High, High & Medium, Low, Medium, Total
# -----------------------------------------------------------------------------
num_properties <-
    raw |>
    filter(
        str_detect(Area_Code, "^W06") &
            Risk_ItemName_ENG == "Total"
    ) |>
    select(
        lad_code = Area_Code,
        flood_type = FloodingType_Code,
        num_properties = Data
    ) |>
    mutate(num_properties = as.integer(num_properties)) |>
    group_by(lad_code) |>
    summarise(total_num_properties = sum(num_properties))

flood_risk <-
    num_properties |>
    left_join(population_lad_20_codes_21, by = c("lad_code" = "lad_21_code")) |>
    mutate(
        properties_flood_risk_per_1000 = total_num_properties / total_population * 1000
    ) |>
    select(lad_code, properties_flood_risk_per_1000)



# ---- Save ----
write_rds(
    flood_risk,
    "data/shocks/disasters-emergencies/wales/flood.rds"
)