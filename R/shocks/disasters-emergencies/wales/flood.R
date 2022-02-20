library(tidyverse)
library(statswalesr)
library(stringr)

# Download data
# Source: https://statswales.gov.wales/Catalogue/Environment-and-Countryside/Flooding/environment-and-countryside-state-of-the-environment-our-local-environment-properties-at-risk-of-flooding
df <- statswales_get_dataset("ENVI0211")

# FloodingType_Code: River, Surface, Tidal
df_clean <-
    df %>%
    filter(
        str_detect(Area_Code, "^W06") &
        Risk_ItemName_ENG == "Total"
    ) %>%
    select(
        lad_code = Area_Code,
        variable = FloodingType_Code,
        value = Data
    ) %>%
    group_by(variable, lad_code) %>%  # This is preparation for pivot table
    mutate(
        row = row_number()  # This is preparation for pivot table
    ) %>%
    pivot_wider(
        names_from = variable,
        values_from = value
    ) %>%
    mutate(
        total = as.numeric(River) + as.numeric(Surface) + as.numeric(Tidal)
    ) %>%
    select(lad_code, total)

# more flood, more deprived
write_rds(
    df_clean,
    "data/shocks/disasters-emergencies/wales/flood-lad.rds"
)
