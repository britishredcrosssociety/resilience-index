library(tidyverse)
library(statswalesr)
library(stringr)

# Download data
# Source: https://statswales.gov.wales/Catalogue/Community-Safety-and-Social-Inclusion/Community-Safety/Fire-Incidents/Fires-and-False-Alarms/fires-by-localauthority-financialyear-motive
df <- statswales_get_dataset("CSAF0066")

df_clean <-
    df %>%
    filter(
        str_detect(LocalAuthority_AltCode1, "^W06") &
        Year_ItemName_ENG == "2020-21" &
        Typeoffires_ItemName_ENG == "All Fires" &
        Grasslandmarker_ItemName_ENG == "All fires" &
        Motive_ItemName_ENG == "All Motives"
    ) %>%
    select(
        lad_code = LocalAuthority_AltCode1,
        value = Data

    )

write_rds(
    df_clean,
    "data/shocks/disasters-emergencies/wales/fire-lad.rds"
)
