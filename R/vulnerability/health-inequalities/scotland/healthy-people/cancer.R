library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Cancer registrations

# Interactively generate the data by:
#   1. Navigating the the source (above)
#   2. Clicking the 'Data' box
#   3. Selecting the relevant indicator (above)
#   4. Ticking 'All available geographies'
#   5. Moving the time period slider until the latest data shows
#   6. Right-clicking on 'Download data' and clicking 'Copy Link Location'
#   7. Pasting the link into the GET request below
#   8. Running the code below

GET(
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_b300d15f/session/7cbf179006384fb00ead7245ba86ec5d/download/download_table_csv?w=b300d15f",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

cancer_raw <-
  read_csv(tf)

unlink(tf)
rm(tf)

cancer <-
  cancer_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    cancer_registrations_per_100000 = measure
  )

write_rds(cancer, "data/vulnerability/health-inequalities/scotland/healthy-people/cancer.rds")