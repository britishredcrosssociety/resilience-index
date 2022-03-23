library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Developmental concerns at 27-30 months

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_6e7d3244/session/dd8c3ad2e0839649080e20e6a675705f/download/download_table_csv?w=6e7d3244",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

eyd_raw <-
  read_csv(tf)

unlink(tf)
rm(tf)

early_development <-
  eyd_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    early_development_percent = measure
  )

write_rds(early_development, "data/vulnerability/health-inequalities/scotland/healthy-lives/early-years-development.rds")