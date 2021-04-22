library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Deaths from suicide

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_2988fba7/session/d739c71562733de533b30f3512c6133e/download/download_table_csv?w=2988fba7",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

suicides_raw <-
  read_csv(tf)

suicides <-
  suicides_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    suicides_per_100000 = measure
  )

write_rds(suicides, "data/vulnerability/health-inequalities/scotland/healthy-people/suicides.rds")