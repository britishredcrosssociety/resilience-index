library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: School exclusion rate

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_bd032e08/session/5988c5462255845c9828e43c026dfc53/download/download_table_csv?w=bd032e08",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

absence_raw <-
  read_csv(tf)

unlink(tf)
rm(tf)

pupil_absence <-
  absence_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    pupil_absence_per_1000 = measure
  )

write_rds(pupil_absence, "data/vulnerability/health-inequalities/scotland/healthy-lives/pupil-absence.rds")