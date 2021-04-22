library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Teenage pregnancies

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_fc77483c/session/67a67a28dbe325e8ddab44ab6037e5c5/download/download_table_csv?w=fc77483c",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

pregnancy_raw <-
  read_csv(tf)

unlink(tf)
rm(tf)

pregnancy <-
  pregnancy_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    teenage_pregnancies_per_1000 = measure
  )

write_rds(pregnancy, "data/vulnerability/health-inequalities/scotland/healthy-lives/teenage-pregnancy.rds")