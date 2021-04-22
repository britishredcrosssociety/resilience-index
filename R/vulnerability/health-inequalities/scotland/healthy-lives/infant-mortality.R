library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Infant deaths, ages 0-1 years

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_bf38394a/session/b36bf6618e0e15fa0175acb6560866c3/download/download_table_csv?w=bf38394a",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

infant_mortality_raw <-
  read_csv(tf)

unlink(tf)
rm(tf)

infant_mortality <-
  infant_mortality_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    infant_mortality_per_1000 = measure
  )

write_rds(infant_mortality, "data/vulnerability/health-inequalities/scotland/healthy-lives/infant-mortality.rds")