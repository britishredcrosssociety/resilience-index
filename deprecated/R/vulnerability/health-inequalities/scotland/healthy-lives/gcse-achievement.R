library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: School leavers with 1 or more qualification at SCQF level 4

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_e3757109/session/916a18a5bed26d0040ff4fb1019c9e9d/download/download_table_csv?w=e3757109",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

gcse_raw <-
  read_csv(tf)

gcse_achievement <-
  gcse_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    gcse_qualifications_percent = measure
  )

# Impute missing data
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/methodologies/methodsusedtodevelopthehealthindexforengland2015to2018
# Reason for missing: too few values to compute ('values less than 5 are suppressed')
# Strategy: replace with mean value for dataset. The mean value for the region (here: HB)
#           cannot be computed because the lad to hb exists in a 1:1 lookup.
gcse_achievement <-
  gcse_achievement %>%
  mutate(
    gcse_qualifications_percent = if_else(
      gcse_qualifications_percent == 0,
      mean(gcse_qualifications_percent),
      gcse_qualifications_percent
    )
  )

write_rds(gcse_achievement, "data/vulnerability/health-inequalities/scotland/healthy-lives/gcse-achievement.rds")