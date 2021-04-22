library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Alcohol-related hospital admissions

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_d0735ee5/session/e928962541065310a3e0e6c55915106a/download/download_table_csv?w=d0735ee5",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

alcohol_raw <-
  read_csv(tf)

unlink(tf)
rm(tf)

alcohol <-
  alcohol_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    alcohol_admissions_per_100000 = measure
  )

write_rds(alcohol, "data/vulnerability/health-inequalities/scotland/healthy-lives/alcohol-misuse.rds")