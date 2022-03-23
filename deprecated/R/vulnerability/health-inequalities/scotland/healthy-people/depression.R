library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Population prescribed drugs for anxiety/depression/psychosis

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_dfb86157/_w_25ad8111/session/452a71856bf5874dfd9c3c42658c4acf/download/download_table_csv?w=25ad8111",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

depression_raw <-
  read_csv(tf)

depression <-
  depression_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    depression_percent = measure
  ) %>%
  mutate(depression_percent = depression_percent / 100)

write_rds(depression, "data/vulnerability/health-inequalities/scotland/healthy-people/depression.rds")