library(readr)
library(httr)
library(dplyr)
library(stringr)

source("R/utils.R")

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Profiles: Mental Health, Children & Young People, Health & Wellbeing

# Interactively generate the data by:
#   1. Navigating the the source (above)
#   2. Clicking the 'Data' box
#   3. Selecting the relevant profiles (above)
#   4. Ticking 'All available geographies'
#   5. Moving the time period slider to the date range 2009-14
#   6. Right-clicking on 'Download data' and clicking 'Copy Link Location'
#   7. Pasting the link into the GET request below
#   8. Running the code below

GET(
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_e6b08726/session/68053c8c89a3bb5057b118eb534e8e86/download/download_table_csv?w=e6b08726",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

raw <-
  read_csv(tf)

raw_indicators <-
  raw %>%
  filter(area_type == "Council area") %>%
  filter(str_detect(indicator, "^S\\d")) %>%
  select(
    indicator,
    lad_code = area_code,
    measure
  )

# Some indicators are by percentage, some are by mean (questionnaire score out of 70).
# Percentage indicators, higher = worse
# Mean indicators, lower = worse
# Scale and rank each indicator accordingly.
scaled_ranked_indicators <-
  raw_indicators %>%
  group_by(indicator) %>%
  mutate(
    ranked = if_else(
      str_detect(indicator, "percentage"),
      rank_na_first(measure),
      inverse_rank(measure)
    )
  ) %>%
  mutate(scaled_ranked = scale_ranks(ranked)) %>%
  ungroup()

# Combine scaled and ranked scores to get a total score per area
child_mental_health <-
  scaled_ranked_indicators %>%
  group_by(lad_code) %>%
  summarise(child_mental_health_scaled_ranked = mean(scaled_ranked))

write_rds(child_mental_health, "data/vulnerability/health-inequalities/scotland/healthy-people/child-mental-health.rds")