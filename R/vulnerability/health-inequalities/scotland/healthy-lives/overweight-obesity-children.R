library(readr)
library(httr)
library(dplyr)
library(geographr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Child healthy weight in primary 1

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_7cc7e89a/session/f576cc4dd52b8b5f0d22db794bb83854/download/download_table_csv?w=7cc7e89a",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

child_weight_raw <-
  read_csv(tf)

child_weight <-
  child_weight_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    healthy_weight_percent = measure
  ) %>%
  mutate(
    unhealthy_weight_percent = (100 - healthy_weight_percent) / 100
  ) %>%
  select(-healthy_weight_percent)

# Impute missing data
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/methodologies/methodsusedtodevelopthehealthindexforengland2015to2018
# Reason for missing: unkown
# Strategy: replace with mean for the LAD's HB's where available or data set
#           when not

hb_scores <-
  lookup_hb_lad %>%
  select(ends_with("code")) %>%
  left_join(child_weight, by = "lad_code") %>%
  group_by(hb_code) %>%
  mutate(hb_score = mean(unhealthy_weight_percent, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    hb_score = if_else(
      is.nan(hb_score),
      NA_real_,
      hb_score
    )
  )

child_weight <-
  hb_scores %>%
  mutate(
    child_unhealthy_weight_percent = case_when(
      is.na(unhealthy_weight_percent) & !is.na(hb_score) ~ hb_score,
      is.na(unhealthy_weight_percent) & is.na(hb_score) ~ mean(unhealthy_weight_percent, na.rm = TRUE),
      TRUE ~ unhealthy_weight_percent,
    )
  ) %>%
  select(lad_code, child_unhealthy_weight_percent)

write_rds(child_weight, "data/vulnerability/health-inequalities/scotland/healthy-lives/overweight-children.rds")