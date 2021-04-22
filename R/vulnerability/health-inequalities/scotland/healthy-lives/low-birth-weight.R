library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Healthy birth weight

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_1f998bbe/session/24180ffd708f74879b57a09212462e75/download/download_table_csv?w=1f998bbe",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

low_birth_weight_raw <-
  read_csv(tf)

unlink(tf)
rm(tf)

low_birth_weight <-
  low_birth_weight_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    healthy_birth_weight_percent = measure
  ) %>% 
  mutate(
    unhealthy_birth_weight_percent = (100 - healthy_birth_weight_percent)/100
  ) %>% 
  select(-healthy_birth_weight_percent)

write_rds(low_birth_weight, "data/vulnerability/health-inequalities/scotland/healthy-lives/low-birth-weight.rds")
