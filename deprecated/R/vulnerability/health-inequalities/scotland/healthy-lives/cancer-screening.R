library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Bowel screening uptake

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_4b5bef35/session/d3363e91131c1e11b471592329ee0ea9/download/download_table_csv?w=4b5bef35",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

cancer_screening_raw <-
  read_csv(tf)

unlink(tf)
rm(tf)

cancer_screening <-
  cancer_screening_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    cancer_screening_percent = measure
  )

write_rds(cancer_screening, "data/vulnerability/health-inequalities/scotland/healthy-lives/cancer-screening.rds")