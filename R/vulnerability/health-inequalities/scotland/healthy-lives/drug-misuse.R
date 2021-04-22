library(readr)
library(httr)
library(dplyr)

# Source:https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Indicator: Drug-related hospital admissions

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
  "https://scotland.shinyapps.io/ScotPHO_profiles_tool/_w_2d95cf86/session/0e0bd240aaee0883942e38a71c49ca53/download/download_table_csv?w=2d95cf86",
  write_disk(tf <- tempfile(fileext = ".csv"))
)

drugs_raw <-
  read_csv(tf)

unlink(tf)
rm(tf)

drugs <-
  drugs_raw %>%
  filter(area_type == "Council area") %>%
  select(
    lad_code = area_code,
    drug_admissions_per_100000 = measure
  )

write_rds(drugs, "data/vulnerability/health-inequalities/scotland/healthy-lives/drug-misuse.rds")