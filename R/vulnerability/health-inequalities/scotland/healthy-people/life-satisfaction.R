library(httr)
library(readxl)
library(readr)
library(dplyr)
library(stringr)

# Source: https://www.ons.gov.uk/datasets/wellbeing-local-authority/editions/time-series/versions/1
GET(
  "https://download.ons.gov.uk/downloads/datasets/wellbeing-local-authority/editions/time-series/versions/1.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

life_satisfaction_raw <-
  read_excel(tf, sheet = "Dataset", skip = 2)

# The 'Average (mean)' estimate provides the score out of 0-10. The other estimates are
# thresholds (percentages) described in the QMI: https://www.ons.gov.uk/peoplepopulationandcommunity/wellbeing/methodologies/personalwellbeingintheukqmi
life_satisfaction <-
  life_satisfaction_raw %>%
  filter(Estimate == "Average (mean)") %>%
  filter(MeasureOfWellbeing == "Life Satisfaction") %>%
  filter(str_detect(`Geography code`, "^S")) %>%
  select(
    lad_code = `Geography code`,
    life_satisfaction_score_out_of_10 = `2019-20`
  )

# Replace old LAD codes with updates 2019 codes, and remove Scotland aggregate (S92000003)
# Lookup: https://github.com/drkane/geo-lookups/blob/master/la_all_codes.csv
lookup <-
  read_csv("https://raw.githubusercontent.com/drkane/geo-lookups/master/la_all_codes.csv") %>%
  select(lad_code_old = LADCD, lad_code = LAD20CD) %>%
  filter(str_detect(lad_code_old, "^S"))

life_satisfaction <-
  life_satisfaction %>%
  filter(lad_code != "S92000003") %>%
  rename(lad_code_old = lad_code) %>%
  left_join(lookup, by = "lad_code_old") %>%
  select(lad_code, life_satisfaction_score_out_of_10)

write_rds(life_satisfaction, "data/vulnerability/health-inequalities/scotland/healthy-people/life-satisfaction.rds")