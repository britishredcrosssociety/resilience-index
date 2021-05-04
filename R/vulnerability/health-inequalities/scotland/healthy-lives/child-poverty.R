library(readODS)
library(httr)
library(dplyr)
library(stringr)
library(readr)

# Source: https://www.gov.uk/government/statistics/children-in-low-income-families-local-area-statistics-201415-to-201819
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/895808/children-in-low-income-families-local-area-statistics-2014-15-to-2018-19.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

child_poverty_raw <-
  read_ods(tf, sheet = "Table_1", range = "B10:M384") %>%
  as_tibble(.name_repair = "unique")

child_poverty <-
  child_poverty_raw %>%
  slice(-(1:3)) %>%
  select(
    lad_code = `Area Code`,
    child_poverty_percent = `2018/19p...12`
  ) %>%
  filter(str_detect(lad_code, "^S"))

write_rds(child_poverty, "data/vulnerability/health-inequalities/scotland/healthy-lives/child-poverty.rds")

# Does Table_1 need switching to Table_2 to use absolute rather than relative values?
# It appears the England definition uses absolute?
# From stat-xplore:

# Absolute: Absolute low-income is defined as a family whose equivalised income is below
#           60 per cent of the 2010/11 median income adjusted for inflation. 
# Relative: Relative low-income is defined as a family whose equivalised income is below
#           60 per cent of contemporary median income.