library(httr)
library(readxl)
library(readr)
library(dplyr)

# Load population estimates
# source: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2019
GET(
  "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-19/mid-year-pop-est-19-data.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

pop_raw <- read_excel(tf, sheet = "Table 2", range = "A4:C38")

# Calculate population estimates
pop <-
  pop_raw %>%
  slice(-(1:2)) %>%
  select(
    lad_code = `Area code1`,
    lad_name = `Area name`,
    pop_count = `All Ages`
  )

# Source: https://www.gov.scot/publications/homelessness-scotland-update-30-september-2020/
GET(
  "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2021/03/homelessness-scotland-update-30-september-2020/documents/tables-march-2021/tables-march-2021/govscot%3Adocument/tables-march-2021.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

# Use no. open homelessness cases table
rough_sleeping_raw <-
  read_excel(
    tf,
    sheet = "Table 6",
    range = "A7:L38",
    col_names = c(
      "lad_name",
      "mar_18",
      "jun_18",
      "sep_18",
      "dec_18",
      "mar_19",
      "jun_19",
      "sep_19",
      "dec_19",
      "mar_20",
      "jun_20",
      "sep_20"
    )
  )

# Select latest data and join to population counts
rough_sleeping <-
  rough_sleeping_raw %>%
  select(lad_name, rough_sleeping_count = sep_20) %>%
  mutate(
    lad_name = case_when(
      lad_name == "Argyll & Bute" ~ "Argyll and Bute",
      lad_name == "Dumfries & Galloway" ~ "Dumfries and Galloway",
      lad_name == "Edinburgh" ~ "City of Edinburgh",
      lad_name == "Eilean Siar" ~ "Na h-Eileanan Siar",
      lad_name == "Orkney" ~ "Orkney Islands",
      lad_name == "Perth & Kinross" ~ "Perth and Kinross",
      lad_name == "Shetland" ~ "Shetland Islands",
      TRUE ~ lad_name
    )
  ) %>%
  left_join(pop, by = "lad_name") %>%
  relocate(lad_code) %>%
  select(-lad_name)

# Normalise by population size
rough_sleeping <-
  rough_sleeping %>%
  mutate(rough_sleeping_percent = rough_sleeping_count / pop_count) %>%
  select(lad_code, rough_sleeping_percent)

write_rds(rough_sleeping, "data/vulnerability/health-inequalities/scotland/healthy-places/rough-sleeping.rds")