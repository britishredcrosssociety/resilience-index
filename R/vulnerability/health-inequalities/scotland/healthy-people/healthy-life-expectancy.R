library(httr)
library(readr)
library(readxl)
library(dplyr)

# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyatbirthandatage65bylocalareasuk
GET(
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyatbirthandatage65bylocalareasuk/current/hsleatbirthandatage65byukla201618.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

male_hle <-
  read_excel(
    tf,
    sheet = "HE - Male at birth",
    range = "A445:I476",
    col_names = c(
      "lad_code",
      "blank_1",
      "blank_2",
      "lad_name",
      "le",
      "le_lcl",
      "le_ucl",
      "blank_3",
      "hle"
    )
  ) %>%
  select(lad_code, hle)

female_hle <-
  read_excel(
    tf,
    sheet = "HE - Female at birth",
    range = "A445:I476",
    col_names = c(
      "lad_code",
      "blank_1",
      "blank_2",
      "lad_name",
      "le",
      "le_lcl",
      "le_ucl",
      "blank_3",
      "hle"
    )
  ) %>%
  select(lad_code, hle) %>%
  mutate(lad_code = if_else(lad_code == "S12000015", "S12000047", lad_code))

hle <-
  bind_rows(
    male_hle,
    female_hle
  ) %>%
  group_by(lad_code) %>%
  summarise(healthy_life_expectancy_average = mean(hle))

# Match 2019 LAD codes
# https://www.opendata.nhs.scot/dataset/geography-codes-and-labels/resource/967937c4-8d67-4f39-974f-fd58c4acfda5
# Look at the 'CADateArchived' column to view changes
hle <-
  hle %>%
  mutate(
    lad_code = case_when(
      lad_code == "S12000046" ~ "S12000049",
      lad_code == "S12000044" ~ "S12000050",
      TRUE ~ lad_code
    )
  )

write_rds(hle, "data/vulnerability/health-inequalities/scotland/healthy-people/healthy-life-expectancy.rds")