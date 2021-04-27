library(httr)
library(readxl)
library(readr)
library(dplyr)
library(stringr)

# Source: https://www.ons.gov.uk/economy/environmentalaccounts/datasets/accesstogardensandpublicgreenspaceingreatbritain
GET(
  "https://www.ons.gov.uk/file?uri=/economy/environmentalaccounts/datasets/accesstogardensandpublicgreenspaceingreatbritain/accesstogardenspacegreatbritainapril2020/osprivateoutdoorspacereferencetables.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

pos_raw <-
  read_excel(tf, sheet = "LAD gardens", range = "A2:X373")

pos <-
  pos_raw %>%
  select(
    lad_code = ...5,
    private_outdoor_space_percent = `Percentage of adresses with private outdoor space...23`
  ) %>%
  filter(str_detect(lad_code, "^S"))

write_rds(pos, "data/vulnerability/health-inequalities/scotland/healthy-places/private-outdoor-spaces.rds")