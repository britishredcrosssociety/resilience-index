library(httr)
library(readxl)
library(readr)
library(dplyr)
library(stringr)

# Source: https://www.ons.gov.uk/economy/environmentalaccounts/datasets/accesstogardensandpublicgreenspaceingreatbritain
GET(
  "https://www.ons.gov.uk/file?uri=/economy/environmentalaccounts/datasets/accesstogardensandpublicgreenspaceingreatbritain/accesstopublicparksandplayingfieldsgreatbritainapril2020/ospublicgreenspacereferencetables.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

pgs_raw <-
  read_excel(tf, sheet = "LAD Parks only", range = "A1:J372")

pgs <-
  pgs_raw %>%
  select(
    lad_code = `LAD code`,
    public_green_space_average_distance = `Average distance to nearest Park, Public Garden, or Playing Field (m)`
  ) %>%
  filter(str_detect(lad_code, "^S"))

write_rds(pgs, "data/vulnerability/health-inequalities/scotland/healthy-places/public-green-space.rds")

# There is currently a bug in the ONS Ouput: the parks only and parks and fields data sets are identical
# The ONS have been emailed 27.04.21

# parks_only <-
#   read_excel(tf, sheet = "LAD Parks only")
# parks_and_fields <-
#   read_excel(tf, sheet = "LAD Parks and Playing Fields")
# identical(parks_only, parks_and_fields)