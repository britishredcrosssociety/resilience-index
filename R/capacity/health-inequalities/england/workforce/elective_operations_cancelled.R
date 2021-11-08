# Load libs
library(tidyverse)

source("R/utils.R") #for download_file()

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

# Download the data 
tf <- download_file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/02/Cancelled-Ops-Q3-2019-20-4hiU8.xlsx", "xlsx")

raw <-
  read_excel(
    tf,
    sheet = "Provider",
    skip = 15
  )

# Remove any rows where Organisation Code is '-' or NA
ops_empty_org_code <- raw %>%
  filter(!(is.na(`Organisation Code`) | `Organisation Code` == "-"))

# Select variables of interest
ops_vars <- ops_empty_org_code %>%
  select(`Organisation Code`, `Number of last minute elective operations cancelled for non clinical reasons`)

# Filter to only open trusts
ops_open <-
  open_trusts |>
  left_join(ops_vars, by = c("trust_code" = "Organisation Code"))


# TODO:
# Trusts need to be matched to MSOA's, and then aggregated up to LTLA.
# Issue with current lookup 'lookup_trust_msoa' which is being investigated so waiting until then. 

ops_open |>
  left_join(lookup_trust_msoa) 
