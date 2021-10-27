library(tidyverse)
library(readxl)
library(geographr)

source("R/utils.R")

raw_file <-
  download_file(
    "https://www.communities-ni.gov.uk/system/files/publications/communities/bss-carers-allowance-may-2021.XLSX",
    "xlsx"
  )

raw <-
  read_excel(
    raw_file,
    sheet = "Table 8",
    skip = 1
  )

carers_allowance <-
  raw |>
  select(
    `Local Government District`,
    carers_allowance_recipients_rate = `% of Eligible Population`
  ) |>
  slice(1:11)