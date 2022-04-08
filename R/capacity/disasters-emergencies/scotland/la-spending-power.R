# Load packages
library(geographr)
library(demographr)
library(sf)
library(tidyverse)
library(readxl)
library(fuzzyjoin)

source("R/utils.R")

# Data Source: Scottish Local Government Finance Statistics (SLGFS) 2020-21 (Scottish Government) ----
tf <- download_file("https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2022/03/scottish-local-government-finance-statistics-slgfs-2020-21/documents/scottish-local-government-finance-statistics-slgfs-2020-21-publication-tables-revised-7-april-2022/scottish-local-government-finance-statistics-slgfs-2020-21-publication-tables-revised-7-april-2022/govscot%3Adocument/scottish-local-government-finance-statistics-slgfs-2020-21-publication-tables-revised-7-april-2022.xlsx", ".xlsx")

raw <- read_excel(tf, sheet = "Chart 3.4", skip = 5)

spending <- raw |>
  select(lad_21_name = "Local Authority", 
         cap_exp_person = "Capital \r\nExpenditure,\r\n£ per person") |>
  mutate(cap_exp_person = as.numeric(cap_exp_person)) |>
  filter(lad_21_name != "ALL COUNCILS")

# Check LAD names
spending |>
  distinct(lad_21_name)

geographr_lads <- boundaries_lad_21 |>
  filter(str_detect(lad_21_code, "^S"))

geographr_lads |>
  distinct(lad_21_name)

# Correct mistmatches
spending$lad_21_name <- gsub('&','and', spending$lad_21_name)

# Check if LAD names are the same as in geographr
spending_names <- spending |>
  distinct(lad_21_name) |>
  pull()

geographr_lads_names <- geographr_lads |>
  distinct(lad_21_name) |>
  pull()

if(!(setequal(spending_names, geographr_lads_names))) {
  stop("LADS don't match")
} else {
  "LADS match"
}

# Add LAD codes to the dataset
spending_lad <- left_join(spending, lad, by = "lad_21_name") |>
  select(lad_21_name, lad_21_code, cap_exp_person)

# Plot the LADs spending power
spending_lad |>
  mutate(lad_21_name = fct_reorder(lad_21_name, desc(cap_exp_person))) |>
  ggplot(aes(x = lad_21_name, y = cap_exp_person))+
  geom_point() +
  theme_classic() +
  labs(title = "Capital Expenditure per Person of Scottish Local Authorities",
       x = "Local Authority District",
       y = "Capital Expenditure per Person") +
  guides(x = guide_axis(angle = 90))

# # Save data ----
# spending_lad |>
#   write_rds("data/capacity/disasters-emergencies/scotland/la-spending-power.rds")