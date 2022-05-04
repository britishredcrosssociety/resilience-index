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
  select(lad_name = "Local Authority", 
         cap_exp_person = "Capital \r\nExpenditure,\r\n£ per person") |>
  mutate(cap_exp_person = as.numeric(cap_exp_person)) |>
  filter(lad_name != "ALL COUNCILS")

# Check LAD names
spending |>
  distinct(lad_name)

geographr_lads <- boundaries_ltla21 |>
  select(lad_name = ltla21_name,
         lad_code = ltla21_code) |>
  filter(str_detect(lad_code, "^S"))

geographr_lads |>
  distinct(lad_name)

# Correct mistmatches
spending$lad_name <- gsub('&','and', spending$lad_name)

# Check if LAD names are the same as in geographr
spending_names <- spending |>
  distinct(lad_name) |>
  pull()

geographr_lads_names <- geographr_lads |>
  distinct(lad_name) |>
  pull()

if(!(setequal(spending_names, geographr_lads_names))) {
  stop("LADS don't match")
} else {
  "LADS match"
}

# Add LAD codes to the dataset
spending_lad <- left_join(spending, geographr_lads, by = "lad_name") |>
  select(lad_name, lad_code, cap_exp_person)

# Plot the LADs spending power
spending_lad |>
  mutate(lad_name = fct_reorder(lad_name, desc(cap_exp_person))) |>
  ggplot(aes(x = lad_name, y = cap_exp_person))+
  geom_point() +
  theme_classic() +
  labs(title = "Capital Expenditure per Person of Scottish Local Authorities",
       x = "Local Authority District",
       y = "Capital Expenditure per Person") +
  guides(x = guide_axis(angle = 90))

# Save data ----
spending_lad |>
  write_rds("data/capacity/disasters-emergencies/scotland/la-spending-power.rds")