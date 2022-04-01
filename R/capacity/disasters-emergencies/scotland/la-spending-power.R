# Load packages
library(geographr)
library(demographr)
library(sf)
library(tidyverse)
library(readxl)
library(fuzzyjoin)

source("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/R/utils.R")

# Spending power data ----
tf <- download_file("https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2022/03/scottish-local-government-finance-statistics-slgfs-2020-21/documents/scottish-local-government-finance-statistics-slgfs-2020-21-publication-tables/scottish-local-government-finance-statistics-slgfs-2020-21-publication-tables/govscot%3Adocument/scottish-local-government-finance-statistics-slgfs-2020-21-publication-tables.xlsx", ".xlsx")

raw <- read_excel(tf, sheet = "Chart 3.4", skip = 5)

clean <- raw |>
  select(lad_21_name = "Local Authority", 
         cexp = "Capital \r\nExpenditure,\r\n£ per person") |>
  mutate(cexp = as.numeric(cexp)) |>
  filter(lad_21_name != "ALL COUNCILS")

clean$lad_21_name <- gsub('&','and', clean$lad_21_name)

lad <- boundaries_lad_21 |>
  filter(str_detect(lad_21_code, "^S"))

named <- inner_join(clean, lad, by = "lad_21_name") |>
  select(lad_21_name, lad_21_code, cexp)

