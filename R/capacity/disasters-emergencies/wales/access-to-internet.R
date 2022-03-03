library(tidyverse)
library(readxl)
library(stringr)
library(geographr)
source("R/utils.R")

internet_raw <- read_csv("data/on-disk/CACI-access-to-internet/CACI-access-to-internet/vul_zscores.csv/vul_zscores.csv")

internet_clean <-
  internet_raw %>%
  filter(region == "Wal") %>%
  select(
      postcode = "postcode",
      speed = "vul_dig_brdbnd",   # Lower decile values indicate lower fixed internet speed, most are around 0  # nolint
      online_purchasing = "vul_dig_buyonl",   # Lower decile values indicate less online purchasing, normal checked # nolint
      online_finance = "vul_dig_manca_net",   # Lower decile values indicate less likely to manage current account online, normal checked # nolint
      mobile_phone = "vul_dig_mobnon",    # Lower decile values indicate less likely to have a mobile phone, normal checked # nolint
      internet_users = "vul_dig_netusr",  # Lower decile values indicate less likely to be an internet user, skew to right # nolint
      confuse = "vul_dig_confuse" # Lower decile values indicate more likely to be confused by computers, normal checked # nolint
  ) %>%
  mutate(
    speed = case_when(
      is.na(speed) ~ mean(speed, na.rm = TRUE),  # fill na with mean, 0.27% of speed is NA
      TRUE ~ as.numeric(speed)
    )
  ) %>%
  mutate(
      # Calculate a sum column
      sum = rowSums(across(where(is.numeric)),  na.rm = TRUE),
      postcode = str_replace_all(postcode, " ", "")
  ) %>%
  left_join(lookup_postcode_oa_lsoa_msoa_lad, by = "postcode") %>%
  left_join(population_oa, by = "lsoa_code") %>%

  calculate_extent(
    var = sum,
    higher_level_geography = lad_code,
    population = total_population,
    weight_high_scores = FALSE  # lower score means more deprived
  ) %>%
  rename(access_to_internet = extent)
    
write_rds(
    internet_clean,
    "data/capacity/disasters-emergencies/wales/access_to_internet_lad.rds"
)
