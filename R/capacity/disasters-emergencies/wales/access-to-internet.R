library(tidyverse)
library(readxl)
library(stringr)
library(geographr)
source("R/utils.R")

internet_raw <- read_csv("data/on-disk/CACI-access-to-internet/CACI-access-to-internet/vul_zscores.csv/vul_zscores.csv")

# Please be aware that the scores are in a range of 0-1
# A higher score indicates higher vulnerability in this case,
# Opposite to how the decile scores work.
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
        is.na(speed) ~ mean(speed, na.rm = TRUE),  # fill na with mean
        TRUE ~ as.numeric(speed)
      )
    ) %>%  # 0.27% of speed is NA
    # internet_users and speed are skew to right, others are normal
    mutate(
      speed = sqrt(speed),
      internet_users = sqrt(internet_users)
    ) %>%
    mutate(across(where(is.numeric), normalise)) %>%   # normalise each column

    mutate(
        # Calculate a sum column
        sum = rowSums(across(where(is.numeric)),  na.rm = TRUE),
        postcode = str_replace_all(postcode, " ", "")
    ) %>%
    left_join(lookup_postcode_oa_lsoa_msoa_lad, by = "postcode") %>%
    group_by(lsoa_code) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%   # for each LA, add up in the same column
    left_join(lookup_lsoa_msoa, by = "lsoa_code") %>%
    left_join(lookup_msoa_lad, by = "msoa_code") %>%
    left_join(population_lsoa, by = "lsoa_code") %>% 

    calculate_extent(
      var = sum,
      higher_level_geography = lad_code,
      population = total_population,
      weight_high_scores = FALSE  # lower score means more deprived
    )
    
write_rds(
    internet_clean,
    "data/capacity/disasters-emergencies/wales/access_to_internet_lad.rds"
)
