library(tidyverse)
library(dplyr)
library(statswalesr)
library(stringr)
library(geographr)
source("R/utils.R")

# Download dat
# Source: https://statswales.gov.wales/Catalogue/Local-Government/Finance/Revenue/Budgets/budgetedrevenueexpenditure-by-authority-service
df <- statswalesr::statswales_get_dataset("lgfs0030")


# Generate a list of distinct wales lad code
wales_lad_code <- 
    lookup_msoa_lad %>% 
        filter(
            str_detect(lad_code, "^W")
        ) %>% 
        distinct(lad_code)

# Clean data
df_clean <-
    df %>%
    filter(
        Year_ItemName_ENG == "2021-22" &  # Use data 2021-2022 
        str_detect(Measure_ItemName_ENG, pattern = "thousand") &  # Two measure: ￡ thousand; ￡ per head. We use ￡ thousand
        str_detect(Authority_AltCode1, "^W060000")   # Only select local authority 
    ) %>% 
    select(
        lad_code = Authority_AltCode1,
        variable = Description_ItemName_ENG,
        value = Data
    ) %>%
    group_by(variable,lad_code) %>%  # This is preparation for pivot table
    mutate(
        row = row_number()  # This is preparation for pivot table
    ) %>%
    pivot_wider(
        names_from = variable,
        values_from = value
    ) %>% 
    select(-row) %>%  # Drop unnecessary column
    group_by(lad_code) %>% 
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))  # for each LA, add up in the same column

df_sum <-
    df_clean %>%
    mutate(across(where(is.numeric), normalise)) %>%  # normalise each column
    mutate(
        sum = rowSums(across(where(is.numeric)),  na.rm = TRUE)  # Calculate a sum column
    ) %>% 
    select(lad_code, sum)

# For all ranks: 1 is most deprived
spending_power_lad <-
  df_sum %>%
  mutate(rank = rank(sum)) %>%
  mutate(
    deciles = quantise(
      rank,
      num_quantiles = 10
    )
  ) %>%
  select(-sum, -rank)

write_rds(
    spending_power_lad,
    "data/capacity/disasters-emergencies/wales/la-spending-power-lad.rds"
)
