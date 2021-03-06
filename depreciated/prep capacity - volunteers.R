# ---- Load libraries ----
library(tidyverse)
library(readxl)

# ---- Load data ----
# Functions
source("functions.R")

# Load population estimates
pop <- read_csv("data/population estimates msoa11 lad17 lad19 tacticall cell.csv")

# TC lookup
lookup <- read_csv("data/lookup mosa11 to lad17 to lad19 to tactical cell.csv")

# La Names/Codes
lookup_names <- read_csv("output/vulnerability-LA.csv") %>% 
  select(Code, Name)

# Load BRC volunteer capacity
# Data cannot be shared in the public domain - please email for more information
brc <- read_csv("data/raw/brc-supply.csv")

# Load RVS capacity
# Data cannot be shared in the public domain - please email for more information
vol <- read_csv("data/raw/rvs-supply.csv")

# ---- Clean data ----
#- RVS -
# 1 = “Generally more volunteers than tasks”
# 2 = “About the right amount”
# 3 = “Less volunteers than tasks”
vol <-
  vol %>% 
  select(la_name = `Local Authority`,
         vol_supply = `Vol supply number`)

lookup_names <- lookup_names %>% 
  select(LAD19CD = Code,
         la_name = Name) %>% 
  distinct() %>% 
  drop_na()

# Find codes from name
vol <- vol %>% 
  mutate(la_name = if_else(la_name == "Shepway",
                           "Folkestone and Hythe",
                           la_name)) %>%  # replace incorrect la_name
  left_join(lookup_names,
            by = "la_name") %>% 
  select(-la_name)

lookup <- lookup %>% 
  select(LAD19CD, TacticalCell) %>% 
  distinct(LAD19CD, .keep_all = TRUE)

# Join to lookup
vol <- vol %>% 
  left_join(lookup, by = "LAD19CD") %>% 
  select(LAD19CD,
         TacticalCell,
         vol_supply)

# - BRC -
# Prep pop data
pop <-
  pop %>% 
  select(LAD19CD, pop = pop_lad19) %>% 
  distinct(LAD19CD, .keep_all = TRUE)

# Remove first row
brc <- 
  brc %>% 
  slice(-1)

# Select cols
brc <-
  brc %>% 
  select(la_name = `Local Authority`,
         crvs_active = `'Workforce'[People - CRVs (Active)]`,
         crvs = CRVs)

# Replace NA's
brc <-
  brc %>% 
  replace_na(list(crvs_active = 0, crvs = 0))

# Join LA names to codes
brc <-
  brc %>% 
  left_join(lookup_names,
            by = "la_name") %>% 
  select(-la_name) %>% 
  relocate(LAD19CD)

# Aggregate over LAs
aggr_cap <-
  capacity %>% 
  group_by(LAD19CD) %>% 
  summarise(crvs_active = sum(crvs_active),
            crvs = sum(crvs)) %>% 
  ungroup()

# Calculate relative volunteer score
brc_score <-
  aggr_cap %>% 
  left_join(pop, by = "LAD19CD") %>% 
  mutate(crvs_perc = crvs/pop * 100,
         brc_score = rank(crvs_perc),
         brc_score = scale_ranks(brc_score),
         brc_score = calc_risk_quantiles(brc_score,
                                         quants = 3,
                                         highest.quantile.is.worst = FALSE))

# Combine RVS and BRC volunteer data
vol_score <-
  lookup %>% 
  left_join(
    (brc_score %>% select(LAD19CD, brc_score)),
    by = "LAD19CD"
  ) %>% 
  left_join(
    (vol %>% select(LAD19CD, vol_supply)),
    by = "LAD19CD"
  )

# Calculate aggregate score
# Take the
vol_score <-
  vol_score %>% 
  mutate(worst_score = if_else(!is.na(vol_supply) & vol_supply > brc_score,
                               vol_supply,
                               brc_score),
         best_score = if_else(!is.na(vol_supply) & vol_supply < brc_score,
                              vol_supply,
                              brc_score),
         mean_score = (vol_supply + brc_score) / 2)

# Save
write_csv(vol_score,
          "data/processed/volunteer scores.csv")
