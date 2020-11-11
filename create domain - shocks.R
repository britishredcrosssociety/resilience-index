##
## Create a resilience domain for 'shocks' (floods and fires)
## This won't involve creating a singular index, but will aggregate neighbourhood-level shocks into higher geographies
##
library(tidyverse)
library(Hmisc)

source("functions.R")

# ---- Load data ----
flood_risks_msoa <- read_csv("data/processed/flood risks - msoa.csv")
flood_indicents_msoa <- read_csv("data/processed/flood incidents - msoa.csv")
fires_msoa <- read_csv("data/processed/fires - msoa.csv")

pop <- read_csv("data/population estimates msoa11 lad17 lad19 tacticall cell.csv")

lad_names <- read_csv("https://opendata.arcgis.com/datasets/35de30c6778b463a8305939216656132_0.csv")

# ---- Aggregate into Local Authorities ----
# Calculate number/% of people in LAs in flood risk areas
flood_risks_lad <- 
  flood_risks_msoa %>% 
  
  left_join(pop, by = "MSOA11CD") %>% 
  group_by(LAD19CD) %>% 
  summarise(`Total people in flood risk areas` = sum(`No. people in flood risk areas`),
            `% people in flood risk areas` = `Total people in flood risk areas` / pop_lad19) %>% 
  distinct() %>% 
  ungroup() %>% 
  
  mutate(`Flood risk quintile` = as.integer(cut2(`% people in flood risk areas`, g = 5)))

# Calculate per-capita flood incidents
flood_indicents_lad <- 
  flood_indicents_msoa %>% 
  
  left_join(pop, by = "MSOA11CD") %>% 
  group_by(LAD19CD) %>% 
  summarise(`Total historical flooding incidents` = sum(`No. flooding incidents`),
            `Flooding incidents per capita` = `Total historical flooding incidents` / pop_lad19) %>% 
  distinct() %>% 
  ungroup() %>% 
  
  mutate(`Flood incidents quintile` = as.integer(cut2(`Flooding incidents per capita`, g = 5)))

# Calculate per-capita dwelling fires
fires_lad <- 
  fires_msoa %>% 
  
  left_join(pop, by = "MSOA11CD") %>% 
  group_by(LAD19CD) %>% 
  summarise(`Total dwelling fires (three-year average)` = sum(`Number of fires (three-year average)`),
            `Dwelling fire incidents per capita` = `Total dwelling fires (three-year average)` / pop_lad19) %>% 
  distinct() %>% 
  ungroup() %>% 
  
  mutate(`Fire incidents quintile` = as.integer(cut2(`Dwelling fire incidents per capita`, g = 5)))

# ---- Combine and save ----
shocks_lad <- 
  lad_names %>% 
  select(LAD19CD, Name = LAD19NM) %>% 
  
  left_join(flood_risks_lad, by = "LAD19CD") %>% 
  left_join(flood_indicents_lad, by = "LAD19CD") %>% 
  left_join(fires_lad, by = "LAD19CD")

write_csv(shocks_lad, "data/processed/shocks.csv")
