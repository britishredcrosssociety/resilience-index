##
## Create Resilience Index dataframe for Local Authorities, containing data on vulnerability, capacity, and shocks
##
library(tidyverse)

# ---- Load data ----
vi <- read_csv("data/processed/vulnerability index - la.csv")
capacity <- read_csv("data/processed/capacity index.csv")
shocks <- read_csv("data/processed/shocks.csv")

# ---- Combine into single LA dataframe for England ----
ri <- 
  vi %>% 
  filter(str_sub(LAD19CD, 1, 1) == "E") %>% 
  
  left_join(capacity, by = "LAD19CD") %>% 
  left_join(shocks, by = "LAD19CD")

write_csv(ri, "data/processed/resilience index.csv")
