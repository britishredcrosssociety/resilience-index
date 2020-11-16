##
## Create an overall 'capacity index'
## - LA spending power
## - VCS volunteer capacity
## - Fire and Rescue response times
##
library(tidyverse)

source("functions.R")

# ---- Load data ----
spending <- read_csv("data/processed/LA spending power.csv")
volunteer <- read_csv("data/processed/volunteer scores.csv")
fire_response <- read_csv("data/processed/fire and rescue response times.csv")

# ---- Create capacity index -----
# Build volunteer domain
volunteer <-
  volunteer %>%
  filter(str_detect(LAD19CD, "^E")) %>% 
  select(LAD19CD, `Worst volunteer capacity rating` = worst_score) %>%
  mutate_if(
    is.numeric,
    list(`Volunteer score` = function(x) standardised(rank2(x)))
  ) %>%
  mutate(`Volunteer rank` = rank(`Volunteer score`))

# Build emergency response domain
fire_response <-
  fire_response %>%
  filter(str_detect(LAD19CD, "^E")) %>% 
  select(LAD19CD, `Fire and Rescue response time (three-year average)`) %>%
  mutate(`Emergency response score` = standardised(rank2(`Fire and Rescue response time (three-year average)`))) %>%
  mutate(`Emergency response rank` = rank(`Emergency response score`))

# Join domains and calculate capacity index
capacity_index <-
  spending %>%
  left_join(volunteer, by = "LAD19CD") %>%
  left_join(fire_response, by = "LAD19CD") %>%
  select(
    LAD19CD,
    ends_with("rank")
  ) %>%
  calc_domain_scores(bespoke.domains = TRUE, rank.indicators = FALSE) %>%
  rename(`Capacity score` = `Vulnerability score`, `Capacity rank` = `Vulnerability rank`, `Capacity decile` = `Vulnerability decile`) %>% 
  
  # calculate quintiles
  mutate(`Capacity quintile` = calc_risk_quantiles(`Capacity rank`, quants = 5))

capacity_index %>% 
  write_csv("data/processed/capacity index.csv")
