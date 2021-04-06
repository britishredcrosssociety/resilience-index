library(readr)
library(tidyr)
library(dplyr)
library(stringr)

scotland_intermediatezone_elderly <- function(){
# source: https://www.opendata.nhs.scot/dataset/population-estimates/resource/93df4c88-f74b-4630-abd8-459a19b12f47
pop_sco_raw <- read_csv("https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/93df4c88-f74b-4630-abd8-459a19b12f47/download/iz2011-pop-est_02042020.csv")

pop_sco <-
  pop_sco_raw %>% 
  filter(Year == max(Year) & IntZone != "S92000003" & Sex == "All") %>% 
  pivot_longer(cols = Age0:Age90plus, names_to = "Age", values_to = "n_people") %>% 
  mutate(Age = as.integer(str_extract(Age, "[0-9]+"))) %>%    # extract ages as numbers
  rename(code = IntZone) %>% 
  group_by(code) %>% 
  summarise(`No. people` = sum(n_people),
            `No. people over 70` = sum(n_people[Age >= 70], na.rm = TRUE)) %>% 
  mutate(`Proportion people over 70` = `No. people over 70` / `No. people`) %>% 
  select(code, proportion_over_70 = `Proportion people over 70`)

return(pop_sco)
}

scotland_intermediatezone_elderly()
