library(tidyverse)
library(geographr)

flood_risk <- read_csv("depreciated/data/processed/Flood risks - MSOA.csv")
flood_incidents <- read_csv("depreciated/data/processed/flood incidents - msoa.csv")

# Covid cases
covid <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingRate&metric=newCasesBySpecimenDateRollingSum&format=csv")

covid <- 
  covid %>% 
  select(msoa_code = areaCode, date, covid_rate = newCasesBySpecimenDateRollingRate, covid_sum = newCasesBySpecimenDateRollingSum)

msoa <- geographr::population_msoa %>% 
  select(msoa_code, msoa_name) %>% 
  
  left_join(covid, by = "msoa_code") %>% 
  left_join(flood_risk, by = c("msoa_code" = "MSOA11CD")) %>% 
  left_join(flood_incidents, by = c("msoa_code" = "MSOA11CD")) %>%
  
  mutate(
    is_flood_risk_area = ifelse(is.na(`No. people in flood risk areas`), "No", "Yes"),
    is_flood_incident_area = ifelse(is.na(`No. flooding incidents`), "No", "Yes"),
  )

msoa %>% 
  ggplot(aes(x = date, y = covid_rate)) +
  # geom_line(aes(group = msoa_code), colour = "grey", alpha = 0.1) +
  geom_smooth(aes(colour = is_flood_risk_area))

msoa %>% 
  ggplot(aes(x = date, y = covid_rate)) +
  # geom_line(aes(group = msoa_code), colour = "grey", alpha = 0.1) +
  geom_smooth(aes(colour = is_flood_incident_area))
