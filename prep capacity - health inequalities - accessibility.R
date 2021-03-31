##
## Accessibility/availability sub-domain for health inequalities
##
library(tidyverse)
library(readxl)
library(httr)

# ---- Load data ----
# Mid-2019: April 2019 local authority district population estimates
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
GET("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2019localauthoritydistrictcodes/ukmidyearestimates20192019ladcodes.xls",
    write_disk(tf <- tempfile(fileext = ".xls")))

la_pop <- read_excel(tf, sheet = "MYE2 - Persons", skip = 4)
unlink(tf); rm(tf)

# Calculate no. people aged 65+ in each LA
la_pop <- la_pop %>% 
  filter(!Geography1 %in% c("Country", "Region") &
           str_sub(Code, 1, 1) == "E") %>% 
  
  # convert age columns to long format
  rename(`90` = `90+`) %>% 
  pivot_longer(cols = `0`:`90`, names_to = "Age", values_to = "n_people") %>% 
  mutate(Age = as.integer(Age)) %>% 
  
  # sum whole population and people over 70
  group_by(Code) %>% 
  summarise(`No. people` = sum(n_people),
            `No. people aged 65+` = sum(n_people[Age >= 65], na.rm = TRUE))

# Local Authority Districts (December 2019) Names and Codes in the United Kingdom
# Source: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-names-and-codes-in-the-united-kingdom
la_names <- read_csv("https://opendata.arcgis.com/datasets/35de30c6778b463a8305939216656132_0.csv") %>% 
  select(Code = LAD19CD, Name = LAD19NM)

# Counties (April 2019) Names and Codes in England
# Source: https://geoportal.statistics.gov.uk/datasets/counties-april-2019-names-and-codes-in-england
county_names <- read_csv("https://opendata.arcgis.com/datasets/b3d60eecd2e5483384fcd5cf03a82d27_0.csv") %>% 
  select(Code = CTY19CD, Name = CTY19NM)

geog_names <- bind_rows(la_names, county_names)


# ---- Social care ----
# - Load data -
# Care directory with filters
# Source: https://www.cqc.org.uk/about-us/transparency/using-cqc-data#directory
GET("https://www.cqc.org.uk/sites/default/files/02%20November%202020%20HSCA%20Active%20Locations%20%281%29.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

cqc_filter <- read_excel(tf, sheet = "HSCA Active Locations", col_types = "text")
unlink(tf); rm(tf)

# - Care home beds (per 1,000 older people) -
care_home_beds <- cqc_filter %>% 
  # filter(`Service type - Care home service without nursing` == "Y") %>% 
  
  group_by(`Location Local Authority`) %>% 
  summarise(`No. care home beds` = sum(`Care homes beds`, na.rm = TRUE)) %>% 
  
  left_join(geog_names, by = c("Location Local Authority" = "Name")) %>% 
  left_join(la_pop, by = "Code") %>% 
  
  mutate(`Care home beds per 1,000 people aged 65+` = `No. care home beds` / `No. people aged 65+` * 1000) %>% 
  
  select(Code, Name = `Location Local Authority`, everything())

# - Care home beds with nursing (per 1,000 older people) –
nursing_beds <- cqc_filter %>% 
  filter(`Service type - Care home service with nursing` == "Y") %>% 
  
  group_by(`Location Local Authority`) %>% 
  summarise(`No. care home beds with nursing` = sum(`Care homes beds`, na.rm = TRUE)) %>% 
  
  left_join(geog_names, by = c("Location Local Authority" = "Name")) %>% 
  left_join(la_pop, by = "Code") %>% 
  
  mutate(`Care home beds with nursing per 1,000 people aged 65+` = `No. care home beds with nursing` / `No. people aged 65+` * 1000) %>% 
  
  select(Code, Name = `Location Local Authority`, everything())

# - Domiciliary care services registered -
dom_care <- cqc_filter %>% 
  filter(`Service type - Domiciliary care service` == "Y") %>% 
  count(`Location Local Authority`) %>% 
  left_join(geog_names, by = c("Location Local Authority" = "Name")) %>% 
  select(Code, Name = `Location Local Authority`, `No. domiciliary services` = n)


# ---- Primary and secondary care ----
# - Bed availability -
la_beds <- read_csv("data/raw/health/beds-la.csv")

# Calculate per capita bed availability
la_beds <- la_beds %>% 
  left_join(la_pop, by = c("LAD19CD" = "Code")) %>% 
  
  mutate(`Hospital bed availability per 1,000 people` = `Total beds available` / `No. people` * 1000)

# - Waiting lists -
la_waits <- read_csv("data/raw/health/waiting-times-la.csv")
