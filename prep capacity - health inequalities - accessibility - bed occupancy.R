##
## NHS Trust overnight bed occupancy rates
## source: https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-overnight/
##
library(tidyverse)
library(lubridate)
library(readxl)
library(httr)

trust_lad <- read_csv("data/lookup-nhs-trust-to-la.csv")

# ---- Download and process bed occupancy data for 2020-21 ----
# Quarter 2
GET("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
    write_disk("data/raw/health/Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx"))

beds_20_21_q2 <- read_excel("data/raw/health/Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
                               sheet = "NHS Trust by Sector", skip = 14)

beds_20_21_q2 <- beds_20_21_q2 %>% 
  select(Year, Quarter = `Period End`, Trust_code = `Org Code`, Trust_name = `Org Name`, Percent_beds_occd = Total...18) %>% 
  slice(-(1:2)) %>% 
  mutate(Date = dmy("01/09/2019"))  # arbitrary date for this quarter

# Quarter 1
GET("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/2021-Q1-Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
    write_disk("data/raw/health/2021-Q1-Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx"))

beds_20_21_q1 <- read_excel("data/raw/health/2021-Q1-Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx", 
                            sheet = "NHS Trust by Sector", skip = 14)

beds_20_21_q1 <- beds_20_21_q1 %>% 
  select(Year, Quarter = `Period End`, Trust_code = `Org Code`, Trust_name = `Org Name`, Percent_beds_occd = Total...18) %>% 
  slice(-(1:2)) %>% 
  mutate(Date = dmy("01/06/2019"))  # arbitrary date for this quarter

# Bind the quarters together
beds_20_21 <- bind_rows(beds_20_21_q1, beds_20_21_q2) %>% 
  mutate(Percent_beds_occd = as.numeric(Percent_beds_occd)) %>% 
  
  # some Trusts changed names over this period of time - use only their most current names
  left_join(lookup_trusts, by = c("Trust_name" = "Name_bed_occupancy")) %>% 
  mutate(Trust_name = ifelse(!is.na(Name_scraped), str_to_upper(Name_scraped), Trust_name)) %>% 
  select(-Name_scraped)

# Link these mean occupancy rates to Local Authority codes for Trusts in `trust_lad`
beds_20_21 <- beds_20_21 %>% 
  # to match on Trust names, do some string manipulation/tidying first
  mutate(Trust_name = str_replace_all(Trust_name, " AND ", " & ")) %>%
  
  left_join(trust_lad %>% mutate(Trust = str_replace_all(str_to_upper(Trust), " AND ", " & ")),
            by = c("Trust_name" = "Trust"))

# Some LAs have multiple Trusts - take the worst mean occupancy rate in each LA
la_bed_occupancy = beds_20_21 %>% 
  # first, calculate mean occupancy rate over the time period for each Trust linked to each LA
  group_by(Trust_code, LAD19CD) %>% 
  summarise(mean_bed_rate = mean(Percent_beds_occd, na.rm = TRUE)) %>% 
  
  group_by(LAD19CD) %>% 
  summarise(`Worst bed occupancy rate` = max(mean_bed_rate, na.rm = TRUE))

# Save
write_csv(la_bed_occupancy, "data/raw/health/bed-occupancy-rates-la.csv")

# Plot bed occupancy rates over time
# beds_20_21 %>% 
#   ggplot(aes(x = Date, y = Percent_beds_occd, group = Trust_code)) +
#   geom_line(colour = "grey", alpha = 0.2) +
#   theme_classic()
