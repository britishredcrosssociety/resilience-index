##
## Calculate mean waiting list sizes over the course of 2020 for each LA
##
library(tidyverse)
library(lubridate)
library(readxl)
library(httr)

trust_lad <- read_csv("data/lookup-nhs-trust-to-la.csv")

# ---- Monthly diagnostics data ----
# Source: https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2020-21/

# Helper function for downloading and processing data
get_waiting_list <- function(url, date) {
  GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
  
  read_excel(tf, sheet = "Provider", skip = 13) %>% 
    select(Trust_code = `Provider Code`, Trust_name = `Provider Name`, `Total Waiting List`) %>% 
    slice(-(1:2)) %>% 
    mutate(Date = dmy(date))
}

# Download waiting list stats for 2020
sep_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Monthly-Diagnostics-Web-File-Provider-September-2020_1ME27.xls", "01/09/2020")
aug_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/10/Monthly-Diagnostics-Web-File-Provider-August-2020_o1lg9.xls", "01/08/2020")
jul_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/09/Monthly-Diagnostics-Web-File-Provider-July-2020.xls", "01/07/2020")
jun_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/08/Monthly-Diagnostics-Web-File-Provider-June-2020.xls", "01/06/2020")
may_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/07/Monthly-Diagnostics-Web-File-Provider-May-2020.xls", "01/05/2020")
apr_20 <- get_waiting_list("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/06/Monthly-Diagnostics-Web-File-Provider-April-2020.xls", "01/04/2020")

# Calculate average number of people waiting over the year so far in each LA
diagnosis_waiting <- bind_rows(sep_20, aug_20, jul_20, jun_20, may_20, apr_20) %>% 
  left_join(trust_lad, by = c("Trust_code" = "Code")) %>% 
  
  group_by(LAD19CD) %>% 
  summarise(`Mean waiting list size` = mean(`Total Waiting List`, na.rm = TRUE))

write_csv(diagnosis_waiting, "data/raw/health/waiting-times-la.csv")
