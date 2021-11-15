# Load libs
library(tidyverse)
library(geographr)
library(sf)

source("R/utils.R") # for download_file()

# NHS trust level & site level data on maintenance backlog -----

# Download trust level data
tf <- download_file("https://files.digital.nhs.uk/84/07227E/ERIC%20-%20201920%20-%20TrustData.csv", "csv")

raw_trust <-
  read_csv(
    tf,
    locale = locale(encoding = "latin1") #to deal with special characters in column names
   )

trust_columns <- raw_trust %>%
  select("Trust Code", "Trust Name", "Trust Type", "Investment to reduce backlog maintenance (£)")

# Download site level data
tf <- download_file("https://files.digital.nhs.uk/11/BC1043/ERIC%20-%20201920%20-%20SiteData%20-%20v2.csv", "csv")

raw_site <-
  read_csv(
    tf,
    locale = locale(encoding = "latin1") #to deal with special characters in column names
  )

site_maintance_columns <- c("Cost to eradicate high risk backlog (£)", "Cost to eradicate significant risk backlog (£)", "Cost to eradicate moderate risk backlog (£)", "Cost to eradicate low risk backlog (£)")

site_columns <- raw_site %>%
  select("Trust Code", "Trust Name", "Site Code", "Site Name", "Site Type", site_maintance_columns) %>%
  mutate_at(site_maintance_columns, ~ifelse(.x == "Not Applicable", NA, .x)) %>%
  mutate_at(site_maintance_columns, ~as.numeric(str_remove_all(.x, ",")))

site_agg <- site_columns %>%
  group_by(`Trust Code`) %>%
  summarise_if(is.numeric, ~sum(.x, na.rm = TRUE)) 

site_agg_total  <- site_agg %>%  
  rowwise(`Trust Code`) %>% 
  mutate(total_cost = rowSums(across(where(is.numeric))))

# Comparison of trust level and aggregated site level data -----

combined_data <- trust_columns |>
  left_join(site_agg_total, by = "Trust Code")
# cost != investment at trust level 


  