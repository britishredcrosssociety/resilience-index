# ---- Load libraries ----
library(tidyverse)
library(readxl)
library(httr)

# ---- Load data ----
# Functions
source("functions.R")

# Private sector social care workers
# Source: https://www.skillsforcare.org.uk/adult-social-care-workforce-data/Workforce-intelligence/publications/local-information/My-local-authority-area.aspx
# GET("https://www.skillsforcare.org.uk/adult-social-care-workforce-data/Workforce-intelligence/documents/Raw-data/LA-data-download-2019-20.xlsx",
#     write_disk(tf <- tempfile(fileext = ".xlsx")))
# social <- read_excel(tf, sheet = "LA level data")
# unlink(tf); rm(tf)

social <- read_excel("data/raw/health/LA-data-download-2019-20.xlsx", sheet = "LA level data") # Load cached copy

# Local Authority Districts (December 2019) Names and Codes in the United Kingdom
lad_names <- read_csv("https://opendata.arcgis.com/datasets/35de30c6778b463a8305939216656132_0.csv") %>% 
  select(LAD19CD, LAD19NM)

# Load county lookup
lad_county = read_csv("https://opendata.arcgis.com/datasets/1512e57c5aa34d049569750083765eba_0.csv")

# Popoulation estimates
pop <- read_csv("data/population estimates msoa11 lad17 lad19 tacticall cell.csv") %>% 
  distinct(LAD19CD, pop_lad19)

# ---- Clean data ----
# Select cols
social <- 
  social %>% 
  select(la_name = `CSSR (LA area)`,
         sector = SECTOR,
         service = Service_GP,
         job_role = `Job role`,
         turnover = `Turnover rate`,
         vacancy = `Vacancy rate`,
         sickness_days_mean = `Sickness - Mean (divide by Employees)`,
         zero_hour_yes = `Zero Hour Contracts - Yes`)

# Select all services and sectors
social <-
  social %>% 
  filter(sector == "All sectors" &
           service == "All services")

# Select only job roles which provide direct care
social <-
  social %>% 
  filter(
    job_role == "Care_worker (individual job role)" |
      job_role == "Direct_care (job role group)" |
      job_role == "Occupational_therapist (individual job role)" |
      job_role == "Registered_nurse (individual job role)" |
      job_role == "Senior_care_worker (individual job role)" |
      job_role == "Social_worker (individual job role)"
  )

# Replace suppressed data (where there isn't enough jobs) with zero (see Notes tab in raw data)
# and convert to class double for computation
social <-
  social %>% 
  mutate(across(turnover:zero_hour_yes, ~str_replace_all(., "\\*", "0")),
         across(turnover:zero_hour_yes, as.double))

# Calculate mean values per LA
social <-
  social %>% 
  group_by(la_name) %>% 
  summarise(across(turnover:zero_hour_yes, mean))

# ---- Align la's with ONS codes ----
# The Local Authorities are actually a mixture of County Councils and local authorities

# - Match LA's -
soc_la <-
  social %>% 
  inner_join(lad_names, by = c("la_name" = "LAD19NM")) %>% 
  select(-la_name,
         LAD19CD) %>% 
  relocate(LAD19CD)

# - Match counties, weighting figures by population -
# Match counties
soc_county <-
  social %>% 
  anti_join(lad_names, by = c("la_name" = "LAD19NM")) %>% 
  inner_join(lad_county, by = c("la_name" = "CTY19NM")) %>% 
  select(-la_name, -FID, -LAD19NM) %>% 
  relocate(LAD19CD, CTY19CD)

# Join to population data
soc_county <-
  soc_county %>% 
  left_join(pop, by = "LAD19CD")

# Weight by population
soc_county <-
  soc_county %>% 
  group_by(CTY19CD) %>% 
  mutate(pop_total = sum(pop_lad19),
         weight = pop_lad19/pop_total) %>% 
  ungroup() %>% 
  select(-pop_lad19,
         -pop_total) %>% 
  mutate(across(turnover:zero_hour_yes, ~ . * weight))

soc_county <-
  soc_county %>% 
  select(-CTY19CD,
         -weight)

# - Manually match anything that didn't match to an LA or county -
soc_remainder <-
  social %>% 
  anti_join(lad_names, by = c("la_name" = "LAD19NM")) %>% 
  anti_join(lad_county, by = c("la_name" = "CTY19NM")) %>% 
  mutate(LAD19CD = case_when(
    la_name == "Redcar & Cleveland" ~ "E06000003",
    la_name == "Stockton on Tees" ~ "E06000004",
    la_name == "Durham" ~ "E06000047",
    la_name == "Kingston upon Hull" ~ "E06000010",
    la_name == "St Helens" ~ "E08000013",
    la_name == "Stoke on Trent" ~ "E06000021",
    la_name == "Herefordshire" ~ "E06000019",
    la_name == "Telford & Wrekin" ~ "E06000020",
    la_name == "Windsor & Maidenhead" ~ "E06000040",
    la_name == "Southend on Sea" ~ "E06000033",
    la_name == "Hammersmith & Fulham" ~ "E09000013",
    la_name == "Kensington & Chelsea" ~ "E09000020",       
    la_name == "Barking & Dagenham" ~ "E09000002",
    la_name == "Bournemouth" ~ "E06000058",
    la_name == "Poole" ~ "E06000058",                    
    la_name == "Brighton & Hove" ~ "E06000043",
    la_name == "Cornwall and Isles of Scilly" ~ "E06000052/E06000053",
    la_name == "Bristol" ~ "E06000023",                   
    la_name == "Cheshire West & Chester" ~ "E06000050"
  )) %>% 
  # separate Cornwall and Isles of Scilly
  separate_rows(LAD19CD, sep = "/") 

# Create population weighted values for Cornwall and Isles of Scilly
soc_remainder <-
  soc_remainder %>% 
  left_join(pop, by = "LAD19CD") %>% 
  group_by(la_name) %>% 
  mutate(pop_total = sum(pop_lad19),
         weight = pop_lad19/pop_total) %>% 
  ungroup() %>% 
  select(-pop_lad19,
         -pop_total) %>% 
  mutate(across(turnover:zero_hour_yes, ~ . * weight)) %>% 
  select(-la_name,
         -weight) %>% 
  relocate(LAD19CD)

# Join Bournemouth and Poole
soc_remainder <-
  soc_remainder %>% 
  group_by(LAD19CD) %>% 
  mutate(across(turnover:zero_hour_yes, sum)) %>% 
  ungroup() %>% 
  distinct(LAD19CD, .keep_all = TRUE)

# Join all LAD19CD
social <-
  bind_rows(
    soc_la,
    soc_county,
    soc_remainder
  )

# ---- Calculate social score ----
# Create two domains:
# - (1) Coping capacity
# - (2) Workforce flexibility

# - Coping capacity domain -
# PCA to weight indicators
# Note: the higher the score, the worse the coping capacity is
coping <-
  social %>% 
  select(LAD19CD, `Staff turnover rate` = turnover, `Mean sickness days` = sickness_days_mean) %>% 
  weighted_domain_scores(model = "PCA") %>% 
  rename_with(str_replace, pattern = "Vulnerability", replacement = "Adult Social Care - Coping", matches("Vulnerability"))

# - Workforce flexibility -
# PCA to weight indicators
# Note: the higher the score, the better the flexibility is, so invert.
flexibility <-
  social %>% 
  select(LAD19CD, `Vacancy rate` = vacancy, zero_hour_yes) %>% 
  # invert zero hour contracts rate so higher number = worse (because fewer zero hours contracts means less flexibility)
  mutate(`Mean % of non-zero-hours contracts` = 1 - zero_hour_yes) %>%
  select(-zero_hour_yes) %>% 
  
  weighted_domain_scores(model = "PCA") %>% 
  rename_with(str_replace, pattern = "Vulnerability", replacement = "Adult Social Care - Flexibility", matches("Vulnerability"))

coping %>% 
  left_join(flexibility, by = "LAD19CD") %>% 
  write_csv("data/raw/health/adult-social-care-workforce.csv")

# ---- Save a copy with overall scores/ranks/deciles for Adult Social Care workforce ----
lad_names <- read_csv("https://opendata.arcgis.com/datasets/c3ddcd23a15c4d7985d8b36f1344b1db_0.csv") %>% 
  select(LAD19CD, LAD19NM)

coping %>% 
  left_join(flexibility, by = "LAD19CD") %>% 
  
  select(
    LAD19CD,
    ends_with("rank")
  ) %>%
  
  calc_domain_scores(bespoke.domains = TRUE, rank.indicators = FALSE) %>%
  
  left_join(lad_names, by = "LAD19CD") %>% 
  
  select(Code = LAD19CD, 
         `Local Authority Name` = LAD19NM,
         # `Adult Social Care workforce score` = `Vulnerability score`, 
         `Adult Social Care workforce rank` = `Vulnerability rank`, 
         `Adult Social Care workforce decile` = `Vulnerability decile`) %>% 
  
  arrange(desc(`Adult Social Care workforce rank`)) %>% 
  
  write_csv("analysis - LA capacity/adult-social-care-workforce-ranks.csv")
