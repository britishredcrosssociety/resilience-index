##
## Create lookup table between NHSE Trusts and Local Authorities based on where each Trust's hospitals are located
##
library(tidyverse)
library(httr)

# ---- Load data ----
# List of NHS hospitals in England
# hosp_eng <- read_csv("data/raw/health/england-hospitals.csv")
hosp_eng <- read_csv("https://github.com/matthewgthomas/nhs-scraper/raw/master/England%20hospitals.csv")

# NHS Trust names and codes scraped from NHS Digital's ODS Portal: https://odsportal.hscic.gov.uk/Organisation/Search
trust_names <- read_csv("https://github.com/matthewgthomas/nhs-scraper/raw/master/England%20Trusts%20names%20and%20codes.csv")

# ---- Get Local Authorities for each hospital ----
# Load postcode lookup to LAs
# download and unzip Postcode to Output Area to Lower Layer Super Output Area to Middle Layer Super Output Area to Local Authority District (May 2020) Lookup in the UK
# source: https://geoportal.statistics.gov.uk/datasets/postcode-to-output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-may-2020-lookup-in-the-uk
GET("https://www.arcgis.com/sharing/rest/content/items/a8d42df48f374a52907fe7d4f804a662/data",
    write_disk(tf <- tempfile(fileext = ".zip")))

unzip(tf, exdir = "data/raw/postcodes")
unlink(tf); rm(tf)

# load postcode directory and keep only relevant fields - we just need MSOA
postcodes_raw <- read_csv("data/raw/postcodes/PCD_OA_LSOA_MSOA_LAD_MAY20_UK_LU.csv")
postcodes <- postcodes_raw %>% dplyr::select(Postcode = pcd7, ladcd)

# the ONS data truncates 7-character postcodes to remove spaces (e.g. CM99 1AB --> CM991AB); get rid of all spaces in both datasets to allow merging
# Mutate to upper case
postcodes <- postcodes %>%
  mutate(Postcode2 = str_to_upper(Postcode),
         Postcode2 = str_replace_all(Postcode, " ", ""))

hosp_eng <- hosp_eng %>%
  mutate(Postcode2 = str_to_upper(Postcode),
         Postcode2 = str_replace_all(Postcode, " ", ""))

# ---- Make lookup table with Trust codes and LA codes ----
# Make list of Trusts and the LAs they cover
trust_lad <- hosp_eng %>% 
  select(Name, Trust, Postcode2) %>% 
  left_join(postcodes, by = "Postcode2") %>% 
  select(Trust, LAD19CD = ladcd) %>% 
  distinct()

# Get Trust codes
trust_lad <- trust_lad %>% 
  mutate(Trust = str_to_upper(Trust)) %>% 

  # manual lookup of some Trust names that otherwise wouldn't match
  mutate(Trust = case_when(
    Trust == "LIVERPOOL HEART AND CHEST NHS FOUNDATION TRUST" ~ "LIVERPOOL HEART AND CHEST HOSPITAL NHS FOUNDATION TRUST",
    # Trust == "LUTON AND DUNSTABLE UNIVERSITY HOSPITAL NHS FOUNDATION TRUST" ~ "",  # no replacement for this one
    Trust == "PORTSMOUTH HOSPITALS NHS TRUST" ~ "PORTSMOUTH HOSPITALS UNIVERSITY NATIONAL HEALTH SERVICE TRUST",
    Trust == "ROBERT JONES AND AGNES HUNT ORTHOPAEDIC AND DISTRICT HOSPITAL NHS TRUST" ~ "THE ROBERT JONES AND AGNES HUNT ORTHOPAEDIC HOSPITAL NHS FOUNDATION TRUST",
    Trust == "ROYAL BROMPTON AND HAREFIELD NHS FOUNDATION TRUST" ~ "ROYAL BROMPTON & HAREFIELD NHS FOUNDATION TRUST",
    Trust == "ROYAL SURREY NHS FOUNDATION TRUST" ~ "ROYAL SURREY COUNTY HOSPITAL NHS FOUNDATION TRUST",
    Trust == "SHREWSBURY AND TELFORD HOSPITAL NHS TRUST" ~ "THE SHREWSBURY AND TELFORD HOSPITAL NHS TRUST",
    Trust == "ST HELENS AND KNOWSLEY HOSPITALS NHS TRUST" ~ "ST HELENS AND KNOWSLEY TEACHING HOSPITALS NHS TRUST",
    Trust == "TAMESIDE HOSPITAL NHS FOUNDATION TRUST" ~ "TAMESIDE AND GLOSSOP INTEGRATED CARE NHS FOUNDATION TRUST",
    Trust == "THE QUEEN ELIZABETH HOSPITAL, KING'S LYNN. NHS FOUNDATION TRUST" ~ "THE QUEEN ELIZABETH HOSPITAL, KING'S LYNN, NHS FOUNDATION TRUST",
    Trust == "UNIVERSITY HOSPITAL OF DERBY AND BURTON NHS FOUNDATION TRUST" ~ "UNIVERSITY HOSPITALS OF DERBY AND BURTON NHS FOUNDATION TRUST",
    Trust == "UNIVERSITY HOSPITALS OF NORTH MIDLANDS" ~ "UNIVERSITY HOSPITALS OF NORTH MIDLANDS NHS TRUST",
    Trust == "WARRINGTON AND HALTON HOSPITALS NHS FOUNDATION TRUST" ~ "WARRINGTON AND HALTON TEACHING HOSPITALS NHS FOUNDATION TRUST",
    TRUE ~ Trust
  )) %>% 
  
  left_join(trust_names, #%>% filter(Status == "Open"), 
            by = c("Trust" = "Name")) %>% 
  
  distinct()

write_csv(trust_lad, "data/lookup-nhs-trust-to-la.csv")
