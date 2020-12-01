##
## Create lookup table between NHSE Trusts and Local Authorities
##
library(tidyverse)
library(httr)

# ---- Load data ----
# List of NHS hospitals in England
hosp_eng <- read_csv("data/raw/health/england-hospitals.csv")

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

# Keep list of Trusts and the LAs they cover
trust_lad <- hosp_eng %>% 
  select(Name, Trust, Postcode2) %>% 
  left_join(postcodes, by = "Postcode2") %>% 
  select(Trust, LAD19CD = ladcd) %>% 
  distinct()

write_csv(trust_lad, "data/lookup-nhs-trust-to-la.csv")
