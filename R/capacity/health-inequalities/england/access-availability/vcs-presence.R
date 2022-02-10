# Note: the output values are for relative comparison only and don't include counts for charities which
# operate all across England or all across UK so shouldn't be used as a figure for total charity count at each UTLA or LTLA.
# And it only includes health/social VCS orgs only charities.

# ---- Load libs ----
library(tidyverse)
library(httr)
library(geographr)
library(sf)

source("R/utils.R")

# ---- Load data ----
# Source: https://register-of-charities.charitycommission.gov.uk/register/full-register-download
# Chairty list
GET(
  "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

charities_list_raw <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity.txt",
      full.names = TRUE
    )
  )


# Charity areas of operation
GET(
  "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity_area_of_operation.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

charities_areas_raw <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity_area_of_operation.txt",
      full.names = TRUE
    )
  )

# Chairty classification
GET(
  "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity_classification.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

charities_classification_raw <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity_classification.txt",
      full.names = TRUE
    )
  )

# ---- Clean data ----
# Charity list
charity_list_cols <-
  charities_list_raw |>
  select(
    organisation_number,
    charity_name,
    charity_registration_status,
    charity_insolvent,
    charity_in_administration
  )

# Keep only registered charities that are not insolvent or in administration
charities_active <-
  charity_list_cols |>
  filter(charity_registration_status == "Registered") |>
  filter(charity_insolvent == FALSE) |>
  filter(charity_in_administration == FALSE) |>
  select(
    organisation_number,
    charity_name
  )

# Charity areas of operation (only keep actives)
charities_areas_active <-
  charities_areas_raw |>
  select(
    organisation_number,
    geographic_area_type,
    geographic_area_description
  ) |>
  inner_join(charities_active, by = "organisation_number")

# Charity classification (only keep actives)
charities_classification_active <-
  charities_classification_raw |>
  select(
    organisation_number,
    classification_type,
    classification_description
  ) |>
  inner_join(charities_active, by = "organisation_number")
 
# ---- Keep health/social VCS orgs only ----
# Remove classifications that are obviously not related
charities_health <-
  charities_classification_active |>
  filter(
    classification_description == "Accommodation/housing" |
      classification_description == "Children/young People" |
      classification_description == "Disability" |
      classification_description == "Economic/community Development/employment" |
      classification_description == "Education/training" |
      classification_description == "Elderly/old People" |
      classification_description == "People With Disabilities" |
      classification_description == "Sponsors Or Undertakes Research" |
      classification_description == "The Advancement Of Health Or Saving Of Lives" |
      classification_description == "The General Public/mankind" |
      classification_description == "The Prevention Or Relief Of Poverty"
  ) |>
  distinct(organisation_number)

# ---- Assign geographies ----
# Assign Local Authorities to the geography_area columns:
#   - for for geographic_area_type == "Country", remove all as presence will
#     be indistinguishable for any UK orgs.
#   - for geographic_area_type == "Region", currently remove all regions (such as 'Throughout England'), as they do not
#     provide any granular detail as they are at the devolved nation except 'Throughout London'.
#   - for for geographic_area_type == NA, remove all as information is needed.
#   - for geographic_area_type == "Local Authority", just keep the
#     corresponding geographic_area_description and then match to the ONS
#     region. Note the areas are UTLA's.

local_eng_health_charities_org_nums <- charities_health |>
  left_join(charities_areas_active , by = "organisation_number") |>
  filter((geographic_area_type == "Region" & geographic_area_description == "Throughout London") |
    geographic_area_type == "Local Authority") |>
  distinct(organisation_number)

local_eng_health_charities <- charities_areas_active |>
  inner_join(local_eng_health_charities_org_nums, by = "organisation_number")

# A charity may work in a specific LAD/Throughout London but also other areas/countries (see example below)
# Due to this not appropriate to take the income for that charity and allocate to that LAD (original plan)
# since will also be split by the other operating areas potentially outside England/UK.

local_eng_health_charities |>
  filter(organisation_number == "207619")

# Instead will just look at number of charities per capita. 

# - Country, Region, & NA -
# Remove country, region, and NA level data
# i.e. keep only Local Authority level data
health_charities_local_auth <- local_eng_health_charities |>
  filter(geographic_area_type == "Local Authority") |>
  group_by(geographic_area_description) |>
  summarise(count_orgs = n())

# - Local Authorities -
# Create lists of UTLA's (2019) to compare against
utla_list_eng <-
  boundaries_counties_ua |>
  filter(str_detect(county_ua_code, "^E")) |>
  mutate(county_ua_name = str_to_lower(county_ua_name)) |>
  arrange(county_ua_name) |>
  pull(county_ua_name)

# Welsh Local Authorities (so can remove)
utla_list_wales <-
  boundaries_counties_ua |>
  filter(str_detect(county_ua_code, "^W")) |>
  mutate(county_ua_name = str_to_lower(county_ua_name)) |>
  arrange(county_ua_name) |>
  pull(county_ua_name)

# Find names not matched in UTLA list
health_charities_local_auth |>
  mutate(geographic_area_description = str_to_lower(geographic_area_description)) |>
  select(geographic_area_description) |>
  filter(!(geographic_area_description %in% utla_list_eng)) |>
  distinct() |>
  print(n = Inf)

# Match UTLA names and keep only English UTLA's
health_charities_local_auth_updated <-
  health_charities_local_auth |>
  mutate(
    geographic_area_description = str_to_lower(geographic_area_description)
  ) |>
  mutate(
    geographic_area_description = case_when(
      geographic_area_description == "bristol city" ~ "bristol, city of",
      geographic_area_description == "birmingham city" ~ "birmingham",
      geographic_area_description == "plymouth city" ~ "plymouth",
      geographic_area_description == "peterborough city" ~ "peterborough",
      geographic_area_description == "leeds city" ~ "leeds",
      geographic_area_description == "leicester city" ~ "leicester",
      geographic_area_description == "city of westminster" ~ "westminster",
      geographic_area_description == "herefordshire" ~ "herefordshire, county of",
      geographic_area_description == "liverpool city" ~ "liverpool",
      geographic_area_description == "nottingham city" ~ "nottingham",
      geographic_area_description == "city of york" ~ "york",
      geographic_area_description == "cheshire west & chester" ~ "cheshire west and chester",
      geographic_area_description == "sheffield city" ~ "sheffield",
      geographic_area_description == "salford city" ~ "salford",
      geographic_area_description == "durham" ~ "county durham",
      geographic_area_description == "city of wakefield" ~ "wakefield",
      geographic_area_description == "derby city" ~ "derby",
      geographic_area_description == "manchester city" ~ "manchester",
      geographic_area_description == "southampton city" ~ "southampton",
      geographic_area_description == "bradford city" ~ "bradford",
      geographic_area_description == "coventry city" ~ "coventry",
      geographic_area_description == "telford & wrekin" ~ "telford and wrekin",
      geographic_area_description == "st helens" ~ "st. helens",
      geographic_area_description == "kingston upon hull city" ~ "kingston upon hull, city of",
      geographic_area_description == "newcastle upon tyne city" ~ "newcastle upon tyne",
      geographic_area_description == "poole" ~ "bournemouth, christchurch and poole",
      geographic_area_description == "bournemouth" ~ "bournemouth, christchurch and poole",
      geographic_area_description == "stoke-on-trent city" ~ "stoke-on-trent",
      geographic_area_description == "portsmouth city" ~ "portsmouth",
      geographic_area_description == "city of swansea" ~ "swansea",
      geographic_area_description == "newport city" ~ "newport",
      geographic_area_description == "rhondda cynon taff" ~ "rhondda cynon taf",
      TRUE ~ geographic_area_description
    )
  ) |>
  filter(!geographic_area_description %in% utla_list_wales) |>
  rename(county_ua_name = geographic_area_description)

# Check
health_charities_local_auth_updated |>
  filter(!county_ua_name %in% utla_list_eng) |>
  print(n = Inf)


# Split down to LTLA from UTLA
# Assumption: if present in UTLA present across all LTLAs within UTLA
lookup_ltla_ltla <-
  lookup_counties_ua_lad |>
  filter(str_detect(county_ua_code, "^E")) |>
  mutate(county_ua_name = str_to_lower(county_ua_name))

health_charities_ltla_count <- health_charities_local_auth_updated |>
  left_join(lookup_ltla_ltla, by = "county_ua_name") |>
  group_by(lad_code) |>
  summarise(count_orgs = sum(count_orgs))

# Regional London charities -----
health_charities_london <- local_eng_health_charities |>
  filter(geographic_area_type == "Region", geographic_area_description == "Throughout London") |>
  group_by(geographic_area_description) |>
  summarise(count_orgs = n())

london_ltla <- lookup_lad_region |>
  filter(region_name == "London") |>
  distinct(lad_code) |>
  mutate(count_orgs = health_charities_london$count_orgs)

# Combining London regional and all UTLA level incomes ----
ltla_combined <- health_charities_ltla_count |>
  bind_rows(london_ltla) |>
  group_by(lad_code) |>
  summarise(count_orgs = sum(count_orgs))

# Bring in LTLA population figures
ltla_pop <- population_lad |>
  select(lad_code, total_population) |>
  filter(str_detect(lad_code, "^E"))

# Check lad codes are 2021 ----
if(
  anti_join(
    ltla_combined,
    lookup_lad_over_time,
    by = c("lad_code" = "LAD21CD")
  ) |>
  pull(lad_code) |>
  length() != 0
) {
  stop("Lad codes need changing to 2021 - check if 2019 or 2020")
}

if(
  anti_join(
    ltla_pop,
    lookup_lad_over_time,
    by = c("lad_code" = "LAD21CD")
  ) |>
  pull(lad_code) |>
  length() != 0
) {
  stop("Lad codes need changing to 2021 - check if 2019 or 2020")
}

# Update indicator from 2019 to 2020 and population from 2020 to 2021
# Aggregation only of LADs between 2019 to 2021
ltla_combined_update <- ltla_combined |>
  left_join(lookup_lad_over_time, by = c("lad_code" = "LAD19CD")) |>
  group_by(LAD21CD) |>
  summarise(across(where(is.numeric), sum))

ltla_pop_update <- ltla_pop |>
  left_join(lookup_lad_over_time, by = c("lad_code" = "LAD20CD")) |>
  group_by(LAD21CD) |>
  summarise(across(where(is.numeric), sum))

# Normalise by population of LTLA
ltla_vcs_presence <- ltla_combined_update |>
  left_join(ltla_pop_update, by = "LAD21CD") |>
  rename(ltla_pop = total_population) |>
  mutate(vcs_presence = count_orgs / ltla_pop) |>
  select(lad_code = LAD21CD, vcs_presence)

# Save ----
ltla_vcs_presence |>
  write_rds("data/capacity/health-inequalities/england/access-availability/vsc-presence.rds")
