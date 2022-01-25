# This script outputs an NHS Trust to LAD population catchment table based on Public Health England data 
# It uses output from the scripes 'trust_types.R' and 'trust_changes.R' 

# Load libraries 
library(geographr)
library(sf)
library(tidyverse)
library(httr)
library(readxl)


# This is the data the geographr::lookup_trust_msoa is on but for this need extra columns
query_url <- "https://app.box.com/index.php?rm=box_download_shared_file&shared_name=qh8gzpzeo1firv1ezfxx2e6c4tgtrudl&file_id=f_877983829510"

# Make GET request
GET(
  query_url,
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

# All admissions
catchment_populations <- read_excel(tf, sheet = "All Admissions")

# Keep only 2019 data
catchment_proportions <-
  catchment_populations %>%
  filter(CatchmentYear == 2019) %>%
  select(
    msoa_code = msoa,
    trust_code = TrustCode,
    patients,
    total_patients,
    proportion,
    msoa_total_catchment,
    trust_total_catchment
  )


# Although there are Trust catchments in the dataset, these are meant to be the sum of the msoa_catchment across each trusts but figures do not align. 
catchment_proportions |> 
  group_by(trust_code) |>
  mutate(calculated_trust_total_catchment = sum(msoa_total_catchment))

# Also note the total of the patients column for an MSOA is not the same as the total_patients column, is it that some patients for a given MSOA which can't be attributed to a Trust?
# This means that the proportion column does not sum to 100% when grouped by Trust
catchment_proportions |> 
  select(msoa_code, patients, total_patients) |>
  group_by(msoa_code) |>
  mutate(calculated_total_patients = sum(patients))

# Some trust codes may need updated so aligns with 'open_trust_types.feather' ----
open_trusts <- read_rds("data/open_trust_types.rds")
trust_changes <- read_rds("data/trust_changes.rds")

trusts_to_update <- catchment_proportions |>
  anti_join(open_trusts) |>
  distinct(trust_code) |>
  pull()

updated_trusts <- catchment_proportions |>
  filter(trust_code %in% trusts_to_update) |>
  inner_join(trust_changes, by = c("trust_code" = "old_code")) |>
  select(-c(trust_code, date)) |>
  rename(trust_code = new_code) |>
  relocate(trust_code, .after = msoa_code)

catchment_proportions_updated <- catchment_proportions |>
  filter(!trust_code %in% trusts_to_update) |>
  bind_rows(updated_trusts)|>
  group_by(msoa_code, trust_code) 

# Check through updating trust codes may be duplicated combinations of msoa_code and trust_code 
# so need to combine rows to only distinct combination of msoa_code and trust_code 
# the total_patients column won't change because this is the total number of patients across all trusts for a given msoa 
# and since it is the trust code changing (not msoa) will stay the same. 
catchment_proportions_updated_grouped <- catchment_proportions_updated |>
  group_by(msoa_code, trust_code, total_patients) |>
  summarise(across(where(is.numeric), sum)) |>
  ungroup() |>
  relocate(total_patients, .after = patients)
  
# Check in open trusts and not in PHE data (although know PHE data does not include non-acute services e.g. ambulance, community, mental health etc)
open_trusts |> 
  anti_join(catchment_proportions) |>
  arrange(primary_category) |> 
  print(n = Inf)

# Following similar logic to Victims of Maths using this data to attribute COVID deaths at Trust level to LAs
# https://github.com/VictimOfMaths/COVID-19/blob/5803fa341b3af99e7a8d8b4eda708e6a3d2ab052/Heatmaps/COVIDAdmissionsLTLAPhasePlot.R#L73 
lookup_trust_msoa_full <- catchment_proportions |>
  select(msoa_code, trust_code, proportion, msoa_catchment_for_trust = msoa_total_catchment) |>
  group_by(trust_code) |>
  mutate(trust_catchment = sum(msoa_catchment_for_trust)) |>
  ungroup() |>
  rename(msoa_prop_by_trust = proportion) |>
  mutate(trust_prop_by_msoa = msoa_catchment_for_trust/trust_catchment)

lookup_trust_lad <- lookup_trust_msoa_full |>
  left_join(lookup_msoa_lad) |>
  group_by(trust_code, lad_code, lad_name) |>
  summarise(lad_catchment_for_trust = sum(msoa_catchment_for_trust)) |>
  ungroup() |>
  group_by(trust_code) |>
  mutate(trust_catchment = sum(lad_catchment_for_trust)) |> 
  mutate(trust_prop_by_lad = lad_catchment_for_trust/trust_catchment) |>
  ungroup() |>
  group_by(lad_code) |>
  mutate(lad_pop = sum(lad_catchment_for_trust)) |> 
  mutate(lad_prop_by_trust = lad_catchment_for_trust/lad_pop) |>
  ungroup() |>
  select(lad_code, lad_name, trust_code, trust_prop_by_lad, lad_prop_by_trust)

write_rds(lookup_trust_lad, "data/lookup_trust_lad.rds")
