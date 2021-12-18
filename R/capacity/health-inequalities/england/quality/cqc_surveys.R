# Load packages
library(tidyverse)
library(geographr)
library(sf)
library(readODS)

source("R/utils.R") # for download_file()

# Info ----

# Number of surveys on CQC site: https://www.cqc.org.uk/publications/surveys/surveys
# Some are for 2020: 1. adult inpatient, 2. urgent & emergency care, 3. COVID inpatient, 4. Community mental health
# As at 11/21 others further in past (and so not included): 1. Maternity (2019), 2, Children & young people (2018), 3. Ambulance (2013/14), 4. Outpatient (2011)
# The COVID inpatient survey did not split by trust so have not included.

# IMPORTANT NOTE: Documentation states some of the currently out of date surveys will become available for 2020 at end of 2021 so might able to be added to the RI https://www.cqc.org.uk/sites/default/files/20211020%20Website%20forward%20programme%20v89.odt

# For each of these surveys there is data at trust level (the scores are calculated by comparing trusts against each other, rather than absolute performance).
# Within each survey there are a variety of specific questions about performance but there is one ‘overall’ question where rate experience (on scale 0 to 10) so only extract this answer.
# Need to examine each survey in turn to determine which question number it is
# (may need to check in future refreshes the question numbers still align, as in data only refers to the question number, not what is being asked).

# Each survey dataset has a sheet (roughly named) 'Trust_Respondents' with the demographic info on the respondents to the survey and 'Trust_Scores' with the summary of scores.
# The survey questions can be found linked in the 'Further information' section of the front page of the datasets (then 'Survey Materials' section of linked page)

# Notes on the trust level scores:
# Weights were calculated to adjust for any variation between trusts that resulted from differences in the age, sex and route of admission groupings of respondents.
# The reason for weighting the data was that respondents may answer questions differently, depending on certain characteristics



# Function ----

#' Survey data download and filtering
#'
#' @param url url to data download (assumes ODS file)
#' @param question_num Which question in the survey is the ‘overall’ question
#' @param question_coding Which encoding used in the sheet (for outpatient_minor_inj seems to be 't1', 't2' etc. rather than 'q1' etc. which is what is meant to be from the key)
#' @param category What survey is it (mental health etc.) so can have in column names


survey_data_download <- function(url, question_num, sheet_name, question_coding, category) {
  tf <- download_file(url, "ods")

  raw <-
    read_ods(
      tf,
      sheet = sheet_name,
    )

  q_col_name <- paste0(question_coding, question_num)
  new_num_respon_name <- paste0("num_respon_", category)

  subset_data <- raw |>
    rename_with(tolower) |>
    select(`trustcode`, `trustname`, `n_tpat`, contains(q_col_name)) |>
    rename(!!new_num_respon_name := `n_tpat`) |>
    rename_with(str_replace_all, pattern = q_col_name, replacement = category)

  return(subset_data)
}

# Download survey datasets ----

inpatient_survey <- survey_data_download("https://www.cqc.org.uk/sites/default/files/20210917%20IP20%20Trust-level%20benchmark%20ODS%20V1.ods", 45, "Trust_Scores", "q", "inp")
mental_health_survey <- survey_data_download("https://www.cqc.org.uk/sites/default/files/20201124_cmh20_trustresults.ods", 35, "CMH20_Trust_Scores", "q", "mh")

# For the outpatient survey there was 2 separate surveys:
# Type 1 services include A&E departments, and may also be known as casualty or emergency departments.
# Type 3 services include urgent treatment centres, and may also be known as minor injury units. The survey only includes services directly run by an acute NHS trust.
outpatient_ae <- survey_data_download("https://www.cqc.org.uk/sites/default/files/20210915_uec20_type1_benchmark-data.ods", 47, "UEC20_Trust_Scores", "q", "ae")
outpatient_minor_inj <- survey_data_download("https://www.cqc.org.uk/sites/default/files/20210915_uec20_type3_benchmark-data.ods", 39, "UEC20_Trust_Scores", "t", "min")

# Since not all Trusts provide all the services (so for different surveys will have different missing Trusts)
# Complete for one of the surveys as starter
ae_survey <- outpatient_ae |>
  select(trust_code = trustcode, num_respon_ae, meanae)


# NHS Trusts table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")

# Check the matching of survey data & trust table in geographr package 
open_trusts |>
  anti_join(ae_survey, by = c("trust_code")) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n())
# 90 missing - community, mental health, ambulance and specialist trusts. These may not provide A&E services. 

ae_survey |>
  anti_join(open_trusts, by = c("trust_code")) |>
  left_join(geographr::points_nhs_trusts, by = c("trust_code" = "nhs_trust_code"))
# 3 missing but are all closed


# Trust to MSOA (then to LA) lookup ----

# Trust to MSOA table only has data for acute trusts
ae_survey |>
  left_join(open_trusts) |>
  left_join(lookup_trust_msoa) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LAD
ae_survey_joined <- ae_survey |>
  left_join(open_trusts) |>
  inner_join(lookup_trust_msoa)

# Check is any of acute trusts don't have a survey score
ae_survey_joined |>
  distinct(trust_code, `Provider Primary Inspection Category`, meanae) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(prop_no_survey = sum(is.na(meanae)) / n(), count = n())

ae_survey_msoa <- ae_survey_joined |>
  mutate(meanae_weighted = proportion * meanae) |>
  group_by(msoa_code) |>
  summarise(weighted_score = sum(meanae_weighted))

# Distributions
summary(ae_survey_msoa$weighted_score)
summary(ae_survey$meanae)

# Aggregate from MSOA to LA ----

msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)

ae_survey_lad <- ae_survey_msoa |>
  left_join(lookup_msoa_lad) |>
  left_join(msoa_pop) |>
  calculate_extent(
    var = weighted_score,
    higher_level_geography = lad_code,
    population = total_population
  )

ae_survey_lad |>
  group_by(extent) |>
  summarise(count = n()/nrow(ae_survey_lad)) |>
  print(n = Inf)
# 52% : extent = 0
# 3%: extent = 1

# TO DO: Get this checked, repeat for other surveys?
