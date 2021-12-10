# Load packages
library(tidyverse)
library(geographr)
library(sf)
library(readODS)

source("R/utils.R") # for download_file()
source("R/capacity/health-inequalities/england/trust_types/trust_types.R") # run trust types code to create open_trust_types.feather

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

# NHS Trusts table in geographr package -----

# Load in open trusts table created in trust_types.R
open_trusts <- arrow::read_feather("R/capacity/health-inequalities/england/trust_types/open_trust_types.feather")

# Check the matching of survey data & trust table in geographr package --------

open_trusts |>
  anti_join(avg_survey_scores, by = c("trust_code"))
# all matched

avg_survey_scores |>
  anti_join(open_trusts, by = c("trust_code"))
# all matched

# Join data survey data to open trust data --------

combined_survey_data <- open_trusts |>
  left_join(inpatient_survey, by = c("trust_code" = "trustcode")) |>
  left_join(mental_health_survey, by = c("trust_code" = "trustcode")) |>
  left_join(outpatient_ae, by = c("trust_code" = "trustcode")) |>
  left_join(outpatient_minor_inj, by = c("trust_code" = "trustcode"))

# Not every trust will provide all the service types (e.g. a&e service, mental health service etc) so won't have surveys for all (see next section for check)

# Checking the types of trusts that have missing data for each category --------

# Downloading CQC rating data as has information on what is the primary type of care trust provides
# This is used to check against the trusts with no survey data for each survey type

# Check missing data for each category
combined_survey_data |>
  select(trust_code, `Provider Primary Inspection Category`, starts_with("num_respon")) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(across(where(is.numeric), ~ sum(!is.na(.x))), count = n())

# Ambulance services primary providers haven't completed any of the surveys since they don't provide any of the services in the 4 surveys currently including.

# Combining the survey scores ---------

# Have a think/research if way to combine the variability (i.e the upper and lower confidence intervals) for the scores - or maybe this is overkill?
# Will combine the means for just now (consider if medians would be more appropriate)
avg_survey_scores <- combined_survey_data |>
  select(trust_code, `Provider Primary Inspection Category`, meaninp, meanmh, meanae, meanmin) |>
  mutate(avg_survey_score = rowSums(across(where(is.numeric)), na.rm = T) / rowSums(!is.na(across(where(is.numeric)))))

avg_survey_scores |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(prop_no_survey = sum(is.na(avg_survey_score)) / n(), count = n())
# some trusts have no survey score (consider after join on MSOA lookup)

# Trust to MSOA (then to LA) lookup ----

# Trust to MSOA table only has data for acute trusts
avg_survey_scores |>
  left_join(lookup_trust_msoa) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LSOA and proportion to per capita level
avg_survey_scores_full <- avg_survey_scores |>
  inner_join(lookup_trust_msoa, by = "trust_code")

# Check is any of acute trusts don't have a survey score
avg_survey_scores_full |>
  distinct(trust_code, `Provider Primary Inspection Category`, avg_survey_score) |>
  group_by(`Provider Primary Inspection Category`) |>
  summarise(prop_no_survey = sum(is.na(avg_survey_score)) / n(), count = n())
# 20% of specialists trusts (i.e. 3 trusts) have no survey score - affected 2.5k MSOAs they map into


# Re-proportion for the trusts with no data
# TO DO: get this checked
avg_survey_scores_reprop <- avg_survey_scores_full |>
  filter(!is.na(avg_survey_score)) |>
  group_by(msoa_code) |>
  mutate(denominator_msoa = sum(proportion)) |>
  mutate(reweighted_proportion = proportion / denominator_msoa)

avg_survey_scores_msoa <- avg_survey_scores_reprop |>
  mutate(avg_survey_score_prop = avg_survey_score * reweighted_proportion) |>
  group_by(msoa_code) |>
  summarise(avg_score_msoa = sum(avg_survey_score_prop))

msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)

rating_lad <- avg_survey_scores_msoa |>
  left_join(lookup_msoa_lad) |>
  left_join(msoa_pop) |>
  calculate_extent(
    var = avg_score_msoa,
    higher_level_geography = lad_code,
    population = total_population
  ) 

# TO DO: Think about the combining of the scores (i.e. averaging of averages) as CI is also available.
