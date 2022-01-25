# Load packages
library(tidyverse)
library(geographr)
library(sf)
library(readODS)

source("R/utils.R") # for download_file(), survey_data_download() & updating_trusts_for_survey_data()

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


#' Survey data download and filtering for CQC surveys (England health capacity indicator)
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
    select(trust_code = trustcode, trustname, n_tpat, contains(q_col_name)) |>
    rename(!!new_num_respon_name := n_tpat) |>
    rename_with(str_replace_all, pattern = q_col_name, replacement = category)
  
  return(subset_data)
}

#' Updating Trusts in the CQC survey data (England health capacity indicator)
#'
#' @param data survey data created from survey_data_download() function
#' @param response_column column name of the number of responses
#' @param mean_column column name of the mean survey value
#' @param open_trusts_data dataset with open trusts saved R/capacity/health-inequalities/england/trust_calculations/open_trust_types.feather
#' @param trust_changes_data dataset with trust changes saved R/capacity/health-inequalities/england/trust_calculations/trust_changes.feather

updating_trusts_for_survey_data <- function(data, response_column, mean_column, open_trusts_data, trust_changes_data) {
  
  data_selected <- data |>
    select(trust_code, num_respon = {{ response_column }}, mean = {{ mean_column }})
  
  old_new_lookup <- data_selected |>
    anti_join(open_trusts_data) |>
    rename(old_code = trust_code) |>
    inner_join(trust_changes_data, by = "old_code") |>
    group_by(new_code) |>
    mutate(new_code_count = n()) |>
    ungroup() |>
    group_by(old_code) |>
    mutate(old_code_count = n()) |>
    ungroup()
  
  
  if (max(old_new_lookup$old_code_count) > 1) {
    
    stop("Trust has been split to two different new Trusts")
    
  } else {
    new_trusts <- old_new_lookup |>
      group_by(new_code) |>
      mutate(weight = num_respon / sum(num_respon), weighted_mean = weight * mean) |>
      group_by(new_code) |>
      summarise(mean = sum(weighted_mean), num_respon = sum(num_respon)) |>
      select(trust_code = new_code, num_respon, mean)
    
    data_updated <- data_selected |>
      semi_join(open_trusts) |>
      bind_rows(new_trusts)
    
    # Average any duplicates Trust data caused by Trust changes
    data_updated_combined <- data_updated |>
      group_by(trust_code) |>
      mutate(weight = num_respon / sum(num_respon), weighted_mean = weight * mean) |>
      group_by(trust_code) |>
      summarise(mean = sum(weighted_mean), num_respon = sum(num_respon)) |>
      select(trust_code, num_respon, mean) |>
      rename(!!response_column := num_respon, !!mean_column := mean) 
    
    return(data_updated_combined)
  }
}

# Download survey datasets ----

inpatient_survey <- survey_data_download("https://www.cqc.org.uk/sites/default/files/20210917%20IP20%20Trust-level%20benchmark%20ODS%20V1.ods", 45, "Trust_Scores", "q", "inp")
mental_health_survey <- survey_data_download("https://www.cqc.org.uk/sites/default/files/20201124_cmh20_trustresults.ods", 35, "CMH20_Trust_Scores", "q", "mh")

# For the outpatient survey there was 2 separate surveys:
# Type 1 services include A&E departments, and may also be known as casualty or emergency departments.
# Type 3 services include urgent treatment centres, and may also be known as minor injury units. The survey only includes services directly run by an acute NHS trust.
outpatient_ae <- survey_data_download("https://www.cqc.org.uk/sites/default/files/20210915_uec20_type1_benchmark-data.ods", 47, "UEC20_Trust_Scores", "q", "ae")
outpatient_minor_inj <- survey_data_download("https://www.cqc.org.uk/sites/default/files/20210915_uec20_type3_benchmark-data.ods", 39, "UEC20_Trust_Scores", "t", "min")

# Load in open trusts table created in trust_types.R
open_trusts <- read_rds("data/open_trust_types.rds")

# Check if any trusts in the surveys not in open_trusts dataset in case any changed trust codes
# Surveys at different years so may have different stages of trust changes
inpatient_survey |>
  anti_join(open_trusts)

mental_health_survey |>
  anti_join(open_trusts)

outpatient_ae |>
  anti_join(open_trusts)

outpatient_minor_inj |>
  anti_join(open_trusts)
# No updated needed

trust_changes <- read_rds("data/trust_changes.rds")

# Update trust codes using updating_trusts_for_survey_data() function
inpatient_survey_updated <- updating_trusts_for_survey_data(inpatient_survey, "num_respon_inp", "meaninp", open_trusts, trust_changes)
mental_health_survey_updated <- updating_trusts_for_survey_data(mental_health_survey, "num_respon_mh", "meanmh", open_trusts, trust_changes)
outpatient_ae_updated <- updating_trusts_for_survey_data(outpatient_ae, "num_respon_ae", "meanae", open_trusts, trust_changes)

outpatient_minor_inj_updated <- outpatient_minor_inj |>
  select(trust_code, num_respon_min, meanmin)

# Join data survey data to open trust data --------
combined_survey_data <- open_trusts |>
  left_join(inpatient_survey_updated, by = "trust_code") |>
  left_join(mental_health_survey_updated, by = "trust_code") |>
  left_join(outpatient_ae_updated, by = "trust_code") |>
  left_join(outpatient_minor_inj_updated, by = "trust_code")

# Not every trust will provide all the service types (e.g. a&e service, mental health service etc) so won't have surveys for all (see next section for check)
combined_survey_data |>
  select(trust_code, primary_category, starts_with("num_respon")) |>
  group_by(primary_category) |>
  summarise(across(where(is.numeric), ~ sum(!is.na(.x))), count = n())
# Ambulance services primary providers haven't completed any of the surveys since they don't provide any of the services in the 4 surveys currently including.
# May be similar for others

# Combining the survey scores ---------

# Have a think/research if way to combine the variability (i.e the upper and lower confidence intervals) for the scores
response_columns <- str_subset(colnames(combined_survey_data), "num_respon")

# Come up with a better/more reproducible way of writing this query
avg_survey_scores <- combined_survey_data |>
  rowwise() %>%
  mutate(
    total_responders = sum(
      c_across(starts_with("num_respon")) ,
      na.rm = T
    )
  ) |>
  mutate_at(vars(contains("num_respon")), ~ . / total_responders) |>
  mutate(avg_survey_score = sum(c(num_respon_inp * meaninp, num_respon_mh * meanmh, num_respon_ae * meanae, num_respon_min * meanmin), na.rm = T)) |>
  mutate(avg_survey_score = ifelse(is.na(meaninp) & is.na(meanmh) & is.na(meanae) & is.na(meanmin), NA, avg_survey_score)) |>
  select(trust_code, primary_category, total_responders, avg_survey_score)

avg_survey_scores |>
  group_by(primary_category) |>
  summarise(prop_no_survey = sum(is.na(avg_survey_score)) / n(), count = n())
# some Trusts have no survey score, likely due to the service provide (consider after join on MSOA lookup)

# Trust to MSOA (then to LA) lookup ----

# Trust to LAD table only has data for acute trusts
avg_survey_scores |>
  left_join(open_trusts) |>
  left_join(lookup_trust_msoa) |>
  group_by(primary_category) |>
  summarise(count = n(), prop_with_lookup = sum(!is.na(msoa_code)) / n())

# Current approach is to drop information on non-acute trusts since can't proportion these to MSOA
# For the acute trusts proportion these to MSOA and then aggregate to LAD
avg_survey_scores_joined <- avg_survey_scores |>
  left_join(open_trusts) |>
  inner_join(lookup_trust_msoa)

# Check is any of acute trusts don't have a survey score
avg_survey_scores_joined |>
  distinct(trust_code, primary_category, avg_survey_score) |>
  group_by(primary_category) |>
  summarise(prop_no_survey = sum(is.na(avg_survey_score)) / n(), count = n())

avg_survey_scores_joined |>
  filter(is.na(avg_survey_score)) |>
  distinct(trust_code)

# Since Trusts with no survey score re-proportion to deal with missing Trust data
avg_survey_scores_msoa <- avg_survey_scores_joined |>
  filter(!is.na(avg_survey_score)) |>
  group_by(msoa_code) |>
  mutate(reweighted_proportion = proportion / sum(proportion)) |>
  mutate(weighted_survey_score = reweighted_proportion * avg_survey_score) |>
  summarise(weighted_score = sum(weighted_survey_score))

# Distributions
summary(avg_survey_scores_msoa$weighted_score)
summary(inpatient_survey_updated$meaninp)
summary(mental_health_survey_updated$meanmh)
summary(outpatient_ae_updated$meanae)
summary(outpatient_minor_inj_updated$meanmin)

# Aggregate from MSOA to LA ----

msoa_pop <- geographr::population_msoa |>
  select(msoa_code, total_population)

avg_survey_lad <- avg_survey_scores_msoa |>
  left_join(lookup_msoa_lad) |>
  left_join(msoa_pop) |>
  calculate_extent(
    var = weighted_score,
    higher_level_geography = lad_code,
    population = total_population
  )

avg_survey_lad |>
  group_by(extent) |>
  summarise(count = n() / nrow(avg_survey_lad)) |>
  print(n = Inf)
# 60% : extent = 0
# 5%: extent = 1

# Save ----
avg_survey_lad |>
  write_rds("data/capacity/health-inequalities/england/cqc-surveys.rds")
