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
inpatient_survey_updated <- updating_trusts_for_survey_data(inpatient_survey, num_respon_inp, meaninp, open_trusts, trust_changes)
mental_health_survey_updated <- updating_trusts_for_survey_data(mental_health_survey, num_respon_mh, meanmh, open_trusts, trust_changes)
outpatient_ae_updated <- updating_trusts_for_survey_data(outpatient_ae, num_respon_ae, meanae, open_trusts, trust_changes)

outpatient_minor_inj_updated <- outpatient_minor_inj |>
  select(trust_code, num_respon = num_respon_min, mean = meanmin)

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
  mutate(total_responders = sum(c(num_respon.x, num_respon.y, num_respon.x.x, num_respon.y.y), na.rm = T)) |>
  mutate_at(vars(contains("num_respon")), ~ . / total_responders) |>
  mutate(avg_survey_score = sum(c(num_respon.x * mean.x, num_respon.y * mean.y, num_respon.x.x * mean.x.x, num_respon.y.y * mean.y.y), na.rm = T)) |>
  mutate(avg_survey_score = ifelse(is.na(mean.x) & is.na(mean.y) & is.na(mean.x.x) & is.na(mean.y.y), NA, avg_survey_score)) |>
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
summary(inpatient_survey_updated$mean)
summary(mental_health_survey_updated$mean)
summary(outpatient_ae_updated$mean)
summary(outpatient_minor_inj_updated$mean)

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
