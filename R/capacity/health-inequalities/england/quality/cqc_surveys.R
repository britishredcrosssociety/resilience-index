# Load libs
library(tidyverse)
library(geographr)
library(sf)

source("R/utils.R") # for download_file()

# Info ----

# Number of surveys on CQC site: https://www.cqc.org.uk/publications/surveys/surveys
# Some are for 2020: 1. adult inpatient, 2. urgent & emergency care, 3. COVID inpatient, 4. Community mental health
# Others further in past (and so not included): 1. Maternity (2019), 2, Children & young people (2018), 3. Ambulance (2013/14), 4. Outpatient (2011)
# The COVID inpatient survey did not split by trust so have not included.

# For each of these surveys there is data at trust level (the scores are calculated by comparing trusts against each other, rather than absolute performance).
# Within each survey there are a variety of specific questions about performance but there is one ‘overall’ question where rate experience (on scale 0 to 10) so only extract this answer.
# Need to examine each survey in turn to determine which question number it is
# (may need to check in future refreshes the question numbers still align, as in data only refers to the question number, not what is being asked).

# Each survey dataset has a sheet (roughly named) 'Trust_Respondents' with the demographic info on the respondents to the survey and 'Trust_Scores' with the summary of scores.
# The survey questions can be found linked in the 'Further information' section of the front page of the datasets (then 'Survey Materials' section of linked page)

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
    rename_with(str_replace_all, pattern = q_col_name, replacement =  category)
    
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

# NHS TRUST table in geographr package -----

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

# Join data --------

open_trusts |>
  left_join(inpatient_survey, by = c("trust_code" = "trustcode")) |>
  left_join(mental_health_survey, by = c("trust_code" = "trustcode")) |>
  left_join(outpatient_ae, by = c("trust_code" = "trustcode")) |>
  left_join(outpatient_minor_inj, by = c("trust_code" = "trustcode"))
  
  
  
  

