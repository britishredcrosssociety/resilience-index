library(httr)
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(geographr)

# Source: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/10743annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2018andapril2019
GET(
  "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/10743annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2018andapril2019/20182019livingwagebyworkgeography.zip",
  write_disk(tf <- tempfile(".zip"))
)

unzip(tf, exdir = tempdir())

low_pay_raw <-
  read_excel(
    list.files(tempdir(),
      pattern = "Work Geography LW Table 7.1a   lwfmgx 2019.xls",
      full.names = TRUE
    ),
    sheet = "All",
    range = "B394:D425",
    col_names = c("lad_code", "count_jobs", "low_pay_percent")
  )

# Match 2019 LAD codes
# https://www.opendata.nhs.scot/dataset/geography-codes-and-labels/resource/967937c4-8d67-4f39-974f-fd58c4acfda5
# Look at the 'CADateArchived' column to view changes
low_pay_raw <-
  low_pay_raw %>%
  mutate(
    lad_code = case_when(
      lad_code == "S12000015" ~ "S12000047",
      lad_code == "S12000024" ~ "S12000048",
      TRUE ~ lad_code
    )
  )

low_pay <-
  low_pay_raw %>%
  select(-count_jobs) %>%
  mutate(low_pay_percent = str_replace_all(low_pay_percent, "x", NA_character_)) %>%
  mutate(low_pay_percent = as.double(low_pay_percent))

# Impute missing data
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/methodologies/methodsusedtodevelopthehealthindexforengland2015to2018
# Reason for missing: "Estimates with a CV greater than 20% are suppressed from
#                      publication on quality grounds, along with those for which
#                      there is a risk of disclosure of individual employees or employers."
# Strategy: replace with mean for the LAD's HB's

# Only one LA is not in a 1:1 relationship with a HB so NA's carry forward
hb_scores <-
  low_pay %>%
  left_join(
    lookup_hb_lad %>% select(ends_with("code")),
    by = "lad_code"
  ) %>%
  group_by(hb_code) %>%
  mutate(hb_score = mean(low_pay_percent, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    hb_score = if_else(
      is.nan(hb_score),
      NA_real_,
      hb_score
    )
  )

low_pay <-
  hb_scores %>%
  mutate(
    low_pay_percent = case_when(
      is.na(low_pay_percent) & !is.na(hb_score) ~ hb_score,
      is.na(low_pay_percent) & is.na(hb_score) ~ mean(low_pay_percent, na.rm = TRUE),
      TRUE ~ low_pay_percent,
    )
  ) %>%
  select(lad_code, low_pay_percent)

write_rds(low_pay, "data/vulnerability/health-inequalities/scotland/healthy-lives/low-pay.rds")