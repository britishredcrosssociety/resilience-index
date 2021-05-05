library(readr)
library(dplyr)
library(geographr)

# Source: https://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fpercent&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F2016-01-01T00%3A00%3A00%2FP4Y
health_survey <-
  read_csv("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data")

physical_activity_raw <-
  health_survey %>%
  filter(
    `Scottish Health Survey Indicator` == "Summary activity levels: Meets recommendations",
    Measurement == "Percent",
    Sex == "All",
    DateCode == "2016-2019"
  ) %>%
  select(
    code = FeatureCode,
    activity_levels_met_percent = Value
  )

# Remove Scotland aggregate to leave LAD and HB codes
physical_activity <-
  physical_activity_raw %>%
  filter(code != "S92000003")

# There is a mixture of council areas and health boards. Some Health boards and Local
# Authorities cover the same boundaries.
# Strategy:
# - For missing LAD's, lookup corresponding HB score. This assumes all LAD's within a
#   a HB have the same score.
physical_activity <-
  lookup_hb_lad %>%
  select(ends_with("code")) %>%
  left_join(physical_activity, by = c("lad_code" = "code")) %>%
  rename(activity_levels_met_percent_lad = activity_levels_met_percent) %>%
  left_join(physical_activity, by = c("hb_code" = "code")) %>%
  rename(activity_levels_met_percent_hb = activity_levels_met_percent) %>%
  mutate(
    activity_levels_met_percent = if_else(
      !is.na(activity_levels_met_percent_lad),
      activity_levels_met_percent_lad,
      activity_levels_met_percent_hb
    )
  ) %>%
  select(lad_code, activity_levels_met_percent)

write_rds(physical_activity, "data/vulnerability/health-inequalities/scotland/healthy-lives/physical-activity.rds")