# ---- Load libraries ----
library(tidyverse)
library(readxl)
library(httr)
library(geographr)
library(sf)

# ---- Load data ----
source("R/utils.R")

lad_lookup <-
  boundaries_lad |>
  st_drop_geometry()

lookup_counties_lad <-
  lookup_counties_ua_lad |>
  select(-lad_name, -county_ua_code)

# 2019 LAD populations
GET(
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2019localauthoritydistrictcodes/ukmidyearestimates20192019ladcodes.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

raw_pop <-
  read_excel(
    tf,
    sheet = "MYE2 - Persons",
    skip = 4
  )

pop_lad <-
  raw_pop |>
  inner_join(
    lad_lookup,
    by = c("Code" = "lad_code")
  ) |>
  select(
    lad_code = Code,
    total_pop = `All ages`
  )

GET(
  "https://www.skillsforcare.org.uk/adult-social-care-workforce-data/Workforce-intelligence/documents/Raw-data/LA-data-download-Oct-2021.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

raw <-
  read_excel(
    tf,
    sheet = "LA level data"
  )

# ---- Clean data ----
# Select cols
social <-
  raw |>
  select(
    lad_name = `CSSR (LA area)`,
    sector = Sector,
    service = Service,
    job_role = `Job role`,
    turnover = `Turnover rate`,
    vacancy = `Vacancy rate`,
    sickness_days_mean = `Sickness - Mean (divide by Employees)`,
    zero_hour_yes = `Zero Hour Contracts - Yes`
  )

# Select all services and sectors
all_services_sectors <-
  social |>
  filter(sector == "All sectors" &
    service == "All services")

# Select only job roles which provide direct care
direct_care_roles <-
  all_services_sectors |>
  filter(
    job_role == "Care worker" |
      job_role == "Direct care" |
      job_role == "Occupational therapist" |
      job_role == "Registered nurse" |
      job_role == "Senior care worker" |
      job_role == "Social worker"
  )

# Replace suppressed data (where there isn't enough jobs) with zero
# (see Notes tab in raw data) and convert to class double for computation
replace_suppressed <-
  direct_care_roles |>
  mutate(
    across(turnover:zero_hour_yes, ~ str_replace_all(., "\\*", "0")),
    across(turnover:zero_hour_yes, as.double)
  )

# Calculate mean values per LA
lad_means <-
  replace_suppressed |>
  group_by(lad_name) |>
  summarise(
    across(
      turnover:zero_hour_yes, mean
    )
  )

# ---- Match LAD Codes ----
# The Local Authorities are a mixture of UTLA's & LTLA's
# - Match LTLA's -
match_ltlas <-
  lad_means |>
  inner_join(lad_lookup) |>
  select(
    -lad_name
  ) |>
  relocate(lad_code)

# - Match counties, weighting figures by population -
# Match counties
match_county <-
  lad_means |>
  anti_join(lad_lookup) |>
  inner_join(
    lookup_counties_lad,
    by = c("lad_name" = "county_ua_name")
  ) |>
  relocate(lad_code)

# Join to population data
match_county_pop <-
  match_county |>
  left_join(pop_lad)

# Weight by population
match_county_weighted <-
  match_county_pop |>
  group_by(lad_name) |>
  mutate(
    pop_sum = sum(total_pop),
    weight = total_pop / pop_sum
  ) |>
  ungroup() |>
  select(
    -total_pop,
    -pop_sum
  ) |>
  mutate(
    across(
      turnover:zero_hour_yes,
      ~ . * weight
    )
  ) |>
  select(
    -lad_name,
    -weight
  )

# - Manually match anything that didn't match to an LA or county -
match_remainder <-
  lad_means |>
  anti_join(lad_lookup) |>
  anti_join(
    lookup_counties_lad,
    by = c("lad_name" = "county_ua_name")
  ) |>
  mutate(
    lad_code = case_when(
      lad_name == "Redcar & Cleveland" ~ "E06000003",
      lad_name == "Stockton on Tees" ~ "E06000004",
      lad_name == "Durham" ~ "E06000047",
      lad_name == "Kingston upon Hull" ~ "E06000010",
      lad_name == "St Helens" ~ "E08000013",
      lad_name == "Stoke on Trent" ~ "E06000021",
      lad_name == "Herefordshire" ~ "E06000019",
      lad_name == "Telford & Wrekin" ~ "E06000020",
      lad_name == "Windsor & Maidenhead" ~ "E06000040",
      lad_name == "Southend on Sea" ~ "E06000033",
      lad_name == "Hammersmith & Fulham" ~ "E09000013",
      lad_name == "Kensington & Chelsea" ~ "E09000020",
      lad_name == "Barking & Dagenham" ~ "E09000002",
      lad_name == "Bournemouth Christchurch and Poole" ~ "E06000058",
      lad_name == "Brighton & Hove" ~ "E06000043",
      lad_name == "Cornwall and Isles of Scilly" ~ "E06000052/E06000053",
      lad_name == "Bristol" ~ "E06000023",
      lad_name == "Cheshire West & Chester" ~ "E06000050"
    )
  ) |>
  # separate Cornwall and Isles of Scilly
  separate_rows(lad_code, sep = "/") |>
  relocate(lad_code)

# Create population weighted values for Cornwall and Isles of Scilly
match_remainder_weighted <-
  match_remainder |>
  left_join(pop_lad) |>
  group_by(lad_name) |>
  mutate(
    pop_sum = sum(total_pop),
    weight = total_pop / pop_sum
  ) |>
  ungroup() |>
  select(
    -total_pop,
    -pop_sum
  ) |>
  mutate(
    across(
      turnover:zero_hour_yes,
      ~ . * weight
    )
  ) |>
  select(
    -lad_name,
    -weight
  )

# Join all LAD19CD
match_all <-
  bind_rows(
    match_ltlas,
    match_county_weighted,
    match_remainder_weighted
  )

# TODO:
# 1. Review the above weighting methods after matching LADS. Does it make sense
#    to weight areas by population when the indicator values are relative and not
#    absolute? I think not...
# 2. Review commented-out code below


# ===================
# ==== TO REVIEW ====
# ===================

# # ---- Calculate social score ----
# # Create two domains:
# # - (1) Coping capacity
# # - (2) Workforce flexibility

# # - Coping capacity domain -
# # PCA to weight indicators
# # Note: the higher the score, the worse the coping capacity is
# coping <-
#   social |>
#   select(LAD19CD, `Staff turnover rate` = turnover, `Mean sickness days` = sickness_days_mean) |>
#   weighted_domain_scores(model = "PCA") |>
#   rename_with(str_replace, pattern = "Vulnerability", replacement = "Adult Social Care - Coping", matches("Vulnerability"))

# # - Workforce flexibility -
# # PCA to weight indicators
# # Note: the higher the score, the better the flexibility is, so invert.
# flexibility <-
#   social |>
#   select(LAD19CD, `Vacancy rate` = vacancy, zero_hour_yes) |>
#   # invert zero hour contracts rate so higher number = worse (because fewer zero hours contracts means less flexibility)
#   mutate(`Mean % of non-zero-hours contracts` = 1 - zero_hour_yes) |>
#   select(-zero_hour_yes) |>
#   weighted_domain_scores(model = "PCA") |>
#   rename_with(str_replace, pattern = "Vulnerability", replacement = "Adult Social Care - Flexibility", matches("Vulnerability"))

# coping |>
#   left_join(flexibility, by = "LAD19CD") |>
#   write_csv("data/raw/health/adult-social-care-workforce.csv")

# # ---- Save a copy with overall scores/ranks/deciles for Adult Social Care workforce ----
# lad_names <- read_csv("https://opendata.arcgis.com/datasets/c3ddcd23a15c4d7985d8b36f1344b1db_0.csv") |>
#   select(LAD19CD, LAD19NM)

# coping |>
#   left_join(flexibility, by = "LAD19CD") |>
#   select(
#     LAD19CD,
#     ends_with("rank")
#   ) |>
#   calc_domain_scores(bespoke.domains = TRUE, rank.indicators = FALSE) |>
#   left_join(lad_names, by = "LAD19CD") |>
#   select(
#     Code = LAD19CD,
#     `Local Authority Name` = LAD19NM,
#     # `Adult Social Care workforce score` = `Vulnerability score`,
#     `Adult Social Care workforce rank` = `Vulnerability rank`,
#     `Adult Social Care workforce decile` = `Vulnerability decile`
#   ) |>
#   arrange(desc(`Adult Social Care workforce rank`))