# ---- Load libs ----
library(tidyverse)
library(httr)
library(readxl)
library(geographr)
library(sf)
source("R/utils.R")

# ---- Load data ----
pop_lad <-
  population_lad |> 
  select(lad_code, pop = total_population)

GET(
  "https://files.digital.nhs.uk/74/93D6A1/gp-reg-pat-prac-lsoa-male-female-october-21.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

raw <-
  read_csv(
    list.files(
      tempdir(),
      pattern = "gp-reg-pat-prac-lsoa-all.csv",
      full.names = TRUE
    )
  )

raw_reduced <-
  raw |>
  select(
    lsoa_code = LSOA_CODE,
    count = NUMBER_OF_PATIENTS
  )

lsoa_summaries <-
  raw_reduced |>
  group_by(lsoa_code) |>
  summarise(count = sum(count))

# Remove non-English LSOA's
lsoa_registrations <-
  lsoa_summaries |>
  filter(str_detect(lsoa_code, "^E"))

msoa_registrations <-
  lsoa_registrations |>
  left_join(lookup_lsoa_msoa) |>
  group_by(msoa_code) |>
  summarise(count = sum(count))

lad_registrations <-
  msoa_registrations |>
  left_join(lookup_msoa_lad) |>
  group_by(lad_code) |>
  summarise(count = sum(count))

gp_registrations <-
  lad_registrations |>
  left_join(pop_lad) |>
  mutate(perc_registered_gp = count / pop * 100) |>
  select(lad_code, perc_registered_gp)

# TODO:
# 1. There are four NA values in gp_registrations due to missing pop values.
#    It is assumed these values are missing because 2021 lad codes have been
#    used and so don't align. Check this assumption and align codes. See
#    carers-allowance.R script for potential alignment solution.
# 2. How do we handle the fact that only 10 LADs have less than 100% gp
#    registration? The issue is that because we use deciles mainly, even places
#    ranked as 100% registration will get lumped into the most vulnerable
#    decile. This doesn't make sense.