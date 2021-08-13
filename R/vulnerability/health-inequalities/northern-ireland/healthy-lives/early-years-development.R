library(tidyverse)
library(readODS)
library(httr)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Children%20Education%20and%20Skills/Primary%20Pupils%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "A4:Q15"
  )

raw_tibble <-
  raw |>
  as_tibble()

raw_selected <-
  raw_tibble |>
  select(
    lad_code = `LGD2014 Code`,
    sen_stage_1_4_count = `Pupils at SEN stage 1 - 4`,
    sen_stage_5_count = `Statemented pupils`,
    starts_with("Year")
  )

pupil_count <-
  raw_selected |>
  rowwise() |>
  mutate(total_pupils = sum(c_across(starts_with("Year")))) |>
  ungroup() |>
  select(-starts_with("Year"))

sen_total <-
  pupil_count |>
  rowwise() |>
  mutate(total_sen = sum(c_across(starts_with("sen")))) |>
  ungroup() |>
  select(-starts_with("sen"))

development <-
  sen_total |>
  mutate(percentage_sen_registered = total_sen / total_pupils) |>
  select(-starts_with("total"))

# Save
write_rds(development, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/early-years-development.rds")