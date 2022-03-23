library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Health%20and%20Social%20Care/Childhood%20BMI%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "B4:H15"
  )

obesity <-
  raw |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    rate_primary_1 = `% Overweight or Obese (Primary 1): All`,
    rate_year_8 = `% Overweight or Obese (Year 8): All`
  ) |>
  rowwise() |>
  mutate(
    overweight_obese_children_rate = mean(
      c(rate_primary_1, rate_year_8),
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  select(lad_code, overweight_obese_children_rate)

# Save
write_rds(obesity, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/overweight-obesity-children.rds")