library(tidyverse)
library(httr)
library(readODS)

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Children%20Education%20and%20Skills/Attendance%20Rates%20for%20Post-Primary%20Pupils%20by%20Pupil%20Residence%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw_post_primary <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "A4:F15"
  )

post_primary <-
  raw_post_primary |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    unauth_absence = `Unauthorised absence (% of half days)`
  )

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Children%20Education%20and%20Skills/Attendance%20Rates%20for%20Primary%20Pupils%20by%20Pupil%20Residence%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw_primary <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "A4:F15"
  )

primary <-
  raw_primary |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    unauth_absence = `Unauthorised absence (% of half days)`
  )

GET(
  "https://www.ninis2.nisra.gov.uk/Download/Children%20Education%20and%20Skills/Attendance%20Rates%20for%20Special%20School%20Pupils%20by%20Pupil%20Residence%20(administrative%20geographies).ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

raw_special <-
  read_ods(
    tf,
    sheet = "LGD2014",
    range = "A4:F15"
  )

special <-
  raw_special |>
  as_tibble() |>
  select(
    lad_code = `LGD2014 Code`,
    unauth_absence = `Unauthorised absence (% of half days)`
  )

absence <-
  bind_rows(
    post_primary,
    primary,
    special
  ) |>
  group_by(lad_code) |>
  summarise(unauth_absence = mean(unauth_absence))

# Save
write_rds(absence, "data/vulnerability/health-inequalities/northern-ireland/healthy-lives/pupil-absence.rds")