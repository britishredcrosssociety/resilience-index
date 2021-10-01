# Load libs
library(tidyverse)
library(httr)
library(readxl)
library(sf)
library(geographr)

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

# Load raw data
GET(
  "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/June-2021-AE-by-provider-j47iI.xls",
  write_disk(tf <- tempfile(fileext = ".xls"))
)

raw <-
  read_excel(
    tf,
    sheet = "Provider Level Data",
    skip = 15
  )

# remove first two entries (one is totals, other is blank)
ae_sliced <-
  raw |>
  slice(-(1:2))

# Remove empty rows at the end of the spreadsheet
ae_remove_empty <-
  ae_sliced |>
  drop_na()

# Keep vars of interest
ae_vars <-
  ae_remove_empty |>
  select(
    trust_code = Code,
    `% Total <= 4 hours` = `Percentage in 4 hours or less (all)`
  )

# Replace '-' character with NA
ae_replace <-
  ae_vars |>
  mutate(
    across(
      .cols = !c(trust_code),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Change cols to double
ae_double <-
  ae_replace |>
  mutate(
    across(
      .cols = !c(trust_code),
      as.double
    )
  )

# Filter to only open trusts
ae_open <-
  open_trusts |>
  left_join(ae_double)

# Drop NA
ae_drop_na <-
  ae_open |>
  drop_na()

# TODO:
# Trusts need to be matched to MSOA's, and then aggregated up to LTLA.
# As can be seen in the code snippet below, not all trusts are matching the
# lookup table. This needs fixing before aggregation can happen. Also, does it
# matter that there were some NA's in the above steps that get dropped? Why is
# this data not available?

source("R/utils.R") # For keep_na()

ae_drop_na |>
  left_join(lookup_trust_msoa) |>
  keep_na() |>
  pull(trust_code) -> not_matched

not_matched %in% trust_changes$new_code # trust_changes from https://github.com/britishredcrosssociety/geographr/blob/main/data-raw/lookup_trust_msoa.R#L111