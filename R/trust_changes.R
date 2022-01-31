# This script creates a table with trust code changes 
# The code originates from the geographr package (https://github.com/britishredcrosssociety/geographr/blob/main/data-raw/lookup_trust_msoa.R)

# Load libraries 
library(tidyverse)
library(devtools)
library(httr)
library(readxl)

# Update trusts 
# Source: https://digital.nhs.uk/services/organisation-data-service/file-downloads/miscellaneous
# The "Successor Organisation" and "Archived Successor Organisations" document
# Trust changes. The former covering the past financial year, and the latter all
# historic changes before that.

# Load successor data
GET(
  "https://files.digital.nhs.uk/assets/ods/current/succ.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

successor_raw <-
  read_csv(
    list.files(tempdir(),
               pattern = "succ.csv",
               full.names = TRUE
    ),
    col_names = c(
      "old_code",
      "new_code",
      "R",
      "date",
      "blank"
    )
  )

successor <-
  successor_raw %>%
  select(
    old_code,
    new_code,
    date
  )

# Load Archived Successor Organisations
GET(
  "https://files.digital.nhs.uk/assets/ods/current/succarc.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

successor_archived_raw <-
  read_csv(
    list.files(tempdir(),
               pattern = "succarc.csv",
               full.names = TRUE
    ),
    col_names = c(
      "old_code",
      "new_code",
      "R",
      "date",
      "blank"
    )
  )

successor_archived <-
  successor_archived_raw %>%
  select(
    old_code,
    new_code,
    date
  )

# Join all trust changes (note the data also contains non trusts)
trust_changes <-
  bind_rows(
    successor,
    successor_archived
  )

write_rds(trust_changes, "data/trust_changes.rds")
