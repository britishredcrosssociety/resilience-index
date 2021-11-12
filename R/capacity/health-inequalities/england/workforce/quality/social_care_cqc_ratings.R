# Load libs
library(tidyverse)
library(readODS)
library(geographr)
library(sf)

source("R/utils.R") # for download_file()

# Download the data
tf <- download_file("https://www.cqc.org.uk/sites/default/files/01_November_2021_Latest_ratings.ods", "ods")

raw <-
  read_ods(
    tf,
    sheet = "Providers",
  )


# Investigate the "Provider Type" & "Provider Primary Inspection Category"
raw |>
  distinct(`Provider Type`, `Provider Primary Inspection Category`) |>
  arrange(`Provider Type`)
# To investigate these different categories


# NHS TRUST ratings -----

# Filter only `Provider Type` for only NHS Trusts
# Filter only the overall ratings (as also provides broken down ratings - more info here: https://www.cqc.org.uk/guidance-providers/nhs-trusts/your-ratings-nhs-trusts)
# Filter only the overall group for `Service / Population Group` (there is many breakdowsn e.g. Critical care, Community health services for adults, Urgent care centre)
cqc_nhs_trusts_overall <- raw |>
  filter(`Provider Type` == "NHS Healthcare Organisation") |>
  filter(Domain == "Overall") |>
  filter(`Service / Population Group` == "Overall") |> 
  select(`Provider ID`, `Latest Rating`)
  
# Check distinct mapping i.e. only 1 score for 1 provider due to comment in the rating documentation stating:
# 'In some cases, a location or provider may have had more than one rating published on the same date, and in this case both ratings are shown in this file. It will be necessary to check on the CQC website to see which is the current rating for these organisations.'
cqc_nhs_trusts_overall |>
  group_by(`Provider ID`) |>
  summarise(rating_count = n()) |>
  filter(rating_count > 1)
# Check ok

# Create trust lookup of open trusts
open_trusts <-
  points_nhs_trusts |>
  as_tibble() |>
  filter(status == "open") |>
  select(
    trust_code = nhs_trust_code
  )

# Look at intersection of cqc_ratings_overall and open_trusts
open_trusts |>
  left_join(cqc_nhs_trusts_overall, by = c("trust_code" = "Provider ID"))