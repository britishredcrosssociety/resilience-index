# Load libs
library(tidyverse)
library(readODS)

source("R/utils.R") # for download_file()

# Download the data
tf <- download_file("https://www.cqc.org.uk/sites/default/files/01_November_2021_Latest_ratings.ods", "ods")

raw <-
  read_ods(
    tf,
    sheet = "Providers",
  )

# Filter only the overall ratings (as also provides broken down ratings - more info here: https://www.cqc.org.uk/guidance-providers/nhs-trusts/your-ratings-nhs-trusts)
cqc_ratings_overall <- raw |>
  filter(Domain == "Overall")

# Investigate the "Provider Type" & "Provider Primary Inspection Category"
cqc_ratings_overall |>
  distinct(`Provider Type`, `Provider Primary Inspection Category`) |>
  arrange(`Provider Type`)