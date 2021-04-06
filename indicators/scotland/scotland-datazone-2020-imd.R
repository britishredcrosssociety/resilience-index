library(dplyr)
library(httr)
library(readxl)
source("functions.R")

scotland_datazone_imd <- function() {

  # Source: https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/
  GET(
    url = "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/documents/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/govscot%3Adocument/SIMD%2B2020v2%2B-%2Branks.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx"))
  )

  # For all ranks: 1 is most deprived, 6,976 is least deprived
  imd_raw <- read_excel(tf, sheet = "SIMD 2020v2 ranks")

  imd_clean <-
    imd_raw %>%
    select(
      data_zone = Data_Zone,
      overall_rank = SIMD2020v2_Rank,
      income_rank = SIMD2020v2_Income_Domain_Rank,
      employment_rank = SIMD2020_Employment_Domain_Rank,
      health_rank = SIMD2020_Health_Domain_Rank,
      education_rank = SIMD2020_Education_Domain_Rank,
      access_rank = SIMD2020_Access_Domain_Rank,
      crime_rank = SIMD2020_Crime_Domain_Rank,
      housing_rank = SIMD2020_Housing_Domain_Rank
    )

  imd <-
    imd_clean %>%
    mutate(overall_deciles = calc_risk_quantiles(overall_rank, highest.quantile.is.worst = FALSE, quants = 10))

  return(imd)
}

scotland_datazone_imd()
