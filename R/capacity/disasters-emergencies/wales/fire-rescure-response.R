# ---- Load Libs ----
library(tidyverse)
library(readxl)
library(geographr)
library(demographr)
library(rvest)
library(xml2)
library(lubridate)
source("R/utils.R")

# ---- Analyse ----
# Fire and Rescue Service response times here is defined as the time station
# received the phone call to the time on scene

# Data is split across three regions: North, Mid-and west, and South

# - North -
# Source: https://www.whatdotheyknow.com/request/fire_and_rescue_service_response#incoming-1993766
north_2019 <-
    read_excel("data/on-disk/Fire-and-Rescue-Service-response-times/North Response times by Lower Layer Super Output Area.xlsx",
        sheet = "2019", skip = 1
    )

north_2020 <-
    read_excel("data/on-disk/Fire-and-Rescue-Service-response-times/North Response times by Lower Layer Super Output Area.xlsx",
        sheet = "2020", skip = 1
    )

north_2021 <-
    read_excel("data/on-disk/Fire-and-Rescue-Service-response-times/North Response times by Lower Layer Super Output Area.xlsx",
        sheet = "2021", skip = 1
    )

north_average <-
    north_2019 |>
    rename(avg_datatime_2019 = "Average of Difference between Call Time Stamp and Arrive Time Stamp") |>
    full_join(
        north_2020 |> rename(avg_datatime_2020 = "Average of Difference between Call Time Stamp and Arrive TimeStamp"),
        by = "LSOA"
    ) |>
    full_join(
        north_2021 |> rename(avg_datatime_2021 = "Average of Difference between Call Time Stamp and Arrive TimeStamp"),
        by = "LSOA"
    ) |>
    mutate(
        avg_second_2019 = hour(avg_datatime_2019) * 3600 + minute(avg_datatime_2019) * 60 + floor(second(avg_datatime_2019)),
        avg_second_2020 = hour(avg_datatime_2020) * 3600 + minute(avg_datatime_2020) * 60 + floor(second(avg_datatime_2020)),
        avg_second_2021 = hour(avg_datatime_2021) * 3600 + minute(avg_datatime_2021) * 60 + floor(second(avg_datatime_2021)),
        ward_name = str_trim(str_replace_all(LSOA, "[:digit:]", ""), side = "right") # format ward_name to later join operation
    ) |>
    rowwise() |>
    mutate(average_time = mean(c(avg_second_2019, avg_second_2020, avg_second_2021), na.rm = TRUE)) |>
    ungroup() |>
    select(lsoa_name = LSOA, average_time)

# The LOSA names use non standard names found elsewhere (e.g., ONS geoportal)
# Create lookup:

# Wales Gov LSOA names -----
# Source: https://gov.wales/docs/statistics/lsoamaps/lsoa.htm
raw <-
    read_html("https://gov.wales/docs/statistics/lsoamaps/lsoa.htm") %>%
    html_nodes("a") |>
    html_text() |>
    as_tibble()

lsoa_codes_walesgov <-
    raw |>
    filter(str_detect(value, "^W[0-9]{8} ")) |>
    separate(
        value,
        c("lsoa_code_walesgov", "lsoa_name_walesgov"),
        sep = " ",
        extra = "merge"
    )

# to fix row 440
# https://gov.wales/docs/statistics/lsoamaps/powys/W01000440.pdf
lsoa_codes_walesgov[440, ]

# ONS Wales LSOA codes ----
lsoa_codes_ons <-
    lookup_lsoa11_msoa11 |>
    distinct(lsoa11_name, lsoa11_code) |>
    filter(str_detect(lsoa11_code, "^W"))

north_average_matched <-
    north_average |>
    left_join(lsoa_codes_walesgov, by = c("lsoa_name" = "lsoa_name_walesgov")) |>
    left_join(lsoa_codes_ons, by = c("lsoa_code_walesgov" = "lsoa11_code")) |>
    select(lsoa_code = lsoa_code_walesgov, average_time) |>
    drop_na() |>
    group_by(lsoa_code) |>
    summarise(average_time = mean(average_time))

# Can't match missing areas to any data set. Don't appear to exist.
missing_areas <-
    north_average |>
    left_join(lsoa_codes_walesgov, by = c("lsoa_name" = "lsoa_name_walesgov")) |>
    left_join(lsoa_codes_ons, by = c("lsoa_code_walesgov" = "lsoa11_code")) |>
    keep_na()

# - South -
# Source: https://www.whatdotheyknow.com/request/fire_and_rescue_service_response_3#incoming-1992620

# Loading data
south <- read_csv("data/on-disk/Fire-and-Rescue-Service-response-times/south.csv", skip = 1)[, 1:7]

# Calculate three years' average
south_average <-
    south |>
    rename(
        lsoa_code = ...1,
        avg_datatime_2019 = "2019",
        avg_datatime_2020 = "2020",
        avg_datatime_2021 = "2021"
    ) |>
    mutate(
        avg_second_2019 = hour(avg_datatime_2019) * 3600 + minute(avg_datatime_2019) * 60 + floor(second(avg_datatime_2019)),
        avg_second_2020 = hour(avg_datatime_2020) * 3600 + minute(avg_datatime_2020) * 60 + floor(second(avg_datatime_2020)),
        avg_second_2021 = hour(avg_datatime_2021) * 3600 + minute(avg_datatime_2021) * 60 + floor(second(avg_datatime_2021))
    ) |>
    rowwise() |>
    mutate(average_time = mean(c(avg_second_2019, avg_second_2020, avg_second_2021), na.rm = TRUE)) |>
    ungroup() |>
    select(lsoa_code, average_time)

# - Mid and west -
# Source: https://www.whatdotheyknow.com/request/fire_and_rescue_service_response_2#incoming-1989382
# Loading data
mid_west <- read_excel("data/on-disk/Fire-and-Rescue-Service-response-times/Mid and West Average Response Times FOI 2019 to 2021 Breakdown.xlsx",
    sheet = "Totals", skip = 3
)

mid_west_average <-
    mid_west |>
    rename(
        lsoa_name = "Row Labels",
        avg_datatime_2019 = "2019",
        avg_datatime_2020 = "2020",
        avg_datatime_2021 = "2021"
    ) |>
    filter(lsoa_name != "Grand Total") |>
    mutate(
        avg_second_2019 = avg_datatime_2019 * 60,
        avg_second_2020 = avg_datatime_2020 * 60,
        avg_second_2021 = avg_datatime_2021 * 60
    ) |>
    rowwise() |>
    mutate(average_time = mean(c(avg_second_2019, avg_second_2020, avg_second_2021), na.rm = TRUE)) |>
    ungroup() |>
    select(lsoa_name, average_time) |>
    left_join(lookup_lsoa11_ltla21, by = c("lsoa_name" = "lsoa11_name")) |>
    select(lsoa_code = lsoa11_code, average_time)

# ---- Aggregate to Local Authority ----
fire_response_times <-
    bind_rows(
        north_average_matched,
        south_average,
        mid_west_average
    ) |>
    left_join(lookup_lsoa11_ltla21, by = c("lsoa_code" = "lsoa11_code")) |>
    left_join(population20_lsoa11, by = c("lsoa_code" = "lsoa11_code")) |>
    select(lsoa_code, ltla21_code, average_time, total_population) |>
    calculate_extent(
        var = average_time,
        higher_level_geography = ltla21_code,
        population = total_population
    ) |>
    rename(lad_code = ltla21_code, fire_extent = extent)

write_rds(
    fire_response_times,
    "data/capacity/disasters-emergencies/wales/fire-rescure-response.rds"
)