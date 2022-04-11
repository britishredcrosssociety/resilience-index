# ---- Load Libs ----
library(tidyverse)
library(readxl)
library(geographr)
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

north_three_years <-
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
    mutate(avg_three = mean(c(avg_second_2019, avg_second_2020, avg_second_2021), na.rm = TRUE)) |> # ignore NA
    select(-LSOA, -avg_datatime_2019, -avg_datatime_2020, -avg_datatime_2021)

north_join <-
    north_three_years |>
    left_join(lookup_lsoa_ward, by = "ward_name") |>
    left_join(lookup_lsoa_msoa, by = "lsoa_code") |>
    left_join(lookup_msoa_lad, by = "msoa_code") |>
    filter(
        str_detect(lsoa_code, "^W")
    ) |>
    select(avg_three, ward_name, lsoa_code, ward_code, msoa_code, lad_code) |>
    group_by(lad_code) |>
    summarize(rescure_time = mean(avg_three, na.rm = TRUE))


# - South -
# Source: https://www.whatdotheyknow.com/request/fire_and_rescue_service_response_3#incoming-1992620

# Loading data
south <- read_csv("data/on-disk/Fire-and-Rescue-Service-response-times/south.csv", skip = 1)[, 1:7]

# Calculate three years' average
south_three_years <-
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
    mutate(avg_three = mean(c(avg_second_2019, avg_second_2020, avg_second_2021), na.rm = TRUE)) |> # ignore NA
    select(lsoa_code, avg_three)

south_join <-
    south_three_years |>
    left_join(lookup_lsoa_msoa, by = "lsoa_code") |>
    left_join(lookup_msoa_lad, by = "msoa_code") |>
    filter(
        str_detect(lsoa_code, "^W")
    ) |>
    select(avg_three, lsoa_code, msoa_code, lad_code) |>
    group_by(lad_code) |>
    summarize(rescure_time = mean(avg_three, na.rm = TRUE))

# - Mid and west -
# Source: https://www.whatdotheyknow.com/request/fire_and_rescue_service_response_2#incoming-1989382
# Loading data
mid_west <- read_excel("data/on-disk/Fire-and-Rescue-Service-response-times/Mid and West Average Response Times FOI 2019 to 2021 Breakdown.xlsx",
    sheet = "Totals", skip = 3
)

mid_west_three <-
    mid_west |>
    rename(
        lsoa_name = "Row Labels",
        avg_datatime_2019 = "2019",
        avg_datatime_2020 = "2020",
        avg_datatime_2021 = "2021"
    ) |>
    mutate(
        avg_second_2019 = avg_datatime_2019 * 60,
        avg_second_2020 = avg_datatime_2020 * 60,
        avg_second_2021 = avg_datatime_2021 * 60
    ) |>
    rowwise() |>
    mutate(avg_three = mean(c(avg_second_2019, avg_second_2020, avg_second_2021), na.rm = TRUE)) |> # ignore NA
    select(lsoa_name, avg_three)

mid_west_join <-
    mid_west_three |>
    left_join(lookup_lsoa_msoa, by = "lsoa_name") |>
    left_join(lookup_msoa_lad, by = "msoa_code") |>
    filter(
        str_detect(lsoa_code, "^W")
    ) |>
    select(avg_three, lsoa_code, msoa_code, lad_code) |>
    group_by(lad_code) |>
    summarize(rescure_time = mean(avg_three, na.rm = TRUE))

# **Missing: W06000001
wales_all <-
    north_join |>
    bind_rows(south_join) |>
    bind_rows(mid_west_join)

# rescure_time is seconds
output <-
    wales_lad |>
    full_join(wales_all) |>
    mutate(
        rescure_time = if_else(is.na(rescure_time), mean(rescure_time, na.rm = TRUE), rescure_time)
    )

write_rds(
    output,
    "data/capacity/disasters-emergencies/wales/fire-rescure-response.rds"
)