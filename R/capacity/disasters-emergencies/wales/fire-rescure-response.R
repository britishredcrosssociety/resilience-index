# ---- Load Libs ----
library(tidyverse)
library(readxl)
library(geographr)
library(demographr)
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
    mutate(average_time = mean(c(avg_second_2019, avg_second_2020, avg_second_2021), na.rm = TRUE)) |>
    ungroup() |>
    select(lsoa_name, average_time)

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
        south_average,
        mid_west_average
    ) |>
    left_join(lookup_lsoa11_ltla21, by = c("lsoa_code" = "lsoa11_code")) |>
    left_join(population_lsoa_20_codes_11, by = c("lsoa_code" = "lsoa_11_code")) |>
    select(lsoa_code, ltla21_code, average_time, total_population) |>
    calculate_extent(
        var = average_time,
        higher_level_geography = ltla21_code,
        population = total_population
    )

write_rds(
    fire_response_times,
    "data/capacity/disasters-emergencies/wales/fire-rescure-response.rds"
)