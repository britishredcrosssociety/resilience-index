# Load packages
library(tidyverse)
library(readxl)
library(geographr)
library(demographr)

source("R/utils.R")


# OSCI Community Needs Index data ----
raw <- read_excel("data/on-disk/SCNI and domains - 2021.xlsx")

engagement <- raw |>
  select(
    msoa_code = "Area code",
    msoa_name = "Area name",
    engagement_score = "ActiveEngaged_Domain",
    engagement_rank = "ActiveEngaged_Domain_rank"
  ) |>
  mutate(msoa_code = gsub('s','S', msoa_code))

cor(engagement$engagement_score, engagement$engagement_rank)

#  Population data -----
pop_msoa <- population_dz_20 |>
  filter_codes(dz_code, "^S") |>
  filter(sex == "All") |>
  select(dz_code,
         total_population) |>
  inner_join(lookup_postcode_oa11_lsoa11_msoa11_ltla20 |>
               filter(str_detect(msoa11_code, "^S")) |>
               select(msoa_code = msoa11_code,
                      dz_code = lsoa11_code) |>
               distinct()) |>
  group_by(msoa_code) |>
  summarise(total_population = mean(total_population))

# Check MSOA with no population data
engagement |>
  left_join(pop_msoa, by = "msoa_code") |>
  filter(is.na(total_population))

# Lookup codes ----
lookup_lad <- lookup_postcode_oa11_lsoa11_msoa11_ltla20 |>
  filter(str_detect(msoa11_code, "^S")) |>
  select(msoa_code = msoa11_code,
         lad_code = ltla20_code) |>
  distinct()

# Check any missing LADs
lookup_lad |>
  anti_join(engagement, by = "msoa_code")
# 0
engagement |>
  anti_join(lookup_lad, by = "msoa_code")
#0

# Calculate extent----
# HIGH SCORE/LOW RANK = LOW CAPABILITIES

engagement_lad <- engagement |>
  left_join(lookup_lad, by = "msoa_code") |>
  left_join(pop_msoa, by = "msoa_code") |>
  calculate_extent(
    var = engagement_score,
    higher_level_geography = lad_code,
    population = total_population,
    weight_high_scores = TRUE #  TRUE when a highest variable score equates to a lower capacity
  ) |>
  rename(engagement_extent = extent)

# Lookup names (for graph readability)
lookup_names <- lookup_ltla_ltla |>
  filter(str_detect(ltla20_code, "^S")) |>
  select(lad_name = ltla20_name,
         lad_code = ltla20_code) |>
  distinct()

engagement_lad |>
  left_join(lookup_names, by ="lad_code") |>
  mutate(lad_name = fct_reorder(lad_name, desc(engagement_extent))) |>
  ggplot(aes(x = lad_name, y = engagement_extent))+
  geom_point() +
  theme_classic() +
  labs(title = "Engagement Extent by LAD",
       x = "LAD",
       y = "Engagement Extent") +
  guides(x = guide_axis(angle = 90))

engagement_lad |>
  ggplot(aes(y = engagement_extent)) +
  geom_boxplot() +
  ylab("Engagement Extent") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Save data -----
engagement_lad |>
  write_rds("data/capacity/disasters-emergencies/scotland/engagement.rds")