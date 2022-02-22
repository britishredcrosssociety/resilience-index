library(sf)
library(tidyverse)
library(geographr)
library(viridis)
source("R/utils.R")

heat_hazard_raw <-
    st_read("data/on-disk/4EI-heat-hazards/4EI-heat-hazards/LSOA_England_Heat_Hazard_v1.shp")

heat_hazard_clean <-
    heat_hazard_raw %>%
    filter(str_detect(LSOA11CD, "^W")) %>%
    rename(lsoa_code = LSOA11CD) %>%
    left_join(lookup_lsoa_msoa, by = "lsoa_code") %>%
    left_join(lookup_msoa_lad, by = "msoa_code") %>%
    left_join(population_lsoa, by = "lsoa_code") %>%

    calculate_extent(
        var = hazard,
        higher_level_geography = lad_code,
        population = total_population,
        weight_high_scores = TRUE
    ) %>%
    mutate(quantise = quantise(extent)) %>%  # use quantile
    as_tibble() %>%
    select(-geometry, -extent)

heat_hazard_clean %>%
    write_rds("data/shocks/disasters-emergencies/wales/heat_hazard_lad.rds")

# Visualization
shp <-
  boundaries_lad %>%
  filter(str_detect(lad_code, "^W"))

heat_hazards_shp <-
  shp %>%
  left_join(heat_hazard_clean, by = "lad_code")

heat_hazards_shp %>%
  select(quantise, geometry) %>%
  ggplot() +
  geom_sf(
    mapping = aes(fill = quantise),
    # color = "black",
    size = 0.1
  ) +
  scale_fill_viridis(
    # breaks = seq(1, 5, by = 1),
    # labels = seq(1, 5, by = 1),
    # na.value = "transparent",
    option = "magma",
    # name = expression(paste("Heat Hazard \n(5 = worst)")),
    alpha = 0.8,
    begin = 0.1,
    end = 0.9,
    discrete = F,
    direction = -1,
    guide = guide_legend(
      title = "",
      label = TRUE,
      keyheight = unit(8, units = "mm"),
      reverse = T
    )
  ) +
  labs(title = "Heat hazard extent scores quantile in Wales") +
  # theme_map() +
  theme(plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"))