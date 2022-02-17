library(sf)
library(tidyverse)
library(geographr)
library(viridis)

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
        weight_high_scores = FALSE  # For all ranks: 1 is most deprived
    ) %>%
    as_tibble() %>%
    select(-geometry)

heat_hazard_clean <-
    heat_hazard_raw %>%
    filter(str_detect(LSOA11CD, "^W")) %>%
    rename(lsoa_code = LSOA11CD) %>%
    left_join(lookup_lsoa_msoa, by = "lsoa_code") %>%
    left_join(lookup_msoa_lad, by = "msoa_code") %>%
    group_by(lad_code) %>%
    summarise(hazard = sum(hazard)) %>% 
    as_tibble() %>%
    select(-geometry)

heat_hazard_clean %>%
    write_rds("data/shocks/disasters-emergencies/wales/heat_hazard_lad.rds")

shp <-
  boundaries_lad %>%
  filter(str_detect(lad_code, "^W"))

heat_hazards_shp <-
  shp %>%
  left_join(heat_hazard_clean, by = "lad_code")

heat_hazards_shp %>%
  select(hazard, geometry) %>%
  ggplot() +
  geom_sf(
    mapping = aes(fill = hazard),
    color = "black",
    size = 0.1
  ) +
  scale_fill_viridis(
    breaks = seq(1, 5, by = 1),
    labels = seq(1, 5, by = 1),
    na.value = "transparent",
    option = "magma",
    name = expression(paste("Heat Hazard \n(5 = worst)")),
    alpha = 0.8,
    begin = 0.1,
    end = 0.9,
    discrete = F,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T
    )
  ) +
  theme_map() +
  theme(plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"))
