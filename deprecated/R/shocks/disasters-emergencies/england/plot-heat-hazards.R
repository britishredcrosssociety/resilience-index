library(tidyverse)
library(sf)
library(geographr)
library(viridis)

source("R/utils.R")

heat_hazards <-
  read_rds("data/shocks/disasters-emergencies/england/heat-hazard.rds")

shp <-
  boundaries_lad %>%
  filter(str_detect(lad_code, "^E"))

heat_hazards_shp <-
  shp %>%
  left_join(heat_hazards, by = "lad_code")

heat_hazards_shp %>%
  select(heat_hazard_quintiles, geometry) %>%
  ggplot() +
  geom_sf(
    mapping = aes(fill = heat_hazard_quintiles),
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