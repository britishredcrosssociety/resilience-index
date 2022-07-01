# ---- Load ----
library(tidyverse)
library(sf)
library(geographr)
library(demographr)
library(viridis)

source("R/utils.R")

raw <-
  read_sf("data/on-disk/heat-hazard-raw/LSOA_England_Heat_Hazard_v1.shp")

quintiles <-
  raw |>
  st_drop_geometry() |>
  select(
    lsoa11_code = LSOA11CD,
    mean_temp = mean_std_t
  ) |>
  filter(str_detect(lsoa11_code, "^W")) |>
  mutate(quintiles = quantise(mean_temp, 5))

boundaries_lsoa11 |>
  right_join(quintiles) |>
  ggplot() +
  geom_sf(
    mapping = aes(fill = quintiles),
    color = "black",
    size = 0.01
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
  theme_map()