library(tidyverse)
library(sf)
library(geographr)
library(viridis)

heat_hazards <-
  read_rds("data/shocks/disasters-emergencies/england/heat-hazard.rds")

shp <-
  boundaries_lad %>% 
  filter(str_detect(lad_code, "^E"))

heat_hazards_shp <-
  shp %>% 
  left_join(heat_hazards, by = "lad_code")

# White map theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "FiraCode-Retina", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      # Add labs elements
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(
        size = 10, hjust = 0.5,
        margin = margin(
          b = 0.2,
          t = 0.2,
          l = 2,
          unit = "cm"
        ),
        debug = F
      ),
      # captions
      plot.caption = element_text(
        size = 7,
        hjust = .5,
        margin = margin(
          t = 0.2,
          b = 0,
          unit = "cm"
        ),
        color = "#939184"
      ),
      ...
    )
}

heat_hazards_shp %>% 
  select(heat_hazard_quintiles, geometry) %>% 
    ggplot() +
    geom_sf(mapping = aes(fill = heat_hazard_quintiles),
            color = "black",
            size = 0.1) +
    scale_fill_viridis(
      breaks = seq(1,5, by = 1),
      labels = seq(1, 5, by = 1),
      na.value = "transparent",
      option = "magma",
      name = expression(paste("Heat Hazard \n(5 = worst)")),
      alpha = 0.8, # make fill a bit brighter
      begin = 0.1, # this option seems to be new (compared to 2016):
      # with this we can truncate the
      # color scale, so that extreme colors (very dark and very bright) are not
      # used, which makes the map a bit more aesthetic
      end = 0.9,
      discrete = F, # discrete classes, thus guide_legend instead of _colorbar
      direction = -1, # dark is lowest, yellow is highest
      guide = guide_legend(
        keyheight = unit(5, units = "mm"),
        title.position = "top",
        reverse = T)) +
    theme_map() +
    theme(plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"))