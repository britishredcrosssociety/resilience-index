library(tidyverse)
library(sf)
library(geographr)
library(viridis)
library(patchwork)

index <-
  read_csv("data/vulnerability/health-inequalities/northern-ireland/index-unweighted.csv")

shp <-
  boundaries_lad |> 
  filter(str_detect(lad_code, "^N"))

index_shp <-
  shp |> 
  left_join(index, by = "lad_code")

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

healthy_lives <-
  index_shp |> 
  select(healthy_lives_domain_quantiles, geometry) |> 
    ggplot() +
    geom_sf(mapping = aes(fill = healthy_lives_domain_quantiles),
            color = "black",
            size = 0.1) +
    scale_fill_viridis(
      breaks = seq(1, 10, by = 1),
      labels = seq(1, 10, by = 1),
      na.value = "transparent",
      option = "magma",
      name = expression(paste("Healthy Lives \nDomain Deciles")),
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

healthy_people <-
  index_shp |> 
  select(healthy_people_domain_quantiles, geometry) |> 
    ggplot() +
    geom_sf(mapping = aes(fill = healthy_people_domain_quantiles),
            color = "black",
            size = 0.1) +
    scale_fill_viridis(
      breaks = seq(1, 10, by = 1),
      labels = seq(1, 10, by = 1),
      na.value = "transparent",
      option = "magma",
      name = expression(paste("Healthy People \nDomain Deciles")),
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

healthy_places <-
  index_shp |> 
  select(healthy_places_domain_quantiles, geometry) |> 
    ggplot() +
    geom_sf(mapping = aes(fill = healthy_places_domain_quantiles),
            color = "black",
            size = 0.1) +
    scale_fill_viridis(
      breaks = seq(1, 10, by = 1),
      labels = seq(1, 10, by = 1),
      na.value = "transparent",
      option = "magma",
      name = expression(paste("Healthy Places \nDomain Deciles")),
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

composite_score <-
  index_shp |> 
  select(health_inequalities_composite_quantiles, geometry) |> 
    ggplot() +
    geom_sf(mapping = aes(fill = health_inequalities_composite_quantiles),
            color = "black",
            size = 0.1) +
    scale_fill_viridis(
      breaks = seq(1, 10, by = 1),
      labels = seq(1, 10, by = 1),
      na.value = "transparent",
      option = "magma",
      name = expression(paste("Overall Index \nDeciles")),
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

healthy_lives + healthy_people + healthy_places + composite_score