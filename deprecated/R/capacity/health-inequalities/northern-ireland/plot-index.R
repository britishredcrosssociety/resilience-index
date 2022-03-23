library(tidyverse)
library(sf)
library(geographr)
library(viridis)
library(patchwork)

domain_scores <-
  read_csv("data/capacity/health-inequalities/northern-ireland/capacity-domain-scores.csv")

domain_scores_shp <-
  boundaries_trusts_ni %>%
  left_join(domain_scores)

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
      plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
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

access_availability <-
  domain_scores_shp %>%
  select(access_availability_domain_quantiles, geometry) %>%
  ggplot() +
  geom_sf(
    mapping = aes(fill = access_availability_domain_quantiles),
    color = "black",
    size = 0.1
  ) +
  scale_fill_viridis(
    breaks = seq(1, 5, by = 1),
    labels = seq(1, 5, by = 1),
    na.value = "transparent",
    option = "E",
    name = expression(paste("Domain Quintiles \n(5 = Highest Capacity)")),
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
  theme(plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm")) +
  labs(title = "Access & Availability") +
  geom_sf_label(
    data = boundaries_trusts_ni,
    mapping = aes(label = trust_name)
  )

workforce <-
  domain_scores_shp %>%
  select(workforce_domain_quantiles, geometry) %>%
  ggplot() +
  geom_sf(
    mapping = aes(fill = workforce_domain_quantiles),
    color = "black",
    size = 0.1
  ) +
  scale_fill_viridis(
    breaks = seq(1, 5, by = 1),
    labels = seq(1, 5, by = 1),
    na.value = "transparent",
    option = "E",
    name = expression(paste("Domain Quintiles \n(5 = Highest Capacity)")),
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
  theme(plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm")) +
  labs(title = "Workforce") +
  geom_sf_label(
    data = boundaries_trusts_ni,
    mapping = aes(label = trust_name)
  )

quality <-
  domain_scores_shp %>%
  select(quality_domain_quantiles, geometry) %>%
  ggplot() +
  geom_sf(
    mapping = aes(fill = quality_domain_quantiles),
    color = "black",
    size = 0.1
  ) +
  scale_fill_viridis(
    breaks = seq(1, 5, by = 1),
    labels = seq(1, 5, by = 1),
    na.value = "transparent",
    option = "E",
    name = expression(paste("Domain Quintiles \n(5 = Highest Capacity)")),
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
  theme(plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm")) +
  labs(title = "Quality") +
  geom_sf_label(
    data = boundaries_trusts_ni,
    mapping = aes(label = trust_name)
  )

access_availability + workforce + quality + guide_area() +
  plot_layout(guides = "collect")