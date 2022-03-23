# ---- Load ----
library(tidyverse)
library(geographr)
library(readxl)
library(httr)
library(sf)
library(viridis)

# Charity database Scotland 
# Source: https://www.oscr.org.uk/about-charities/search-the-register/charity-register-download/
GET(
  "https://www.oscr.org.uk/umbraco/Surface/FormsSurface/CharityRegDownload",
  write_disk(tf <- tempfile(".zip"))
)

unzip(tf, exdir = tempdir())

charities_raw <-
  read_csv(
    list.files(tempdir(),
      pattern = "CharityExport-20-May-2021.csv",
      full.names = TRUE
    )
  )

# LAD Lookup
lad_lookup <-
  lookup_dz_iz_lad %>% 
  distinct(lad_name, lad_code)

# Load population estimates
# source: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2019
GET(
  "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-19/mid-year-pop-est-19-data.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

pop_raw <- read_excel(tf, sheet = "Table 2", range = "A4:C38")

# Calculate population estimates
pop <-
  pop_raw %>%
  slice(-(1:2)) %>%
  select(
    lad_code = `Area code1`,
    pop_count = `All Ages`
  )

# ---- Model ----
# Keep vars of interest
charities_clean <-
  charities_raw %>% 
  select(
    name = `Charity Name`,
    postcode = Postcode,
    coverage = `Geographical Spread`,
    main_operating_location = `Main Operating Location`,
    purpose = Purposes,
    beneficiaries = Beneficiaries,
  )

# Match main operating areas to Local Authorites
charities_lad <-
  charities_clean %>% 
  filter(main_operating_location != "Outwith Scotland") %>% 
  mutate(
    lad_name = case_when(
      main_operating_location == "Perth & Kinross" ~ "Perth and Kinross",
      main_operating_location == "Aberdeen" ~ "Aberdeen City",
      main_operating_location == "Argyll & Bute" ~ "Argyll and Bute",
      main_operating_location == "Dumfries & Galloway" ~ "Dumfries and Galloway",
      main_operating_location == "Western Isles" ~ "Na h-Eileanan Siar",
      TRUE ~ main_operating_location
    )
  ) %>% 
  left_join(lad_lookup, by = "lad_name")

# Create proxies for main areas of coverage
charities_coverage <-
  charities_lad %>% 
  filter(coverage != "Overseas only") %>% 
  mutate(
    coverage = case_when(
      coverage == "A specific local point, community or neighbourhood" ~ "Community",
      coverage == "Wider, but within one local authority area" ~ "LAD",
      coverage == "UK and overseas" ~ "Scotland",
      coverage == "More than one local authority area in Scotland" ~ "LAD",
      coverage == "Scotland and other parts of the UK" ~ "Scotland",
      coverage == "Operations cover all or most of Scotland" ~ "Scotland",
      coverage == "One or a few bases or facilities serving people who come from a broad area" ~ "LAD"
    )
  )

# LAD VCS capacity model
# As we are interested in relative differences, orgs that have national
# coverage can be ommited
vcs_lad_count <-
  charities_coverage %>% 
  select(coverage, lad_code) %>% 
  filter(coverage == "Community" | coverage == "LAD") %>% 
  count(lad_code)

# Compare num of orgs to population size
vcs_presence <-
  vcs_lad_count %>% 
  left_join(pop) %>% 
  mutate(pop_thousands = pop_count/1000) %>% 
  mutate(vcs_presence_per_1000 = n/pop_thousands) %>% 
  select(lad_code, vcs_presence_per_1000)

# ---- Plot ----
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

shp <-
  boundaries_lad %>% 
  filter(str_detect(lad_code, "^S")) %>% 
  left_join(vcs_presence)

shp %>% 
    ggplot() +
    geom_sf(mapping = aes(fill = vcs_presence_per_1000),
            color = "black",
            size = 0.1) +
    scale_fill_viridis(
      breaks = seq(1, 11, by = 1),
      labels = seq(1, 11, by = 1),
      na.value = "transparent",
      option = "magma",
      name = expression(paste("No. VCS Orgs \nPer 1000 People")),
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

# Relationship between population and VCS org count
vcs_lad_count %>% 
  left_join(pop) %>% 
  mutate(pop_thousands = pop_count/1000) %>% 
  mutate(vcs_presence_per_1000 = n/pop_thousands) %>% 
  ggplot(aes(x = pop_thousands, y = vcs_presence_per_1000)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()+
  labs(x = "Population (thousands)", y = "Number of VCS Organisations")

vcs_presence %>% 
  ggplot(aes(x = vcs_presence_per_1000)) +
  geom_density(fill = "steelblue2", alpha = .3) +
  theme_minimal() +
  labs(x = "No. VCS Orgs Per 1000 People", y = NULL)
