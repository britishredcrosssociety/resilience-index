# Load libraries and functions
library(tidyverse)
library(psych)
library(recipes)
library(corrr)
library(ggplot2)
library(GGally)
source("R/utils.R")

# Load indicators
wales_capacity_indicators <-
    load_indicators(
        path = "data/capacity/disasters-emergencies/wales",
        key = "lad_code"
    )
# plot_histogram(wales_capacity_indicators)

# 1. Scale indicator, high value = high capacity
# 2. Avoid skewness
wales_capacity_scaled <-
    wales_capacity_indicators %>%
    mutate(
        charities = log(charities),  # avoid skewness
        community_assets = sqrt(community_assets) * -1, # High value = high capacity and avoid skewness
        la_spending_power = sqrt(la_spending_power - min(la_spending_power) + 1) # avoid skewness
    )

print("skewness before:")
describe(wales_capacity_indicators)
print("skewness after:")
describe(wales_capacity_scaled)

# normalise indicator
wales_capacity_normlised <-
    wales_capacity_scaled  %>%
    normalise_indicators()

print("After normalised:")
describe(wales_capacity_normlised)
# plot_histogram(wales_capacity_normlised)



# Step1: Correlation check
## Sadly, no strong correlation :(
wales_capacity_normlised %>%
    select(where(is.numeric)) %>%
    correlate() %>%
    rearrange()

  cor.test(wales_capacity_normlised$charities, wales_capacity_normlised$la_spending_power, 
                      method = "pearson")

## Correlation plot
ggpairs(select(wales_capacity_normlised, where(is.numeric))) + theme_bw()

# ***********************************************
# Correlation is not strong, maximun is 0.582 between charities and la_spending_power
# Assume we can use PCA...
# ***********************************************

# PCA
wales_recipe <-
  recipe(~., data = wales_capacity_normlised) %>%
  update_role(lad_code, new_role = "id") %>%
  step_pca(all_predictors(), id = "pca") %>%
  prep()  

# Step2: indentify the number of latent facors
pca_variation_stat <-
  wales_recipe %>%
    tidy(id = "pca", type = "variance") %>%
    filter(
      terms ==  "cumulative percent variance" |
      terms  == "percent variance" |
      terms  == "variance"
    ) %>%
    pivot_wider(names_from = terms, values_from = value) %>% 
    select(-id) %>% 
    rename(
      percent_variance = `percent variance`,
      cumulative_percent_variance = `cumulative percent variance`
    )

# Step3: rotation of factos
pca_rotation <-
  wales_recipe %>%
  tidy(id = "pca") %>%
  pivot_wider(names_from = terms, values_from = value) %>%
  select(-id)

# Squared factor loading (scaled to unity sum)
squared_factor_loading <-
  pca_rotation %>%
  select(-component) %>%
  mutate_all(function(x) (x**2) / sum(x**2))

# Concat all pca statistical infomation together
pca_stat <-
  bind_cols(
    squared_factor_loading, pca_variation_stat
  )


# calculate the weight of each variable
pca_weight <-
  pca_stat %>%
  slice_head(n = 2) %>%  # only keep 2 components
  mutate(
    percent_variance = percent_variance / 100,  # convert to decimal
    charities = charities * percent_variance,
    community_assets = community_assets * percent_variance,
    la_spending_power = la_spending_power * percent_variance
  ) %>%
  select(charities, community_assets, la_spending_power) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
  
tt = as_tibble(cbind(nms = names(pca_weight), t(pca_weight)))

variable_weight <-
  tt %>%
  mutate(
    weight = as.numeric(V2)
  ) %>% 
  select(-V2) %>%
  select(where(is.numeric)) %>%
  as.matrix()

wales_matrix <- 
  wales_capacity_normlised %>%
  select(where(is.numeric)) %>% 
  as.matrix()
  
score =  wales_matrix %*% variable_weight

output <- 
  score %>% 
  cbind(wales_capacity_normlised["lad_code"]) %>% 
  mutate(rank = rank(weight)) %>% 
  mutate(
    deciles = quantise(
      rank,
      num_quantiles = 10,
      highest_quantile_worst = FALSE
    )
  ) %>%
  select(lad_code, deciles)

write_csv(
  output,
  "data/capacity/disasters-emergencies/wales/index-weighted.csv"
)

# capacity plot

library(sf)
library(geographr)
library(viridis)
# vulnerability visualization
shp <-
  boundaries_lad %>%
  filter(str_detect(lad_code, "^W"))

wales_capacity_shp <-
  shp %>%
  left_join(output, by = "lad_code")

wales_capacity_shp %>%
  select(deciles, geometry) %>%
  ggplot() +
  geom_sf(
    mapping = aes(fill = deciles),
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "magma",
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
  labs(title = "capacity deciles in Wales") +
  theme(plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"))



# Some plots

# 1.Examine how much variance each component accounts for
wales_recipe %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#b6dfe2") + 
  xlim(c(0, 5)) + 
  ylab("% of total variance")

# Examine how much cumulative variance each component accounts for
wales_recipe %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "cumulative percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#b6dfe2") + 
  xlim(c(0, 5)) + 
  ylab("cumulative percent variance")

# plot loadings by principal component
library(ggplot2)
library(tidytext)
wales_recipe %>%
  tidy(id = "pca") %>% 
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )