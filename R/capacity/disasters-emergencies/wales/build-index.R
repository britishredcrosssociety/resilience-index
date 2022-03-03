# Load libraries and functions
library(tidyverse)
library(psych)
library(recipes)
source("R/utils.R")

# Load indicators
wales_capacity_indicators <-
    load_indicators(
        path = "data/capacity/disasters-emergencies/wales",
        key = "lad_code"
    )
plot_histogram(wales_capacity_indicators)

# Scale indicator, high value = high capacity
wales_capacity_scaled <-
    wales_capacity_indicators %>%
    mutate(
        # avoid skewness, access_to_internet is still moderate skewed
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
plot_histogram(wales_capacity_normlised)

# PCA
library(corrr)
wales_capacity_normlised %>% 
    select(where(is.numeric)) %>%
    correlate() %>% 
    rearrange()

# ***
# Correlation is not strong, maximun is 0.582 between charities and la_spending_power
# ***


# wales_recipe <-
#   recipe(~., data = wales_capacity_normlised) %>%
#   update_role(lad_code, new_role = "id") %>% 
#   step_naomit(all_predictors()) %>% 
#   step_normalize(all_predictors()) %>%
#   step_pca(all_predictors(), id = "pca") %>% 
#   prep()

# wales_pca <- 
#   wales_recipe %>% 
#   tidy(id = "pca") 

# wales_capacity_normlised %>% 
#   select(where(is.numeric)) %>% 
#   drop_na() %>% 
#   scale() %>% 
#   prcomp() %>%  
#   .$rotation

# # Examine how much variance each component accounts for
# wales_recipe %>% 
#   tidy(id = "pca", type = "variance") %>% 
#   filter(terms == "percent variance") %>% 
#   ggplot(aes(x = component, y = value)) + 
#   geom_col(fill = "#b6dfe2") + 
#   xlim(c(0, 5)) + 
#   ylab("% of total variance")

# # plot loadings by principal component
# library(ggplot2)
# library(tidytext)
# wales_pca %>%
#   mutate(terms = reorder_within(terms, abs(value), component)) %>%
#   ggplot(aes(abs(value), terms, fill = value > 0)) +
#   geom_col() +
#   facet_wrap(~component, scales = "free_y") +
#   scale_y_reordered() +
#   scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
#   labs(
#     x = "Absolute value of contribution",
#     y = NULL, fill = "Positive?"
#   )

# # get pca loadings into wider format
# wales_wider <- wales_pca %>%
#   pivot_wider(names_from = component, id_cols = terms)

# # define arrow style
# arrow_style <- arrow(length = unit(.05, "inches"),
#                      type = "closed")


# # Plot PCA loadings + scores
# pca_plot <-
#   juice(wales_recipe) %>%
#   ggplot(aes(PC1, PC2)) +
#   geom_point(aes(color = lad_code, shape = lad_code), 
#              alpha = 0.8, 
#              size = 2) +
#   scale_colour_manual(values = c("darkorange","purple","cyan4")) 

# pca_plot +
#   geom_segment(data = wales_wider,
#                aes(xend = PC1, yend = PC2), 
#                x = 0, 
#                y = 0, 
#                arrow = arrow_style) + 
#   geom_text(data = wales_wider,
#             aes(x = PC1, y = PC2, label = terms), 
#             hjust = 0, 
#             vjust = 1,
#             size = 5, 
#             color = '#0A537D') 
