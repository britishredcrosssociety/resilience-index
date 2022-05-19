# Fire station response time is not included for now (17/03) as don't have LTLA level data 
# (only FRA level). Include again when get this from Gov. 
# ---- Load libraries and Functions ----
library(tidyverse)
library(demographr)
library(geographr)

source("R/utils.R")

# Load indicators ----
indicators <-
  load_indicators(
    path = "data/capacity/disasters-emergencies/scotland",
    key = "lad_code"
  )

# Check entry for all 2020 LAD codes
lads_20 <- lookup_postcode_oa11_lsoa11_msoa11_ltla20 |>
   filter(str_detect(ltla20_code, "^S")) |>
   distinct(lad_code = ltla20_code) 

lads_20 |>
  anti_join(indicators, by = "lad_code")

indicators |>
  anti_join(lads_20, by = "lad_code")

# Check for NAs
indicators |>
  dplyr::filter(if_any(everything(), ~is.na(.x))) 

# Align direction so that high score = low capacity 
indicators_aligned <- indicators |>
  mutate(cap_exp_person = cap_exp_person * -1)

# Check normality of indicators
indicators_aligned |>
  pivot_longer(civic_assets_extent:cap_exp_person, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(vars(variable), ncol = 3, scales = "free")

for(i in 2:4) {
  print(colnames(indicators[, i]))
  print(shapiro.test(indicators[[i]])$p.value)
  if (shapiro.test(indicators[[i]])$p.value < 0.05) {
    print("Not normally distributed")
  }
  else {
    print("Normally distributed")
  }
  cat("\n")  
  
}

#all the variables are not normally distributed

# Exponential transformation ----

ranked <- indicators_aligned |>
  mutate_if(is.numeric, rank)

scale01 <- function(x) (x - min(x))/diff(range(x))

scale_ind <- ranked |>
  mutate_if(is.numeric, scale01)

exponential = function(x) (-23*log(1-x*(1-exp(1)^(-100/23))))

exp_ind <- scale_ind |>
  mutate_if(is.numeric, exponential)

for(i in 2:4) {
  print(colnames(exp_ind[, i]))
  print(shapiro.test(exp_ind[[i]])$p.value)
  if (shapiro.test(exp_ind[[i]])$p.value < 0.05) {
    print("Not normally distributed")
  }
  else {
    print("Normally distributed")
  }
  cat("\n")  
  
}

library(psych)
for(i in 2:4) {
  print(colnames(exp_ind[, i]))
  print(skew(exp_ind[[i]]))
  cat("\n")  
  
}

transformed <- exp_ind |>
  mutate(civic_assets_extent = log10(civic_assets_extent+1),
         engagement_extent = log10(engagement_extent+1),
         cap_exp_person = log10(cap_exp_person+1))

for(i in 2:4) {
  print(colnames(transformed[, i]))
  print(shapiro.test(transformed[[i]])$p.value)
  if (shapiro.test(transformed[[i]])$p.value < 0.05) {
    print("Not normally distributed")
  }
  else {
    print("Normally distributed")
  }
  cat("\n")  
  
}

# After the transformation engagement_extent is still not normally distributed

# Normalise indicators
normalised <- transformed |>
  normalise_indicators(ignore_nas = T)

normalised |>
  pivot_longer(civic_assets_extent:cap_exp_person, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(vars(variable), ncol = 3, scales = "free")

# MFLA
standardised = function(x) (x - mean(x))/sd(x)
rank2 = function(x) rank(x, na.last = FALSE)

mfla_score <- function(d) {
  
  # Rank and normalise indicators to mean 0, SD 1.
  d <- d %>%
    mutate_if(is.numeric, list(scaled = function(x) standardised(rank2(x))))
  
  # Extract weights
  d_weights <- d %>%
    select(ends_with("_scaled")) %>%
    factanal(factors = 1) %>%
    tidy() %>%
    select(-uniqueness, weights = fl1) %>%
    mutate(weights = abs(weights),
           weights = weights/sum(weights))
  
  # Multiply model weights by respective column to get weighted indicators
  d_weighted_ind <- d %>%
    select(d_weights$variable) %>%
    map2_dfc(d_weights$weights, `*`) %>%
    select_all(list(~ str_remove(., "_scaled"))) %>%
    select_all(list(~ str_c(., "_weighted")))
  
  # Combine weighted indicators with original data
  d <- bind_cols(d, d_weighted_ind)
  
  # Sum weighted indicators
  d <- d %>%
    mutate(mfla_score = reduce(select(., ends_with("_weighted")), `+`))
  
  # Return data
  return(d)
  
}

#Apply the function
indicators_mfla <- normalised |>
  mfla_score() |>
  select(lad_code,
         mfla_score)

# Domain scores
indicators_scores <- indicators_mfla |>
  calculate_domain_scores(domain_name = "capacity")

# Inverting ranks and deciles so that higher scores = higher capacity
indicators_invert <- indicators_scores |>
  mutate(capacity_domain_rank = inverse_rank(capacity_domain_rank),
         capacity_domain_quantiles = invert_this(capacity_domain_quantiles)) |>
  select(lad_code,
         deciles = capacity_domain_quantiles)

# Save index
indicators_invert |>
  write_csv("data/capacity/disasters-emergencies/scotland/capacity-index.csv")