library(tidyverse)

#' Rank indicators with NAs first (i.e. 1 = worst)
#' @param x Data to rank

rank_na_first <- function(x) rank(x, na.last = FALSE)

#' Inverse ranking with NAs first (i.e. 1 = best)
#' @param x Data to rank

inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)

#' Normalise ranks to a range between 0 and 1
#' @param x Ranks to normalise

scale_ranks <- function(x) (x - 1) / (length(x) - 1)

#' Invert a vector (e.g., deciles, ranks, percentiles, etc.)
#' For example, with deciles, a score of 10 -> 1 and a score of 1 -> 10
#' @param x Vector of data to invert

invert_this <- function(x) (max(x, na.rm = TRUE) + 1) - x

#' Calculate the 'extent' scores when aggreating up small areas
#'
#' "Extent" is the proportion of the local population that live in areas
#' classified as among the most deprived in the higher geography. The
#' calculation of extent is taken from the IMD technical report Appendix N:
#'
#' "The population living in the most deprived 11 to 30 per cent of Lower-layer
#' Super Output Areas receive a sliding weight, ranging from 0.95 for those in
#' the most deprived eleventh percentile, to 0.05 for those in the most deprived
#' thirtieth percentile. In practice this means that the weight starts from 0.95
#' in the most deprived eleventh percentile, and then decreases by
#' (0.95-0.05)/19 for each of the subsequent nineteen percentiles until it
#' reaches 0.05 for the most deprived thirtieth percentile, and zero for areas
#' outside the most deprived 30 per cent"
#'
#' @param data Data frame containing a variable to be aggregated, lower level
#'        geography population estimates, and a higher level geographical
#'        grouping variable
#' @param var Name of the variable in the data frame containing the variable to
#'        be aggregated (e.g., score) for the lower level geography
#' @param higher_level_geography Name of the variable in the data frame containing the
#'        higher level geography names/codes
#' @param population Name of the variable in the data frame containing
#'        the population estimates of the lower level geography

aggregate_extent <-
  function(data,
           var,
           higher_level_geography,
           population) {
    data %>%
      mutate(percentile = ntile({{ var }}, 100)) %>%
      mutate(
        extent = case_when(
          percentile <= 10 ~ {{ population }},
          percentile == 11 ~ {{ population }} * 0.95,
          percentile > 11 & percentile <= 30 ~ {{ population }} * (0.95 - ((0.9 / 19) * (percentile - 11))),
          TRUE ~ 0
        )
      ) %>% 
      group_by({{ higher_level_geography }}) %>% 
      summarise(extent = sum(extent) / sum({{ population }})
  }