# ---- Load libraries ----
library(tidyverse)
library(broom)
library(classInt)

# ---- Functions----
#' Keep rows containing missing values
#'
#' @param .data A data frame
keep_na <-
  function(.data) {
    .data |>
      anti_join(drop_na(.data))
  }

#' Print all values in a tibble
#'
print_inf <-
  function(.data) {
    .data |>
      print(n = Inf)
  }

#' Filter geographical codes by a regex pattern
#'
#' @param .data A data frame
#' @param codes The column containing geographical codes
#' @param pattern Pattern to look for. Use a regular expression
filter_codes <-
  function(.data, codes, pattern) {
    filter(
      .data,
      str_detect({{ codes }}, {{ pattern }})
    )
  }

#' Download a file temporarily to disk
#'
#' @param url A URL for the request
#' @param file_extension A character vector detailing the file extension (e.g.,
#'         ".xlsx")
download_file <-
  function(url, file_extension) {
    stopifnot(
      !missing(url),
      !missing(file_extension),
      is.character(url),
      is.character(file_extension)
    )

    temp_path <-
      tempfile(fileext = file_extension)

    httr2::request(url) |>
      httr2::req_perform(
        path = temp_path
      )

    return(temp_path)
  }

#' Rank indicators with NAs first (i.e. 1 = worst)
#'
#' @param x Data to rank
rank_na_first <- function(x) rank(x, na.last = FALSE)

#' Inverse ranking with NAs first (i.e. 1 = best)
#'
#' @param x Data to rank
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)

#' Normalise ranks to a range between 0 and 1
#'
#' @param x Ranks to normalise
scale_ranks <- function(x) (x - 1) / (length(x) - 1)

#' Invert a vector (e.g., deciles, ranks, percentiles, etc.)
#' For example, with deciles, a score of 10 -> 1 and a score of 1 -> 10
#'
#' @param x Vector of data to invert
invert_this <- function(x) (max(x, na.rm = TRUE) + 1) - x

#' Normalise a vector where mean = 0 & SD = 1.
#'
#' @param x Vector of data to normalise
#' @param ignore_nas Whether should ignore missing values when normalising. The
#'        default is FALSE so flags these values. 
normalise <- 
  function(x, 
           ignore_nas = FALSE) {
    (x - mean(x, na.rm = ignore_nas)) / sd(x, na.rm = ignore_nas)
  } 

#' Quantise a vector of ranks
#'
#' @param vec The vector of ranks to quantise
#' @param num_quantiles The Number of quantiles
#' @param highest_quantile_worst Should a high quantile represent the worst
#'        outcome?
#' @return A vector containing the risk quantiles
quantise <-
  function(vec,
           num_quantiles = 10,
           highest_quantile_worst = TRUE) {
    if (length(unique(vec)) <= 1) {
      stop("The vector cannot be quantised as there is only one unique value.")
    }

    quantile_breaks <-
      classInt::classIntervals(
        vec,
        num_quantiles,
        style = "quantile"
      )

    quantiles <-
      as.integer(
        cut(
          vec,
          breaks = quantile_breaks$brks,
          include.lowest = TRUE
        )
      )

    if (!highest_quantile_worst) {
      max_quant <- max(quantiles, na.rm = TRUE)
      quantiles <- (max_quant + 1) - quantiles
    }

    if (
      !(
        tibble(quantiles = quantiles) |>
          count(quantiles) |>
          mutate(
            equal_bins = if_else(
              n >= (length(vec) / num_quantiles) - 1 &
                n <= (length(vec) / num_quantiles) + 1,
              TRUE,
              FALSE
            )
          ) |>
          pull(equal_bins) |>
          all()
      )

    ) {
      warning("Quantiles are not in equal bins.")
    }

    return(quantiles)
  }

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
#' The direction of the scale of data inputted by the functioin always matches
#' that of the data outputted. For example if data is inputted into the function
#' where high scores equals high vulnerability, the outputted data set will hold
#' this relationship true.
#' 
#' @param data Data frame containing a variable to be aggregated, lower level
#'        geography population estimates, and a higher level geographical
#'        grouping variable
#' @param var Name of the variable in the data frame containing the variable to
#'        be aggregated (e.g., score) for the lower level geography
#' @param higher_level_geography Name of the variable in the data frame
#'        containing the higher level geography names/codes
#' @param population Name of the variable in the data frame containing
#'        the population estimates of the lower level geography
#' @param weight_high_scores If TRUE higher scores are weighted, else lower
#'        scores are weighted. For indicators like 'Alchol Misuse' and 'Ambulance Wait
#'        Time' this should be set to TRUE. This is because higher values in these
#'        outcomes indicate worse outcomes (higher vulnerability and lower capacity)
#'        and this is where the weighting should be focused. For indicators like
#'        'Physical Activity' and 'Bed Availability' it should be set to FALSE. This is
#'        because lower values in these outcomes indicate worse outcomes (higher
#'        vulnerability and lower capacity) and this is where the weighting should be 
#'        focused.
calculate_extent <-
  function(data,
           var,
           higher_level_geography,
           population,
           weight_high_scores = TRUE) {
    data <-
      data |>
      mutate(percentile = ntile({{ var }}, 100))

    if (weight_high_scores) {
      data <-
        data |>
        mutate(percentile = invert_this(percentile))
    }

    data <-
      data |>
      mutate(
        extent = case_when(
          percentile <= 10 ~ {{ population }},
          percentile == 11 ~ {{ population }} * 0.95,
          percentile > 11 & percentile <= 30 ~ {{ population }} * (0.95 - ((0.9 / 19) * (percentile - 11))),
          TRUE ~ 0
        )
      ) |>
      group_by({{ higher_level_geography }}) |>
      summarise(extent = sum(extent) / sum({{ population }}))

    if (!weight_high_scores) {
      data <-
        data |>
        mutate(extent = extent * -1)
    }

    return(data)
  }

#' ================================ DEPRECIATED ================================
#' This function should no longer be used, and is only kept here
#' for legacy reasons until code is updated with the new and revised version
#' =============================================================================
#' 
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
#' @param higher_level_geography Name of the variable in the data frame
#'        containing the higher level geography names/codes
#' @param population Name of the variable in the data frame containing
#'        the population estimates of the lower level geography
#' @param invert_percentiles Should percentiles be inverted? Should be set to
#'        TRUE when a higher variable score equates to a worse outcome
calculate_extent_depreciated <-
  function(data,
           var,
           higher_level_geography,
           population,
           invert_percentiles = TRUE) {
    data <-
      data |>
      mutate(percentile = ntile({{ var }}, 100))

    if (invert_percentiles) {
      data <-
        data |>
        mutate(percentile = invert_this(percentile))
    }

    data <-
      data |>
      mutate(
        extent = case_when(
          percentile <= 10 ~ {{ population }},
          percentile == 11 ~ {{ population }} * 0.95,
          percentile > 11 & percentile <= 30 ~ {{ population }} * (0.95 - ((0.9 / 19) * (percentile - 11))),
          TRUE ~ 0
        )
      ) |>
      group_by({{ higher_level_geography }}) |>
      summarise(extent = sum(extent) / sum({{ population }}))

    return(data)
  }

#' Load indicators saved as .rds files within a folder into a tibble, joined by
#' a key
#' @param path Relative path to the folder containing the .rds files to be read
#' @param key The key to join the variables
load_indicators <-
  function(path, key) {
    file_list <-
      dir(
        path,
        pattern = ".rds",
        full.names = TRUE
      )
    file_list |>
      map(read_rds) |>
      reduce(
        left_join,
        by = key
      )
  }

#' Normalise indicators to Mean = 0, SD = 1. This function will calculate over
#' all numeric variables in a dataframe.
#'
#' @param data Data frame containing indicators to normalise.
#' @param ignore_nas Whether should ignore missing values when normalising. The
#'        default is FALSE so flags these values and is an input in the 
#'        normalise function. 
normalise_indicators <-
  function(data,
           ignore_nas = FALSE) {
    data <-
      data |>
      mutate(across(where(is.numeric), ~normalise(.x, ignore_nas)))

    return(data)
  }

#' Weight indicators within a domain using MFLA. This function will calculate
#' over all numeric variables in a dataframe.
#'
#' Method:
#'  1. Normalise each indicator to Mean = 0, SD = 1
#'  2. Perform MLFA and extract weights for that domain
#'  3. Multiply model weights by respective column to get weighted indicators
#'
#' @param data Data frame containing indicators to be weighted
weight_indicators_mfla <-
  function(data) {
    data <-
      data |>
      mutate(across(where(is.numeric), normalise))

    weights <-
      data |>
      select(where(is.numeric)) |>
      factanal(factors = 1) |>
      tidy() |>
      select(-uniqueness, weights = fl1) |>
      mutate(
        weights = abs(weights),
        weights = weights / sum(weights)
      )

    weighted_indicators <-
      data |>
      select(weights$variable) |>
      map2_dfc(weights$weights, `*`) |>
      bind_cols(data |> select(!where(is.numeric))) |>
      relocate(!where(is.numeric))

    return(weighted_indicators)
  }

#' Calculate domain scores, ranks, and quantiles from normalised indicators. The
#' indicators can be weighted or unweighted. This function will calculate over
#' all numeric variables in a dataframe.
#'
#' @param data Data frame containing weighted indicators
#' @param domain_name A string identifier to prefix to column names. Use
#'        snake_case.
#' @param quantiles The Number of quantiles
#' @param ignore_nas Whether should ignore missing values when summing across 
#'        the row. The default is FALSE so flags these values.
calculate_domain_scores <-
  function(data, 
           domain_name, 
           num_quantiles = 10,
           ignore_nas = FALSE) {
    data <-
      data |>
      rowwise(!where(is.numeric)) |>
      summarise(domain_score = sum(c_across(where(is.numeric)), na.rm = ignore_nas)) |>
      ungroup() |>
      mutate(domain_rank = rank(domain_score)) |>
      mutate(domain_quantiles = quantise(domain_rank, num_quantiles)) |>
      rename_with(
        ~ str_c(domain_name, .x, sep = "_"),
        where(is.numeric)
      )

    return(data)
  }

#' Calculate composite scores, ranks, and quantiles from domain scores. This
#' function will calculate over all numeric variables in a dataframe.
#'
#' @param data Data frame containing domain scores
#' @param index_name A string identifier to prefix to column names. Use
#'        snake_case.
#' @param quantiles The Number of quantiles
calculate_composite_score <-
  function(data, index_name, num_quantiles = 10) {
    data <-
      data |>
      mutate(across(where(is.numeric), rank)) |>
      mutate(across(where(is.numeric), scale_ranks)) |>
      rowwise(!where(is.numeric)) |>
      summarise(composite_score = sum(c_across(where(is.numeric)))) |>
      ungroup() |>
      mutate(composite_rank = rank(composite_score)) |>
      mutate(composite_quantiles = quantise(composite_rank, num_quantiles)) |>
      rename_with(
        ~ str_c(index_name, .x, sep = "_"),
        where(is.numeric)
      )

    return(data)
  }

# ---- Themes ----
theme_map <-
  function(...) {
    theme_minimal() +
      theme(
        text = element_text(family = "FiraCode-Retina", color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        legend.background = element_rect(fill = "#ffffff", color = NA),
        panel.border = element_blank(),
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
