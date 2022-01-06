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
    .data %>%
      anti_join(drop_na(.data))
  }

#' Print all values in a tibble
#'
print_inf <-
  function(.data) {
    .data %>%
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
normalise <- function(x) (x - mean(x)) / sd(x)

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
        tibble(quantiles = quantiles) %>%
          count(quantiles) %>%
          mutate(
            equal_bins = if_else(
              n >= (length(vec) / num_quantiles) - 1 &
                n <= (length(vec) / num_quantiles) + 1,
              TRUE,
              FALSE
            )
          ) %>%
          pull(equal_bins) %>%
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
calculate_extent <-
  function(data,
           var,
           higher_level_geography,
           population,
           invert_percentiles = TRUE) {
    data <-
      data %>%
      mutate(percentile = ntile({{ var }}, 100))

    if (invert_percentiles) {
      data <-
        data %>%
        mutate(percentile = invert_this(percentile))
    }

    data <-
      data %>%
      mutate(
        extent = case_when(
          percentile <= 10 ~ {{ population }},
          percentile == 11 ~ {{ population }} * 0.95,
          percentile > 11 & percentile <= 30 ~ {{ population }} * (0.95 - ((0.9 / 19) * (percentile - 11))),
          TRUE ~ 0
        )
      ) %>%
      group_by({{ higher_level_geography }}) %>%
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
    file_list %>%
      map(read_rds) %>%
      reduce(
        left_join,
        by = key
      )
  }

#' Normalise indicators to Mean = 0, SD = 1. This function will calculate over
#' all numeric variables in a dataframe.
#'
#' @param data Data frame containing indicators to normalise.
normalise_indicators <-
  function(data) {
    data <-
      data %>%
      mutate(across(where(is.numeric), normalise))

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
      data %>%
      mutate(across(where(is.numeric), normalise))

    weights <-
      data %>%
      select(where(is.numeric)) %>%
      factanal(factors = 1) %>%
      tidy() %>%
      select(-uniqueness, weights = fl1) %>%
      mutate(
        weights = abs(weights),
        weights = weights / sum(weights)
      )

    weighted_indicators <-
      data %>%
      select(weights$variable) %>%
      map2_dfc(weights$weights, `*`) %>%
      bind_cols(data %>% select(!where(is.numeric))) %>%
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
calculate_domain_scores <-
  function(data, domain_name, num_quantiles = 10) {
    data <-
      data %>%
      rowwise(!where(is.numeric)) %>%
      summarise(domain_score = sum(c_across(where(is.numeric)))) %>%
      ungroup() %>%
      mutate(domain_rank = rank(domain_score)) %>%
      mutate(domain_quantiles = quantise(domain_rank, num_quantiles)) %>%
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
      data %>%
      mutate(across(where(is.numeric), rank)) %>%
      mutate(across(where(is.numeric), scale_ranks)) %>%
      rowwise(!where(is.numeric)) %>%
      summarise(composite_score = sum(c_across(where(is.numeric)))) %>%
      ungroup() %>%
      mutate(composite_rank = rank(composite_score)) %>%
      mutate(composite_quantiles = quantise(composite_rank, num_quantiles)) %>%
      rename_with(
        ~ str_c(index_name, .x, sep = "_"),
        where(is.numeric)
      )

    return(data)
  }


#' Survey data download and filtering for CQC surveys (England health capacity indicator)
#'
#' @param url url to data download (assumes ODS file)
#' @param question_num Which question in the survey is the ‘overall’ question
#' @param question_coding Which encoding used in the sheet (for outpatient_minor_inj seems to be 't1', 't2' etc. rather than 'q1' etc. which is what is meant to be from the key)
#' @param category What survey is it (mental health etc.) so can have in column names


survey_data_download <- function(url, question_num, sheet_name, question_coding, category) {
  tf <- download_file(url, "ods")
  
  raw <-
    read_ods(
      tf,
      sheet = sheet_name,
    )
  
  q_col_name <- paste0(question_coding, question_num)
  new_num_respon_name <- paste0("num_respon_", category)
  
  subset_data <- raw |>
    rename_with(tolower) |>
    select(trust_code = trustcode, trustname, n_tpat, contains(q_col_name)) |>
    rename(!!new_num_respon_name := n_tpat) |>
    rename_with(str_replace_all, pattern = q_col_name, replacement = category)
  
  return(subset_data)
}


#' Updating Trusts in the CQC survey data (England health capacity indicator)
#'
#' @param data survey data created from survey_data_download() function
#' @param response_column column name of the number of responses
#' @param mean_column column name of the mean survey value
#' @param open_trusts_data dataset with open trusts saved R/capacity/health-inequalities/england/trust_calculations/open_trust_types.feather
#' @param trust_changes_data dataset with trust changes saved R/capacity/health-inequalities/england/trust_calculations/trust_changes.feather

updating_trusts_for_survey_data <- function(data, response_column, mean_column, open_trusts_data, trust_changes_data) {
  
  data_selected <- data |>
    select(trust_code, num_respon = {{ response_column }}, mean = {{ mean_column }})
  
  old_new_lookup <- data_selected |>
    anti_join(open_trusts_data) |>
    rename(old_code = trust_code) |>
    inner_join(trust_changes_data, by = "old_code") |>
    group_by(new_code) |>
    mutate(new_code_count = n()) |>
    ungroup() |>
    group_by(old_code) |>
    mutate(old_code_count = n()) |>
    ungroup()
  
  
  if (max(old_new_lookup$old_code_count) > 1) {
    
    stop("Trust has been split to two different new Trusts")
    
  } else {
    new_trusts <- old_new_lookup |>
      group_by(new_code) |>
      mutate(weight = num_respon / sum(num_respon), weighted_mean = weight * mean) |>
      group_by(new_code) |>
      summarise(mean = sum(weighted_mean), num_respon = sum(num_respon)) |>
      select(trust_code = new_code, num_respon, mean)
    
    data_updated <- data_selected |>
      semi_join(open_trusts) |>
      bind_rows(new_trusts)
    
    # Average any duplicates Trust data caused by Trust changes
    data_updated_combined <- data_updated |>
      group_by(trust_code) |>
      mutate(weight = num_respon / sum(num_respon), weighted_mean = weight * mean) |>
      group_by(trust_code) |>
      summarise(mean = sum(weighted_mean), num_respon = sum(num_respon)) |>
      select(trust_code, num_respon, mean)
    
    return(data_updated_combined)
  }
}
