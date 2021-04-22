#' Rank indicators with NAs first (i.e. 1 = worst)
#' @param x Data to rank

rank_na_first <- function(x) rank(x, na.last = FALSE)

#' Inverse ranking with NAs first (i.e. 1 = best)
#' @param x Data to rank

inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)

#' Normalise ranks to a range between 0 and 1
#' @param x Ranks to normalise

scale_ranks <- function(x) (x - 1) / (length(x) - 1)