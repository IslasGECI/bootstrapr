library(tidyverse)

get_p_value_for_difference <- function(before_distribution, after_distribution) {
  validate_input(before_distribution, after_distribution)
  set.seed(5)
  n_distribution <- nrow(before_distribution)
  before_sample <- sample_n(before_distribution, n_distribution)
  after_sample <- sample_n(after_distribution, n_distribution)
  differences <- before_sample[[1]] - after_sample[[1]]
  positive_difference <- sum(differences > 0) / n_distribution
  negative_difference <- sum(differences < 0) / n_distribution
  p_value <- 1 - abs(positive_difference - negative_difference)
  return(p_value)
}

validate_input <- function(first_tibble, second_tibble) {
  is_tibble_first <- "tbl" %in% class(first_tibble)
  testthat::expect_true(is_tibble_first, "First argument should be a tibble")
  is_tibble_second <- "tbl" %in% class(second_tibble)
  testthat::expect_true(is_tibble_second, "Second argument should be a tibble")
  n_first <- nrow(first_tibble)
  n_second <- nrow(second_tibble)
  testthat::expect_equal(n_first, n_second, info = "Tibbles should be of the same length")
}
