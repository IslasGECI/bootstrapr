library("tidyverse")

unos <- tibble("col1" = c(1, 1, 1), "col2" = c(1, 1, 1), "col3" = c(1, 1, 1))
diferentes <- tibble("col1" = c(1, 1, 1), "col2" = c(2, 2, 2), "col3" = c(2, 2, 2))

test_that("Hace las diferencias de entre cada temporada", {
  expect_equal(make_combination_difference(unos), c(1, 1, 1))
  expect_equal(make_combination_difference(diferentes), c(0, 0, 1))
})
