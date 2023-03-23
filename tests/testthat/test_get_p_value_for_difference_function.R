library("tidyverse")

ones <- tibble(c(1, 1, 1))
twos <- tibble(c(2, 2, 2))
almost_ones <- tibble(c(0.99, 0.99, 0.99, 0.99))
no_ones <- tibble(c(1.01, 0.99, 0.99, 0.99))

test_that("Los valores de p valor son los correctos", {
  expect_equal(get_p_value_for_difference(ones, ones), c(1))
  expect_equal(get_p_value_for_difference(twos, ones), c(0))
  expect_equal(get_p_value_for_difference(ones, twos), c(0))
  expect_equal(get_p_value_for_difference(almost_ones, no_ones), c(0.75))
})

describe("validate_input()", {
  it("should PASS if both inputs are tibbles", {
    expect_no_error(validate_input(tibble(), tibble()))
  })
  it("should FAIL if first input is not a tibble", {
    expect_error(validate_input(1, tibble()))
  })
  it("should FAIL if second input is not a tibble", {
    expect_error(validate_input(tibble(), 1))
  })
  it("should FAIL if tibbles have different number of columns", {
    expect_no_error(validate_input(ones, twos))
  })
  it("should FAIL if tibbles have different number of columns", {
    expect_error(validate_input(ones, no_ones))
  })
})
