library("tidyverse")

unos <- tibble("interes" = c(1, 1, 1))
function_uno <- function(datos) {
  return(1)
}
function_dos <- function(datos) {
  return(2)
}
resampling_number <- 3
test_that("Prueba que hace el remuestreo", {
  expect_equal(statistc_bootstrapping(unos, function_uno, resampling_number), rep(1, resampling_number))
  expect_equal(statistc_bootstrapping(unos, function_dos, resampling_number), rep(2, resampling_number))
})
