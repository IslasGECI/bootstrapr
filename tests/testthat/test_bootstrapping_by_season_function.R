library("tidyverse")

function_uno <- function(datos) {
  return(1)
}
function_dos <- function(datos) {
  return(2)
}

expect_bootsrapping_by_season_equal_df <- function(scalar, resampling_number, datos, a_function) {
  unique_sesons <- sort(unique(datos$temporada))
  n_col <- length(unique_sesons)
  output_matrix <- matrix(rep(scalar, resampling_number * n_col), ncol = n_col)
  colnames(output_matrix) <- unique_sesons
  expected_df <- as_tibble(output_matrix)
  expect_equal(bootstrapping_by_season(datos, a_function, resampling_number), expected_df)
}

describe("Prueba que hace el remuestreo", {
  resampling_number <- 3
  temporada <- c(2018, 2019, 2018, 2019, 2019)
  interes <- c(12, 23, 34, 45, 56)
  datos <- tibble(temporada, interes)

  it("expect ones", {
    scalar <- 1
    a_function <- function_uno
    expect_bootsrapping_by_season_equal_df(scalar, resampling_number, datos, a_function)
  })
  it("expect twos", {
    scalar <- 2
    a_function <- function_dos
    expect_bootsrapping_by_season_equal_df(scalar, resampling_number, datos, a_function)
  })
  it("expect twos with three years", {
    temporada <- c(2018, 2019, 2018, 2019, 2019, 2017)
    interes_3 <- c(12, 23, 34, 45, 56, 11)
    datos <- tibble(temporada, interes_3)

    scalar <- 2
    a_function <- function_dos
    expect_bootsrapping_by_season_equal_df(scalar, resampling_number, datos, a_function)
  })
})
