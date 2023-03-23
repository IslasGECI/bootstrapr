Temporada <- c(2018, 2018, 2019, 2019, 2019)
no_interesa <- c(12, 23, 34, 45, 56)
interesa <- c(21, 32, 43, 54, 65)
datos <- tibble(Temporada, no_interesa, interesa)
datos_filtrados <- tibble(Temporada, interesa)

test_that("El filtrado sea el correcto", {
  expect_equal(make_filter(datos, "interesa"), datos_filtrados)
})
