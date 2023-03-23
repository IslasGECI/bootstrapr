library("tidyverse")
Temporada <- c(2018, 2018, 2019, 2019, 2019)
Madrigueras_con_actividad_aparente <- c(12, 23, 34, 45, 56)
Area_del_cuadrante <- c(1, 1, 1, 1, 1)
datos <- tibble(Temporada, Madrigueras_con_actividad_aparente, Area_del_cuadrante)
datos_salida <- tibble(Temporada, densidad = Madrigueras_con_actividad_aparente)


test_that("Prueba que hace el remuestreo", {
  expect_equal(make_filter_density(datos), datos_salida)
})
