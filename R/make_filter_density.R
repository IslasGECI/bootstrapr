#' Calcular densidad de madrigueras
#'
#' Calcula la densidad de madrigueras con actividad aparente por unidad de area.
#'
#' @param data Un data frame que contiene las columnas `Madrigueras_con_actividad_aparente`,
#'   `Area_del_cuadrante` y `Temporada`.
#'
#' @return Un data frame con las columnas `Temporada` y `densidad`.
#'
#' @export
make_filter_density <- function(data) {
  data <- data |>
    dplyr::mutate(densidad = Madrigueras_con_actividad_aparente / Area_del_cuadrante) |>
    dplyr::select(Temporada, densidad)
  return(data)
}
