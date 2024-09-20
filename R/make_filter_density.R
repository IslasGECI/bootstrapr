library(tidyverse)

#' @export
make_filter_density <- function(data) {
  data <- data %>%
    dplyr::mutate(densidad = Madrigueras_con_actividad_aparente / Area_del_cuadrante) %>%
    dplyr::select(Temporada, densidad)
  return(data)
}
