library(tidyverse)

make_table <- function(data) {
  temporada <- names(data)
  ntemporadas <- length(temporada)
  table <- make_cuantiles(data, 1)
  round_number <- 2
  for (i in 2:ntemporadas) {
    quantil <- make_cuantiles(data, i)
    table <- rbind(table, quantil)
  }
  table <- table %>%
    mutate("minimo" = round(q3 - q1, round_number), "maximo" = round(q5 - q3, round_number), "central" = round(q3, round_number)) %>%
    cbind(temporada) %>%
    select(c("temporada", "central", "minimo", "maximo"))
  return(table)
}


make_table_without_round <- function(data) {
  temporada <- names(data)
  ntemporadas <- length(temporada)
  table <- make_cuantiles(data, 1)
  for (i in 2:ntemporadas) {
    quantil <- make_cuantiles(data, i)
    table <- rbind(table, quantil)
  }
  table <- table %>%
    mutate("minimo" = q3 - q1, "maximo" = q5 - q3, "central" = q3) %>%
    cbind(temporada) %>%
    select(c("temporada", "central", "minimo", "maximo"))
  return(table)
}
