library(tidyverse)

make_table <- function(data) {
  temporada <- names(data)
  table <- .setup_table(data)
  round_number <- 2
  table <- .add_min_max_central_columns(table) %>%
    cbind(temporada) %>%
    select(c("temporada", "central", "minimo", "maximo"))
  return(table)
}

.add_min_max_central_columns <- function(table) {
  round_number <- 2
  table <- table %>%
    mutate("minimo" = round(q3 - q1, round_number), "maximo" = round(q5 - q3, round_number), "central" = round(q3, round_number))
  return(table)
}

.setup_table <- function(data) {
  ntemporadas <- ncol(data)
  table <- make_cuantiles(data, 1)
  for (i in 2:ntemporadas) {
    quantil <- make_cuantiles(data, i)
    table <- rbind(table, quantil)
  }
  return(table)
}

make_table_without_round <- function(data) {
  temporada <- names(data)
  table <- .setup_table(data)
  table <- table %>%
    mutate("minimo" = q3 - q1, "maximo" = q5 - q3, "central" = q3) %>%
    cbind(temporada) %>%
    select(c("temporada", "central", "minimo", "maximo"))
  return(table)
}
