library(tidyverse)

make_table <- function(data) {
  make_table_round(data, round = TRUE)
}

make_table_without_round <- function(data) {
  make_table_round(data, round = FALSE)
}

make_table_round <- function(data, round) {
  temporada <- names(data)
  table <- .setup_table(data)
  table <- .add_min_max_central_columns_round(table, round) %>%
    .select_right_columns(temporada)
  return(table)
}

.select_right_columns <- function(table, temporada) {
  table_with_right_columns <- table %>%
    cbind(temporada) %>%
    select(c("temporada", "central", "minimo", "maximo"))
  return(table_with_right_columns)
}

.add_min_max_central_columns_round <- function(table, round) {
  if (round) {
    return(.add_min_max_central_columns(table))
  }
  return(.add_min_max_central_columns_without_round(table))
}

.add_min_max_central_columns <- function(table) {
  round_number <- 2
  table <- table %>%
    mutate("minimo" = round(q3 - q1, round_number), "maximo" = round(q5 - q3, round_number), "central" = round(q3, round_number))
  return(table)
}

.add_min_max_central_columns_without_round <- function(table) {
  table <- table %>%
    mutate("minimo" = q3 - q1, "maximo" = q5 - q3, "central" = q3)
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
