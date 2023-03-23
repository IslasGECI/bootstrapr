make_combination_difference <- function(data_set) {
  p_value_difference <- c()
  n_col <- ncol(data_set) - 1
  for (i in 1:n_col) {
    for (j in i:n_col) {
      p_value <- get_p_value_for_difference(data_set[, i], data_set[, (j + 1)])
      p_value_difference <- append(p_value_difference, round(p_value, 3))
    }
  }
  return(p_value_difference)
}
