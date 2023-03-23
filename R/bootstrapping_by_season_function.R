library("tidyverse")

bootstrapping_by_season <- function(data, applied_function, b = 2000) {
  set.seed(5)
  columns <- names(data)
  seasons <- levels(factor(data[[1]]))
  num_columns <- length(seasons)
  exit <- (matrix(rep(0, num_columns * b), ncol = num_columns))
  colnames(exit) <- seasons
  exit <- as_tibble(exit)
  for (i_season in seq(num_columns)) {
    season_statistic <- c()
    for (i_sample in seq(b)) {
      sample <- data %>%
        filter(.data[[columns[1]]] == seasons[i_season]) %>%
        sample_n(n(), replace = T)
      statistical <- applied_function(sample[[2]])
      season_statistic <- append(season_statistic, statistical)
    }
    exit[, i_season] <- season_statistic
  }
  return(exit)
}
