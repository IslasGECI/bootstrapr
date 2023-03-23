statistc_bootstrapping <- function(data_set, statistic, resampling_number = 2000) {
  set.seed(5)
  bootsttapped_statistic <- c()
  for (i_sample in seq(resampling_number)) {
    sample <- data_set %>% sample_n(n(), replace = T)
    statistical <- statistic(sample[[1]])
    bootsttapped_statistic <- append(bootsttapped_statistic, statistical)
  }
  return(bootsttapped_statistic)
}
