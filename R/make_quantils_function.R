library(tidyverse)

make_cuantiles <- function(data, i) {
  alpha <- 0.05
  half_alpha <- alpha / 2
  quantiles <- summarise(data,
    "q1" = quantile(data[[i]], half_alpha),
    "q3" = quantile(data[[i]], 0.5),
    "q5" = quantile(data[[i]], 1 - half_alpha)
  )
  return(quantiles)
}
