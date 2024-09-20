library(tidyverse)

make_filter <- function(data, interest) {
  data <- data %>% dplyr::select(Temporada, all_of(interest))
}
