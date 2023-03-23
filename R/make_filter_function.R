library(tidyverse)

make_filter <- function(data, interest) {
  data <- data %>% select(Temporada, all_of(interest))
}
