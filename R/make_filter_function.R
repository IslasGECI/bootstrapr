library(tidyverse)

make_filter <- function(data, interest) {
  data <- data %>% select(Temporada, interest)
}
