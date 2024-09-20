make_filter <- function(data, interest) {
  data <- data |> dplyr::select(Temporada, dplyr::all_of(interest))
}
