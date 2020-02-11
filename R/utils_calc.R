calculate_alpha_ <- function(x) {
  1 + (1 / mean(log(x / min(x)), na.rm = TRUE))
}