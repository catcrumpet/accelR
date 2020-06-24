#' @export
calculate_magnitude <- function(...) {
  purrr::pmap_dbl(rlang::list2(...), ~sqrt(sum(c(...)^2)))
}

#' @export
add_magnitude <- function(acc_data, ..., magnitude = "magnitude") {
  dplyr::mutate(acc_data, !!magnitude := calculate_magnitude(!!rlang::enquo(...)))
}