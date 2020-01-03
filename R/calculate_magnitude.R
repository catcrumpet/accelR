#' @export
calculate_magnitude <- function(...) {
  purrr::pmap_dbl(list2(...), ~sqrt(sum(c(...)^2)))
}

#' @export
add_magnitude <- function(acc_data, ..., magnitude = "magnitude") {
  acc_data %>%
    mutate_acc_(!!magnitude :=
                  calculate_magnitude(!!enquo(...)))
}