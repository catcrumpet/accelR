#' @export
add_magnitude <- function(acc_data, magnitude = "magnitude") {
  axis_vars <- name_acc_type_(acc_data, "axis")

  acc_data %>%
    mutate_acc_(!!magnitude :=
                  calculate_magnitude(!!!acc_data[, axis_vars]) %>%
                  set_acc_attr_("magnitude", components = axis_vars))
}

#' @export
calculate_magnitude <- function(...) {
  purrr::pmap_dbl(list2(...), ~sqrt(sum(c(...)^2)))
}

use_magnitude_ <- function(acc_data) {
  if (any(is_acc_type_(acc_data, "magnitude"))) {
    acc_data
  } else {
    message("No extant magnitude column. Adding to data.")
    add_magnitude(acc_data)
  }
}