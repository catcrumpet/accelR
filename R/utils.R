#' Get settings from accelerometer data
#'
#' @param acc_data Accelerometer tsibble object.
#' @param setting Setting to extract as a string.
#' @return The value of the stored setting. Most of the time an atomic vector of length 1.
#' @export
get_setting <- function(acc_data, setting) {
  attributes(acc_data)$settings[[setting]]
}

#' List settings from accelerometer data
#'
#' @param acc_data Accelerometer tsibble object.
#' @return A list of stored setting names as a string vector.
#' @export
list_settings <- function(acc_data) {
  names(attributes(acc_data)$settings)
}

#' Get epoch length of accelerometer data
#'
#' @param acc_data Accelerometer tsibble object.
#' @return The epoch length in seconds as an integer value.
#' @export
get_epochlength <- function(acc_data) {
  as.integer(tsibble::interval(acc_data)$second)
}

#' Get timezone of accelerometer data
#'
#' @param acc_data Accelerometer tsibble object.
#' @return The timezone as a string value.
#' @export
get_timezone <- function(acc_data) {
  attr(acc_data$timestamp,"tzone")
}

calc_mag_ <- function(acc_data) {
  acc_data %>%
    tsibble::as_tibble() %>%
    select(starts_with("axis")) %>%
    purrr::pmap_dbl(~sqrt(sum(c(...) ^ 2)))
}

add_mag_ <- function(acc_data, magnitude = "magnitude") {
  mutate(acc_data, !!magnitude := calc_mag_(acc_data))
}

set_attr_ <- function(x, which, value) {
  `attr<-`(x, which, value)
}

reset_acc_attr_ <- function(new_data, old_data) {
  c("type", "settings") %>%
    purrr::map(~function(new, old) set_attr_(new, .x, attr(old, .x))) %>%
    purrr::reduce(~function(new, old) .x(new, old) %>% .y(old)) %>%
    {.(new_data, old_data)}
}

mutate_acc_ <- function(.data, ...) {
  reset_acc_attr_(mutate(.data, ...), .data)
}

expand_periods_ <- function(acc_data, periods) {
  purrr::map2(periods$.from, periods$.to,
              ~with(acc_data, .x >= timestamp & .y < timestamp)) %>%
    purrr::pmap_lgl(function(...) any(...))
}

add_period_ <- function(acc_data, periods, varname) {
  mutate_acc_(acc_data, !!varname := expand_periods_(acc_data, periods))
}