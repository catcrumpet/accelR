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
  as.integer(tsibble::interval(acc_data)$second +
               tsibble::interval(acc_data)$minute * 60L)
}

#' Get timezone of accelerometer data
#'
#' @param acc_data Accelerometer tsibble object.
#' @return The timezone as a string value.
#' @export
get_timezone <- function(acc_data) {
  attr(acc_data$timestamp, "tzone")
}

#' Get timezone of accelerometer data
#'
#' @param acc_data Accelerometer tsibble object.
#' @param tz Timezone to change to.
#' @return Accelerometer data with updated timezone.
#' @export
change_timezone <- function(acc_data, tz = Sys.timezone()) {
  assert_that(tz %in% OlsonNames())
  attr(acc_data$timestamp, "tzone") <- tz
  acc_data
}