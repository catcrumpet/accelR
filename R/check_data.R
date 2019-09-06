check_data_starttime <- function(acc_data) {
  if (first(acc_data$timestamp) !=
      ext_setting_(acc_data, "startdatetime")) {
    stop("Data start time does not match setting start time.")
  }
  invisible(TRUE)
}

check_data_stoptime <- function(acc_data) {
  if ((last(acc_data$timestamp) +
       lubridate::seconds(attr(acc_data, "epochlength"))) !=
      ext_setting_(acc_data, "stopdatetime")) {
    stop("Data stop time does not match setting stop time.")
  }
  invisible(TRUE)
}

check_data_gaps <- function(acc_data) {
  if (any(tsibble::has_gaps(acc_data)$.gaps)) {
    stop("Data contains gaps in observations.")
  }
  invisible(TRUE)
}

check_data_epochlength <- function(acc_data) {

  assert_that(has_attr(acc_data, "epochlength"))

  if (!tsibble::interval(acc_data)$second == attr(acc_data, "epochlength")) {
    stop("Epoch length does not match data interval.")
  }
  if (60 %% attr(acc_data, "epochlength")) {
    stop("Epochs should be exact divisors of 60.")
  }
  invisible(TRUE)
}

check_data_missing <- function(acc_data, var) {
  if (anyNA(acc_data[[var]])) {
    stop("Data contains missing observations on ", var, ".")
  }
  invisible(TRUE)
}

check_data_names <- function(acc_data, var) {
  assert_that(has_name(acc_data, "timestamp"),
              msg = "Data missing timestamp column.")
  assert_that(has_name(acc_data, "axis1"),
              msg = "Data missing axis1 column.")
  invisible(TRUE)
}


