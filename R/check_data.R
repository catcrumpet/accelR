
# chk_d_start_
check_data_starttime <- function(acc_data) {
  if (first(acc_data$timestamp) !=
      get_setting(acc_data, "startdatetime")) {
    stop("Data start time does not match setting start time.")
  }
  invisible(TRUE)
}

# chk_d_stop_
check_data_stoptime <- function(acc_data) {
  if ((last(acc_data$timestamp) +
       lubridate::seconds(get_epochlength(acc_data))) !=
      get_setting(acc_data, "stopdatetime")) {
    stop("Data stop time does not match setting stop time.")
  }
  invisible(TRUE)
}

# chk_d_gaps_
check_data_gaps <- function(acc_data) {
  if (any(tsibble::has_gaps(acc_data)$.gaps)) {
    stop("Data contains gaps in observations.")
  }
  invisible(TRUE)
}

# chk_d_epoch_
check_data_epochlength <- function(acc_data) {
  if (get_epochlength(acc_data) != get_setting(acc_data, "epochlength")) {
    stop("Epoch length does not match data interval.")
  }
  if (60L %% get_epochlength(acc_data)) {
    stop("Epochs should be exact divisors of 60.")
  }
  invisible(TRUE)
}

# chk_d_miss_
check_data_missing <- function(acc_data, var) {
  if (anyNA(acc_data[[var]])) {
    stop("Data contains missing observations on ", var, ".")
  }
  invisible(TRUE)
}

# chk_d_names_
check_data_names <- function(acc_data, var) {
  assert_that(has_name(acc_data, "timestamp"),
              msg = "Data missing timestamp column.")
  assert_that(has_name(acc_data, "axis1"),
              msg = "Data missing axis1 column.")
  invisible(TRUE)
}


